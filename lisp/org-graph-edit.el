;;; org-graph-edit.el --- #+begin_graph: editing support ---completion, eldoc, C-u RET, font-lock  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Musa Al-hassy

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Part of org-graph-block.el: what helps while writing a graph, in any Org buffer's graph blocks
;; and in the `org-graph-edit-mode' buffer that C-c ' opens.

;;; Code:

(require 'org-graph-read)
(require 'org-graph-attributes)
(require 'org-graph-smartdiagram)
(require 'org-special-block-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive attribute insertion

(defun org-graph--table-context ()
  "The table at point ↦ what its rows describe, by the directive above it, past any tables between, as
`org--graph-read-scope' reads it: `graphs' under #+graph:, `edges' under #+edge:, `record' under #+record: ---cells,
not keys---, else `nodes': under #+node:, an item, or a #+name: template."
  (save-excursion
    ;; At a row, Org's element at point is that row; we want its table.
    (let ((el (org-element-lineage (org-element-at-point) '(table) t)))
      (while (and (eq (org-element-type el) 'table) (not (org-element-property :name el))
                  (progn (goto-char (org-element-property :begin el))
                         (skip-chars-backward " \t\n")
                         (not (bobp))))
        (setq el (let ((at (org-element-at-point))) (or (org-element-lineage at '(table) t) at))))
      (pcase (and (eq (org-element-type el) 'keyword) (downcase (org-element-property :key el)))
        ("graph" 'graphs) ("edge" 'edges) ("record" 'record) (_ 'nodes)))))

(defun org-graph--pick-color ()
  "Launch a helm color picker; RET selects the color immediately.
Returns a string of the form \"\\\"#rrggbb\\\" # Name\", or nil if the user
aborts — allowing the caller to fall back to `read-string'."
  (require 'helm-color nil t)
  (when (and (fboundp 'helm-colors-init)
             (fboundp 'helm-colors-get-rgb)
             (fboundp 'helm-colors-get-name))
    (let (selected-hex selected-name)
      (helm :sources
            (helm-build-in-buffer-source "Pick a color"
              :init     #'helm-colors-init
              :get-line #'buffer-substring
              :keymap   helm-color-map
              :action   (lambda (candidate)
                          (setq selected-hex  (helm-colors-get-rgb  candidate)
                                selected-name (helm-colors-get-name candidate))))
            :buffer "*helm pick color*")
      (when selected-hex
        (format "\"%s\" # %s" selected-hex selected-name)))))

(defun org-graph--insert-attribute-row (&optional arg)
  "Interactively insert a | key | value | row into the table at point.

Only acts when ARG (prefix argument) is non-nil, point is on an Org
table row, and point is inside a #+begin_graph block.

Filters `org-graph-attributes' to entries valid for the current table
context, prompts for a key via `completing-read', then prompts for a
value.  For color attributes (color, fillcolor, fontcolor, bgcolor),
a helm color picker opens when available — select a color and press RET;
the hex code and name are inserted automatically.  Falls back to
`read-string' when helm is unavailable or the user aborts.
The new row is inserted below the current line and the table is aligned."
  (interactive "P")
  (when (and arg (org-graph--at-key-table-p))
    (let* ((context (org-graph--table-context))
           (valid   (org-graph--attributes-on context))
           (choices (--map (format "%s — %s"
                                   (plist-get it :name)
                                   (plist-get it :description))
                           valid))
           (chosen  (completing-read
                     (format "Attribute (%s): " context)
                     choices nil t))
           (key     (car (s-split " — " chosen)))
           (entry   (--first (equal (plist-get it :name) key) valid))
           (values  (plist-get entry :values))
           (value   (cond
                     (values
                      (completing-read (format "Value for %s: " key) values nil t))
                     ((and (member key '("color" "fillcolor" "fontcolor" "bgcolor"))
                           (fboundp 'helm-colors-init))
                      (or (org-graph--pick-color)
                          (read-string (format "Value for %s: " key))))
                     (t
                      (read-string (format "Value for %s: " key))))))
      (end-of-line)
      (insert (format "\n| %s | %s |" key value))
      (org-table-align))))

(defconst org-graph--return-commands '(org-return org-return-indent org-return-and-maybe-indent)
  "The commands that RET may run in Org, each calling `org-return'; e.g., a config's (local-set-key [return]
\='org-return-indent).  Only these hand C-u to `org-graph--maybe-insert-attribute-row' by `current-prefix-arg'.")

(defun org-graph--maybe-insert-attribute-row (&optional _indent arg _interactive)
  "Advice for `org-return': intercept C-u RET inside graph block tables.

When ARG ---or, when RET runs one of `org-graph--return-commands', which may call `org-return' sans it,
`current-prefix-arg'--- is non-nil and point is on an Org table row inside a #+begin_graph block, run
`org-graph--insert-attribute-row' and return non-nil to suppress the normal `org-return' behaviour."
  (let ((arg (or arg (and (memq this-command org-graph--return-commands) current-prefix-arg))))
    (when (and arg (org-graph--at-key-table-p))
      (org-graph--insert-attribute-row arg)
      t)))

(advice-add 'org-return :before-until #'org-graph--maybe-insert-attribute-row)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completion and eldoc for table keys, and the #+begin_graph line

(defconst org-graph--reserved-key-docs
  '((":id"            . "This node's dot id, so other items can refer to it; also spelled <<ID>> in the item's text.")
    (":edge."         . "An attribute of the arc from the parent; e.g., | :edge.label | calls |, also spelled + calls :: CHILD.")
    (":edge[TARGET]." . "An attribute of this node's arc to TARGET, from a [[TARGET]] link; e.g., | :edge[church].style | dotted |.")
    (":inherit"       . "Inherit the rows of #+name: templates N₁ … Nₖ; this item's own rows win.  Also spelled :N₁:…:Nₖ: after the item.")
    (":children-rank" . "Put this item's immediate children on one rank: same, min, max, source, or sink."))
  "The table keys meaningful to the `graph' block itself ↦ what they do; shown by completion and eldoc.")

(defconst org-graph--header-key-docs
  '((":file"     . "The image file C-c C-c and export write; else graph-HASH.png.")
    (":svg"      . "HTML export embeds an inline <svg>, its tooltips as prose's; with nil, a base64 PNG.")
    (":directed" . "With nil, an undirected graph, whose arcs are --; else a digraph.")
    (":mindmap"  . "With t, a mindmap: undirected, radiating from the first item, by twopi; its nodes coloured by depth.")
    (":wrap"     . "With N, node and arc labels fill to lines of N characters; one broken by \\\\ stays as written.")
    (":notes"    . "With inline, notes ---an item's later paragraphs, #+begin_note blocks--- show in labels; else tooltips.")
    (":unflatten" . "With N, a node's many leaves stagger over N ranks, via Graphviz's unflatten, not one wide row.")
    (":unknown-tags" . "With error, a tag naming no #+name: template is an error; by default, it gets a made-up one.")
    (":C-c_C-c"  . "With dot, C-c C-c shows the dot source, and C-u C-c C-c renders; else the reverse.")
    (":smartdiagram" . "With TYPE, such as constellation, LaTeX's smartdiagram typesets the list, rather than Graphviz.")
    (":dpi"      . "Under :smartdiagram, the dots per inch of the PNG; 300 by default.")
    (":node."    . "A default attribute of every node; e.g., :node.shape box.")
    (":edge."    . "A default attribute of every edge; e.g., :edge.color grey."))
  "The #+begin_graph keys that are not graph attributes ↦ what they do; shown by completion and eldoc.")

(defconst org-graph--header-key-values
  `((":directed" "nil" "t") (":svg" "t" "nil") (":C-c_C-c" "dot" "png")
    (":notes" "tooltip" "inline") (":unknown-tags" "random-templates" "error")
    (":smartdiagram" ,@(mapcar #'car org-graph-smartdiagram-types)))
  "The #+begin_graph keys that are not graph attributes ↦ the values completion offers them.")

(defun org-graph--block-at-point ()
  "The innermost #+begin_graph block element around point, looking past the #+begin_cluster and #+begin_note blocks
within it; else nil."
  (let ((block (org-special-block-at-point)) found)
    (while (and block (not found))
      (pcase (downcase (org-element-property :type block))
        ("graph"   (setq found block))
        ((or "cluster" "note") (setq block (org-element-lineage block '(special-block))))
        (_         (setq block nil))))
    found))

(defconst org-graph--smartdiagram-header-keys '(":smartdiagram" ":file" ":svg" ":dpi" ":C-c_C-c")
  "The #+begin_graph keys that mean something under :smartdiagram; Graphviz's attributes do not.")

(defun org-graph--smartdiagram-p ()
  "Non-nil when the graph around point ---or the one this `org-graph-edit-mode' buffer edits--- has :smartdiagram on
its #+begin_graph line; so completion and eldoc offer smartdiagram's keys, not Graphviz's."
  (let ((check (lambda () (-some--> (org-graph--block-at-point) (org-element-property :parameters it)
                                    (string-match-p ":smartdiagram\\_>" it)))))
    ;; The marker is local to the edit buffer, so we hold it before leaving.
    (-if-let (origin (and (derived-mode-p 'org-graph-edit-mode) (bound-and-true-p org-src--beg-marker)))
        (with-current-buffer (marker-buffer origin)
          (save-excursion (goto-char origin) (funcall check)))
      (funcall check))))

(defun org-graph--in-graph-p ()
  "Non-nil when point is in a graph block's body: in one in an Org buffer, or anywhere in its `org-graph-edit-mode' buffer."
  (or (derived-mode-p 'org-graph-edit-mode) (org-graph--block-at-point)))

(defun org-graph--at-key-table-p ()
  "Non-nil when point is on a graph table of | key | value | rows; not on a #+record: table, whose rows are cells."
  ;; The cheap table check first: asking whether we are in a graph parses.
  (and (org-at-table-p) (org-graph--in-graph-p) (not (eq (org-graph--table-context) 'record))))

(defun org-graph--attributes-on (context)
  "CONTEXT, such as `edges', ↦ the `org-graph-attributes' entries valid on it."
  (--filter (memq context (plist-get it :valid-on)) org-graph-attributes))

(defun org-graph--key-attribute (key)
  "A table KEY ↦ the attribute it sets: \":edge[x].color\" ↦ \"color\", \":node.shape\" ↦ \"shape\", and \"shape\" ↦ \"shape\"."
  (if (string-match "\\`:\\(?:edge\\(?:\\[[^]]*\\]\\)?\\|node\\)\\.\\(.*\\)\\'" key) (match-string 1 key) key))

(defun org-graph--header->table-key (key)
  "A #+begin_graph KEY ↦ the table key setting the same attribute: \":rankdir\" ↦ \"rankdir\", \":node.shape\" unchanged."
  (if (string-match-p "\\`:\\(?:node\\|edge\\)\\." key) key (substring key 1)))

(defun org-graph--attribute (key)
  "A table KEY ↦ the `org-graph-attributes' entry of the attribute it sets; else nil."
  (let ((name (org-graph--key-attribute key)))
    (--first (equal (plist-get it :name) name) org-graph-attributes)))

(defun org-graph--key-doc (key)
  "A table KEY ↦ its one-line description, from `org-graph--reserved-key-docs' or `org-graph-attributes'; else nil.
An attribute's description ends with its default, when Graphviz states one.  In a smartdiagram, the key is
smartdiagram's: a #+graph: row's, from `org-graph-smartdiagram-keys'; an item's, from `org-graph-smartdiagram-item-keys'."
  (if (org-graph--smartdiagram-p)
      (or (cdr (assoc key org-graph-smartdiagram-item-keys))
          (-when-let (entry (org-graph--smartdiagram-key key))
            (concat (plist-get entry :description) (-some->> (plist-get entry :default) (format "  Default: %s.")))))
    (org-graph--graphviz-key-doc key)))

(defun org-graph--graphviz-key-doc (key)
  "A Graphviz graph's table KEY ↦ its one-line description; see `org-graph--key-doc'."
  (or (cdr (assoc key org-graph--reserved-key-docs))
      (-when-let (attr (org-graph--attribute key))
        (concat (plist-get attr :description)
                (-some->> (plist-get attr :default) (format "  Default: %s."))))))

(defun org-graph--header-key-doc (key)
  "A #+begin_graph KEY, such as \":rankdir\" or \":node.shape\", ↦ its one-line description; else nil."
  (or (cdr (assoc key org-graph--header-key-docs))
      (org-graph--graphviz-key-doc (org-graph--header->table-key key))))

(defun org-graph--header-token-at-point ()
  "On a #+begin_graph, or #+begin_cluster, line ↦ (BEG END PREVIOUS): the bounds of the word at point, and the word
before it; else nil."
  (when (save-excursion (beginning-of-line) (looking-at-p "[ \t]*#\\+begin_\\(?:graph\\|cluster\\)\\_>"))
    (let* ((bol (line-beginning-position))
           (beg (save-excursion (skip-chars-backward "^ \t" bol) (point)))
           (end (save-excursion (skip-chars-forward "^ \t" (line-end-position)) (point))))
      (list beg end (save-excursion (goto-char beg) (skip-chars-backward " \t" bol)
                                    (buffer-substring (save-excursion (skip-chars-backward "^ \t" bol) (point)) (point)))))))

(defun org-graph--header-completion ()
  "On a #+begin_graph line, complete a :KEY ---a graph attribute, :node.KEY, :edge.KEY, or the block's own---, or its value."
  (-when-let ((beg end previous) (org-graph--header-token-at-point))
    (cond
     ((eq (char-after beg) ?:)
      (list beg end
            (cond
             ;; A cluster's line takes the attributes of a cluster, and node and arc defaults; none of the block's own keys.
             ((org-graph--cluster-line-p)
              (append (--map (concat ":" (plist-get it :name)) (org-graph--attributes-on 'clusters))
                      (--map (concat ":node." (plist-get it :name)) (org-graph--attributes-on 'nodes))
                      (--map (concat ":edge." (plist-get it :name)) (org-graph--attributes-on 'edges))))
             ((org-graph--smartdiagram-p) org-graph--smartdiagram-header-keys)
             (t
              (append (--remove (s-ends-with-p "." it) (mapcar #'car org-graph--header-key-docs))
                      (--map (concat ":" (plist-get it :name)) (org-graph--attributes-on 'graphs))
                      (--map (concat ":node." (plist-get it :name)) (org-graph--attributes-on 'nodes))
                      (--map (concat ":edge." (plist-get it :name)) (org-graph--attributes-on 'edges)))))
            :annotation-function (lambda (candidate) (-some->> (org-graph--header-key-doc candidate) (concat "  ")))
            :exclusive 'no))
     ((s-starts-with-p ":" previous)
      (-some--> (or (cdr (assoc previous org-graph--header-key-values))
                    (plist-get (org-graph--attribute (org-graph--header->table-key previous)) :values))
        (list beg end it :exclusive 'no))))))

(defun org-graph--cell-at-point ()
  "Point on a table row ↦ (COLUMN BEG END KEY): the cell's 1-based index, its text's bounds, and the row's first cell."
  (when (org-at-table-p)
    (let ((bol (line-beginning-position)) (eol (line-end-position)))
      (list (cl-count ?| (buffer-substring bol (point)))
            (save-excursion (skip-chars-backward "^| \t" bol) (point))
            (save-excursion (skip-chars-forward "^| \t" eol) (point))
            (car (org--graph-parse-row (buffer-substring bol eol)))))))

(defun org-graph--completion-at-point ()
  "Complete a graph table's key, or value, from `org-graph-attributes'; after `:edge.' or `:edge[T].', only edge ones.
On a #+begin_graph line, complete its keys and values instead; in a smartdiagram, smartdiagram's keys."
  (or
   (org-graph--header-completion)
   (-when-let ((column beg end key) (and (org-graph--at-key-table-p) (org-graph--cell-at-point)))
     (if (org-graph--smartdiagram-p)
         (org-graph--smartdiagram-completion column beg end key)
       (pcase column
         (1 (let* ((typed  (buffer-substring beg (point)))
                   (prefix (and (string-match "\\`:edge\\(?:\\[[^]]*\\]\\)?\\." typed) (match-string 0 typed))))
              (list beg end
                    (append (--map (concat prefix (plist-get it :name))
                                   (org-graph--attributes-on (if prefix 'edges (org-graph--table-context))))
                            (unless prefix (mapcar #'car org-graph--reserved-key-docs)))
                    :annotation-function (lambda (candidate) (-some->> (org-graph--key-doc candidate) (concat "  ")))
                    :exclusive 'no)))
         (2 (-some--> (plist-get (org-graph--attribute key) :values)
              (list beg end it :exclusive 'no))))))))

(defun org-graph--smartdiagram-completion (column beg end key)
  "In a smartdiagram's table, at COLUMN between BEG and END, on KEY's row ↦ completion: a #+graph: row's key from
`org-graph-smartdiagram-keys', an item's from `org-graph-smartdiagram-item-keys'; or a value, for keys with few."
  (pcase column
    (1 (-some--> (pcase (org-graph--table-context)
                   ('graphs (--map (plist-get it :name) org-graph-smartdiagram-keys))
                   ('nodes  (mapcar #'car org-graph-smartdiagram-item-keys)))
         (list beg end it
               :annotation-function (lambda (candidate) (-some->> (org-graph--key-doc candidate) (concat "  ")))
               :exclusive 'no)))
    (2 (-some--> (plist-get (org-graph--smartdiagram-key key) :values) (list beg end it :exclusive 'no)))))

(defconst org-graph--cluster-doc
  (concat "A box around the items within: a subgraph of the graph, its title the label, then :KEY V cluster attributes,\n"
          "and :node.KEY and :edge.KEY defaults within it; a table opening the block styles the box.  Clusters nest;\n"
          "an arc to one lands on its first node, clipped at its border.  An untitled cluster is an anonymous box.")
  "What eldoc says of a #+begin_cluster line, beside its signature.")

(defun org-graph--eldoc (callback &rest _)
  "Hand eldoc's CALLBACK the description of the #+begin_graph, or #+begin_cluster, key at point, or of the key on this
graph-table row, or of the built-in tag at point; on #+begin_cluster itself, what a cluster is; else nil."
  (-if-let ((beg end) (org-graph--header-token-at-point))
      (let ((key (buffer-substring beg end)))
        (cond ((and (s-starts-with-p ":" key) (org-graph--header-key-doc key))
               (funcall callback (org-graph--header-key-doc key) :thing key :face 'font-lock-variable-name-face))
              ((string-match-p "\\`#\\+begin_cluster\\'" key)
               (funcall callback org-graph--cluster-doc :thing "#+begin_cluster TITLE :KEY V …"
                        :face 'font-lock-function-name-face))))
    (-if-let (doc (and (org-graph--in-graph-p) (-some->> (org-graph--tag-at-point) org-graph--builtin-tag-doc)))
        (funcall callback doc)
      (-when-let* (((_ _ _ key) (and (org-graph--at-key-table-p) (org-graph--cell-at-point)))
                   (doc (and key (org-graph--key-doc key))))
        (funcall callback doc :thing key :face 'font-lock-variable-name-face)))))

(define-derived-mode org-graph-edit-mode org-mode "Org-Graph"
  "Org mode for a graph block's body, as C-c ' opens it: `completion-at-point' offers attributes, and eldoc explains them.
Those come from `org-graph--setup-org-buffer', as in any Org buffer; here, the whole buffer is the graph."
  (eldoc-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Links show in full

(defun org-graph--reveal-links (limit)
  "Font-lock matcher: un-hide each link up to LIMIT that is in a graph, since both [[TARGET][LABEL]] parts matter there.
Org hides a described link's brackets and target via `invisible'; this always returns nil, so it adds no face itself.
A buffer with no graph block pays only one search per fontification, rather than one parse per link."
  (when (or (derived-mode-p 'org-graph-edit-mode)
            (save-excursion (goto-char (point-min)) (re-search-forward "^[ \t]*#\\+begin_graph\\_>" nil t)))
    (while (re-search-forward org-link-bracket-re limit t)
      ;; Bounds first: parsing, to ask whether we are in a graph, clobbers the match data.
      (let ((beg (match-beginning 0)) (end (match-end 0)))
        (when (save-excursion (goto-char beg) (org-graph--in-graph-p))
          (with-silent-modifications
            (remove-text-properties beg end '(invisible nil)))))))
  nil)

(defconst org-graph--tag-res
  (let ((bullet "^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+"))
    (list (concat bullet ".*?" org--graph-tags-re)
          (concat bullet "\\(?:.*?[ \t]\\)?" org--graph-tag-group-re "[ \t]+::\\(?:[ \t]\\|$\\)")
          (concat "^[ \t]*#\\+\\(?:graph\\|node\\|edge\\|name\\):.*?[ \t]" org--graph-tag-group-re "[ \t]*$")
          (concat "\\]\\][ \t]+" org--graph-tag-group-re "[ \t]*|")))
  "Where a graph's tags sit, each with the tags as group 1: after an item; in its description term; after a
#+graph:, #+node:, #+edge:, or #+name: keyword; and after a #+record: cell's link.")

(defface org-graph-builtin-tag '((t :inherit (font-lock-builtin-face org-tag)))
  "The face of a graph's tag naming a template every graph has, from `org-defgraph-tag', which no #+name: shadows."
  :group 'org-special-block-extras)

(defvar org-graph-tag-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'org-graph-follow-tag)
    (define-key map [mouse-2] #'org-graph-follow-tag)
    (define-key map (kbd "C-c C-o") #'org-graph-follow-tag)
    map)
  "The keys on a graph's tag: each follows it, by `org-graph-follow-tag'.")

(defun org-graph--tag-at-point ()
  "Returns the name of the graph tag at point, sans colons, such as \"state\" amid :start:state:; else nil."
  (save-excursion
    (skip-chars-backward "[:alnum:]_@#%-")
    (and (eq (char-before) ?:)
         (looking-at "\\([[:alnum:]_@#%-]+\\):")
         (match-string-no-properties 1))))

(defun org-graph--template-definition (name)
  "Returns the position of the #+name: NAME line that defines template NAME for the graph at point: the graph's own,
else one outside every graph; else nil.  Since a graph's own templates win, as `org--graph-parse' says."
  (let ((re (format "^[ \t]*#\\+name:[ \t]+%s\\(?:[ \t]\\|$\\)" (regexp-quote name)))
        (bounds (org-graph--graph-bounds))
        (case-fold-search t))
    (save-excursion
      (or (and bounds (progn (goto-char (car bounds)) (re-search-forward re (cdr bounds) t)) (line-beginning-position))
          (progn (goto-char (point-min))
                 (cl-loop while (re-search-forward re nil t)
                          unless (save-excursion (org-graph--graph-bounds))
                          return (line-beginning-position)))))))

(defun org-graph--builtin-tag-doc (name)
  "Returns what the built-in tag NAME is about: its docstring, then its rows; else nil."
  (-when-let (doc (alist-get name org-graph-builtin-tag-docs nil nil #'equal))
    (format ":%s: is a tag every graph has; see `org-defgraph-tag'.\n%s\n  %s" name doc
            (s-join " " (--map (format "%s=%s" (car it) (if (string-match-p "\\`[^ \"]+\\'" (cadr it)) (cadr it)
                                                          (format "%S" (cadr it))))
                               (alist-get name org-graph-builtin-templates nil nil #'equal))))))

(defun org-graph-follow-tag (&optional event)
  "Follow the graph tag at point, or at mouse EVENT: jump to the #+name: line defining its template; else, for a tag
every graph has, show what it is about; else say that it names no template."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event) (mouse-set-point event))
  (-if-let (name (org-graph--tag-at-point))
      (-if-let (definition (org-graph--template-definition name))
          (progn (push-mark) (goto-char definition))
        (display-message-or-buffer
         (or (org-graph--builtin-tag-doc name)
             (format ":%s: names no template, so a graph makes one up, or errs, as its :unknown-tags says" name))
         "*Graph tag*"))
    (user-error "No graph tag at point")))

(defvar org-graph--tags-end nil
  "Where the group of tags that font-lock last matched in a graph ends; it resumes there, past each tag's own match.")

(defun org-graph--tag-face (name)
  "Returns the font-lock face, and properties, of the graph tag NAME: clickable, by `org-graph-tag-map'; and wearing
`org-graph-builtin-tag' when it is one every graph has, which no #+name: shadows."
  (list 'face (if (and (assoc name org-graph-builtin-templates) (not (org-graph--template-definition name)))
                  'org-graph-builtin-tag
                'org-tag)
        'keymap org-graph-tag-map 'mouse-face 'highlight 'follow-link t
        'help-echo (or (-some--> (alist-get name org-graph-builtin-tag-docs nil nil #'equal) (car (s-lines it)))
                       "mouse-1: jump to this template's #+name: line")))

(defun org-graph--match-tags (re limit)
  "Font-lock matcher: the next match of RE up to LIMIT that is in a graph; its group 1 is the tags.
Org gives tags their `org-tag' face only on headlines; in a graph, tags name templates, so they look the part."
  (let (found)
    (while (and (not found) (re-search-forward re limit t))
      ;; Asking whether we are in a graph clobbers the match data, which font-lock reads.
      (when (save-match-data (save-excursion (goto-char (match-beginning 0)) (org-graph--in-graph-p)))
        (setq found t)))
    found))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Directives, and their KEY=VALUE pairs, stand out; a key Graphviz does not know there stays plain

(defface org-graph-directive '((t :inherit font-lock-keyword-face :weight bold))
  "The face of a graph's #+graph:, #+node:, #+edge:, and #+name: keywords."
  :group 'org-special-block-extras)

(defface org-graph-key '((t :inherit font-lock-variable-name-face :weight bold))
  "The face of a KEY=VALUE pair's KEY on a graph's directive line, when the key means something there; see
`org-graph--pair-key-face'.  A key with no face is one Graphviz does not know there: a typo, say.")

(defvar org-graph--directive nil
  "The directive, such as \"edge\", whose line `org-graph--match-directive' last matched.")

(defun org-graph--match-directive (limit)
  "Font-lock matcher: the next #+graph:, #+node:, #+edge:, or #+name: keyword up to LIMIT, in a graph, as group 1;
`org-graph--directive' then names it, for `org-graph--pair-key-face'."
  (let (found)
    (while (and (not found) (re-search-forward "^[ \t]*\\(#\\+\\(graph\\|node\\|edge\\|name\\):\\)" limit t))
      (when (save-match-data (save-excursion (goto-char (match-beginning 0)) (org-graph--in-graph-p)))
        (setq found t org-graph--directive (downcase (match-string 2)))))
    found))

(defun org-graph--pair-key-face ()
  "Returns `org-graph-key' when the KEY just matched, group 1, means something on its `org-graph--directive' line: a
graph's, node's, or arc's Graphviz attribute on #+graph:, #+node:, or #+edge:; any on #+name:, a template's; under
:smartdiagram, smartdiagram's own.  Else nil: an unknown key stays plain, so a typo stands out by not standing out."
  (let ((key (match-string 1)))
    (when (if (org-graph--smartdiagram-p)
              (or (org-graph--smartdiagram-key key) (assoc key org-graph-smartdiagram-item-keys))
            (-when-let (attribute (org-graph--attribute key))
              (or (equal org-graph--directive "name")
                  (memq (intern (concat org-graph--directive "s"))
                        (plist-get attribute :valid-on)))))
      'org-graph-key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nested clusters: Org pairs a block with the first end of its name, so we pair them ourselves
;;
;; Within one graph, #+begin_cluster … #+end_cluster blocks nest; Org's own TAB, and its parser, would end the outer
;; block at the inner one's end.  `org--graph-number-clusters' already reads them aright; here, TAB folds each down to
;; its own end, and each wears a background of its own, so where a cluster begins and ends shows at a glance.

(defcustom org-graph-cluster-backgrounds
  '((light "#eaf2fd" "#fdf0e0" "#e8f6ea" "#f4e9fb")
    (dark  "#1d2a38" "#33291a" "#1c2e21" "#2c2036"))
  "The backgrounds that #+begin_cluster blocks wear in Emacs, in turn, by the frame's background mode; the Nth cluster
of a graph wears the Nth, so a nested cluster differs from its parent."
  :type '(alist :key-type (choice (const light) (const dark)) :value-type (repeat color))
  :group 'org-special-block-extras)

(defun org-graph--cluster-line-p ()
  "Returns whether point is on a #+begin_cluster line."
  (save-excursion (beginning-of-line) (looking-at-p "[ \t]*#\\+begin_cluster\\_>")))

(defun org-graph--graph-bounds ()
  "Returns (BEGIN . END), the starts of the #+begin_graph and #+end_graph lines around point; else nil."
  (save-excursion
    (let ((at (point)) (case-fold-search t))
      (end-of-line)
      (when (re-search-backward "^[ \t]*#\\+begin_graph\\_>" nil t)
        (let ((begin (line-beginning-position)))
          (when (re-search-forward "^[ \t]*#\\+end_graph\\_>" nil t)
            (and (<= at (line-end-position)) (cons begin (line-beginning-position)))))))))

(defun org-graph--clusters (bounds)
  "Returns the (BEGIN END INDEX) of each cluster within BOUNDS, a graph's, in order: BEGIN and END start its #+begin_
and #+end_cluster lines, paired counting the clusters nested within; INDEX counts the clusters before it."
  (save-excursion
    (goto-char (car bounds))
    (let ((case-fold-search t) (index 0) open clusters)
      (while (re-search-forward "^[ \t]*#\\+\\(begin\\|end\\)_cluster\\_>" (cdr bounds) t)
        (if (equal (downcase (match-string 1)) "begin")
            (push (cons (line-beginning-position) (prog1 index (cl-incf index))) open)
          (-when-let ((begin . n) (pop open))
            (push (list begin (line-beginning-position) n) clusters))))
      (sort clusters (lambda (a b) (< (car a) (car b)))))))

(defun org-graph--cluster-at-line ()
  "Returns the (BEGIN END INDEX) of the cluster whose #+begin_ or #+end_cluster line point is on; else nil."
  (-when-let (bounds (org-graph--graph-bounds))
    (let ((line (line-beginning-position)))
      (--first (or (= (car it) line) (= (cadr it) line)) (org-graph--clusters bounds)))))

(defun org-graph--cycle-cluster ()
  "On a #+begin_ or #+end_cluster line of a graph, fold or unfold the cluster down to its own end, nested clusters
counted, as TAB does any block; for `org-cycle-tab-first-hook'."
  (-when-let ((begin end) (org-graph--cluster-at-line))
    (let ((from (save-excursion (goto-char begin) (line-end-position)))
          (to (save-excursion (goto-char end) (line-end-position))))
      (org-fold-region from to (not (org-fold-folded-p from 'block)) 'block)
      (when (invisible-p (max (1- (point)) (point-min))) (goto-char begin))
      t)))

(add-hook 'org-cycle-tab-first-hook #'org-graph--cycle-cluster)

(defun org-graph--cluster-face (index)
  "Returns the face of a graph's INDEXth cluster: its background, from `org-graph-cluster-backgrounds', to the line's end."
  (let ((colours (alist-get (if (eq (frame-parameter nil 'background-mode) 'dark) 'dark 'light)
                            org-graph-cluster-backgrounds)))
    (list :background (nth (mod index (length colours)) colours) :extend t)))

(defvar org-graph--cluster-index 0
  "The index of the cluster `org-graph--match-cluster' last matched.")

(defun org-graph--match-cluster (limit)
  "Font-lock matcher: the next #+begin_cluster line up to LIMIT, in a graph, matched through its own #+end_cluster line,
nesting counted; `org-graph--cluster-index' then holds its index, for `org-graph--cluster-face'."
  (let (found)
    (while (and (not found) (re-search-forward "^[ \t]*#\\+begin_cluster\\_>" limit t))
      (save-match-data
        (-when-let (cluster (save-excursion (beginning-of-line) (org-graph--cluster-at-line)))
          (setq found cluster))))
    (when found
      (-let [(begin end index) found]
        (setq org-graph--cluster-index index)
        (set-match-data (list begin (save-excursion (goto-char end) (min (point-max) (1+ (line-end-position))))))
        (goto-char (save-excursion (goto-char begin) (line-end-position)))
        t))))

(defcustom org-graph-template-backgrounds
  '((light "#fff4c2" "#fde0ea" "#d9f3ee" "#e9e1fb")
    (dark  "#3a3416" "#3a1f29" "#163330" "#2a2340"))
  "The backgrounds that a graph's #+name: templates wear in Emacs, in turn, by the frame's background mode: each
template, its name's line and the table below it, wears its own; apart from `org-graph-cluster-backgrounds', so a
template within a cluster reads apart from it."
  :type '(alist :key-type (choice (const light) (const dark)) :value-type (repeat color))
  :group 'org-special-block-extras)

(defun org-graph--templates (bounds)
  "Returns the (BEGIN END INDEX) of each #+name: template within BOUNDS, a graph's: BEGIN starts its name's line, END
ends the table right below it, else that line; INDEX counts the templates before it."
  (save-excursion
    (goto-char (car bounds))
    (let ((case-fold-search t) (index 0) templates)
      (while (re-search-forward "^[ \t]*#\\+name:" (cdr bounds) t)
        (let ((begin (line-beginning-position)))
          (forward-line 1)
          (while (and (< (point) (cdr bounds)) (looking-at-p "[ \t]*|")) (forward-line 1))
          (push (list begin (1- (point)) index) templates)
          (cl-incf index)))
      (nreverse templates))))

(defun org-graph--template-face (index)
  "Returns the face of a graph's INDEXth template: its background, from `org-graph-template-backgrounds', to the
line's end."
  (let ((colours (alist-get (if (eq (frame-parameter nil 'background-mode) 'dark) 'dark 'light)
                            org-graph-template-backgrounds)))
    (list :background (nth (mod index (length colours)) colours) :extend t)))

(defvar org-graph--template-index 0
  "The index of the template `org-graph--match-template' last matched.")

(defun org-graph--match-template (limit)
  "Font-lock matcher: the next #+name: template up to LIMIT, in a graph, matched from its name's line through the
table below it; `org-graph--template-index' then holds its index, for `org-graph--template-face'."
  (let (found)
    (while (and (not found) (re-search-forward "^[ \t]*#\\+name:" limit t))
      (let ((line (line-beginning-position)))
        (save-match-data
          (-when-let (bounds (org-graph--graph-bounds))
            (setq found (--first (= (car it) line) (org-graph--templates bounds)))))))
    (when found
      (-let [(begin end index) found]
        (setq org-graph--template-index index)
        (set-match-data (list begin (min (point-max) (1+ end))))
        (goto-char (save-excursion (goto-char begin) (line-end-position)))
        t))))

(defun org-graph--extend-to-clusters ()
  "Widen font-lock's region to the clusters, and templates, it cuts through, so each is painted whole; for
`font-lock-extend-region-functions'."
  (defvar font-lock-beg) (defvar font-lock-end)
  (let ((changed nil))
    (-when-let (bounds (save-excursion (goto-char font-lock-beg) (org-graph--graph-bounds)))
      (pcase-dolist (`(,begin ,end ,_) (append (org-graph--clusters bounds) (org-graph--templates bounds)))
        (when (and (< begin font-lock-beg) (> end font-lock-beg))
          (setq font-lock-beg begin changed t))))
    changed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Every Org buffer

(defun org-graph--setup-org-buffer ()
  "In this Org buffer's graph blocks: show links verbatim, paint each cluster's background, and complete and explain
keys, with C-M-i and eldoc."
  (font-lock-add-keywords nil '((org-graph--match-cluster 0 (org-graph--cluster-face org-graph--cluster-index) prepend)) 'append)
  ;; After the clusters', so a template within a cluster wears its own background.
  (font-lock-add-keywords nil '((org-graph--match-template 0 (org-graph--template-face org-graph--template-index) prepend))
                          'append)
  (font-lock-add-keywords nil '((org-graph--match-directive
                                 (1 'org-graph-directive prepend)
                                 ("[ \t]\\([^ \t=:\"]+\\)=" (line-end-position) nil (1 (org-graph--pair-key-face) prepend t))))
                          'append)
  (add-hook 'font-lock-extend-region-functions #'org-graph--extend-to-clusters nil t)
  (font-lock-add-keywords nil (cons '(org-graph--reveal-links)
                                    ;; Each tag of a group, such as :start:state:, alone: its own face, and click.
                                    (--map `(,(lambda (limit) (org-graph--match-tags it limit))
                                             ("\\([[:alnum:]_@#%-]+\\):"
                                              (prog1 (setq org-graph--tags-end (match-end 1))
                                                (goto-char (1+ (match-beginning 1))))
                                              (goto-char org-graph--tags-end)
                                              (1 (org-graph--tag-face (match-string-no-properties 1)) prepend)))
                                           org-graph--tag-res))
                          'append)
  (dolist (prop '(keymap mouse-face follow-link help-echo))
    (add-to-list (make-local-variable 'font-lock-extra-managed-props) prop))
  (add-hook 'completion-at-point-functions #'org-graph--completion-at-point nil t)
  (add-hook 'eldoc-documentation-functions #'org-graph--eldoc nil t))

(add-hook 'org-mode-hook #'org-graph--setup-org-buffer)

(provide 'org-graph-edit)
;;; org-graph-edit.el ends here
