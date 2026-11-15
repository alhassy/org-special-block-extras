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

(require 'org-graph-dot)
(require 'org-graph-attributes)
(require 'org-smartdiagram-block)
(require 'org-special-block-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive attribute insertion

(defun org-graph--table-context ()
  "The table at point ↦ what its rows describe, by the directive above it, past any tables between, as
`org--graph-read-scope' reads it: `graphs' under #+graph:, `edges' under #+edge:, `nodes' under #+node: or a #+name:
template; under an item, `nodes' too, unless its other rows make it a record ---cells, not keys---, then `record';
or it is a container, as :container: or a | shape | cluster | row says, then `containers', which style its box.
The row at point does not count, so a key half typed is still a key."
  (save-excursion
    ;; At a row, Org's element at point is that row; we want its table.
    (let* ((here (org-element-lineage (org-element-at-point) '(table) t))
           (row (line-number-at-pos))
           (el here))
      (while (and (eq (org-element-type el) 'table) (not (org-element-property :name el))
                  (progn (goto-char (org-element-property :begin el))
                         (skip-chars-backward " \t\n")
                         (not (bobp))))
        (setq el (let ((at (org-element-at-point))) (or (org-element-lineage at '(table) t) at))))
      (pcase (and (eq (org-element-type el) 'keyword) (downcase (org-element-property :key el)))
        ("graph" 'graphs) ("edge" 'edges)
        ((guard (and here (not (org-element-property :name here)) (not (eq (org-element-type el) 'keyword))
                     (not (org--graph-attribute-table-p (org-graph--table-lines-sans here row)))))
         'record)
        ;; A container's table styles its box: an item tagged :container:, or, strictly, a | shape | cluster | row.
        ((guard (and here (eq (org-element-type el) 'paragraph)
                     (or (member "container" (cdr (org--graph-split-tags
                                                   (s-collapse-whitespace
                                                    (buffer-substring-no-properties (org-element-property :begin el)
                                                                                    (org-element-property :end el))))))
                         (--any? (equal (org--graph-parse-row it) '("shape" "cluster"))
                                 (org-graph--table-lines-sans here 0)))))
         'containers)
        (_ 'nodes)))))

(defun org-graph--table-lines-sans (table line)
  "Returns TABLE's trimmed row lines, sans the one on buffer LINE."
  (let ((first (line-number-at-pos (org-element-property :post-affiliated table))))
    (--remove (s-blank-str-p it)
              (-map-indexed (lambda (i l) (if (= (+ first i) line) "" (s-trim l)))
                            (s-lines (buffer-substring-no-properties (org-element-property :post-affiliated table)
                                                                     (org-element-property :end table)))))))

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

(defun org-graph--read-attribute (context)
  "Prompts for an attribute valid on CONTEXT, such as `edges', then its value; returns (KEY . VALUE).
A key's description shows beside it; an attribute of few values completes them; a color opens a helm color picker,
when available, else `read-string'."
  (let* ((valid   (org-graph--attributes-on context))
         (chosen  (completing-read (format "Attribute (%s): " context)
                                   (--map (format "%s — %s" (plist-get it :name) (plist-get it :description)) valid)
                                   nil t))
         (key     (car (s-split " — " chosen)))
         (values  (plist-get (--first (equal (plist-get it :name) key) valid) :values)))
    (cons key
          (cond (values (completing-read (format "Value for %s: " key) values nil t))
                ((and (member key '("color" "fillcolor" "fontcolor" "bgcolor")) (fboundp 'helm-colors-init))
                 (or (org-graph--pick-color) (read-string (format "Value for %s: " key))))
                (t (read-string (format "Value for %s: " key)))))))

(defun org-graph--insert-attribute-row (&optional arg)
  "Inserts a | key | value | row, prompting for both, below the table row at point; on a #+graph:, #+node:, #+edge:,
or #+name: line, adds it to the table beneath, else appends KEY=VALUE to the line.

Only acts when ARG, the prefix argument, is non-nil, and point is on a graph's key table or on such a directive line;
see `org-graph--directive-at-point'.  The attributes offered are those valid there, by `org-graph--read-attribute'."
  (interactive "P")
  (when arg
    (cond
     ((org-graph--at-key-table-p)
      (-let [(key . value) (org-graph--read-attribute (org-graph--table-context))]
        (end-of-line)
        (insert (format "\n| %s | %s |" key value))
        (org-table-align)))
     ((org-graph--directive-at-point)
      (-let* ((context (org-graph--directive-at-point))
              ((key . value) (org-graph--read-attribute context))
              (table (save-excursion (forward-line) (and (org-at-table-p) (point)))))
        (if table
            (progn (goto-char table)
                   (goto-char (org-table-end))
                   (skip-chars-backward " \t\n")
                   (insert (format "\n| %s | %s |" key value))
                   (org-table-align))
          (end-of-line)
          (insert " " key "=" (org-graph--pair-value value))))))))

(defun org-graph--pair-value (value)
  "Returns VALUE as a KEY=VALUE pair writes it: bare when a single word, else \"quoted\"; a color picked by
`org-graph--pick-color', \"\\\"#ff0000\\\" # Red\", is its hex alone, #ff0000."
  (let ((value (if (string-match "\\`\"\\(#[[:xdigit:]]+\\)\"" value) (match-string 1 value) value)))
    (if (string-match-p "\\`[^ \t\"]+\\'" value) value
      (format "\"%s\"" (replace-regexp-in-string "\"" "\\\"" value t t)))))

(defun org-graph--directive-at-point ()
  "Returns the attributes' context a #+graph:, #+node:, #+edge:, or #+name: line at point sets: `graphs', `nodes',
`edges', or `templates'; else nil.  A #+name: line counts in a graph, or, outside, when it names a template: it holds
KEY=VALUE pairs, or a | key | value | table follows it; so a named data table keeps C-u RET as Org's."
  (save-excursion
    (beginning-of-line)
    (let ((case-fold-search t))
      (when (looking-at "[ \t]*#\\+\\(graph\\|node\\|edge\\|name\\):\\(.*\\)$")
        (let ((key (downcase (match-string 1))) (value (match-string 2)))
          (when (or (org-graph--in-graph-p)
                    (and (equal key "name")
                         (or (string-match-p "[^ \t=]=" value)
                             (progn (forward-line)
                                    (and (org-at-table-p)
                                         (org--graph-attribute-table-p
                                          (org--graph-table-lines (org-element-at-point))))))))
            (pcase key ("graph" 'graphs) ("node" 'nodes) ("edge" 'edges) ("name" 'templates))))))))

(defconst org-graph--return-commands '(org-return org-return-indent org-return-and-maybe-indent)
  "The commands that RET may run in Org, each calling `org-return'; e.g., a config's (local-set-key [return]
\='org-return-indent).  Only these hand C-u to `org-graph--maybe-insert-attribute-row' by `current-prefix-arg'.")

(defun org-graph--maybe-insert-attribute-row (&optional _indent arg _interactive)
  "Advice for `org-return': intercept C-u RET inside graph block tables.

When ARG ---or, when RET runs one of `org-graph--return-commands', which may call `org-return' sans it,
`current-prefix-arg'--- is non-nil and point is on a graph's tag, run `org-graph--insert-tag'; on a graph's key
table, or on a #+graph:, #+node:, #+edge:, or #+name: line, as `org-graph--directive-at-point' says, run
`org-graph--insert-attribute-row' and return non-nil to suppress the normal `org-return' behaviour."
  (let ((arg (or arg (and (memq this-command org-graph--return-commands) current-prefix-arg))))
    (when arg
      (cond ((org-graph--graph-tag-at-point) (org-graph--insert-tag) t)
            ((or (org-graph--at-key-table-p) (org-graph--directive-at-point))
             (org-graph--insert-attribute-row arg)
             t)))))

(advice-add 'org-return :before-until #'org-graph--maybe-insert-attribute-row)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completion and eldoc for table keys, and the #+begin_graph line

(defconst org-graph--reserved-key-docs
  '((":edge."         . "An attribute of the arc from the parent; e.g., | :edge.label | calls |, also spelled + calls :: CHILD.")
    (":edge[TARGET]." . "An attribute of this node's arc to TARGET, from a [[TARGET]] link; e.g., | :edge[church].style | dotted |."))
  "The table keys meaningful to the `graph' block itself ↦ what they do; shown by completion and eldoc.")

(defconst org-graph--header-key-docs
  '((":file"     . "The image file C-c C-c and export write; else graph-HASH.png.")
    (":wrap"     . "With N, node and arc labels fill to lines of N characters; one broken by \\\\ stays as written.")
    (":notes"    . "With inline, notes ---an item's later paragraphs, #+begin_note blocks--- show in labels; else tooltips.")
    (":unflatten" . "With N, a node's many leaves stagger over N ranks, via Graphviz's unflatten, not one wide row.")
    (":unknown-tags" . "With error, a tag naming no #+name: template is an error; by default, it gets a made-up one.")
    (":direction" . "Which way arcs flow: down, up, right, or left; Graphviz's rankdir TB, BT, LR, or RL.  Default: down.")
    (":theme"    . "With NAME, the graph wears that theme of org-graph-themes; with nil, Graphviz's look.  Default: org-graph-theme.")
    (":C-c_C-c"  . "With dot, C-c C-c shows the dot source, and C-u C-c C-c renders; else the reverse.")
    (":node."    . "A default attribute of every node; e.g., :node.shape box.")
    (":edge."    . "A default attribute of every edge; e.g., :edge.color grey."))
  "The #+begin_graph keys that are not graph attributes ↦ what they do; shown by completion and eldoc.")

(defconst org-graph--header-key-values
  `((":C-c_C-c" "dot" "png")
    (":notes" "tooltip" "inline") (":unknown-tags" "random-templates" "error")
    (":direction" "down" "right" "up" "left")
    (":theme" "nil" ,@(--map (symbol-name (car it)) org-graph-themes)))
  "The #+begin_graph keys that are not graph attributes ↦ the values completion offers them.")

(defconst org-graph--begin-re "^[ \t]*#\\+begin_\\(?:graph\\|smartdiagram\\)\\_>"
  "The first line of a block whose body is a graph's Org: a #+begin_graph, or a #+begin_smartdiagram.")

(defun org-graph--block-at-point ()
  "Returns the innermost #+begin_graph, or #+begin_smartdiagram, block element around point, looking past the
#+begin_note blocks within it; else nil."
  (let ((block (org-special-block-at-point)) found)
    (while (and block (not found))
      (pcase (downcase (org-element-property :type block))
        ((or "graph" "smartdiagram") (setq found block))
        ("note"    (setq block (org-element-lineage block '(special-block))))
        (_         (setq block nil))))
    found))

(defconst org-graph--smartdiagram-header-key-docs
  '((":type"       . "The diagram: circular, flow, descriptive, priority-descriptive, bubble, constellation, …")
    (":file"       . "The name of the images it renders to, images/NAME.{svg,pdf,png}, exactly; see #+begin_image.")
    (":resolution" . "The PNG's pixels per inch; else org-image-block-png-resolution.")
    (":as-image"   . "With t, an image along LaTeX too, rather than typeset in place."))
  "The #+begin_smartdiagram keys ↦ what they do; shown by completion and eldoc.")

(defun org-graph--smartdiagram-p ()
  "Returns non-nil when the block around point ---or the one this `org-graph-edit-mode' buffer edits--- is a
#+begin_smartdiagram; so completion and eldoc offer smartdiagram's keys, not Graphviz's."
  (let ((check (lambda () (-some--> (org-graph--block-at-point)
                            (equal "smartdiagram" (downcase (org-element-property :type it)))))))
    ;; The marker is local to the edit buffer, so we hold it before leaving.
    (-if-let (origin (and (derived-mode-p 'org-graph-edit-mode) (bound-and-true-p org-src--beg-marker)))
        (with-current-buffer (marker-buffer origin)
          (save-excursion (goto-char origin) (funcall check)))
      (funcall check))))

(defun org-graph--in-graph-p ()
  "Non-nil when point is in a graph block's body: in one in an Org buffer, or anywhere in its `org-graph-edit-mode' buffer."
  (or (derived-mode-p 'org-graph-edit-mode) (org-graph--block-at-point)))

(defun org-graph--at-key-table-p ()
  "Non-nil when point is on a graph table of | key | value | rows; not on a record table, whose rows are cells."
  ;; The cheap table check first: asking whether we are in a graph parses.
  (and (org-at-table-p) (org-graph--in-graph-p) (not (eq (org-graph--table-context) 'record))))

(defun org-graph--attributes-on (context)
  "CONTEXT, such as `edges', ↦ the `org-graph-attributes' entries valid on it; `templates' ↦ those of nodes, edges,
or containers, since a #+name: template styles any of them."
  (let ((contexts (if (eq context 'templates) '(nodes edges containers) (list context))))
    (--filter (-intersection contexts (plist-get it :valid-on)) org-graph-attributes)))

(defun org-graph--key-attribute (key)
  "A table KEY ↦ the attribute it sets: \":edge[x].color\" ↦ \"color\", \":node.shape\" ↦ \"shape\", and \"shape\" ↦ \"shape\"."
  (if (string-match "\\`:\\(?:edge\\(?:\\[[^]]*\\]\\)?\\|node\\)\\.\\(.*\\)\\'" key) (match-string 1 key) key))

(defun org-graph--header->table-key (key)
  "A #+begin_graph KEY ↦ the table key setting the same attribute: \":bgcolor\" ↦ \"bgcolor\", \":node.shape\" unchanged."
  (if (string-match-p "\\`:\\(?:node\\|edge\\)\\." key) key (substring key 1)))

(defun org-graph--attribute (key)
  "A table KEY ↦ the `org-graph-attributes' entry of the attribute it sets; else nil."
  (let ((name (org-graph--key-attribute key)))
    (--first (equal (plist-get it :name) name) org-graph-attributes)))

(defun org-graph--key-doc (key)
  "A table KEY ↦ its one-line description, from `org-graph--reserved-key-docs' or `org-graph-attributes'; else nil.
An attribute's description ends with its default, when Graphviz states one.  In a smartdiagram, the key is
smartdiagram's: a #+graph: row's, from `org-smartdiagram-keys'; an item's, from `org-smartdiagram-item-keys'."
  (if (org-graph--smartdiagram-p)
      (or (cdr (assoc key org-smartdiagram-item-keys))
          (-when-let (entry (org-smartdiagram--key key))
            (concat (plist-get entry :description) (-some->> (plist-get entry :default) (format "  Default: %s.")))))
    (org-graph--graphviz-key-doc key)))

(defun org-graph--graphviz-key-doc (key)
  "A Graphviz graph's table KEY ↦ its one-line description; see `org-graph--key-doc'."
  (or (cdr (assoc key org-graph--reserved-key-docs))
      (-when-let (attr (org-graph--attribute key))
        (concat (plist-get attr :description)
                (-some->> (plist-get attr :default) (format "  Default: %s."))))))

(defun org-graph--header-key-doc (key)
  "A #+begin_graph KEY, such as \":bgcolor\" or \":node.shape\", ↦ its one-line description; else nil.
On a #+begin_smartdiagram line, its own keys' only."
  (if (org-graph--smartdiagram-p) (cdr (assoc key org-graph--smartdiagram-header-key-docs))
    (or (cdr (assoc key org-graph--header-key-docs))
        (org-graph--graphviz-key-doc (org-graph--header->table-key key)))))

(defun org-graph--header-token-at-point ()
  "On a #+begin_graph line ↦ (BEG END PREVIOUS): the bounds of the word at point, and the word before it; else nil."
  (when (save-excursion (beginning-of-line) (looking-at-p (substring org-graph--begin-re 1)))
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
             ((org-graph--smartdiagram-p) (mapcar #'car org-graph--smartdiagram-header-key-docs))
             (t
              (append (--remove (s-ends-with-p "." it) (mapcar #'car org-graph--header-key-docs))
                      ;; The header spells rankdir as :direction.
                      (--map (concat ":" (plist-get it :name))
                             (--remove (equal "rankdir" (plist-get it :name)) (org-graph--attributes-on 'graphs)))
                      (--map (concat ":node." (plist-get it :name)) (org-graph--attributes-on 'nodes))
                      (--map (concat ":edge." (plist-get it :name)) (org-graph--attributes-on 'edges)))))
            :annotation-function (lambda (candidate) (-some->> (org-graph--header-key-doc candidate) (concat "  ")))
            :exclusive 'no))
     ((s-starts-with-p ":" previous)
      (-some--> (or (and (org-graph--smartdiagram-p) (equal previous ":type") (mapcar #'car org-smartdiagram-types))
                    (cdr (assoc previous org-graph--header-key-values))
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
`org-smartdiagram-keys', an item's from `org-smartdiagram-item-keys'; or a value, for keys with few."
  (pcase column
    (1 (-some--> (pcase (org-graph--table-context)
                   ('graphs (--map (plist-get it :name) org-smartdiagram-keys))
                   ('nodes  (mapcar #'car org-smartdiagram-item-keys)))
         (list beg end it
               :annotation-function (lambda (candidate) (-some->> (org-graph--key-doc candidate) (concat "  ")))
               :exclusive 'no)))
    (2 (-some--> (plist-get (org-smartdiagram--key key) :values) (list beg end it :exclusive 'no)))))

(defun org-graph--eldoc (callback &rest _)
  "Hand eldoc's CALLBACK the description of the #+begin_graph key at point, of the key on this graph-table row, of the
built-in tag at point, or of the KEY=VALUE pair at point on a #+graph:, #+node:, #+edge:, or #+name: line ---elsewhere
on such a line, the hint that C-u RET adds an attribute; else nil."
  (-if-let ((beg end) (org-graph--header-token-at-point))
      (let ((key (buffer-substring beg end)))
        (when (and (s-starts-with-p ":" key) (org-graph--header-key-doc key))
          (funcall callback (org-graph--header-key-doc key) :thing key :face 'font-lock-variable-name-face)))
    (-if-let (tag (org-graph--graph-tag-at-point))
        (funcall callback (concat (-some--> (org-graph--builtin-tag-doc tag) (concat it "\n"))
                                  (format "C-u RET inserts another tag beside :%s:, from this graph's, this buffer's, \
or every graph's templates." tag)))
      (-if-let (context (org-graph--directive-at-point))
          (-if-let* ((key (org-graph--pair-key-at-point)) (doc (org-graph--key-doc key)))
              (funcall callback doc :thing key :face 'font-lock-variable-name-face)
            (funcall callback (org-graph--directive-hint context)))
        (-when-let* (((_ _ _ key) (and (org-graph--at-key-table-p) (org-graph--cell-at-point)))
                     (doc (and key (org-graph--key-doc key))))
          (funcall callback doc :thing key :face 'font-lock-variable-name-face))))))

(defun org-graph--directive-hint (context)
  "Returns eldoc's hint on a directive line of CONTEXT, such as `edges': C-u RET picks from its Graphviz attributes."
  (format "C-u RET adds an attribute: pick from all %d Graphviz attributes of %s, each described%s."
          (length (org-graph--attributes-on context))
          (if (eq context 'templates) "nodes, edges, and containers" context)
          (if (org-graph--smartdiagram-p) "; but a smartdiagram takes few of them" "")))

(defun org-graph--pair-key-at-point ()
  "Returns the KEY of the KEY=VALUE pair at point, on a directive line; else nil."
  (save-excursion
    (let ((bol (line-beginning-position)) (point (point)))
      (skip-chars-backward "^ \t" bol)
      (when (and (looking-at "\\([^ \t=:\"]+\\)=\\(?:\"\\(?:[^\"\\\\]\\|\\\\.\\)*\"\\|[^ \t]*\\)")
                 (<= point (match-end 0)))
        (match-string-no-properties 1)))))

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
            (save-excursion (goto-char (point-min)) (re-search-forward org-graph--begin-re nil t)))
    (while (re-search-forward org-link-bracket-re limit t)
      ;; Bounds first: parsing, to ask whether we are in a graph, clobbers the match data.
      (let ((beg (match-beginning 0)) (end (match-end 0)))
        (when (save-excursion (goto-char beg) (org-graph--in-graph-p))
          (with-silent-modifications
            (remove-text-properties beg end '(invisible nil)))))))
  nil)

(defconst org-graph--tag-res
  (let ((bullet "^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+"))
    (list (concat bullet "\\(?:.*?[ \t]\\)?" org--graph-tag-group-re "[ \t]*$")
          (concat bullet "\\(?:.*?[ \t]\\)?" org--graph-tag-group-re "[ \t]+::\\(?:[ \t]\\|$\\)")
          (concat "^[ \t]*#\\+\\(?:graph\\|node\\|edge\\|name\\):.*?[ \t]" org--graph-tag-group-re "[ \t]*$")
          (concat "\\]\\][ \t]+" org--graph-tag-group-re "[ \t]*|")))
  "Where a graph's tags sit, each with the tags as group 1: after an item; in its description term; after a
#+graph:, #+node:, #+edge:, or #+name: keyword; and after a record cell's link.")

(defface org-graph-builtin-tag '((t :inherit (font-lock-builtin-face org-tag) :box (:line-width (1 . -1))))
  "The face of a graph's tag naming a template every graph has, from `org-defgraph-tag', which no #+name: shadows."
  :group 'org-special-block-extras)

(defun org-graph--tag-color-face (name)
  "Returns the face of the graph tag, or template, NAME: a box in a color of NAME's own, as org-modern draws tags.
The color comes from NAME's very spelling, so every :hot: matches its #+name: hot line, whereas a typo, :hott:, or
#+name: hott, stands out in a color of its own: colors as type checking."
  (require 'color)
  (let* ((hue (/ (mod (string-to-number (substring (md5 name) 0 6) 16) 360) 360.0))
         (dark (eq (frame-parameter nil 'background-mode) 'dark))
         (hex (lambda (lightness) (apply #'color-rgb-to-hex (append (color-hsl-to-rgb hue 0.65 lightness) '(2)))))
         (fg (funcall hex (if dark 0.75 0.35))))
    `(:foreground ,fg :background ,(funcall hex (if dark 0.2 0.92)) :box (:line-width (1 . -1) :color ,fg))))

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
  "Returns what the built-in tag NAME is about: its docstring, then its rows; else nil.  A :size_N: is one too."
  (-when-let (doc (or (alist-get name org-graph-builtin-tag-docs nil nil #'equal)
                      (and (org--graph-pattern-template name)
                           "Writes a node's, or an arc's, label at the size its name says, in points.")))
    (format ":%s: is a tag every graph has; see `org-defgraph-tag'.\n%s\n  %s" name doc
            (s-join " " (--map (format "%s=%s" (car it) (if (string-match-p "\\`[^ \"]+\\'" (cadr it)) (cadr it)
                                                          (format "%S" (cadr it))))
                               (or (alist-get name org-graph-builtin-templates nil nil #'equal)
                                   (org--graph-pattern-template name)))))))

(defun org-graph--unknown-tag-doc (name)
  "Returns what the tag NAME means when no template of that name exists: one made up for it."
  (format ":%s: names no #+name: template, nor one every graph has; so the graph makes one up: the Nth unknown tag
gets the Nth color, shape, and font, and every :%s: of the graph alike.

So we may sketch first and name later: tag what belongs together, see the groups at once, and a legend,
- :%s: :: MEANING, explains them; define #+name: %s only once the look matters.

Was it a typo?  Its color here differs from the tag meant, as each name has its own.  To make an unknown tag an
error instead, give the block `:unknown-tags error'." name name name name))

(defun org-graph-follow-tag (&optional event)
  "Follow the graph tag at point, or at mouse EVENT: jump to the #+name: line defining its template; else, for a tag
every graph has, show what it is about; else say that it names no template."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event) (mouse-set-point event))
  (-if-let (name (org-graph--tag-at-point))
      (-if-let (definition (org-graph--template-definition name))
          (progn (push-mark) (goto-char definition))
        (display-message-or-buffer (or (org-graph--builtin-tag-doc name) (org-graph--unknown-tag-doc name))
                                   "*Graph tag*"))
    (user-error "No graph tag at point")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C-u RET on a tag: insert another

(defun org-graph--graph-tag-at-point ()
  "Returns the name of the tag at point when it is a graph's: on an item, a description term, a directive line, or a
record cell, as `org-graph--tag-res' place them, in a graph; else nil."
  (-when-let (name (org-graph--tag-at-point))
    (and (org-graph--in-graph-p)
         (save-excursion (beginning-of-line)
                         (--any? (save-excursion (re-search-forward it (line-end-position) t)) org-graph--tag-res))
         name)))

(defun org-graph--rows-summary (rows)
  "Returns ROWS as a #+name: line's KEY=VALUE pairs, such as \"color=red style=\\\"bold,dashed\\\"\"."
  (s-join " " (--map (format "%s=%s" (car it) (org-graph--pair-value (format "%s" (cadr it)))) rows)))

(defun org-graph--local-templates ()
  "Returns the #+name: templates of the graph at point, as (NAME . SUMMARY): its line's pairs, else its table's rows."
  (-when-let (bounds (org-graph--graph-bounds))
    (save-excursion
      (goto-char (car bounds))
      (let ((case-fold-search t) templates)
        (while (re-search-forward "^[ \t]*#\\+name:[ \t]+\\([^ \t\n]+\\)\\(.*\\)$" (cdr bounds) t)
          (let ((name (match-string-no-properties 1)) (rest (s-trim (match-string-no-properties 2))) rows)
            (save-excursion
              (forward-line 1)
              (while (looking-at "[ \t]*\\(|.*\\)$")
                (-some--> (org--graph-parse-row (match-string 1)) (push it rows))
                (forward-line 1)))
            (push (cons name (s-trim (concat rest " " (org-graph--rows-summary (nreverse rows))))) templates)))
        (nreverse templates)))))

(defun org-graph--tag-candidates ()
  "Returns every tag the graph at point may use, as (NAME . WHENCE): its own #+name: templates, then the buffer's, then
those every graph has, by `org-defgraph-tag'; a nearer definition shadows a farther one of the same name."
  (let ((buffer (unless (derived-mode-p 'org-graph-edit-mode) (org--graph-buffer-templates))) candidates)
    (dolist (it (org-graph--local-templates))
      (push (cons (car it) (format "this graph's: %s" (cdr it))) candidates))
    (when buffer
      (maphash (lambda (name rows) (push (cons name (format "this buffer's: %s" (org-graph--rows-summary rows)))
                                         candidates))
               buffer))
    (dolist (it org-graph-builtin-templates)
      (push (cons (car it) (format "every graph's: %s"
                                   (car (s-lines (alist-get (car it) org-graph-builtin-tag-docs "" nil #'equal)))))
            candidates))
    (cl-remove-duplicates (nreverse candidates) :key #'car :test #'equal :from-end t)))

(defun org-graph--insert-tag ()
  "Prompts for a tag the graph may use, by `org-graph--tag-candidates', and inserts it right after the tag at point:
:a: becomes :a:new:.  Tags already in the group are not offered."
  (let* ((group (save-excursion
                  (skip-chars-backward "[:alnum:]_@#%:-")
                  (and (looking-at "\\(:[[:alnum:]_@#%:-]+:\\)") (s-split ":" (match-string 1) t))))
         (choices (--map (format "%s — %s" (car it) (cdr it))
                         (--remove (member (car it) group) (org-graph--tag-candidates))))
         (name (car (s-split " — " (completing-read "Tag: " choices nil t)))))
    (skip-chars-forward "[:alnum:]_@#%-")
    (forward-char 1)
    (insert name ":")))

(defvar org-graph--tags-end nil
  "Where the group of tags that font-lock last matched in a graph ends; it resumes there, past each tag's own match.")

(defun org-graph--tag-face (name)
  "Returns the font-lock face, and properties, of the graph tag NAME: clickable, by `org-graph-tag-map'; wearing
`org-graph-builtin-tag' when it is one every graph has, which no #+name: shadows; else a color of its own, by
`org-graph--tag-color-face'."
  (list 'face (if (and (or (assoc name org-graph-builtin-templates) (org--graph-pattern-template name))
                       (not (org-graph--template-definition name)))
                  'org-graph-builtin-tag
                (list (org-graph--tag-color-face name) 'org-tag))
        'keymap org-graph-tag-map 'mouse-face 'highlight 'follow-link t
        'help-echo (or (-some--> (org-graph--builtin-tag-doc name) (cadr (s-lines it)))
                       "mouse-1: jump to this template's #+name: line, or say what an undefined tag means")))

(defun org-graph--match-template-name (limit)
  "Font-lock matcher: the next #+name: line's name up to LIMIT, in a graph, as group 1; it wears its tags' color."
  (let (found)
    (while (and (not found) (re-search-forward "^[ \t]*#\\+name:[ \t]+\\([^ \t\n]+\\)" limit t))
      (when (save-match-data (save-excursion (goto-char (match-beginning 0)) (org-graph--in-graph-p)))
        (setq found t)))
    found))

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
  "The face of a graph's #+graph:, #+node:, #+edge:, and #+name: keywords; and of a #+name: template's outside graphs."
  :group 'org-special-block-extras)

(defface org-graph-key '((t :inherit font-lock-variable-name-face :weight bold))
  "The face of a KEY=VALUE pair's KEY on a graph's directive line, when the key means something there; see
`org-graph--pair-key-face'.  A key with no face is one Graphviz does not know there: a typo, say.")

(defvar org-graph--directive nil
  "The directive, such as \"edge\", whose line `org-graph--match-directive' last matched.")

(defun org-graph--match-directive (limit)
  "Font-lock matcher: the next #+graph:, #+node:, #+edge:, or #+name: keyword up to LIMIT, in a graph, as group 1;
or a #+name: template's outside one, as `org-graph--directive-at-point' says.  `org-graph--directive' then names it,
for `org-graph--pair-key-face'."
  (let (found)
    (while (and (not found) (re-search-forward "^[ \t]*\\(#\\+\\(graph\\|node\\|edge\\|name\\):\\)" limit t))
      (when (save-match-data (save-excursion (goto-char (match-beginning 0)) (org-graph--directive-at-point)))
        (setq found t org-graph--directive (downcase (match-string 2)))))
    found))

(defun org-graph--pair-key-face ()
  "Returns `org-graph-key' when the KEY just matched, group 1, means something on its `org-graph--directive' line: a
graph's, node's, or arc's Graphviz attribute on #+graph:, #+node:, or #+edge:; any on #+name:, a template's; under
 a #+begin_smartdiagram, smartdiagram's own.  Else nil: an unknown key stays plain, so a typo stands out by not standing out."
  (let ((key (match-string 1)))
    (when (if (org-graph--smartdiagram-p)
              (or (org-smartdiagram--key key) (assoc key org-smartdiagram-item-keys))
            (-when-let (attribute (org-graph--attribute key))
              (or (equal org-graph--directive "name")
                  (memq (intern (concat org-graph--directive "s"))
                        (plist-get attribute :valid-on)))))
      'org-graph-key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Templates wear backgrounds, so where each begins and ends shows at a glance

(defun org-graph--graph-bounds ()
  "Returns (BEGIN . END), the starts of the #+begin_graph and #+end_graph lines around point; else nil."
  (save-excursion
    (let ((at (point)) (case-fold-search t))
      (end-of-line)
      (when (re-search-backward org-graph--begin-re nil t)
        (let ((begin (line-beginning-position)))
          (when (re-search-forward "^[ \t]*#\\+end_\\(?:graph\\|smartdiagram\\)\\_>" nil t)
            (and (<= at (line-end-position)) (cons begin (line-beginning-position)))))))))

(defcustom org-graph-template-backgrounds
  '((light "#fff4c2" "#fde0ea" "#d9f3ee" "#e9e1fb")
    (dark  "#3a3416" "#3a1f29" "#163330" "#2a2340"))
  "The backgrounds that a graph's #+name: templates wear in Emacs, in turn, by the frame's background mode: each
template, its name's line and the table below it, wears its own."
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
  (let ((colors (alist-get (if (eq (frame-parameter nil 'background-mode) 'dark) 'dark 'light)
                            org-graph-template-backgrounds)))
    (list :background (nth (mod index (length colors)) colors) :extend t)))

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

(defun org-graph--extend-to-templates ()
  "Widen font-lock's region to the templates it cuts through, so each is painted whole; for
`font-lock-extend-region-functions'."
  (defvar font-lock-beg) (defvar font-lock-end)
  (let ((changed nil))
    (-when-let (bounds (save-excursion (goto-char font-lock-beg) (org-graph--graph-bounds)))
      (pcase-dolist (`(,begin ,end ,_) (org-graph--templates bounds))
        (when (and (< begin font-lock-beg) (> end font-lock-beg))
          (setq font-lock-beg begin changed t))))
    changed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Every Org buffer

(defun org-graph--setup-org-buffer ()
  "In this Org buffer's graph blocks: show links verbatim, paint each template's background, make tags clickable, and
complete and explain keys, with C-M-i and eldoc."
  (font-lock-add-keywords nil '((org-graph--match-template 0 (org-graph--template-face org-graph--template-index) prepend))
                          'append)
  (font-lock-add-keywords nil '((org-graph--match-directive
                                 (1 'org-graph-directive prepend)
                                 ("[ \t]\\([^ \t=:\"]+\\)=" (line-end-position) nil (1 (org-graph--pair-key-face) prepend t))))
                          'append)
  (add-hook 'font-lock-extend-region-functions #'org-graph--extend-to-templates nil t)
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
  (font-lock-add-keywords nil '((org-graph--match-template-name
                                 (1 (list (org-graph--tag-color-face (match-string-no-properties 1))) prepend)))
                          'append)
  (dolist (prop '(keymap mouse-face follow-link help-echo))
    (add-to-list (make-local-variable 'font-lock-extra-managed-props) prop))
  (add-hook 'completion-at-point-functions #'org-graph--completion-at-point nil t)
  (add-hook 'eldoc-documentation-functions #'org-graph--eldoc nil t))

(add-hook 'org-mode-hook #'org-graph--setup-org-buffer)

(provide 'org-graph-edit)
;;; org-graph-edit.el ends here
