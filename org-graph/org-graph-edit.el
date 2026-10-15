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

(defun org-graph--maybe-insert-attribute-row (&optional _indent arg _interactive)
  "Advice for `org-return': intercept C-u RET inside graph block tables.

When ARG is non-nil and point is on an Org table row inside a
#+begin_graph block, run `org-graph--insert-attribute-row' and return
non-nil to suppress the normal `org-return' behaviour."
  (when (and arg (org-graph--at-key-table-p))
    (org-graph--insert-attribute-row arg)
    t))

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
    (":svg"      . "With t, HTML export embeds an inline <svg>, keeping tooltips; else a base64 PNG.")
    (":directed" . "With nil, an undirected graph, whose arcs are --; else a digraph.")
    (":wrap"     . "With N, node and arc labels fill to lines of N characters; one broken by \\\\ stays as written.")
    (":C-c_C-c"  . "With dot, C-c C-c shows the dot source, and C-u C-c C-c renders; else the reverse.")
    (":node."    . "A default attribute of every node; e.g., :node.shape box.")
    (":edge."    . "A default attribute of every edge; e.g., :edge.color grey."))
  "The #+begin_graph keys that are not graph attributes ↦ what they do; shown by completion and eldoc.")

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
An attribute's description ends with its default, when Graphviz states one."
  (or (cdr (assoc key org-graph--reserved-key-docs))
      (-when-let (attr (org-graph--attribute key))
        (concat (plist-get attr :description)
                (-some->> (plist-get attr :default) (format "  Default: %s."))))))

(defun org-graph--header-key-doc (key)
  "A #+begin_graph KEY, such as \":rankdir\" or \":node.shape\", ↦ its one-line description; else nil."
  (or (cdr (assoc key org-graph--header-key-docs))
      (org-graph--key-doc (org-graph--header->table-key key))))

(defun org-graph--header-token-at-point ()
  "On a #+begin_graph line ↦ (BEG END PREVIOUS): the bounds of the word at point, and the word before it; else nil."
  (when (save-excursion (beginning-of-line) (looking-at-p "[ \t]*#\\+begin_graph\\_>"))
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
            (append (--remove (s-ends-with-p "." it) (mapcar #'car org-graph--header-key-docs))
                    (--map (concat ":" (plist-get it :name)) (org-graph--attributes-on 'graphs))
                    (--map (concat ":node." (plist-get it :name)) (org-graph--attributes-on 'nodes))
                    (--map (concat ":edge." (plist-get it :name)) (org-graph--attributes-on 'edges)))
            :annotation-function (lambda (candidate) (-some->> (org-graph--header-key-doc candidate) (concat "  ")))
            :exclusive 'no))
     ((s-starts-with-p ":" previous)
      (-some--> (plist-get (org-graph--attribute (org-graph--header->table-key previous)) :values)
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
On a #+begin_graph line, complete its keys and values instead."
  (or
   (org-graph--header-completion)
   (-when-let ((column beg end key) (and (org-graph--at-key-table-p) (org-graph--cell-at-point)))
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
            (list beg end it :exclusive 'no)))))))

(defun org-graph--eldoc (callback &rest _)
  "Hand eldoc's CALLBACK the description of the #+begin_graph key at point, or of the key on this graph-table row; else nil."
  (-if-let ((beg end) (org-graph--header-token-at-point))
      (let ((key (buffer-substring beg end)))
        (-some--> (and (s-starts-with-p ":" key) (org-graph--header-key-doc key))
          (funcall callback it :thing key :face 'font-lock-variable-name-face)))
    (-when-let* (((_ _ _ key) (and (org-graph--at-key-table-p) (org-graph--cell-at-point)))
                 (doc (and key (org-graph--key-doc key))))
      (funcall callback doc :thing key :face 'font-lock-variable-name-face))))

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
;;; Every Org buffer

(defun org-graph--setup-org-buffer ()
  "In this Org buffer's graph blocks: show links verbatim, and complete and explain keys, with C-M-i and eldoc."
  (font-lock-add-keywords nil (cons '(org-graph--reveal-links)
                                    (--map `(,(lambda (limit) (org-graph--match-tags it limit)) 1 'org-tag prepend)
                                           org-graph--tag-res))
                          'append)
  (add-hook 'completion-at-point-functions #'org-graph--completion-at-point nil t)
  (add-hook 'eldoc-documentation-functions #'org-graph--eldoc nil t))

(add-hook 'org-mode-hook #'org-graph--setup-org-buffer)

(provide 'org-graph-edit)
;;; org-graph-edit.el ends here
