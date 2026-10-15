;;; org-graph-read.el --- #+begin_graph: its body ⟶ plain data, via org-element  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Musa Al-hassy

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Part of org-graph-block.el: shared parsing helpers, then reading a graph's body, and its header line,
;; into plain data, so nothing downstream needs a buffer.

;;; Code:

(require 's)
(require 'dash)
(require 'cl-lib)
(require 'org)
(require 'org-defblock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing helpers

(defconst org--graph-tag-group-re "\\(:\\(?:[[:alnum:]_@#%-]+:\\)+\\)"
  "`:t₁:t₂:' tags, as Org spells them on a headline ---and, as template names, kebab-case too: `:killed-by:'--- as group 1.")

(defconst org--graph-tags-re (concat "[ \t]+" org--graph-tag-group-re "[ \t]*$")
  "Trailing `:t₁:t₂:' tags, after some text; group 1 is the tags.")

(defun org--graph-parse-row (line)
  "LINE ↦ its trimmed non-empty cells, e.g. \"| shape | box |\" ↦ (\"shape\" \"box\"); nil for separator rows.
A `\\\\' in a value breaks its line, as in an item: \"| label | a \\\\ b |\" ↦ (\"label\" \"a\\nb\")."
  (let ((cols (-remove #'s-blank-str-p (-map #'s-trim (s-split "|" line)))))
    (unless (or (null cols) (s-matches-p "^[-+]+$" (car cols)))
      (cons (car cols) (--map (if (s-starts-with-p "<" it) it
                                (replace-regexp-in-string "[ \t]*\\\\\\\\[ \t]*" "\\n" it t t))
                              (cdr cols))))))

(defun org--graph-dot-id (name)
  "Return NAME as a valid dot identifier or value, quoting if necessary.
Already-quoted strings, HTML-like labels <…>, pure identifiers (letters, digits,
underscore), and bare numbers are returned as-is.  Everything else is wrapped in
double quotes with any interior double-quotes escaped."
  (cond
   ((and (s-starts-with-p "\"" name) (s-ends-with-p "\"" name) (> (length name) 1)) name)
   ((and (s-starts-with-p "<" name) (s-ends-with-p ">" name)) name)
   ((s-matches-p "^[a-zA-Z_][a-zA-Z_0-9]*$" name) name)
   ((s-matches-p "^-?[0-9]+\\.?[0-9]*$" name) name)
   (t (format "\"%s\"" (s-replace "\"" "\\\"" name)))))

(defun org--graph-dot-port (port)
  "PORT ↦ a record port name, spaces as underscores, e.g. \"mid dle\" ↦ \"mid_dle\".
The same string is then safe both inside a record label's <port> and in an edge's node:port."
  (s-replace " " "_" port))

(defun org--graph-unquote (s)
  "S sans one pair of surrounding double quotes, e.g. \"\\\"Org mode\\\"\" ↦ \"Org mode\"; so both spellings name one node."
  (if (string-match "\\`\"\\(.*\\)\"\\'" s) (match-string 1 s) s))

(defun org--graph-attr-pairs (rows)
  "ROWS ↦ \"k₁=v₁ k₂=v₂ …\", each value quoted as needed by `org--graph-dot-id'."
  (s-join " " (--map (format "%s=%s" (car it) (org--graph-dot-id (cadr it))) rows)))

(defun org--graph-attrs->dot (rows)
  "ROWS ↦ \" [ k₁=v₁ … ]\", ready to follow a dot id or keyword; \"\" when ROWS is empty."
  (if rows (format " [ %s ]" (org--graph-attr-pairs rows)) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reading: a graph's body ⟶ plain data, via `org-element'
;;
;; Org parses the body once; we read its elements into plain data, so nothing after this section needs a buffer:
;;   SCOPE ≈ (:graph-rows :node-rows :edge-rows :ranks :attr-rows :record :notes :items :kind)
;;   ITEM  ≈ (:text TEXT :pre SCOPE :children ITEMS :kind KIND :line LINE)
;; An item's TEXT is its raw first paragraph, bullet aside; its SCOPE, its directives and tables; its ITEMS, its
;; sub-items and #+begin_cluster blocks; its KIND, its sub-list's type: `ordered', `unordered', or `descriptive';
;; and its LINE, where it starts in the body, so a click on its node in the rendered image leads back to it.

(defvar org--graph-anonymous-clusters 0
  "How many untitled #+begin_cluster blocks the graph being read has had so far; each is named by its count.")

(defun org--graph-keyword-tags (value)
  "A #+graph:, #+node:, #+edge:, or #+name: keyword's VALUE ↦ its tags, as an :inherit row; else nil.
  \":wife:base:\"  ↦  (\":inherit\" \"wife base\")"
  (when (string-match (concat "\\`[ \t]*" org--graph-tag-group-re "[ \t]*\\'") value)
    (list ":inherit" (s-join " " (s-split ":" (match-string 1 value) t)))))

(defun org--graph-add-template (templates name rows)
  "(TEMPLATES, NAME, ROWS) ↦ (template . ID), having appended ROWS to template ID in TEMPLATES; from a `#+name: NAME'.
  \"wife :base:\"  ↦  (template . \"wife\"), template wife now inheriting template base"
  (-let [(_ id tags) (s-match "\\`[ \t]*\\([^ \t]*\\)\\(.*\\)\\'" name)]
    (puthash id (append (gethash id templates) (-some--> (org--graph-keyword-tags tags) (list it)) rows) templates)
    (cons 'template id)))

(defun org--graph-table-lines (table)
  "A TABLE element, in the current buffer, ↦ its rows' lines, trimmed; hlines included, e.g. (\"| a | b |\" \"|---|\")."
  (--filter (s-starts-with-p "|" it)
            (-map #'s-trim (s-lines (buffer-substring-no-properties (org-element-property :post-affiliated table)
                                                                    (org-element-property :end table))))))

(defun org--graph-table-rows (table)
  "A TABLE element, in the current buffer, ↦ its rows as `org--graph-parse-row' reads them, sans hlines."
  (-keep #'org--graph-parse-row (org--graph-table-lines table)))

(defun org--graph-elements (elements)
  "ELEMENTS with each section or drawer replaced by its contents, recursively; so a drawer, whatever its name, folds."
  (--mapcat (if (memq (org-element-type it) '(section drawer))
                (org--graph-elements (org-element-contents it))
              (list it))
            elements))

(defun org--graph-read-scope (elements templates)
  "Sibling ELEMENTS ↦ their SCOPE; #+name: tables, wherever they sit, go to TEMPLATES rather than to the scope.

A table belongs to the directive above it, past any tables between: #+graph:, #+node:, #+edge:, #+record:, or #+name:
---Org's own affiliated keyword---; else, it holds the item's own | key | value | rows.  #+rank: lines are id groups."
  (let (rows ranks notes items kind owner)
    (dolist (el (org--graph-elements elements))
      (pcase (org-element-type el)
        ('keyword
         (let ((key (intern (downcase (org-element-property :key el)))) (value (org-element-property :value el)))
           (setq owner nil)
           (pcase key
             ((or 'graph 'node 'edge)
              (setq owner key)
              (-some--> (org--graph-keyword-tags value) (push it (alist-get key rows))))
             ('record (setq owner 'record))
             ('rank (unless (s-blank-str-p value) (push (s-trim value) ranks)))
             ('name (setq owner (org--graph-add-template templates value nil))))))
        ('table
         (-some--> (org-element-property :name el) (setq owner (org--graph-add-template templates it nil)))
         (pcase owner
           (`(template . ,id) (puthash id (append (gethash id templates) (org--graph-table-rows el)) templates))
           ('record (dolist (line (org--graph-table-lines el)) (push line (alist-get 'record rows))))
           (_ (dolist (row (org--graph-table-rows el)) (push row (alist-get (or owner 'attr) rows))))))
        ('plain-list
         (setq owner nil kind (or kind (org-element-property :type el)))
         (dolist (item (org-element-contents el)) (push (org--graph-read-item item templates) items)))
        ('special-block
         (setq owner nil)
         (pcase (downcase (org-element-property :type el))
           ((pred (string-match-p "\\`cluster\\(?:-[0-9]+\\)?\\'")) (push (org--graph-read-cluster el templates) items))
           ("note" (push (org--graph-read-note el) notes))))
        ;; Prose past an item's first paragraph ---its text--- is a note, as a #+begin_note block's body is.
        ('paragraph
         (setq owner nil)
         (push (list :edge nil :text (buffer-substring-no-properties (org-element-property :contents-begin el)
                                                                     (org-element-property :contents-end el)))
               notes))
        (_ (setq owner nil))))
    (list :graph-rows (nreverse (alist-get 'graph rows))
          :node-rows  (nreverse (alist-get 'node rows))
          :edge-rows  (nreverse (alist-get 'edge rows))
          :ranks      (nreverse ranks)
          :attr-rows  (nreverse (alist-get 'attr rows))
          :record     (nreverse (alist-get 'record rows))
          :notes      (nreverse notes)
          :items      (nreverse items)
          :kind       kind)))

(defun org--graph-read-item (item templates)
  "An ITEM element ↦ its ITEM data; its TEXT runs from its bullet to its first paragraph's end, any description term
included.  #+name: tables within it go to TEMPLATES."
  (let* ((contents (org-element-contents item))
         (begin    (org-element-property :contents-begin item))
         (para     (and (eq (org-element-type (car contents)) 'paragraph)
                        (eql (org-element-property :begin (car contents)) begin)
                        (car contents)))
         (start    (save-excursion (goto-char (org-element-property :begin item))
                                   (skip-chars-forward " \t")
                                   (+ (point) (length (s-trim-right (org-element-property :bullet item))))))
         (scope    (org--graph-read-scope (if para (cdr contents) contents) templates)))
    (list :text (buffer-substring-no-properties start (cond (para (org-element-property :contents-end para))
                                                            (begin)
                                                            (t (org-element-property :end item))))
          :pre scope :children (plist-get scope :items) :kind (plist-get scope :kind)
          :line (line-number-at-pos (org-element-property :begin item)))))

(defun org--graph-read-note (block)
  "A #+begin_note BLOCK ↦ (:edge EDGE? :text TEXT): its raw body, a tooltip for whatever its scope describes; or, with
`#+begin_note :edge', for the arc into it.
  \"#+begin_note :edge\\nBy /name/.\\n#+end_note\"  ↦  (:edge t :text \"By /name/.\")"
  (let ((header (org--read-header (org-element-property :parameters block))))
    (unless (member header '(nil (:edge)))
      (user-error "graph: `#+begin_note %s' takes no arguments but :edge" (org-element-property :parameters block)))
    (list :edge (and header t)
          :text (-some--> (org-element-property :contents-begin block)
                  (buffer-substring-no-properties it (org-element-property :contents-end block))))))

(defun org--graph-read-cluster (block templates)
  "A #+begin_cluster BLOCK ↦ the ITEM it abbreviates: one styled | shape | cluster |, whose sub-items are the block's.

  #+begin_cluster Packages :bgcolor grey :node.shape box      + Packages
  + Org                                                  ≈     | shape   | cluster |
  #+end_cluster                                                | bgcolor | grey    |
                                                               #+node:
                                                               | shape | box |
                                                               + Org

The header reads as the #+begin_graph one does: a title, then :KEY graph attributes, :node.KEY and :edge.KEY defaults;
a table opening the block styles the box, as an item's table styles its node.  An untitled cluster is an anonymous box:
it has no label, and no name to draw arcs to.  #+name: tables within it go to TEMPLATES."
  (-let* (;; The title runs up to the first whitespace-then-colon; `Time 12:30' stays whole.
          ((_ title kwds) (s-match "\\`\\(.*?\\)\\(?:\\(?:\\`\\|[ \t]+\\)\\(:.*\\)\\)?\\'"
                                   (s-trim (or (org-element-property :parameters block) ""))))
          ((&plist :graph-rows graph :node-rows node :edge-rows edge) (org--graph-header-rows (org--read-header kwds)))
          (scope (org--graph-read-scope (org-element-contents block) templates)))
    (when (s-blank-str-p title)
      (setq title (format "anonymous %d" (cl-incf org--graph-anonymous-clusters))
            graph (cons '("label" "\"\"") graph)))
    (list :text title
          :pre (org-combine-plists scope (list :attr-rows (append '(("shape" "cluster")) graph
                                                                  (plist-get scope :attr-rows))
                                               :node-rows (append node (plist-get scope :node-rows))
                                               :edge-rows (append edge (plist-get scope :edge-rows))))
          :children (plist-get scope :items) :kind (plist-get scope :kind)
          :line (line-number-at-pos (org-element-property :begin block)))))

(defun org--graph-number-clusters (raw)
  "RAW with each #+begin_cluster … #+end_cluster pair nested at depth d > 1 renamed cluster-d; since Org pairs a block
with the first end of its name, only then do nested clusters parse as nested blocks.
  \"#+begin_cluster A\\n#+begin_cluster B\\n#+end_cluster\\n#+end_cluster\"
    ↦  \"#+begin_cluster A\\n#+begin_cluster-2 B\\n#+end_cluster-2\\n#+end_cluster\""
  (let ((depth 0) (case-fold-search t))
    (s-join "\n" (--map (cond ((string-match "\\`[ \t]*#\\+begin_cluster\\_>" it)
                               (if (> (cl-incf depth) 1) (replace-match (format "\\&-%d" depth) t nil it) it))
                              ((string-match "\\`[ \t]*#\\+end_cluster\\_>" it)
                               (prog1 (if (> depth 1) (replace-match (format "\\&-%d" depth) t nil it) it)
                                 (setq depth (max 0 (1- depth)))))
                              (t it))
                        (s-lines raw)))))

(defun org--graph-read (raw)
  "A graph's RAW body ↦ its top-level SCOPE, plus :name-tables, its #+name: templates wherever they sit.
  \"#+node:\\n| shape | box |\\n+ A\"  ↦  (:name-tables #s(…) :node-rows ((\"shape\" \"box\")) :items ((:text \"A\" …)) …)
Org parses RAW in a buffer of its own, so the elements' positions are those of RAW."
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert (org--graph-number-clusters raw))
    (let ((tree (org-element-parse-buffer))
          (templates (make-hash-table :test #'equal))
          (org--graph-anonymous-clusters 0))
      ;; Org reads an unclosed block as mere text.
      (org-element-map tree 'paragraph
        (lambda (p) (let ((text (buffer-substring (org-element-property :begin p) (org-element-property :end p))))
                      (when (string-match "^[ \t]*#\\+begin_cluster\\(?:-[0-9]+\\)?\\_>[ \t]*\\(.*\\)" text)
                        (user-error "graph: `#+begin_cluster %s' has no #+end_cluster" (match-string 1 text))))))
      (cons :name-tables (cons templates (org--graph-read-scope (org-element-contents tree) templates))))))

(defconst org--graph-header-keys '(:file :svg :directed :wrap :notes :unflatten :unknown-tags :C-c_C-c
                                         :contents-occur-as-link-description :exports :results)
  "Header keywords the `graph' block consumes itself, or Babel habits such as :exports, rather than handing to Graphviz.")

(defun org--graph-header-rows (kwdargs)
  "KWDARGS, from the #+begin_graph line, ↦ (:graph-rows :node-rows :edge-rows); keys in `org--graph-header-keys' are skipped.

  (:rankdir LR :node.shape box :edge.color \"#f00\")  ↦  (:graph-rows ((\"rankdir\" \"LR\")) :node-rows ((\"shape\" \"box\")) :edge-rows ((\"color\" \"#f00\")))"
  (let (graph node edge)
    (pcase-dolist (`(,key ,val) (-partition 2 kwdargs))
      (unless (memq key org--graph-header-keys)
        (let ((key (substring (symbol-name key) 1)) (val (format "%s" val)))
          (cond ((s-starts-with-p "node." key) (push (list (substring key 5) val) node))
                ((s-starts-with-p "edge." key) (push (list (substring key 5) val) edge))
                (t (push (list key val) graph))))))
    (list :graph-rows (nreverse graph) :node-rows (nreverse node) :edge-rows (nreverse edge))))

(provide 'org-graph-read)
;;; org-graph-read.el ends here
