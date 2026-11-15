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
(require 'org-graph-attributes)

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
underscore) other than dot's keywords, such as graph, and bare numbers are returned as-is.  Everything else is wrapped in
double quotes with any interior double-quotes escaped."
  (cond
   ((and (s-starts-with-p "\"" name) (s-ends-with-p "\"" name) (> (length name) 1)) name)
   ((and (s-starts-with-p "<" name) (s-ends-with-p ">" name)) name)
   ((and (s-matches-p "^[a-zA-Z_][a-zA-Z_0-9]*$" name)
         (not (member (downcase name) '("node" "edge" "graph" "digraph" "subgraph" "strict"))))
    name)
   ((s-matches-p "^-?[0-9]+\\.?[0-9]*$" name) name)
   (t (format "\"%s\"" (s-replace "\"" "\\\"" name)))))

(defun org--graph-dot-port (port)
  "PORT ↦ a record port name, spaces as underscores, e.g. \"mid dle\" ↦ \"mid_dle\".
The same string is then safe both inside a record label's <port> and in an edge's node:port."
  (s-replace " " "_" port))

(defun org--graph-unquote (s)
  "S sans one pair of surrounding double quotes, e.g. \"\\\"Org mode\\\"\" ↦ \"Org mode\"; so both spellings name one node."
  (if (string-match "\\`\"\\(.*\\)\"\\'" s) (match-string 1 s) s))

(defvar org--graph-tips nil
  "While a graph exports as an inline <svg>, a hash table of its tooltips, key ↦ Org text, for tooltips.js to show;
else nil.  Then tooltips are Org, not plain text, and dot sees only their keys; see `org--graph-tip-key'.")

(defun org--graph-tip-key (key value)
  "(KEY, VALUE), an attribute, ↦ its value in dot: a tooltip's VALUE is noted in `org--graph-tips', under a fresh key.
  \"tooltip\", \"Born /1958/\"  ↦  \"osbe-tip-g1\", noting osbe-tip-g1 ↦ \"Born /1958/\"; else VALUE, unchanged."
  (let ((tip (org--graph-unquote value)))
    (if (not (and org--graph-tips (s-ends-with-p "tooltip" key) (not (s-blank-str-p tip))))
        value
      (let ((id (format "osbe-tip-%s" (gensym))))
        (puthash id (substring-no-properties tip) org--graph-tips)
        id))))

(defun org--graph-attr-pairs (rows)
  "ROWS ↦ \"k₁=v₁ k₂=v₂ …\", each value quoted as needed by `org--graph-dot-id'.
A reserved :KEY is never Graphviz's, so one reaching here is an error, rather than dot's own syntax error; e.g., an
arc template's :edge.style, where the template already styles the arc, and so wants plain style."
  (-when-let (reserved (--first (string-prefix-p ":" (format "%s" (car it))) rows))
    (user-error "graph: ‘%s’ is no Graphviz attribute here; a template that a tag applies to an arc styles the arc \
itself, so it wants ‘%s’" (car reserved) (replace-regexp-in-string "\\`:edge\\(?:\\[[^]]*\\]\\)?\\." "" (format "%s" (car reserved)))))
  (s-join " " (--map (format "%s=%s" (car it) (org--graph-dot-id (org--graph-tip-key (car it) (cadr it)))) rows)))

(defun org--graph-attrs->dot (rows)
  "ROWS ↦ \" [ k₁=v₁ … ]\", ready to follow a dot id or keyword; \"\" when ROWS is empty."
  (if rows (format " [ %s ]" (org--graph-attr-pairs rows)) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reading: a graph's body ⟶ plain data, via `org-element'
;;
;; Org parses the body once; we read its elements into plain data, so nothing after this section needs a buffer:
;;   SCOPE ≈ (:graph-rows :node-rows :edge-rows :attr-rows :table :notes :items :kind)
;;   ITEM  ≈ (:text TEXT :pre SCOPE :children ITEMS :kind KIND :line LINE)
;; An item's TEXT is its raw first paragraph, bullet aside; its SCOPE, its directives and tables; its ITEMS, its
;; sub-items; its KIND, its sub-list's type: `ordered', `unordered', or `descriptive';
;; and its LINE, where it starts in the body, so a click on its node in the rendered image leads back to it.

(defvar org--graph-strict nil
  "Non-nil while a Graphviz graph is read: then Org syntax that means nothing in a graph, such as a headline or a
#+begin_quote, is a user error; smartdiagram's reading keeps it, as prose.")

(defconst org--graph-inert-elements '(comment comment-block footnote-definition)
  "Elements a graph's body may hold yet ignores; a bare [fn:NAME] finds its definition wherever it sits.")

(defun org--graph-stray (el what)
  "Unless EL is inert, a user error, under `org--graph-strict', saying WHAT, at EL's line, is not graph syntax."
  (when (and org--graph-strict (not (memq (org-element-type el) org--graph-inert-elements)))
    (user-error "graph: line %d: %s is not graph syntax" (line-number-at-pos (org-element-property :begin el)) what)))

(defun org--graph-keyword-tags (value)
  "A #+graph:, #+node:, #+edge:, or #+name: keyword's VALUE ↦ its tags, as an :inherit row; else nil.
  \":wife:base:\"  ↦  (\":inherit\" \"wife base\")"
  (when (string-match (concat "\\`[ \t]*" org--graph-tag-group-re "[ \t]*\\'") value)
    (list ":inherit" (s-join " " (s-split ":" (match-string 1 value) t)))))

(defun org--graph-keyword-rows (key value)
  "A #+graph:, #+node:, or #+edge: keyword KEY's VALUE ↦ its rows: its tags, as an :inherit row, and each KEY=VALUE pair,
as Graphviz's own edge [fontsize=10 color=gray] writes them; a value with spaces is \"quoted\".
  \"fontsize=10 color=gray\"            ↦  ((\"fontsize\" \"10\") (\"color\" \"gray\"))
  \":faint: label=\\\"a b\\\" style=dashed\"  ↦  ((\":inherit\" \"faint\") (\"label\" \"a b\") (\"style\" \"dashed\"))
So a line may say what a two-column table below it would; anything else on it is an error."
  (let ((pos 0) rows)
    (while (string-match "[ \t]*\\([^ \t]\\)" value pos)
      (setq pos (match-beginning 1))
      (cond ((eq pos (string-match (concat org--graph-tag-group-re "\\(?:[ \t]\\|\\'\\)") value pos))
             (push (list ":inherit" (s-join " " (s-split ":" (match-string 1 value) t))) rows)
             (setq pos (match-end 1)))
            ((eq pos (string-match "\\([^ \t=:\"]+\\)=\\(?:\"\\(\\(?:[^\"\\\\]\\|\\\\.\\)*\\)\"\\|\\([^ \t]+\\)\\)" value pos))
             (push (list (match-string 1 value)
                         (or (-some->> (match-string 2 value) (replace-regexp-in-string "\\\\\\(.\\)" "\\1"))
                             (match-string 3 value)))
                   rows)
             (setq pos (match-end 0)))
            (t (user-error "graph: ‘#+%s: %s’ holds ‘%s’, which is neither a :tag: nor a KEY=VALUE pair"
                           key (s-trim value) (car (split-string (substring value pos)))))))
    (nreverse rows)))

(defun org--graph-add-template (templates name rows &optional check)
  "(TEMPLATES, NAME, ROWS) ↦ (template . ID), having appended ROWS to template ID in TEMPLATES; from a `#+name: NAME'.
  \"wife :base:\"           ↦  (template . \"wife\"), template wife now inheriting template base
  \"red color=red :base:\"  ↦  (template . \"red\"), its rows color=red, then base's; as a table below would say
With CHECK, for a graph's own template, see `org--graph-check-template'."
  (-let* (((_ id rest) (s-match "\\`[ \t]*\\([^ \t]*\\)\\(.*\\)\\'" name))
          (pairs (org--graph-keyword-rows 'name rest)))
    (when check (org--graph-check-template id (append (--remove (equal (car it) ":inherit") pairs) rows)))
    (puthash id (append (gethash id templates) pairs rows) templates)
    (cons 'template id)))

(defconst org--graph-name-pairs-re "^\\([ \t]*\\)#\\+name:\\([ \t]+[^ \t\n]+[^\n]*?[^ \t\n=:\"]=[^\n]*\\)$"
  "A #+name: line holding KEY=VALUE pairs, a template on one line: group 1, its indentation; 2, all after the colon.")

(defun org--graph-unaffiliate-names (raw)
  "RAW with each #+name: line of KEY=VALUE pairs as a #+template: line, which Org affiliates with nothing; so several
such lines in a row are several templates, rather than Org's names of one element.  Lines stay put, so an item's
line, which a node's click leads back to, is unchanged.
  \"#+name: red color=red\\n#+name: blue color=blue\"  ↦  \"#+template: red color=red\\n#+template: blue color=blue\""
  (let ((case-fold-search t))
    (replace-regexp-in-string org--graph-name-pairs-re "\\1#+template:\\2" raw t)))

(defun org--graph-table-lines (table)
  "A TABLE element, in the current buffer, ↦ its rows' lines, trimmed; hlines included, e.g. (\"| a | b |\" \"|---|\")."
  (--filter (s-starts-with-p "|" it)
            (-map #'s-trim (s-lines (buffer-substring-no-properties (org-element-property :post-affiliated table)
                                                                    (org-element-property :end table))))))

(defun org--graph-attribute-key-p (key)
  "Returns whether KEY names a Graphviz attribute, as `org-graph-attributes' lists them: ATTR itself, or :edge.ATTR or
:edge[TARGET].ATTR, for an arc.  \"shape\" and \":edge[b].style\" do; \":edge.colour\" and \":id\" do not."
  (let ((attr (if (string-match "\\`:edge\\(?:\\[.+?\\]\\)?\\.\\(.+\\)\\'" key) (match-string 1 key) key)))
    (--any? (equal (plist-get it :name) attr) org-graph-attributes)))

(defun org--graph-check-template (id rows)
  "Signals a user error when a graph's own template ID is spelt `@X', which aligns nodes instead, or its ROWS set a key
that names no Graphviz attribute, as `org--graph-attribute-key-p' says; e.g. | :inherit | base |, since a template
inherits by tags, `#+name: wife :base:'.  The buffer's #+name: tables, Babel's data among them, go unchecked."
  (when (s-starts-with-p "@" id)
    (user-error "graph: `#+name: %s' cannot name a template, since `:%s:' aligns nodes" id id))
  (-when-let (row (--first (not (org--graph-attribute-key-p (car it))) rows))
    (user-error "graph: template `%s' sets `%s', which is no Graphviz attribute; to inherit template T, write \
`#+name: %s :T:'" id (car row) id)))

(defun org--graph-attribute-table-p (lines)
  "Returns whether a table's LINES are | key | value | rows: two cells apiece, each key an attribute, as
`org--graph-attribute-key-p' says.  Any other table is a record: one column, or three; an hline; or an unknown key,
such as | left | right |, or a misspelt | :edge.colour | red |.  So a record whose first cell names an attribute writes
it =label=."
  (--all? (let ((cells (cdr (s-split "|" it))))
            (when (s-blank-str-p (car (last cells))) (setq cells (butlast cells)))
            (and (= (length cells) 2) (org--graph-attribute-key-p (s-trim (car cells)))))
          lines))

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

A table belongs to the directive above it, past any tables between: #+graph:, #+node:, #+edge:, or #+name: ---Org's
own affiliated keyword---; else, it holds the item's own | key | value | rows, or, when it holds anything else, it
is a record, drawn as written: its lines are the item's :table; see `org--graph-attribute-table-p'.
A #+graph:, #+node:, or #+edge: line may hold rows itself, KEY=VALUE pairs; see `org--graph-keyword-rows'."
  (let (rows notes items kind owner)
    (dolist (el (org--graph-elements elements))
      (pcase (org-element-type el)
        ('keyword
         (let ((key (intern (downcase (org-element-property :key el)))) (value (org-element-property :value el)))
           (setq owner nil)
           (pcase key
             ((or 'graph 'node 'edge)
              (setq owner key)
              (dolist (row (org--graph-keyword-rows key value)) (push row (alist-get key rows))))
             ((or 'name 'template) (setq owner (org--graph-add-template templates value nil t)))
             (_ (org--graph-stray el (format "#+%s:" key))))))
        ('table
         (-some--> (org-element-property :name el) (setq owner (org--graph-add-template templates it nil t)))
         (pcase owner
           (`(template . ,id) (let ((rows (org--graph-table-rows el)))
                                (org--graph-check-template id rows)
                                (puthash id (append (gethash id templates) rows) templates)))
           ('nil (let ((lines (org--graph-table-lines el)))
                   (if (org--graph-attribute-table-p lines)
                       (dolist (row (org--graph-table-rows el)) (push row (alist-get 'attr rows)))
                     (dolist (line lines) (push line (alist-get 'table rows))))))
           (_ (let ((table (org--graph-table-rows el)))
                ;; A smartdiagram's #+graph: rows are its own keys; a Graphviz graph's, Graphviz's.
                (-when-let (row (and org--graph-strict (--first (not (org--graph-attribute-key-p (car it))) table)))
                  (user-error "graph: a #+%s: table sets `%s', which is no Graphviz attribute; a template applies by \
its tag, `#+%s: :T:'" owner (car row) owner))
                (dolist (row table) (push row (alist-get owner rows)))))))
        ('plain-list
         (setq owner nil kind (or kind (org-element-property :type el)))
         (dolist (item (org-element-contents el)) (push (org--graph-read-item item templates) items)))
        ('special-block
         (setq owner nil)
         (pcase (downcase (org-element-property :type el))
           ("note" (push (org--graph-read-note el) notes))
           (type (org--graph-stray el (format "#+begin_%s" type)))))
        ;; Prose past an item's first paragraph ---its text--- is a note, as a #+begin_note block's body is.
        ('paragraph
         (setq owner nil)
         (push (list :edge nil :text (buffer-substring-no-properties (org-element-property :contents-begin el)
                                                                     (org-element-property :contents-end el)))
               notes))
        (type (setq owner nil) (org--graph-stray el (format "a %s" (s-replace "-" " " (symbol-name type)))))))
    (list :graph-rows (nreverse (alist-get 'graph rows))
          :node-rows  (nreverse (alist-get 'node rows))
          :edge-rows  (nreverse (alist-get 'edge rows))
          :attr-rows  (nreverse (alist-get 'attr rows))
          :table      (nreverse (alist-get 'table rows))
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

(defun org--graph-read (raw)
  "A graph's RAW body ↦ its top-level SCOPE, plus :name-tables, its #+name: templates wherever they sit, and :radio,
its <<<radio targets>>> as `org--graph-radio' holds them.
  \"#+node:\\n| shape | box |\\n+ A\"  ↦  (:name-tables #s(…) :node-rows ((\"shape\" \"box\")) :items ((:text \"A\" …)) …)
Org parses RAW in a buffer of its own, so the elements' positions are those of RAW."
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert (org--graph-unaffiliate-names raw))
    (let ((tree (org-element-parse-buffer))
          (templates (make-hash-table :test #'equal)))
      (org-update-radio-target-regexp)
      (append (list :name-tables templates
                    :radio (when org-target-link-regexp
                             (cons org-target-link-regexp
                                   (org-element-map tree 'radio-target (lambda (o) (org-element-property :value o))))))
              (org--graph-read-scope (org-element-contents tree) templates)))))

(defconst org--graph-header-keys '(:file :svg :directed :mindmap :wrap :notes :unflatten :unknown-tags :theme :C-c_C-c
                                         :contents-occur-as-link-description :exports :results)
  "Header keywords the `graph' block consumes itself, or Babel habits such as :exports, rather than handing to Graphviz.")

(defconst org--graph-directions '(("down" . "TB") ("up" . "BT") ("right" . "LR") ("left" . "RL"))
  "The values of `:direction', a readable `:rankdir', and the rankdir each means: which way the arcs flow.")

(defun org--graph-header-rows (kwdargs)
  "KWDARGS, from the #+begin_graph line, ↦ (:graph-rows :node-rows :edge-rows); keys in `org--graph-header-keys' are skipped.
`:direction right' is `:rankdir LR', and so on, as `org--graph-directions' says.

  (:rankdir LR :node.shape box :edge.color \"#f00\")  ↦  (:graph-rows ((\"rankdir\" \"LR\")) :node-rows ((\"shape\" \"box\")) :edge-rows ((\"color\" \"#f00\")))"
  (let (graph node edge)
    (pcase-dolist (`(,key ,val) (-partition 2 kwdargs))
      (unless (memq key org--graph-header-keys)
        (let ((key (substring (symbol-name key) 1)) (val (format "%s" val)))
          (cond ((equal key "direction")
                 (push (list "rankdir" (or (cdr (assoc val org--graph-directions))
                                           (user-error "graph: `:direction %s' should be one of %s" val
                                                       (s-join ", " (mapcar #'car org--graph-directions)))))
                       graph))
                ((s-starts-with-p "node." key) (push (list (substring key 5) val) node))
                ((s-starts-with-p "edge." key) (push (list (substring key 5) val) edge))
                (t (push (list key val) graph))))))
    (list :graph-rows (nreverse graph) :node-rows (nreverse node) :edge-rows (nreverse edge))))

(provide 'org-graph-read)
;;; org-graph-read.el ends here
