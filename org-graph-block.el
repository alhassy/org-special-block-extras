;;; org-graph-block.el --- #+begin_graph: Org lists → Graphviz dot  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "27.1") (org "9.6") (org-special-block-extras "4.1.1"))
;; Keywords: org, blocks, graphviz, dot, convenience
;; URL: https://alhassy.github.io/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Provides a #+begin_graph … #+end_graph special block that compiles
;; native Org syntax (nested lists + two-column tables) into Graphviz dot.
;;
;; Syntax overview:
;;
;;   #+macro: gold "#f9e2af"       ; Org's own macros: | color | {{{gold}}} |
;;   #+begin_graph Title :rankdir LR :node.shape box :bgcolor #1e1e2e
;;                                 ; title = graph label; :KEY graph attrs; :directed nil ⇒ undirected
;;   #+graph:                      ; global graph[ ] attrs
;;   | rankdir | TB      |
;;   | bgcolor | #1e1e2e |
;;
;;   #+node:                       ; default node[ ] attrs
;;   | shape | box |
;;
;;   #+edge:                       ; default edge[ ] attrs
;;   | color | #6c7086 |
;;
;;   #+rank: lam [[combinatory]]   ; { rank=same; lam; combinatory }
;;
;;   #+name: faint                 ; a style template, for :faint: tags and :inherit rows; also outside graphs
;;   | style | dotted |
;;
;;   - :faint: :: inferred         ; a legend entry: a sample of template faint, captioned inferred
;;
;;   + <<lam>> *lambda*             ; <<id>>; *bold* /italic/ _u_ +s+ =code= ~pre~ ⇒ HTML label
;;     | shape          | ellipse |  ; node attrs; values are quoted for you; | :id | lam | ≡ <<lam>>
;;     | :children-rank | same    |  ; { rank=same; <immediate children> }
;;     + church \\ ℕ, Bool, Pairs    ; nesting = edge lambda→church; \\ breaks the label's line
;;       | :edge.style | dotted |    ; arc attrs; a parallel arc is a second child item
;;     + lisp [[church][cross]]     ; [[TARGET]] or [[TARGET][LABEL]]: an extra arc
;;     + calls :: eval :faint:      ; arc lambda→eval labelled calls; :faint: splices that template
;;     + apply [fn::of a closure]   ; an inline footnote is the node's tooltip
;;       | :edge[church].style | dotted |  ; attrs of this item's arc to church
;;       + [[combinatory]]          ; a lone link is a reference: an arc to a node declared elsewhere
;;       + [[combinatory]] :faint:  ; a reference's tags style its arc; "rec::port" reaches a record's port
;;     + [X] Org [[https://orgmode.org]]  ; a web link is the node's URL; [[file:x.png]] its image; [X] applies :checked:
;;   + combinatory
;;     1. S                         ; ordered children: one rank, in the order written
;;     2. K
;;   #+begin_cluster Tools :bgcolor grey  ; a box around its items; titled, with attributes, as #+begin_graph is
;;   + emacs
;;   #+end_cluster
;;   #+end_graph
;;
;; Pressing C-c C-c anywhere inside the block renders a PNG immediately.
;; C-M-i completes, and eldoc explains, table keys and #+begin_graph keys; in any Org buffer's graph
;; blocks, and in the `org-graph-edit-mode' buffer C-c ' opens.  The attributes live in org-graph-attributes.el.
;; Use #+begin_graph :file foo.png, or a #+name: foo line above it, to fix the output filename.

;;; Code:

(require 's)
(require 'dash)
(require 'cl-lib)
(require 'org)
(require 'org-special-block-extras)
(require 'org-graph-attributes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing helpers

(defconst org-graph-reserved-keys '(":id" ":inherit" ":children-rank")
  "Table keys meaningful to the `graph' block itself, rather than to Graphviz; alongside the prefixes `:edge.' and `:edge[TARGET].'.")

(defcustom org-graph-checkbox-templates
  '(("checked" ("color" "forestgreen") ("fontcolor" "forestgreen"))
    ("partial" ("color" "darkorange") ("fontcolor" "darkorange")))
  "The default #+name: templates that an item's checkbox applies: [X] ↦ checked, [-] ↦ partial, [ ] ↦ unchecked.
A graph's own #+name: template of the same name wins; a checkbox whose template exists nowhere styles nothing."
  :type '(alist :key-type string :value-type (repeat (list string string)))
  :group 'org-special-block-extras)

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
;;   SCOPE ≈ (:graph-rows :node-rows :edge-rows :ranks :attr-rows :record :items :kind)
;;   ITEM  ≈ (:text TEXT :pre SCOPE :children ITEMS :kind KIND)
;; An item's TEXT is its raw first paragraph, bullet aside; its SCOPE, its directives and tables; its ITEMS, its
;; sub-items and #+begin_cluster blocks; and its KIND, its sub-list's type: `ordered', `unordered', or `descriptive'.

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
  (let (rows ranks items kind owner)
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
         (when (string-match-p "\\`cluster\\(?:-[0-9]+\\)?\\'" (downcase (org-element-property :type el)))
           (push (org--graph-read-cluster el templates) items)))
        (_ (setq owner nil))))
    (list :graph-rows (nreverse (alist-get 'graph rows))
          :node-rows  (nreverse (alist-get 'node rows))
          :edge-rows  (nreverse (alist-get 'edge rows))
          :ranks      (nreverse ranks)
          :attr-rows  (nreverse (alist-get 'attr rows))
          :record     (nreverse (alist-get 'record rows))
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
          :pre scope :children (plist-get scope :items) :kind (plist-get scope :kind))))

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
          :children (plist-get scope :items) :kind (plist-get scope :kind))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing: Org list ⟶ graph statements
;;
;; A parse yields plain data, which `org--graph-stmt->dot' then prints:
;;   (:node ID)                    ---ID's name and attributes live in the context
;;   (:edge SRC DST ATTRS)         ---SRC and DST are raw ids, or "record::port"
;;   (:rank KIND IDS)
;;   (:cluster ID :label L :attrs ROWS :scope PREAMBLE :stmts STATEMENTS)
;;   (:legend ID ROWS LABEL TEMPLATES)  ---a sample of TEMPLATES' ROWS, captioned LABEL

(cl-defstruct (org--graph-ctx (:constructor org--graph-ctx-create))
  "What a parse accumulates, and what printing its statements consults."
  (nodes (make-hash-table :test #'equal))    ; id ↦ (NAME . ATTRS), merged across repeat occurrences
  (records (make-hash-table :test #'equal))  ; record id ↦ t
  (clusters (make-hash-table :test #'equal)) ; cluster id ↦ the first node inside it
  name-tables                                ; template name ↦ rows, from #+name:
  (arc-templates (make-hash-table :test #'equal)) ; template name ↦ t, when it styles some arc
  (legends 0)                                ; how many legend entries so far
  (ranks (make-hash-table :test #'equal))    ; node id ↦ its rank group, under splines=curved; see `org--graph-raw->dot'
  compound)                                  ; non-nil once an arc is clipped at a cluster

(defun org--graph-expand-inherit (rows name-tables &optional seen)
  "ROWS with each | :inherit N₁ … Nₖ | row replaced by templates N₁ … Nₖ's rows, sans keys ROWS sets explicitly ---wherever
:inherit sits.  As with Emacs faces, an earlier template's key wins over a later one's.  Templates may themselves inherit;
one already being expanded, as listed in SEEN, contributes nothing; one that no #+name: defines is an error, rather than
silently contributing nothing."
  (let ((explicit (--remove (equal (car it) ":inherit") rows)) inherited)
    (dolist (name (--mapcat (s-split "[ \t]+" (cadr it) t) (--filter (equal (car it) ":inherit") rows)))
      (unless (member name seen)
        (dolist (row (org--graph-expand-inherit
                      (or (gethash name name-tables)
                          (user-error "graph: `%s' names no #+name: template; for literal text ending in \
`:%s:', write =:%s:= or a | label | row" name name name))
                      name-tables (cons name seen)))
          (unless (or (assoc (car row) explicit) (assoc (car row) inherited))
            (push row inherited)))))
    (append (nreverse inherited) explicit)))

(defun org--graph-expand-scope (scope name-tables)
  "SCOPE, a preamble plist, with :inherit rows expanded in its #+graph:, #+node:, and #+edge: tables."
  (dolist (key '(:graph-rows :node-rows :edge-rows) scope)
    (setq scope (plist-put scope key (org--graph-expand-inherit (plist-get scope key) name-tables)))))

(defun org--graph-item-rows (rows name-tables)
  "An item's table ROWS ↦ (:id :attrs :edge :targeted :children-rank), routing reserved keys away from node attributes.

  ((\":edge[b].style\" \"dotted\") (\"shape\" \"box\"))  ↦  (:attrs ((\"shape\" \"box\")) :targeted ((\"b\" (\"style\" \"dotted\"))) …)

`:edge.ATTR' rows describe the arc from the parent; an ATTR given twice is an error, since a parallel arc is a second item.
`:edge[TARGET].ATTR' rows describe this item's arc to TARGET, from a [[TARGET]] link in its text."
  (let (id attrs edge targeted rank)
    (dolist (row (org--graph-expand-inherit (--filter (cadr it) rows) name-tables))
      (-let [(key val) row]
        (cond
         ((equal key ":id") (setq id (org--graph-unquote val)))
         ((equal key ":children-rank") (setq rank val))
         ((string-match "\\`:edge\\[\\(.+?\\)\\]\\.\\(.+\\)\\'" key)
          (let ((target (org--graph-unquote (match-string 1 key))) (attr (match-string 2 key)))
            (setf (alist-get target targeted nil nil #'equal)
                  (append (alist-get target targeted nil nil #'equal) (list (list attr val))))))
         ((s-starts-with-p ":edge." key)
          (let ((attr (substring key 6)))
            (when (and (assoc attr edge) (not (equal attr ":inherit")))
              (user-error "graph: `%s' is given twice; for a parallel arc, write its child item twice" key))
            (setq edge (append edge (list (list attr val))))))
         ((s-starts-with-p ":" key)
          (user-error "graph: unknown key `%s'; expected one of %s, or :edge.ATTR, :edge[TARGET].ATTR"
                      key org-graph-reserved-keys))
         (t (push row attrs)))))
    (list :id id :attrs (nreverse attrs) :children-rank rank
          :edge (org--graph-expand-inherit edge name-tables)
          :targeted (--map (cons (car it) (org--graph-expand-inherit (cdr it) name-tables)) (nreverse targeted)))))

(defun org--graph-override (rows over)
  "ROWS with OVER's rows replacing those of the same key, e.g. ((a 1) (b 2)) and ((a 3)) ↦ ((b 2) (a 3))."
  (append (--remove (assoc (car it) over) rows) over))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Item text: <<targets>>, [[links]], and *markup*

(defconst org--graph-markup-tags
  '((bold "<B>" "</B>") (italic "<I>" "</I>") (underline "<U>" "</U>") (strike-through "<S>" "</S>")
    (code "<FONT FACE=\"monospace\">" "</FONT>") (verbatim "<FONT FACE=\"monospace\">" "</FONT>"))
  "Org markup ↦ the Graphviz HTML-label tags around it.")

(defun org--graph-html-escape (s)
  "S with the characters that HTML-like labels reserve escaped, e.g. \"a<b\" ↦ \"a&lt;b\"."
  (s-replace-all '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;") ("\"" . "&quot;")) s))

(defun org--graph-item-text (text)
  "An item's TEXT ↦ (:name N :label L :target T :links ((PATH . DESCRIPTION) …) :bare-label D :arc-label A :arc-tags AS
:tags TS :tooltip F).

  \"calls :faint: :: <<lam>> *λ* [[church][encodes]] [fn::Church, 1936] :pure:\"
    ↦  (:name \"λ\" :label \"<<B>λ</B>>\" :target \"lam\" :links ((\"church\" . \"encodes\"))
        :arc-label \"calls\" :arc-tags (\"faint\") :tags (\"pure\") :tooltip \"Church, 1936\")

The name is TEXT sans tag, targets, links, footnotes, and markup; the label is an HTML-like one when TEXT has markup,
else the name.  A TEXT that is a lone [[PATH][D]] names the node PATH, with D labelling the arc from its parent; so does
a description-list term, `A ::', whose own tags name the arc's templates.  Trailing tags name templates, and inline
footnotes become the node's tooltip."
  (-let* ((checkbox (when (string-match "\\`\\[\\([ X-]\\)\\][ \t]+" text)
                      (prog1 (pcase (match-string 1 text) ("X" "checked") ("-" "partial") (_ "unchecked"))
                        (setq text (substring text (match-end 0))))))
          ;; As Org's, greedy; yet `[^\0]', since a `\\\\' may have broken the term's line.
          (term (when (string-match "\\`\\([^\0]*\\)[ \t]+::\\(?:[ \t]+\\|\\'\\)" text)
                  (prog1 (match-string 1 text) (setq text (substring text (match-end 0))))))
          ;; A term's own tags, `+ wed :married: :: Khadija', style the arc it labels.
          (arc-tags (when (and term (string-match (concat "\\(?:\\`\\|[ \t]+\\)" org--graph-tag-group-re "[ \t]*\\'") term))
                      (prog1 (s-split ":" (match-string 1 term) t) (setq term (substring term 0 (match-beginning 0))))))
          (arc-label (unless (s-blank-str-p term) (plist-get (org--graph-item-text-sans-tags term) :label)))
          ((text . tags) (org--graph-split-tags text))
          (parts (org--graph-item-text-sans-tags text)))
    (append parts (list :arc-label arc-label :arc-tags arc-tags :tags tags :checkbox checkbox))))

(defun org--graph-split-tags (text)
  "TEXT ↦ (TEXT′ . TAGS), its trailing `:t₁:t₂:' tags split off: \"a :x:y:\" ↦ (\"a\" \"x\" \"y\"); TAGS is nil sans tags."
  (if (string-match org--graph-tags-re text)
      (cons (substring text 0 (match-beginning 0)) (s-split ":" (match-string 1 text) t))
    (list text)))

(defun org--graph-template-rows (tags name-tables)
  "TAGS ↦ the rows of their #+name: templates, from NAME-TABLES; as `org--graph-expand-inherit' reads | :inherit TAGS |."
  (org--graph-expand-inherit `((":inherit" ,(s-join " " tags))) name-tables))

(defun org--graph-arc-tags (text)
  "An item's parsed TEXT ↦ the tags styling its arc: its term's, and also its trailing ones when it is a reference."
  (append (plist-get text :arc-tags) (when (plist-get text :reference) (plist-get text :tags))))

(defun org--graph-external-link-p (o)
  "Non-nil when O is a link object to anything but a node: a web page, a file, and so on."
  (and (eq (org-element-type o) 'link) (not (equal (org-element-property :type o) "fuzzy"))))

(defun org--graph-image-link-p (o)
  "Non-nil when O is a description-less [[file:PATH]] link to an image, such as [[file:emacs.png]]."
  (and (org--graph-external-link-p o) (equal (org-element-property :type o) "file") (not (org-element-contents o))
       (string-match-p (image-file-name-regexp) (org-element-property :path o))))

(defun org--graph-link-url (o)
  "A link object O ↦ where it points: [[file:a.org]] ↦ \"a.org\", [[https://o.org][O]] ↦ \"https://o.org\"."
  (if (equal (org-element-property :type o) "file") (org-element-property :path o) (org-element-property :raw-link o)))

(defun org--graph-item-text-sans-tags (text)
  "An item's TEXT, sans description tag and trailing tags, ↦ the plist of `org--graph-item-text', bar :arc-label and :tags.
A web or file link becomes the node's URL, its description staying in the label; an image link, its image."
  (let* ((objs (org-element-parse-secondary-string
                text '(bold italic underline strike-through code verbatim link target footnote-reference line-break)))
         (image  (-some--> (org-element-map objs 'link (lambda (o) (and (org--graph-image-link-p o) o)) nil t)
                   (org-element-property :path it)))
         (urls   (org-element-map objs 'link (lambda (o) (and (org--graph-external-link-p o) (not (org--graph-image-link-p o))
                                                              (org--graph-link-url o)))))
         (_      (when (cdr urls) (user-error "graph: a node has one URL, but `%s' links to %s" text (s-join " and " urls))))
         (tooltip (-some--> (org-element-map objs 'footnote-reference
                              (lambda (o) (-some->> (org-element-contents o) (org--graph-item-render nil))))
                    (s-join " " it)))
         (edge-link? (lambda (o) (and (eq (org-element-type o) 'link) (equal (org-element-property :type o) "fuzzy"))))
         (links  (--map (cons (org-element-property :path it)
                              (-some->> (org-element-contents it) (org--graph-item-render nil)))
                        (-filter edge-link? (org-element-map objs 'link #'identity))))
         (target (car (org-element-map objs 'target (lambda (o) (org-element-property :value o)))))
         (name   (s-trim (s-collapse-whitespace (org--graph-item-render nil objs)))))
    (append
     (cond
      ((-all? #'stringp objs) (list :name text :label text))
      ((and (s-blank-str-p name) (not target) (= 1 (length links)))
       (list :name (caar links) :label (caar links) :bare-label (cdar links) :reference t))
      ;; A lone image is its own name, drawn sans text.
      (t (list :name (if (s-blank-str-p name) (or target image) name)
               :label (cond ((org-element-map objs (cons 'line-break (mapcar #'car org--graph-markup-tags)) #'identity nil t)
                             (format "<%s>" (s-trim (s-collapse-whitespace (org--graph-item-render t objs)))))
                            ((not (s-blank-str-p name)) name)
                            (target)
                            (t "\"\""))
               :target target :links links)))
     (list :tooltip tooltip :url (car urls) :image image))))

(defun org--graph-item-render (html? objs)
  "OBJS, a parsed item text, ↦ its plain text; or, when HTML?, its HTML-like label body. Edge links and targets vanish."
  ;; Sans the parse's text properties, which would otherwise ride along into the dot source.
  (substring-no-properties
   (mapconcat
   (lambda (o)
     (let ((type (org-element-type o)))
       ;; Org keeps an object's trailing spaces apart, as its :post-blank.
       (concat
        (cond
         ((stringp o) (if html? (org--graph-html-escape o) o))
         ((memq type '(target footnote-reference)) "")
         ((eq type 'line-break) (if html? "<BR/>" " "))
         ((eq type 'link) (if (or (not (org--graph-external-link-p o)) (org--graph-image-link-p o)) ""
                            (org--graph-item-render html? (or (org-element-contents o) (list (org--graph-link-url o))))))
         (t (-let [(open close) (and html? (alist-get type org--graph-markup-tags))]
              (concat open
                      (if (memq type '(code verbatim))
                          (funcall (if html? #'org--graph-html-escape #'identity) (org-element-property :value o))
                        (org--graph-item-render html? (org-element-contents o)))
                      close))))
        (unless (stringp o) (make-string (or (org-element-property :post-blank o) 0) ?\s)))))
   objs "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Records: a #+record: table ⟶ an HTML-like <TABLE> label

(defun org--graph-cell-html (cell)
  "A record CELL's text ↦ its HTML-like label body: markup as tags, a `\\\\' as <BR/>; targets and links vanish."
  (s-trim (org--graph-item-render t (org-element-parse-secondary-string
                                     (org--graph-break-lines cell)
                                     '(bold italic underline strike-through code verbatim link target line-break)))))

(defun org--graph-break-lines (text)
  "TEXT with each `\\\\' ending a line, as Org's line breaks must; e.g. \"a \\\\ b\" ↦ \"a\\\\\\nb\"."
  (replace-regexp-in-string "[ \t]*\\\\\\\\[ \t]*" "\\\\\\\\\n" text))

(defun org--graph-table-label (lines)
  "A #+record: table's LINES ↦ (LABEL . LINKS), its <TABLE> label and its cells' arcs, each (PORT TARGET DESCRIPTION TAGS);
nil sans LINES.

  | left | <<mid>> middle [[b]] :faint: |  ↦  (\"<<TABLE …><TR><TD>left</TD><TD PORT=\\\"mid\\\">middle</TD></TR></TABLE>>\"
                                                (\"mid\" \"b\" nil (\"faint\")))

A cell's <<target>> is its port; a cell with links but no target is ported by its text.  A cell's trailing `:t:' tags
style its arcs, as a reference's style its arc.  A `^' cell continues the
cell above; an empty one, the cell to its left ---or, in the first column, above.  A row of <l>, <c>, and <r> cookies
aligns its columns.  Hlines draw rules between rows; a table with them has one outer border rather than cell borders."
  (when lines
    (let* ((hline? (lambda (l) (s-matches-p "\\`|-" l)))
           (lines  (->> lines (-drop-while hline?) reverse (-drop-while hline?) reverse))
           (cookie? (lambda (row) (and (--all? (s-matches-p "\\`\\(?:<[lcr]?[0-9]*>\\)?\\'" it) row)
                                       (--some (not (s-blank-str-p it)) row))))
           (rows   (--map (unless (funcall hline? it) (-map #'s-trim (butlast (cdr (s-split "|" it))))) lines))
           (aligns (-some--> (-first (lambda (r) (and r (funcall cookie? r))) rows)
                     (--map (pcase (and (string-match "<\\([lcr]\\)" it) (match-string 1 it))
                              ("l" "LEFT") ("c" "CENTER") ("r" "RIGHT"))
                            it)))
           (rows   (--remove (and it (funcall cookie? it)) rows))
           ;; Each grid row, and whether an hline sits just above it.
           (grid   (let (acc rule) (dolist (r rows (nreverse acc))
                                     (if r (progn (push (cons rule r) acc) (setq rule nil)) (setq rule t)))))
           (width  (-max (--map (length (cdr it)) grid)))
           (owner  (make-hash-table :test #'equal))
           (cell   (lambda (r c) (or (nth c (cdr (nth r grid))) "")))
           links)
      ;; Every grid position ↦ the position of the cell it belongs to.
      (dotimes (r (length grid))
        (dotimes (c width)
          (puthash (cons r c)
                   (let ((text (funcall cell r c)))
                     (cond ((and (equal text "^") (> r 0)) (gethash (cons (1- r) c) owner))
                           ((and (s-blank-str-p text) (> c 0)) (gethash (cons r (1- c)) owner))
                           ((and (s-blank-str-p text) (> r 0)) (gethash (cons (1- r) c) owner))
                           ((member text '("" "^"))
                            (user-error "graph: a #+record: table's `%s' cell, in row %d, column %d, continues nothing"
                                        text (1+ r) (1+ c)))
                           (t (cons r c))))
                   owner)))
      (let ((body
             (s-join "" (-map-indexed
                         (lambda (r row)
                           (concat
                            (when (car row) "<HR/>")
                            "<TR>"
                            (s-join "" (-keep (lambda (c)
                                                (when (equal (gethash (cons r c) owner) (cons r c))
                                                  (-let* (((text . tags) (org--graph-split-tags (funcall cell r c)))
                                                          (parts (org--graph-item-text-sans-tags text))
                                                          (port (-some-> (or (plist-get parts :target)
                                                                             (and (plist-get parts :links) (plist-get parts :name)))
                                                                  org--graph-dot-port))
                                                          (mine (--filter (equal (gethash it owner) (cons r c))
                                                                          (hash-table-keys owner)))
                                                          (rowspan (1+ (- (-max (-map #'car mine)) r)))
                                                          (colspan (1+ (- (-max (-map #'cdr mine)) c)))
                                                          (align (nth c aligns)))
                                                    (when (and tags (not (plist-get parts :links)))
                                                      (user-error "graph: a #+record: cell's tags style its arcs, but `%s' \
has no [[link]]; for literal text ending in `:%s:', write =:%s:=" text (car tags) (car tags)))
                                                    (--each (plist-get parts :links)
                                                      (push (list port (car it) (cdr it) tags) links))
                                                    (concat "<TD"
                                                            (when port (format " PORT=\"%s\"" (org--graph-html-escape port)))
                                                            (when (> rowspan 1) (format " ROWSPAN=\"%d\"" rowspan))
                                                            (when (> colspan 1) (format " COLSPAN=\"%d\"" colspan))
                                                            (when align (format " ALIGN=\"%s\" BALIGN=\"%s\"" align align))
                                                            ">" (org--graph-cell-html text) "</TD>"))))
                                              (number-sequence 0 (1- width))))
                            "</TR>"))
                         grid))))
        (cons (format "<<TABLE %s CELLSPACING=\"0\">%s</TABLE>>"
                      (if (-some #'car grid) "BORDER=\"1\" CELLBORDER=\"0\"" "BORDER=\"0\" CELLBORDER=\"1\"")
                      body)
              (nreverse links))))))

(defun org--graph-item-parts (item)
  "A read ITEM ↦ (TEXT PREAMBLE CHILDREN KIND): its parsed text, its tables and directives, its sub-items, and their KIND.
As in Org, an item's text is its first paragraph, so a long line of [[links]] may wrap; its lines join, and only then
does a `\\\\' break the label's line, even mid-line."
  (list (->> (plist-get item :text) s-trim s-lines (-map #'s-trim) (s-join " ") org--graph-break-lines org--graph-item-text)
        (plist-get item :pre) (plist-get item :children) (plist-get item :kind)))

(defun org--graph-item-id (item)
  "A read ITEM ↦ its node id: its | :id | row, else its <<target>>, else its text."
  (-let [(text pre) (org--graph-item-parts item)]
    (or (-some-> (assoc ":id" (plist-get pre :attr-rows)) cadr org--graph-unquote)
        (plist-get text :target)
        (org--graph-unquote (plist-get text :name)))))

(defun org--graph-item-table (text pre name-tables)
  "An item's parsed TEXT and PREAMBLE ↦ its `org--graph-item-rows', its tags inheriting their #+name: templates.
A tag applies a template to whatever its line describes: after an item, its node; after a reference, or in a
description term, its arc.  Explicit rows still win."
  (let ((arc-tags (org--graph-arc-tags text))
        (node-tags (unless (plist-get text :reference)
                     (append (plist-get text :tags)
                             (-some--> (plist-get text :checkbox) (and (gethash it name-tables) (list it)))))))
    (org--graph-item-rows (append (when node-tags (list (list ":inherit" (s-join " " node-tags))))
                                  (when arc-tags (list (list ":edge.:inherit" (s-join " " arc-tags))))
                                  (plist-get pre :attr-rows))
                          name-tables)))

(defun org--graph-ordered-rank (kind children)
  "An ordered list's CHILDREN, two or more, ↦ the statement setting them side by side; else nil.
\"1. a 2. b\" reads as “first a, then b”, on one rank."
  (when (and (eq kind 'ordered) (cdr children))
    (list (list :rank "same" (-map #'org--graph-item-id children)))))

(defun org--graph-declare (ctx id name attrs)
  "The statements declaring node ID, shown as NAME with ATTRS: (:node ID) the first time, else none.
A repeat occurrence contributes the ATTRS keys not yet set, and warns about those it contradicts."
  (let ((seen (gethash id (org--graph-ctx-nodes ctx))))
    (if (not seen)
        (progn (puthash id (cons name attrs) (org--graph-ctx-nodes ctx))
               (list (list :node id)))
      (dolist (row attrs)
        (let ((old (assoc (car row) (cdr seen))))
          (cond ((null old) (setcdr seen (append (cdr seen) (list row))))
                ((not (equal old row))
                 (display-warning 'org-graph (format "Node `%s' is given %s=%s again, keeping its first %s=%s"
                                                     id (car row) (cadr row) (car old) (cadr old)))))))
      nil)))

(defun org--graph-first-node (stmts)
  "The id of the first node STMTS declare, looking inside clusters; nil when there is none."
  (-some (lambda (s) (pcase (car s)
                       (:node (cadr s))
                       (:cluster (org--graph-first-node (plist-get (cddr s) :stmts)))))
         stmts))

(defun org--graph-parse-item (item parent ctx)
  "A read ITEM under PARENT (an id, or nil at the top) ↦ its statements; its nodes accrue in CTX.

The order is: arcs PARENT → ITEM, ITEM's [[link]] arcs, ITEM's declaration, then its children's statements.
An item that is a lone [[link]] is a reference: it draws arcs to its target, but leaves declaring it to the target's own item."
  (-let* (((text pre children kind) (org--graph-item-parts item))
          (name     (org--graph-unquote (plist-get text :name)))
          (label    (org--graph-unquote (plist-get text :label)))
          (tables   (org--graph-ctx-name-tables ctx))
          (rows     (org--graph-item-table text pre tables))
          (id       (or (plist-get rows :id) (plist-get text :target) name))
          (attrs    (org--graph-override (append (-some--> (plist-get text :tooltip) `(("tooltip" ,it)))
                                                 (-some--> (plist-get text :url) `(("URL" ,it)))
                                                 (-some--> (plist-get text :image) `(("image" ,it))))
                                         (plist-get rows :attrs)))
          (arc-label (or (plist-get text :arc-label) (plist-get text :bare-label)))
          (shape    (cadr (assoc "shape" attrs)))
          (fresh    (not (gethash id (org--graph-ctx-nodes ctx))))
          (targeted (plist-get rows :targeted))
          (to-arc   (lambda (target attrs)
                      (list :edge id target (org--graph-override attrs (alist-get target targeted nil nil #'equal)))))
          (legend?  (and (not parent) (plist-get text :arc-tags)))
          (ranked   (org--graph-ordered-rank kind children))
          (rank-stmts (-if-let (rank (plist-get rows :children-rank))
                          (list (list :rank rank (-map #'org--graph-item-id children)))
                        ranked))
          (table    (org--graph-table-label (plist-get pre :record)))
          (attrs    (if table (org--graph-override `(("shape" "plain") ("label" ,(car table))) attrs) attrs))
          (arcs     (append (when parent
                              (list (list :edge parent id
                                          (org--graph-override (-some--> arc-label `(("label" ,it))) (plist-get rows :edge)))))
                            (--map (funcall to-arc (car it) (-some--> (cdr it) `(("label" ,it))))
                                   (plist-get text :links))
                            ;; A cell's [[link]] is an arc from its port.
                            (-map (-lambda ((port target desc tags))
                                    (list :edge (format "%s::%s" id port) target
                                          (org--graph-override
                                           (org--graph-override
                                            (-some--> desc `(("label" ,it)))
                                            (when tags (org--graph-template-rows tags tables)))
                                           (alist-get target targeted nil nil #'equal))))
                                  (cdr table)))))
    (when (and (not parent) (plist-get text :arc-label))
      (user-error "graph: a `TERM ::' describes the arc from a parent, but `%s' has none; for a legend entry, the TERM is \
just tags: `- :T: :: %s'" id id))
    ;; A legend entry's tags style its sample, not the graph's arcs.
    (unless legend?
      (org--graph-note-arc-templates ctx (append (org--graph-arc-tags text) (-mapcat #'cadddr (cdr table)))
                                     (plist-get pre :attr-rows)))
    (-when-let (stray (--remove (or (assoc it (plist-get text :links)) (assoc it (mapcar #'cdr (cdr table))))
                                (-map #'car targeted)))
      (user-error "graph: `%s' has `:edge[%s].…' rows, but no [[link]] to %s" id (car stray) (car stray)))
    (when table (puthash id t (org--graph-ctx-records ctx)))
    (when (and (plist-get text :reference) (or attrs (plist-get rows :id)))
      (user-error "graph: `[[%s]]' refers to a node declared elsewhere; give it node attributes there, not here" id))
    (cond
     ;; `- :married: :: marriage', with no parent to arc from, is a legend entry.
     (legend?
      (list (list :legend (format "legend_%d" (cl-incf (org--graph-ctx-legends ctx)))
                  (org--graph-template-rows (plist-get text :arc-tags) tables)
                  label (plist-get text :arc-tags))))
     ((plist-get text :reference)
      (append arcs (--mapcat (org--graph-parse-item it id ctx) children) rank-stmts))
     ((and fresh (equal shape "cluster"))
      (org--graph-declare ctx id name nil)
      (let ((stmts (append (--mapcat (org--graph-parse-item it nil ctx) children) ranked)))
        (-some--> (org--graph-first-node stmts) (puthash id it (org--graph-ctx-clusters ctx)))
        ;; The cluster comes first, so that its first node is declared inside it before any arc mentions that node.
        (cons (list :cluster id
                    :label (or (cadr (assoc "label" attrs)) label)
                    :attrs (--remove (member (car it) '("shape" "label")) attrs)
                    :scope (org--graph-expand-scope pre tables)
                    :stmts stmts)
              arcs)))
     (t
      ;; An ordered list keeps its order: `ordering=out' stops Graphviz from reshuffling the arcs to it.
      (append arcs
              (org--graph-declare ctx id label (if (and ranked (not (assoc "ordering" attrs)))
                                                   (append attrs '(("ordering" "out")))
                                                 attrs))
              (--mapcat (org--graph-parse-item it id ctx) children)
              rank-stmts)))))

(defun org--graph-note-arc-templates (ctx tags rows)
  "Record in CTX the templates that style arcs: the arc TAGS, and those of ROWS' :edge.:inherit and :edge[T].:inherit.
So a legend draws such a template as an arc, rather than as a node."
  (dolist (name (append tags (--mapcat (s-split "[ \t]+" (cadr it) t)
                                       (--filter (string-match-p "\\`:edge\\(?:\\[.+\\]\\)?\\.:inherit\\'" (car it)) rows))))
    (puthash name t (org--graph-ctx-arc-templates ctx))))

(defun org--graph-buffer-templates ()
  "The current buffer's #+name: tables, outside graph blocks, ↦ templates, name ↦ rows; so, as with Babel's
named tables, a house style is named once per file rather than once per graph.
`#+name: wife :base:' inherits, as in a graph; and, as #+name: is Org's affiliated keyword, a #+caption: may sit by it."
  (let ((templates (make-hash-table :test #'equal)))
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char (point-min))
        (let ((case-fold-search t))
          (while (re-search-forward "^[ \t]*#\\+\\(?:\\(begin_graph\\_>\\)\\|name:\\)" nil t)
            (if (match-beginning 1)
                ;; A graph's own templates are its own.
                (re-search-forward "^[ \t]*#\\+end_graph" nil 'move)
              (let ((table (save-match-data (org-element-at-point))))
                (when (eq (org-element-type table) 'table)
                  (org--graph-add-template templates (org-element-property :name table)
                                           (org--graph-table-rows table)))))))))
    templates))

(defun org--graph-parse (raw-contents)
  "RAW-CONTENTS of a #+begin_graph block ↦ (PREAMBLE STATEMENTS CTX).
A template is the graph's own #+name: table, else the buffer's, else one of `org-graph-checkbox-templates'."
  (let* ((buffer   (org--graph-buffer-templates))
         (raw      (org--graph-read raw-contents))
         (tables   (plist-get raw :name-tables))
         ;; Before `org--graph-expand-scope' expands them away, `#+edge: :faint:' style arcs.
         (edge-uses (--map (cons (concat ":edge." (car it)) (cdr it)) (plist-get raw :edge-rows)))
         (_        (maphash (lambda (name rows) (unless (gethash name tables) (puthash name rows tables))) buffer))
         (_        (pcase-dolist (`(,name . ,rows) org-graph-checkbox-templates)
                     (unless (gethash name tables) (puthash name rows tables))))
         (preamble (org--graph-expand-scope raw tables))
         (ctx      (org--graph-ctx-create :name-tables tables))
         (_        (org--graph-note-arc-templates ctx nil edge-uses))
         (items    (plist-get preamble :items))
         (stmts    (append (--mapcat (org--graph-parse-item it nil ctx) items)
                           (org--graph-ordered-rank (plist-get preamble :kind) items))))
    (list preamble stmts ctx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Printing: graph statements ⟶ dot

(defun org--graph-endpoint (raw ctx)
  "RAW ↦ (NODE . PORT): \"rec::p q\" ↦ (\"rec\" . \"p_q\") when rec is a record in CTX, as Org's [[file::search]] reaches
into a file; otherwise RAW names a node, sans PORT, such as \"std::vector\"."
  (let ((raw (org--graph-unquote raw)))
    (if (and (string-match "\\`\\(.+?\\)::\\(.+\\)\\'" raw) (gethash (match-string 1 raw) (org--graph-ctx-records ctx)))
        (cons (match-string 1 raw) (org--graph-dot-port (match-string 2 raw)))
      (list raw))))

(defun org--graph-all-stmts (stmts)
  "STMTS, each cluster followed by its own statements, recursively."
  (--mapcat (cons it (when (eq (car it) :cluster) (org--graph-all-stmts (plist-get (cddr it) :stmts)))) stmts))

(defun org--graph-cluster-id (id)
  "A cluster's node ID ↦ its subgraph's dot id, e.g. \"My Group\" ↦ cluster_My_Group and \"my-box\" ↦ \"cluster_my-box\"."
  (org--graph-dot-id (concat "cluster_" (org--graph-dot-port id))))

(defun org--graph-edge->dot (src dst attrs op ctx)
  "(SRC, DST, ATTRS) ↦ \"SRC OP DST [ ATTRS ];\"; a cluster endpoint is drawn at its first node, clipped by lhead/ltail."
  (-let* ((clusters (org--graph-ctx-clusters ctx))
          ((src-node . src-port) (org--graph-endpoint src ctx))
          ((dst-node . dst-port) (org--graph-endpoint dst ctx))
          (clips (append (when (gethash src-node clusters) (list (list "ltail" (org--graph-cluster-id src-node))))
                         (when (gethash dst-node clusters) (list (list "lhead" (org--graph-cluster-id dst-node))))))
          (end (lambda (node port)
                 (concat (org--graph-dot-id (gethash node clusters node)) (when port (concat ":" (org--graph-dot-id port)))))))
    (when clips (setf (org--graph-ctx-compound ctx) t))
    ;; Under splines=curved, Graphviz drops a same-rank arc's label; an xlabel it still draws.
    (when (-some--> (gethash src-node (org--graph-ctx-ranks ctx)) (eq it (gethash dst-node (org--graph-ctx-ranks ctx))))
      (setq attrs (--map (if (equal (car it) "label") (cons "xlabel" (cdr it)) it) attrs)))
    (format "%s %s %s%s;" (funcall end src-node src-port) op (funcall end dst-node dst-port)
            (org--graph-attrs->dot (append attrs clips)))))

(defun org--graph-scope->dot (scope)
  "SCOPE's :graph-rows, :node-rows, :edge-rows, and :ranks ↦ the dot directives for its scope."
  (append
   (-keep (-lambda ((kind . key))
            (-when-let (rows (plist-get scope key))
              (concat kind (org--graph-attrs->dot rows))))
          '(("graph" . :graph-rows) ("node" . :node-rows) ("edge" . :edge-rows)))
   (--map (format "{ rank=same; %s }" (s-join "; " (-map #'org--graph-dot-id (org--graph-rank-ids it))))
          (plist-get scope :ranks))))

(defun org--graph-rank-ids (ranks)
  "A #+rank: line's RANKS ↦ its ids, each a bare word or a link, e.g. \"a [[Org mode]] b\" ↦ (\"Org mode\" \"a\" \"b\")."
  (let ((link-re "\\[\\[\\([^]]+\\)\\]\\(?:\\[[^]]*\\]\\)?\\]"))
    (append (-map #'cadr (s-match-strings-all link-re ranks))
            (s-split "[ \t]+" (s-trim (replace-regexp-in-string link-re " " ranks)) t))))

(defun org--graph-stmt->dot (stmt op ctx)
  "STMT, a parsed graph statement, ↦ its dot lines; OP is the edge operator, \"->\" or \"--\"."
  (pcase stmt
    (`(:node ,id)
     (-let [(name . attrs) (gethash id (org--graph-ctx-nodes ctx))]
       (list (format "%s%s;" (org--graph-dot-id id)
                     (org--graph-attrs->dot (if (assoc "label" attrs) attrs (append attrs `(("label" ,name)))))))))
    (`(:edge ,src ,dst ,attrs)
     (list (org--graph-edge->dot src dst attrs op ctx)))
    ;; A template that styles arcs is shown as a captioned arc between invisible points; any other, as a captioned node.
    ;; The arc runs along the graph's direction, as a same-rank one may lose its caption; e.g., under splines=curved.
    (`(:legend ,id ,rows ,label ,templates)
     ;; Sans `constraint', which shapes the layout rather than the arc, and would let the sample drift out of line.
     (let ((attrs (org--graph-attrs->dot (org--graph-override (--remove (equal (car it) "constraint") rows)
                                                              `(("label" ,label))))))
       (if (--some (gethash it (org--graph-ctx-arc-templates ctx)) templates)
           (let ((end (concat id "_end")))
             (list (format "%s [ shape=point style=invis ];" id)
                   (format "%s [ shape=point style=invis ];" end)
                   (format "%s %s %s%s;" id op end attrs)))
         (list (format "%s%s;" id attrs)))))
    (`(:rank ,kind ,ids)
     (list (format "{ rank=%s; %s }" kind (s-join "; " (-map #'org--graph-dot-id ids)))))
    (`(:cluster ,id . ,props)
     (list (format "subgraph %s {\n    %s\n  }" (org--graph-cluster-id id)
                   (s-join "\n    "
                           (append (list (concat "label=" (org--graph-dot-id (plist-get props :label))))
                                   (-some--> (plist-get props :attrs) (list (org--graph-attr-pairs it)))
                                   (org--graph-scope->dot (plist-get props :scope))
                                   (--mapcat (org--graph-stmt->dot it op ctx) (plist-get props :stmts)))))))))

(defconst org--graph-header-keys '(:file :svg :directed :C-c_C-c :contents-occur-as-link-description :exports :results)
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

(defun org--graph-note-ranks (ctx preamble stmts)
  "Record in CTX each node's rank group, from PREAMBLE's #+rank: lines and STMTS' ranks, clusters' included.
  #+rank: church turing  ↦  church ↦ 0, turing ↦ 0"
  (let ((groups (append (-map #'org--graph-rank-ids (plist-get preamble :ranks))
                        (--mapcat (pcase it
                                    (`(:rank ,_ ,ids) (list ids))
                                    (`(:cluster ,_ . ,props)
                                     (-map #'org--graph-rank-ids (plist-get (plist-get props :scope) :ranks))))
                                  (org--graph-all-stmts stmts)))))
    (--each-indexed groups (dolist (id it) (puthash id it-index (org--graph-ctx-ranks ctx))))))

(defun org--graph-raw->dot (raw-contents main-arg &optional kwdargs)
  "(RAW-CONTENTS, MAIN-ARG, KWDARGS) ↦ the dot source of a #+begin_graph block.

It is a digraph, unless KWDARGS has `:directed nil'.  MAIN-ARG is the graph's title, its label; other KWDARGS are graph attributes, or node and edge defaults when spelled `:node.KEY' and `:edge.KEY'.
Header attributes come before the block's own #+graph:, #+node:, and #+edge: tables, so those tables win."
  (-let* (((preamble stmts ctx) (org--graph-parse raw-contents))
          (directed (or (not (plist-member kwdargs :directed)) (plist-get kwdargs :directed)))
          (title    (unless (s-blank-str-p main-arg) main-arg))
          (header   (org--graph-header-rows kwdargs)))
    (setq header (plist-put header :graph-rows (append (when title `(("label" ,title))) (plist-get header :graph-rows))))
    (dolist (key '(:graph-rows :node-rows :edge-rows))
      (setq preamble (plist-put preamble key (org--graph-override (plist-get header key) (plist-get preamble key)))))
    (when (equal "curved" (cadr (assoc "splines" (plist-get preamble :graph-rows))))
      (org--graph-note-ranks ctx preamble stmts))
    (let ((body (--mapcat (org--graph-stmt->dot it (if directed "->" "--") ctx) stmts)))
      (when (org--graph-ctx-compound ctx)
        (setq preamble (plist-put preamble :graph-rows (append (plist-get preamble :graph-rows) '(("compound" "true"))))))
      (format "%s {\n  %s\n}" (if directed "digraph" "graph")
              (s-join "\n  " (append (org--graph-scope->dot preamble) body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C-c C-c integration

(defun org-graph--insert-results (end-point payload)
  "Insert \"#+RESULTS:\\nPAYLOAD\" on the line after END-POINT, replacing the whole #+RESULTS: element already there.
#+RESULTS: is an affiliated keyword, so its element spans the result proper ---say, a multi-line src block--- too."
  (save-excursion
    (goto-char end-point)
    (forward-line 1)
    (when (looking-at-p "[ \t]*#\\+RESULTS:")
      ;; Its blank lines after belong to what follows, so they stay.
      (delete-region (point) (save-excursion (goto-char (org-element-property :end (org-element-at-point)))
                                             (skip-chars-backward " \t\n")
                                             (forward-line 1)
                                             (point))))
    (insert (format "#+RESULTS:\n%s\n" payload))))

(defun org-graph--run-dot (dot format outfile)
  "(DOT, FORMAT, OUTFILE) ↦ (EXIT-CODE . STDERR) of running `dot -TFORMAT' on DOT source, writing OUTFILE."
  (let ((dotfile  (make-temp-file "org-graph-" nil ".dot"))
        (err-file (make-temp-file "org-graph-err-")))
    (unwind-protect
        (progn
          (write-region dot nil dotfile)
          (cons (call-process "dot" nil (list nil err-file) nil (concat "-T" format) dotfile "-o" outfile)
                (with-temp-buffer
                  (insert-file-contents err-file)
                  (s-trim (buffer-string)))))
      (delete-file dotfile)
      (delete-file err-file))))

(defun org-graph--annotate-dot-error (err-str dot)
  "Enrich ERR-STR from dot with offending lines from DOT and debug hints.
Collects every “line N” reference in ERR-STR, extracts those lines, and
appends a structured debug footer."
  (let* ((src-lines  (split-string dot "\n"))
         (n-lines    (length src-lines))
         (offenders
          (--keep (let ((n (string-to-number (cadr it))))
                    (when (<= 1 n n-lines) (format "  line %d: %s" n (s-trim (nth (1- n) src-lines)))))
                  (s-match-strings-all "line \\([0-9]+\\)" err-str)))
         (debug-footer
          (string-join
           '("\nTo debug further:"
             "  • dot -v -Tpng <file>.dot -o /dev/null   (verbose layout trace)"
             "  • dot -V                                   (check Graphviz version)"
             "  • C-u C-c C-c on the block                (inspect generated dot source)")
           "\n")))
    (concat err-str
            (when offenders
              (concat "\n\nOffending dot lines:\n" (string-join offenders "\n")))
            debug-footer)))

(defun org-graph--show-dot (dot)
  "Display DOT source string in a dedicated *org-graph dot* buffer."
  (let ((buf (get-buffer-create "*org-graph dot*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert dot)
      (when (fboundp 'graphviz-dot-mode) (graphviz-dot-mode))
      (goto-char (point-min)))
    (display-buffer buf)))

(defun org--graph-expand-macros (raw)
  "RAW with each {{{NAME(ARGS)}}} ↦ its expansion by this buffer's #+macro: lines; e.g. {{{gold}}} ↦ \"#f9e2af\".
Textually, so that #+begin_cluster lines, which Org parses no objects in, expand too."
  (if (not (s-contains-p "{{{" raw))
      raw
    (require 'org-macro)
    (org-macro-initialize-templates)
    (let ((templates org-macro-templates))
      (replace-regexp-in-string
       "{{{\\([[:alpha:]][-[:alnum:]_]*\\)\\(?:(\\([^\0]*?\\))\\)?}}}"
       (lambda (macro)
         (let ((key (downcase (match-string 1 macro))) (args (match-string 2 macro)))
           ;; Expanding matches regexps of its own; the replacement needs ours.
           (save-match-data
             (or (org-macro-expand (list 'macro (list :key key :args (and args (org-macro-extract-arguments args))))
                                   templates)
                 (user-error "graph: {{{%s}}} names no #+macro:" key)))))
       raw t t))))

(defun org-graph--named-file (anchor svg)
  "(ANCHOR, SVG) ↦ the file a graph named ANCHOR renders to, e.g. \"lineage\" ↦ \"lineage.png\", or .svg; nil sans ANCHOR."
  (when anchor (format "%s.%s" anchor (if svg "svg" "png"))))

(defun org-graph--cc-handler ()
  "Render the #+begin_graph block at point, or show its dot source.

With no prefix argument and no :C-c_C-c kwdarg (or :C-c_C-c png):
  renders a PNG and inserts a #+RESULTS: [[file:…]] link.

With C-u prefix, or when the block carries :C-c_C-c dot:
  generates dot source, displays it in *org-graph dot*, and inserts a
  #+RESULTS: #+begin_src dot … #+end_src block.

The two controls compose via XOR: if :C-c_C-c dot is set, C-u toggles
back to PNG.  On a checkbox, it first toggles it, as Org's C-c C-c would.
Returns t (claiming C-c C-c)."
  (when (org-at-item-checkbox-p) (org-toggle-checkbox))
  (save-excursion
    ;; The graph around point ---past any #+begin_cluster within it--- not merely the nearest #+begin_graph above it.
    (-some--> (org-graph--block-at-point) (org-element-property :post-affiliated it) (goto-char it))
    (let* ((block      (org-special-block-after-point "graph"))
           (kwdargs    (org-special-block-kwdargs block))
           (raw        (org--graph-expand-macros (org-special-block-contents block)))
           (main-arg   (org-special-block-main-arg block))
           (end-pt     (org-special-block-end-point block))
           (block-mode (plist-get kwdargs :C-c_C-c))
           (show-dot?  (xor (equal (format "%s" block-mode) "dot") current-prefix-arg))
           (dot        (org--graph-raw->dot raw main-arg kwdargs)))
      (if show-dot?
          (progn
            (org-graph--show-dot dot)
            (org-graph--insert-results end-pt (format "#+begin_src dot\n%s\n#+end_src" dot))
            (message "Graph dot source → *org-graph dot*"))
        (let ((outfile (or (-some->> (plist-get kwdargs :file) (format "%s"))
                           (org-graph--named-file (org-special-block-anchor block) nil)
                           (expand-file-name
                            (format "graph-%s.png" (substring (md5 dot) 0 12))
                            temporary-file-directory))))
          (-let [(exit . err-str) (org-graph--run-dot dot "png" outfile)]
            (if (zerop exit)
                (progn
                  (org-graph--insert-results end-pt (format "[[file:%s]]" outfile))
                  (when (fboundp 'org-display-inline-images)
                    (org-display-inline-images))
                  (message "Graph rendered → %s" (expand-file-name outfile)))
              (user-error "dot failed: %s"
                          (if (s-blank-str-p err-str)
                              "is Graphviz installed? (M-x shell-command: dot -V)"
                            (org-graph--annotate-dot-error err-str dot))))))))
    t))

(org-defblock-add-cc-handler "graph" #'org-graph--cc-handler)
;; Within a graph's #+begin_cluster, C-c C-c renders the graph; elsewhere, a cluster block is none of ours.
(org-defblock-add-cc-handler "cluster" (lambda () (and (org-graph--block-at-point) (org-graph--cc-handler))))

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
    (":C-c_C-c"  . "With dot, C-c C-c shows the dot source, and C-u C-c C-c renders; else the reverse.")
    (":node."    . "A default attribute of every node; e.g., :node.shape box.")
    (":edge."    . "A default attribute of every edge; e.g., :edge.color grey."))
  "The #+begin_graph keys that are not graph attributes ↦ what they do; shown by completion and eldoc.")

(defun org-graph--block-at-point ()
  "The innermost #+begin_graph block element around point, looking past the #+begin_cluster blocks within it; else nil."
  (let ((block (org-special-block-at-point)) found)
    (while (and block (not found))
      (pcase (downcase (org-element-property :type block))
        ("graph"   (setq found block))
        ("cluster" (setq block (org-element-lineage block '(special-block))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-defblock registration

(defun org-graph--render (dot kind outfile)
  "(DOT, KIND, OUTFILE) ↦ OUTFILE, once `dot -TKIND' writes it; else nil, the error logged to *Warnings*."
  (-let [(exit . err-str) (org-graph--run-dot dot kind outfile)]
    (if (zerop exit)
        outfile
      (warn "org-graph: dot failed rendering %s:\n%s" outfile (org-graph--annotate-dot-error err-str dot))
      nil)))

(defun org-graph--png-to-base64 (pngfile)
  "Return a base64 string of the binary contents of PNGFILE."
  (base64-encode-string
   (with-temp-buffer
     (set-buffer-multibyte nil)
     (insert-file-contents-literally pngfile)
     (buffer-string))
   t))

(defun org-graph--svg-contents (svgfile)
  "Return the <svg>…</svg> string from SVGFILE, stripping the XML preamble."
  (with-temp-buffer
    (insert-file-contents svgfile)
    (goto-char (point-min))
    (when (re-search-forward "<svg[ \t\n]" nil t)
      (goto-char (match-beginning 0))
      (buffer-substring (point) (point-max)))))

(org-defblock graph (main-arg nil file nil svg nil :edit-indirect-mode org-graph-edit-mode)
  "Render an Org list + tables as a Graphviz dot graph.

MAIN-ARG is the graph's title, its label.
The header's other keys are graph attributes, such as :rankdir LR; :node.KEY and :edge.KEY set node and edge defaults;
and :directed nil makes an undirected graph.  Pass :file foo.png on the #+begin_graph line to fix the output filename;
else a #+name: foo line above the block makes it foo.png, and the graph a figure that [[foo]] reaches and #+caption:
captions; otherwise the name is graph-HASH.png, HASH being that of the dot source,
so re-exporting an unchanged graph rewrites one file rather than adding another.

For HTML export: if :svg t is set, embeds an inline <svg> element (tooltips
are preserved as <title> attributes and appear on hover in browsers).
Otherwise, renders to an inline base64 PNG.
For other backends it emits a #+begin_src dot block.

Within the block body:

  #+graph: table  ↦ global graph[ ] attributes
  #+node:  table  ↦ default node[ ] attributes
  #+edge:  table  ↦ default edge[ ] attributes
  #+rank:  n1 n2  ↦ { rank=same; n1; n2 } for nodes without a common parent; ids may be [[links]]
  #+begin_cluster TITLE :KEY V … #+end_cluster  ↦ subgraph cluster_TITLE { … }; nests, and may sit in an item
  :NAME: … :END:  ↦ a drawer of any name, folding such directives away
  - :T: :: MEANING  ↦ at the top, a legend entry: a sample of template T, captioned MEANING

A #+name: table outside any graph is a template every graph in the buffer may use; the graph's own wins.
A section tagged :graph: exports as a graph of its text, its :PROPERTIES: drawer serving as the header line.

Each list item declares a node.  Nesting encodes edges (parent → child); ordered children, 1. 2. 3., share a rank
in the order written.  An item's text may hold a <<target>>, its id; [[TARGET]] or [[TARGET][LABEL]] links, extra arcs;
and *bold*, /italic/, _underline_, +strike+, =code=, or ~pre~, which make its label an HTML-like one.
A description tag, `+ L :: CHILD', labels the arc into CHILD; trailing `:T:' tags splice #+name: templates;
an inline footnote, [fn::NOTE], is the node's tooltip; a web or file link, its URL; an [[file:IMAGE.png]], its image;
a checkbox, [X], [-], or [ ], applies the template checked, partial, or unchecked, when one exists; and a `\\\\'
breaks the label's line ---in a table value or #+record: cell, too.
Org's {{{macros}}} expand, by the buffer's #+macro: lines.
A two-column inline table on a list item supplies node attributes.

  | key        | val    |  ↦ a Graphviz attribute of the node; values are quoted for you.
  | :id        | alias  |  ↦ cross-reference name for this node.
  | :edge.KEY  | val    |  ↦ attribute of the arc from the parent; a parallel arc is a second child item.
  | :edge[T].KEY | val  |  ↦ attribute of this item's arc to T, from a [[T]] link; \"rec::port\" reaches a record's port.
  | :inherit   | T₁ T₂  |  ↦ inherit the #+name: templates' rows; explicit rows win.  Also :edge.:inherit, and :T₁:T₂: tags:
                            after an item, its node's; after a reference, or a description term, its arc's.
  A child item that is a lone [[T]] link is a reference to T: an arc to it, styled by the child's :edge.KEY rows.
  | :children-rank | same | ↦ constrain immediate children to the same rank;
                            also accepts min, max, source, sink.
  | shape | cluster |      ↦ make this node an inline subgraph cluster_ID { … }.
  #+record: table       ↦ the node's <TABLE> label, as laid out: a cell's <<p>> is port p, its [[T]] an arc from there;
                            ^ or an empty cell spans, a <l>/<c>/<r> row aligns, and hlines rule.

Example:

  #+begin_graph :rankdir LR
  + A
    + calls :: B
      + returns :: C
  #+end_graph"
  (let* ((dot     (org--graph-raw->dot (org--graph-expand-macros raw-contents) main-arg org--handler-args))
         (outfile (or file (org-graph--named-file org--handler-anchor svg)
                      (format "graph-%s.png" (substring (md5 dot) 0 12)))))
    (pcase backend
      ;; A named graph is a figure: an image link, which the block's #+name: and #+caption: label, as with Babel's results.
      ((guard org--handler-anchor)
       (org-graph--render dot (if (s-ends-with-p ".svg" outfile) "svg" "png") outfile)
       (org-parse (format "[[file:%s]]" outfile)))
      (`html
       (let ((tmp (make-temp-file "org-graph-" nil (if svg ".svg" ".png"))))
         (unwind-protect
             ;; An inline <svg> keeps the tooltips' <title> elements; else, an inline base64 PNG.
             (or (when (org-graph--render dot (if svg "svg" "png") tmp)
                   (if svg (org-graph--svg-contents tmp)
                     (format "<img src=\"data:image/png;base64,%s\" alt=\"graph\" />" (org-graph--png-to-base64 tmp))))
                 ;; dot unavailable, or its output sans <svg>: emit the raw dot source in a <pre>
                 (format "<pre>%s</pre>" (s-replace "<" "&lt;" dot)))
           (delete-file tmp))))
      (_
       ;; `org-parse' lets Org export the src block, rather than emit its raw syntax into the output.
       (org-parse (format "#+begin_src dot :file %s\n%s\n#+end_src" outfile dot))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'org-graph-block)
;;; org-graph-block.el ends here
