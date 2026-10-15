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
;;   #+begin_graph Title :rankdir LR :node.shape box :edge.color red
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
;;   #+rank: lam combinatory       ; { rank=same; lam; combinatory }
;;
;;   #+name: faint                 ; a style template, for :eval rows
;;   | style | dotted |
;;
;;   + <<lam>> *lambda*             ; <<id>>; *bold* /italic/ _u_ +s+ =code= ~pre~ ⇒ HTML label
;;     | shape          | ellipse |  ; node attrs; values are quoted for you; | :id | lam | ≡ <<lam>>
;;     | :children-rank | same    |  ; { rank=same; <immediate children> }
;;     + church                     ; nesting = edge lambda→church
;;       | :edge.label | encodes |   ; arc attrs; a repeated key starts a parallel arc
;;       | label | "Church Encoding\nℕ, Bool, Pairs" |
;;     + lisp [[church][cross]]     ; [[TARGET]] or [[TARGET][LABEL]]: an extra arc
;;       | :edge[church].style | dotted |  ; attrs of this item's arc to church
;;       | :to       | combinatory |  ; explicit non-tree edge; "rec.field" reaches a record field
;;       | :to.:eval | faint       |  ; its attrs, or a template's
;;   + combinatory
;;     1. S                         ; ordered children: one rank, in the order written
;;     2. K
;;   #+end_graph
;;
;; Pressing C-c C-c anywhere inside the block renders a PNG immediately.
;; C-M-i completes, and eldoc explains, table keys and #+begin_graph keys; in any Org buffer's graph
;; blocks, and in the `org-graph-edit-mode' buffer C-c ' opens.  The attributes live in org-graph-attributes.el.
;; Use #+begin_graph directed :file foo.png to fix the output filename.

;;; Code:

(require 's)
(require 'dash)
(require 'cl-lib)
(require 'org)
(require 'org-special-block-extras)
(require 'org-graph-attributes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing helpers

(defconst org-graph-reserved-keys '(":id" ":to" ":eval" ":children-rank")
  "Table keys meaningful to the `graph' block itself, rather than to Graphviz; alongside the prefixes `:edge.', `:edge[TARGET].', and `:to.'.")

(defun org--graph-parse-row (line)
  "LINE ↦ its trimmed non-empty cells, e.g. \"| shape | box |\" ↦ (\"shape\" \"box\"); nil for separator rows."
  (let ((cols (-remove #'s-blank-str-p (-map #'s-trim (s-split "|" line)))))
    (unless (or (null cols) (s-matches-p "^[-+]+$" (car cols)))
      cols)))

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
;;; Preamble extraction

(defun org--graph-extract-preamble (raw)
  "Scan RAW block body and separate preamble directives from the Org list body.

Preamble directives may appear either bare at the top level or wrapped
in a :SETTINGS: … :END: drawer, which Org's native folding can collapse.
Both forms are equivalent; the drawer is purely for visual organisation.

Returns a plist:
  :graph-rows  ↦ list of table row-lists for #+graph:
  :node-rows   ↦ list of table row-lists for #+node:
  :edge-rows   ↦ list of table row-lists for #+edge:
  :ranks       ↦ list of strings, each a space-sep group of node IDs
  :name-tables — hash-table of name → list-of-row-lists (style templates)
  :list-body   — string: the lines that form the Org plain list"
  (let (graph-rows node-rows edge-rows ranks list-lines
        ;; name-table-rows accumulates rows per template in reverse; finalised below.
        (mode nil)
        (in-drawer nil)
        (name-tables (make-hash-table :test #'equal))
        (name-table-rows (make-hash-table :test #'equal))
        (current-name nil))
    (dolist (line (s-split "\n" raw))
      (let ((trimmed (s-trim line)))
        (cond
         ;; Drawer boundaries
         ((s-matches-p "^:SETTINGS:" trimmed) (setq in-drawer t))
         ((s-matches-p "^:END:"      trimmed) (setq in-drawer nil))
         ;; #+graph: / #+node: / #+edge: switch table-collection mode.
         ;; Outside a drawer, only unindented lines qualify (indented ones
         ;; live inside list items and belong to org--graph-extract-item-preamble).
         ((s-matches-p "^#\\+graph:" (if in-drawer trimmed line))
          (setq mode 'graph current-name nil))
         ((s-matches-p "^#\\+node:"  (if in-drawer trimmed line))
          (setq mode 'node current-name nil))
         ((s-matches-p "^#\\+edge:"  (if in-drawer trimmed line))
          (setq mode 'edge current-name nil))
         ;; #+name: id — begin collecting rows into a named style template.
         ;; These lines and their table rows are stripped from :list-body.
         ((s-matches-p "^#\\+name:" (if in-drawer trimmed line))
          (setq mode nil
                current-name (s-trim (substring trimmed (length "#+name:")))))
         ;; #+rank: id1 id2 … — space-sep id list, no table
         ((s-matches-p "^#\\+rank:" (if in-drawer trimmed line))
          (setq mode nil current-name nil)
          (let ((ids (s-trim (substring trimmed (length "#+rank:")))))
            (unless (s-blank-str-p ids)
              (push ids ranks))))
         ;; Table rows dispatched to current mode or current named template.
         ;; Inside a drawer all table rows qualify; outside, only unindented ones.
         ((s-matches-p "^|" (if in-drawer trimmed line))
          (let ((cols (org--graph-parse-row trimmed)))
            (when cols
              (cond
               (current-name
                ;; Accumulate in reverse via push; nreversed into name-tables below.
                (puthash current-name
                         (cons cols (gethash current-name name-table-rows))
                         name-table-rows))
               (mode
                (pcase mode
                  ('graph (push cols graph-rows))
                  ('node  (push cols node-rows))
                  ('edge  (push cols edge-rows))))
               (t (push line list-lines))))))
         ;; Everything else: non-blank lines reset mode; outside a drawer they
         ;; feed :list-body, skipping bare # comments and #+name: directives.
         (t
          (unless (s-blank-str-p trimmed) (setq mode nil current-name nil))
          (unless in-drawer
            (unless (and (s-matches-p "^#" trimmed)
                         (or (not (s-matches-p "^#\\+" trimmed))
                             (s-matches-p "^#\\+name:" trimmed)))
              (push line list-lines)))))))
    ;; Finalise name-tables: nreverse each template's accumulated rows.
    (maphash (lambda (name rev-rows)
               (puthash name (nreverse rev-rows) name-tables))
             name-table-rows)
    (list :graph-rows  (nreverse graph-rows)
          :node-rows   (nreverse node-rows)
          :edge-rows   (nreverse edge-rows)
          :ranks       (nreverse ranks)
          :name-tables name-tables
          :list-body   (s-join "\n" (nreverse list-lines)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Item-level preamble

(defun org--graph-extract-item-preamble (table-part)
  "Parse TABLE-PART (the text after a list item's first line) for both
node-attribute rows and embedded preamble directives.

Unlike the top-level `org--graph-extract-preamble', this operates on
the raw string that `org-list-to-lisp' preserves inside a list item —
which may contain #+node:, #+edge:, #+graph:, and #+rank: directives
interleaved with ordinary | key | value | attribute rows.

Returns a plist:
  :attr-rows   ↦ the item's own | rows: attributes and reserved keys (:id, shape, …)
  :node-rows   ↦ rows collected under #+node:
  :edge-rows   ↦ rows collected under #+edge:
  :graph-rows  ↦ rows collected under #+graph:
  :ranks       ↦ list of space-sep id-group strings from #+rank: lines"
  (let (attr-rows node-rows edge-rows graph-rows ranks
        (mode 'attr))
    (dolist (line (s-split "\n" (or table-part "")))
      (let ((trimmed (s-trim line)))
        (cond
         ((s-matches-p "^#\\+node:"  trimmed) (setq mode 'node))
         ((s-matches-p "^#\\+edge:"  trimmed) (setq mode 'edge))
         ((s-matches-p "^#\\+graph:" trimmed) (setq mode 'graph))
         ((s-matches-p "^#\\+rank:"  trimmed)
          (setq mode 'attr)
          (let ((ids (s-trim (substring trimmed (length "#+rank:")))))
            (unless (s-blank-str-p ids) (push ids ranks))))
         ((s-matches-p "^|" trimmed)
          (let ((cols (org--graph-parse-row trimmed)))
            (when cols
              (pcase mode
                ('attr  (push cols attr-rows))
                ('node  (push cols node-rows))
                ('edge  (push cols edge-rows))
                ('graph (push cols graph-rows))))))
         (t (unless (s-blank-str-p trimmed) (setq mode 'attr))))))
    (list :attr-rows  (nreverse attr-rows)
          :node-rows  (nreverse node-rows)
          :edge-rows  (nreverse edge-rows)
          :graph-rows (nreverse graph-rows)
          :ranks      (nreverse ranks))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing: Org list ⟶ graph statements
;;
;; A parse yields plain data, which `org--graph-stmt->dot' then prints:
;;   (:node ID)                    ---ID's name and attributes live in the context
;;   (:edge SRC DST ATTRS)         ---SRC and DST are raw ids, or "record.field"
;;   (:rank KIND IDS)
;;   (:record ID :attrs ROWS :fields FIELDS)
;;   (:cluster ID :label L :attrs ROWS :scope PREAMBLE :stmts STATEMENTS)

(cl-defstruct (org--graph-ctx (:constructor org--graph-ctx-create))
  "What a parse accumulates, and what printing its statements consults."
  (nodes (make-hash-table :test #'equal))    ; id ↦ (NAME . ATTRS), merged across repeat occurrences
  (records (make-hash-table :test #'equal))  ; record id ↦ t
  (clusters (make-hash-table :test #'equal)) ; cluster id ↦ the first node inside it
  name-tables                                ; template name ↦ rows, from #+name:
  ports                                      ; the (RECORD . PORT) pairs that some arc ends at
  compound)                                  ; non-nil once an arc is clipped at a cluster

(defun org--graph-expand-eval (rows name-tables &optional seen)
  "ROWS with each | :eval NAME | row replaced by template NAME's rows, sans keys ROWS sets explicitly ---wherever :eval sits.
Templates may themselves use :eval; one already being expanded, as listed in SEEN, contributes nothing."
  (let ((explicit (--remove (equal (car it) ":eval") rows)))
    (append (--mapcat (unless (member (cadr it) seen)
                        (--remove (assoc (car it) explicit)
                                  (org--graph-expand-eval (gethash (cadr it) name-tables) name-tables
                                                          (cons (cadr it) seen))))
                      (--filter (equal (car it) ":eval") rows))
            explicit)))

(defun org--graph-expand-scope (scope name-tables)
  "SCOPE, a preamble plist, with :eval rows expanded in its #+graph:, #+node:, and #+edge: tables."
  (dolist (key '(:graph-rows :node-rows :edge-rows) scope)
    (setq scope (plist-put scope key (org--graph-expand-eval (plist-get scope key) name-tables)))))

(defun org--graph-item-rows (rows name-tables)
  "An item's table ROWS ↦ (:id :attrs :edges :tos :targeted :children-rank), routing reserved keys away from node attributes.

  ((\":to\" \"b\") (\":to.style\" \"dotted\") (\"shape\" \"box\"))  ↦  (:attrs ((\"shape\" \"box\")) :tos ((\"b\" (\"style\" \"dotted\"))) …)

`:edge.ATTR' rows describe the arc from the parent; repeating an ATTR starts another, parallel, arc.
`:edge[TARGET].ATTR' rows describe this item's arc to TARGET, be it from a `:to' row or a [[TARGET]] link."
  (let (id attrs edges tos targeted rank)
    (dolist (row (org--graph-expand-eval (--filter (cadr it) rows) name-tables))
      (-let [(key val) row]
        (cond
         ((equal key ":id") (setq id (org--graph-unquote val)))
         ((equal key ":children-rank") (setq rank val))
         ((equal key ":to") (push (list val) tos))
         ((s-starts-with-p ":to." key)
          (unless tos (user-error "graph: `| %s |' needs a preceding `| :to | TARGET |' row" key))
          (setcdr (car tos) (append (cdar tos) (list (list (substring key 4) val)))))
         ((string-match "\\`:edge\\[\\(.+?\\)\\]\\.\\(.+\\)\\'" key)
          (let ((target (org--graph-unquote (match-string 1 key))) (attr (match-string 2 key)))
            (setf (alist-get target targeted nil nil #'equal)
                  (append (alist-get target targeted nil nil #'equal) (list (list attr val))))))
         ((s-starts-with-p ":edge." key)
          (let ((attr (substring key 6)))
            (when (or (null edges) (assoc attr (car edges))) (push nil edges))
            (setcar edges (append (car edges) (list (list attr val))))))
         ((member key '("EDGE" "to" "eval"))
          (user-error "graph: `| %s |' rows are now spelled `:edge.ATTR', `:to', and `:eval'" key))
         ((s-starts-with-p ":" key)
          (user-error "graph: unknown key `%s'; expected one of %s, or :edge.ATTR, :edge[TARGET].ATTR, :to.ATTR"
                      key org-graph-reserved-keys))
         (t (push row attrs)))))
    (list :id id :attrs (nreverse attrs) :children-rank rank
          :edges (--map (org--graph-expand-eval it name-tables) (nreverse edges))
          :tos (--map (cons (car it) (org--graph-expand-eval (cdr it) name-tables)) (nreverse tos))
          :targeted (--map (cons (car it) (org--graph-expand-eval (cdr it) name-tables)) (nreverse targeted)))))

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
  "An item's TEXT ↦ (:name N :label L :target T :links ((PATH . DESCRIPTION) …) :bare-label D).

  \"<<lam>> *λ* [[church][encodes]]\"  ↦  (:name \"λ\" :label \"<<B>λ</B>>\" :target \"lam\" :links ((\"church\" . \"encodes\")))

The name is TEXT sans targets, links, and markup; the label is an HTML-like one when TEXT has markup, else the name.
A TEXT that is a lone [[PATH][D]] names the node PATH, with D labelling the arc from its parent."
  (let* ((objs (org-element-parse-secondary-string text '(bold italic underline strike-through code verbatim link target)))
         (edge-link? (lambda (o) (and (eq (org-element-type o) 'link) (equal (org-element-property :type o) "fuzzy"))))
         (links  (--map (cons (org-element-property :path it)
                              (-some->> (org-element-contents it) (org--graph-item-render nil)))
                        (-filter edge-link? (org-element-map objs 'link #'identity))))
         (target (car (org-element-map objs 'target (lambda (o) (org-element-property :value o)))))
         (name   (s-trim (s-collapse-whitespace (org--graph-item-render nil objs)))))
    (cond
     ((-all? #'stringp objs) (list :name text :label text))
     ((and (s-blank-str-p name) (not target) (= 1 (length links)))
      (list :name (caar links) :label (caar links) :bare-label (cdar links)))
     (t (list :name (if (s-blank-str-p name) target name)
              :label (if (org-element-map objs (mapcar #'car org--graph-markup-tags) #'identity nil t)
                         (format "<%s>" (s-trim (s-collapse-whitespace (org--graph-item-render t objs))))
                       (if (s-blank-str-p name) target name))
              :target target :links links)))))

(defun org--graph-item-render (html? objs)
  "OBJS, a parsed item text, ↦ its plain text; or, when HTML?, its HTML-like label body. Edge links and targets vanish."
  (mapconcat
   (lambda (o)
     (let ((type (org-element-type o)))
       ;; Org keeps an object's trailing spaces apart, as its :post-blank.
       (concat
        (cond
         ((stringp o) (if html? (org--graph-html-escape o) o))
         ((eq type 'target) "")
         ((eq type 'link) (if (equal (org-element-property :type o) "fuzzy") ""
                            (org--graph-item-render html? (or (org-element-contents o) (list (org-element-property :raw-link o))))))
         (t (-let [(open close) (and html? (alist-get type org--graph-markup-tags))]
              (concat open
                      (if (memq type '(code verbatim))
                          (funcall (if html? #'org--graph-html-escape #'identity) (org-element-property :value o))
                        (org--graph-item-render html? (org-element-contents o)))
                      close))))
        (unless (stringp o) (make-string (or (org-element-property :post-blank o) 0) ?\s)))))
   objs ""))

(defun org--graph-item-parts (item)
  "An `org-list-to-lisp' ITEM ↦ (TEXT PREAMBLE CHILDREN KIND): its parsed first line, its table and directives,
its sub-items, and whether they are `ordered' or `unordered'."
  (-let [(name-part body) (s-split-up-to "\n" (s-trim (car item)) 1)]
    (list (org--graph-item-text (s-trim name-part)) (org--graph-extract-item-preamble body)
          (-filter #'consp (cdr (cadr item))) (car-safe (cadr item)))))

(defun org--graph-item-id (item)
  "An `org-list-to-lisp' ITEM ↦ its node id: its | :id | row, else its <<target>>, else its text."
  (-let [(text pre) (org--graph-item-parts item)]
    (or (-some-> (assoc ":id" (plist-get pre :attr-rows)) cadr org--graph-unquote)
        (plist-get text :target)
        (org--graph-unquote (plist-get text :name)))))

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
                       ((or :node :record) (cadr s))
                       (:cluster (org--graph-first-node (plist-get (cddr s) :stmts)))))
         stmts))

(defun org--graph-parse-item (item parent ctx)
  "ITEM, an `org-list-to-lisp' item under PARENT (an id, or nil at the top), ↦ its statements; its nodes accrue in CTX.

The order is: arcs PARENT → ITEM, ITEM's `:to' then [[link]] arcs, ITEM's declaration, then its children's statements."
  (-let* (((text pre children kind) (org--graph-item-parts item))
          (name     (org--graph-unquote (plist-get text :name)))
          (label    (org--graph-unquote (plist-get text :label)))
          (rows     (org--graph-item-rows (plist-get pre :attr-rows) (org--graph-ctx-name-tables ctx)))
          (id       (or (plist-get rows :id) (plist-get text :target) name))
          (attrs    (plist-get rows :attrs))
          (shape    (cadr (assoc "shape" attrs)))
          (fresh    (not (gethash id (org--graph-ctx-nodes ctx))))
          (targeted (plist-get rows :targeted))
          (to-arc   (lambda (target attrs)
                      (list :edge id target (org--graph-override attrs (alist-get target targeted nil nil #'equal)))))
          (arcs     (append (when parent
                              (--map (list :edge parent id
                                           (org--graph-override (-some--> (plist-get text :bare-label) `(("label" ,it))) it))
                                     (or (plist-get rows :edges) '(nil))))
                            (--map (funcall to-arc (car it) (cdr it)) (plist-get rows :tos))
                            (--map (funcall to-arc (car it) (-some--> (cdr it) `(("label" ,it))))
                                   (plist-get text :links)))))
    (-when-let (stray (--remove (or (assoc it (plist-get rows :tos)) (assoc it (plist-get text :links))) (-map #'car targeted)))
      (user-error "graph: `%s' has `:edge[%s].…' rows, but no `:to' row or [[link]] to %s" id (car stray) (car stray)))
    (cond
     ((and fresh (member shape '("record" "Mrecord")))
      (org--graph-declare ctx id name nil)
      (puthash id t (org--graph-ctx-records ctx))
      (-let [(fields . stmts) (org--graph-parse-fields children id ctx)]
        (append arcs (list (list :record id :attrs attrs :fields fields)) stmts)))
     ((and fresh (equal shape "cluster"))
      (org--graph-declare ctx id name nil)
      (let ((stmts (append (--mapcat (org--graph-parse-item it nil ctx) children)
                           (org--graph-ordered-rank kind children))))
        (-some--> (org--graph-first-node stmts) (puthash id it (org--graph-ctx-clusters ctx)))
        ;; The cluster comes first, so that its first node is declared inside it before any arc mentions that node.
        (cons (list :cluster id
                    :label (or (cadr (assoc "label" attrs)) label)
                    :attrs (--remove (member (car it) '("shape" "label")) attrs)
                    :scope (org--graph-expand-scope pre (org--graph-ctx-name-tables ctx))
                    :stmts stmts)
              arcs)))
     (t
      ;; An ordered list keeps its order: `ordering=out' stops Graphviz from reshuffling the arcs to it.
      (let ((ranked (org--graph-ordered-rank kind children)))
        (append arcs
                (org--graph-declare ctx id label (if (and ranked (not (assoc "ordering" attrs)))
                                                     (append attrs '(("ordering" "out")))
                                                   attrs))
                (--mapcat (org--graph-parse-item it id ctx) children)
                (-if-let (rank (plist-get rows :children-rank))
                    (list (list :rank rank (-map #'org--graph-item-id children)))
                  ranked)))))))

(defun org--graph-parse-fields (children id ctx)
  "A record ID's CHILDREN ↦ (FIELDS . STATEMENTS).
FIELDS are (:port P :label L :explicit P-or-nil), or (:group FIELDS) for a `{}' child, which flips the layout.
STATEMENTS are the fields' `:to' arcs, from ID:PORT, and their sub-items, parsed as ordinary top-level items."
  (let (fields stmts)
    (dolist (child children)
      (-let* (((text pre grandchildren) (org--graph-item-parts child))
              (name (plist-get text :name))
              (rows (org--graph-item-rows (plist-get pre :attr-rows) (org--graph-ctx-name-tables ctx)))
              (explicit (or (plist-get rows :id) (plist-get text :target))))
        (if (equal name "{}")
            (-let [(inner . inner-stmts) (org--graph-parse-fields grandchildren id ctx)]
              (push (list :group inner) fields)
              (setq stmts (append stmts inner-stmts)))
          ;; A record's label is plain text, so a field keeps its name sans markup.
          (let ((port (org--graph-dot-port (or explicit name))))
            (push (list :port port :label name :explicit explicit) fields)
            (setq stmts (append stmts
                                (--map (list :edge (format "%s.%s" id port) (car it) (cdr it))
                                       (append (plist-get rows :tos)
                                               (--map (cons (car it) (-some--> (cdr it) `(("label" ,it))))
                                                      (plist-get text :links))))
                                (--mapcat (org--graph-parse-item it nil ctx) grandchildren)))))))
    (cons (nreverse fields) stmts)))

(defun org--graph-parse (raw-contents)
  "RAW-CONTENTS of a #+begin_graph block ↦ (PREAMBLE STATEMENTS CTX)."
  (let* ((raw      (org--graph-extract-preamble raw-contents))
         (preamble (org--graph-expand-scope raw (plist-get raw :name-tables)))
         (ctx      (org--graph-ctx-create :name-tables (plist-get preamble :name-tables)))
         (list     (org--parse-plain-list (s-trim (plist-get preamble :list-body)) "graph"))
         (items    (-filter #'consp (cdr list)))
         (stmts    (append (--mapcat (org--graph-parse-item it nil ctx) items)
                           (org--graph-ordered-rank (car list) items))))
    (list preamble stmts ctx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Printing: graph statements ⟶ dot

(defun org--graph-endpoint (raw ctx)
  "RAW ↦ (NODE . PORT): \"rec.p q\" ↦ (\"rec\" . \"p_q\") when rec is a record in CTX; otherwise RAW names a node, sans PORT.
The record is the first prefix, ending just before a dot, that names a record; so a record may itself be named \"org.el\"."
  (let* ((raw (org--graph-unquote raw))
         (dot (--first (gethash (substring raw 0 it) (org--graph-ctx-records ctx))
                       (-map #'car (s-matched-positions-all "\\." raw)))))
    (if dot
        (cons (substring raw 0 dot) (org--graph-dot-port (substring raw (1+ dot))))
      (list raw))))

(defun org--graph-all-stmts (stmts)
  "STMTS, each cluster followed by its own statements, recursively."
  (--mapcat (cons it (when (eq (car it) :cluster) (org--graph-all-stmts (plist-get (cddr it) :stmts)))) stmts))

(defun org--graph-ports (stmts ctx)
  "The (RECORD . PORT) pairs that the arcs among STMTS end at."
  (--mapcat (when (eq (car it) :edge)
              (--filter (cdr it) (list (org--graph-endpoint (nth 1 it) ctx) (org--graph-endpoint (nth 2 it) ctx))))
            (org--graph-all-stmts stmts)))

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
    (format "%s %s %s%s;" (funcall end src-node src-port) op (funcall end dst-node dst-port)
            (org--graph-attrs->dot (append attrs clips)))))

(defun org--graph-record-label (fields id ctx)
  "A record ID's FIELDS ↦ its label, e.g. \"<f0> left|{b|c}\"; a field has a <port> when it has an :id or some arc ends there."
  (s-join "|" (--map (if (eq (car it) :group)
                         (format "{%s}" (org--graph-record-label (cadr it) id ctx))
                       (let ((port (plist-get it :port)))
                         (concat (when (or (plist-get it :explicit) (member (cons id port) (org--graph-ctx-ports ctx)))
                                   (format "<%s> " port))
                                 (s-replace " " "\\ " (plist-get it :label)))))
                     fields)))

(defun org--graph-scope->dot (scope)
  "SCOPE's :graph-rows, :node-rows, :edge-rows, and :ranks ↦ the dot directives for its scope."
  (append
   (-keep (-lambda ((kind . key))
            (-when-let (rows (plist-get scope key))
              (concat kind (org--graph-attrs->dot rows))))
          '(("graph" . :graph-rows) ("node" . :node-rows) ("edge" . :edge-rows)))
   (--map (format "{ rank=same; %s }" (s-join "; " (-map #'org--graph-dot-id (s-split "[ \t]+" (s-trim it) t))))
          (plist-get scope :ranks))))

(defun org--graph-stmt->dot (stmt op ctx)
  "STMT, a parsed graph statement, ↦ its dot lines; OP is the edge operator, \"->\" or \"--\"."
  (pcase stmt
    (`(:node ,id)
     (-let [(name . attrs) (gethash id (org--graph-ctx-nodes ctx))]
       (list (format "%s%s;" (org--graph-dot-id id)
                     (org--graph-attrs->dot (if (assoc "label" attrs) attrs (append attrs `(("label" ,name)))))))))
    (`(:edge ,src ,dst ,attrs)
     (list (org--graph-edge->dot src dst attrs op ctx)))
    (`(:rank ,kind ,ids)
     (list (format "{ rank=%s; %s }" kind (s-join "; " (-map #'org--graph-dot-id ids)))))
    (`(:record ,id . ,props)
     (let ((attrs (plist-get props :attrs)))
       (list (format "%s%s;" (org--graph-dot-id id)
                     (org--graph-attrs->dot
                      (append (list (assoc "shape" attrs)
                                    (list "label" (format "\"%s\"" (org--graph-record-label (plist-get props :fields) id ctx))))
                              (--remove (equal (car it) "shape") attrs)))))))
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

(defun org--graph-raw->dot (raw-contents main-arg &optional kwdargs)
  "(RAW-CONTENTS, MAIN-ARG, KWDARGS) ↦ the dot source of a #+begin_graph block.

It is a digraph, unless MAIN-ARG is \"undirected\" or KWDARGS has `:directed nil'.  Any other MAIN-ARG is the graph's title,
its label; other KWDARGS are graph attributes, or node and edge defaults when spelled `:node.KEY' and `:edge.KEY'.
Header attributes come before the block's own #+graph:, #+node:, and #+edge: tables, so those tables win."
  (-let* (((preamble stmts ctx) (org--graph-parse raw-contents))
          (directed (if (plist-member kwdargs :directed) (plist-get kwdargs :directed) (not (equal main-arg "undirected"))))
          (title    (unless (member main-arg '(nil "" "directed" "undirected")) main-arg))
          (header   (org--graph-header-rows kwdargs)))
    (setq header (plist-put header :graph-rows (append (when title `(("label" ,title))) (plist-get header :graph-rows))))
    (dolist (key '(:graph-rows :node-rows :edge-rows))
      (setq preamble (plist-put preamble key (org--graph-override (plist-get header key) (plist-get preamble key)))))
    (setf (org--graph-ctx-ports ctx) (org--graph-ports stmts ctx))
    (let ((body (--mapcat (org--graph-stmt->dot it (if directed "->" "--") ctx) stmts)))
      (when (org--graph-ctx-compound ctx)
        (setq preamble (plist-put preamble :graph-rows (append (plist-get preamble :graph-rows) '(("compound" "true"))))))
      (format "%s {\n  %s\n}" (if directed "digraph" "graph")
              (s-join "\n  " (append (org--graph-scope->dot preamble) body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C-c C-c integration

(defun org-graph--insert-results (end-point payload)
  "Insert \"#+RESULTS:\\nPAYLOAD\" on the line after END-POINT, replacing a #+RESULTS: (header + one line) already there."
  (save-excursion
    (goto-char end-point)
    (forward-line 1)
    (when (looking-at "[ \t]*#\\+RESULTS:")
      (delete-region (point) (progn (forward-line 2) (point))))
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
          (let (hits (pos 0))
            (while (string-match "line \\([0-9]+\\)" err-str pos)
              (let ((n (string-to-number (match-string 1 err-str))))
                (when (and (> n 0) (<= n n-lines))
                  (push (format "  line %d: %s" n (s-trim (nth (1- n) src-lines)))
                        hits)))
              (setq pos (match-end 0)))
            (nreverse hits)))
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

(defun org-graph--cc-handler ()
  "Render the #+begin_graph block at point, or show its dot source.

With no prefix argument and no :C-c_C-c kwdarg (or :C-c_C-c png):
  renders a PNG and inserts a #+RESULTS: [[file:…]] link.

With C-u prefix, or when the block carries :C-c_C-c dot:
  generates dot source, displays it in *org-graph dot*, and inserts a
  #+RESULTS: #+begin_src dot … #+end_src block.

The two controls compose via XOR: if :C-c_C-c dot is set, C-u toggles
back to PNG.  Returns t (claiming C-c C-c)."
  (save-excursion
    ;; The block dispatch chose ---the innermost around point--- not merely the nearest #+begin_graph above it.
    (-some--> (org-special-block-at-point) (org-element-property :post-affiliated it) (goto-char it))
    (let* ((block      (org-special-block-after-point "graph"))
           (kwdargs    (org-special-block-kwdargs block))
           (raw        (org-special-block-contents block))
           (main-arg   (or (org-special-block-main-arg block) "directed"))
           (end-pt     (org-special-block-end-point block))
           (block-mode (plist-get kwdargs :C-c_C-c))
           (prefix?    (and current-prefix-arg t))
           (show-dot?  (if (equal block-mode "dot") (not prefix?) prefix?))
           (dot        (org--graph-raw->dot raw main-arg kwdargs)))
      (if show-dot?
          (progn
            (org-graph--show-dot dot)
            (org-graph--insert-results end-pt (format "#+begin_src dot\n%s\n#+end_src" dot))
            (message "Graph dot source → *org-graph dot*"))
        (let ((outfile (or (plist-get kwdargs :file)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive attribute insertion

(defun org-graph--table-context ()
  "Return a symbol for the table context at point within a graph block.

Scans backward past blank lines and table rows to find the closest
preceding structural anchor line, then returns:
  `graphs'   — table follows a #+graph: directive
  `edges'    — table follows a #+edge: directive
  `nodes'    — table follows a #+node: directive, a list item, or anything else"
  (save-excursion
    (beginning-of-line)
    (let ((found nil))
      (while (and (not found) (not (bobp)))
        (forward-line -1)
        (let ((trimmed (s-trim (or (thing-at-point 'line t) ""))))
          (cond
           ((or (s-blank-str-p trimmed)
                (s-matches-p "^|" trimmed))
            nil)
           ((s-matches-p "^#\\+graph:" trimmed)  (setq found 'graphs))
           ((s-matches-p "^#\\+node:"  trimmed)  (setq found 'nodes))
           ((s-matches-p "^#\\+edge:"  trimmed)  (setq found 'edges))
           (t (setq found 'nodes)))))
      (or found 'nodes))))

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
  (when (and arg (org-at-table-p) (org-graph--in-graph-p))
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
  (when (and arg (org-at-table-p) (org-graph--in-graph-p))
    (org-graph--insert-attribute-row arg)
    t))

(advice-add 'org-return :before-until #'org-graph--maybe-insert-attribute-row)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completion and eldoc for table keys, and the #+begin_graph line

(defconst org-graph--reserved-key-docs
  '((":id"            . "This node's dot id, so other items can refer to it; also spelled <<ID>> in the item's text.")
    (":to"            . "An extra arc from this node to TARGET; also spelled [[TARGET]], or [[TARGET][LABEL]], in the item's text.")
    (":to."           . "An attribute of the `:to' arc just above; e.g., | :to.style | dotted |.")
    (":edge."         . "An attribute of the arc from the parent; e.g., | :edge.label | calls |.")
    (":edge[TARGET]." . "An attribute of this node's arc to TARGET; e.g., | :edge[church].style | dotted |.")
    (":eval"          . "Splice in the rows of a #+name: template; this item's own rows win.")
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

(defun org-graph--in-graph-p ()
  "Non-nil when point is in a graph block's body: in one in an Org buffer, or anywhere in its `org-graph-edit-mode' buffer."
  (or (derived-mode-p 'org-graph-edit-mode) (org-special-block-at-point-p "graph")))

(defun org-graph--attributes-on (context)
  "CONTEXT, such as `edges', ↦ the `org-graph-attributes' entries valid on it."
  (--filter (memq context (plist-get it :valid-on)) org-graph-attributes))

(defun org-graph--key-attribute (key)
  "A table or header KEY ↦ (CONTEXT . ATTRIBUTE), e.g. \":edge[x].color\" ↦ (edges . \"color\"),
\":node.shape\" ↦ (nodes . \"shape\"), and \"shape\" ↦ (nil . \"shape\")."
  (cond ((string-match "\\`:\\(?:edge\\(?:\\[[^]]*\\]\\)?\\|to\\)\\.\\(.*\\)\\'" key) (cons 'edges (match-string 1 key)))
        ((string-match "\\`:node\\.\\(.*\\)\\'" key) (cons 'nodes (match-string 1 key)))
        (t (cons nil key))))

(defun org-graph--attribute (key)
  "A table or header KEY ↦ the `org-graph-attributes' entry of the attribute it sets; else nil."
  (let ((name (cdr (org-graph--key-attribute key))))
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
      (org-graph--key-doc (if (string-match-p "\\`:\\(?:node\\|edge\\)\\." key) key (substring key 1)))))

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
      (-some--> (plist-get (org-graph--attribute (if (string-match-p "\\`:\\(?:node\\|edge\\)\\." previous)
                                                     previous
                                                   (substring previous 1)))
                           :values)
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
  "Complete a graph table's key, or value, from `org-graph-attributes'; after `:edge.', `:edge[T].', or `:to.', only edge ones.
On a #+begin_graph line, complete its keys and values instead."
  (or
   (org-graph--header-completion)
   ;; The cheap table check first: asking whether we are in a graph parses.
   (-when-let ((column beg end key) (and (org-at-table-p) (org-graph--in-graph-p) (org-graph--cell-at-point)))
     (pcase column
       (1 (let* ((typed  (buffer-substring beg (point)))
                 (prefix (and (string-match "\\`:\\(?:edge\\(?:\\[[^]]*\\]\\)?\\|to\\)\\." typed) (match-string 0 typed))))
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
    (-when-let* (((_ _ _ key) (and (org-at-table-p) (org-graph--in-graph-p) (org-graph--cell-at-point)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Every Org buffer

(defun org-graph--setup-org-buffer ()
  "In this Org buffer's graph blocks: show links verbatim, and complete and explain keys, with C-M-i and eldoc."
  (font-lock-add-keywords nil '((org-graph--reveal-links)) 'append)
  (add-hook 'completion-at-point-functions #'org-graph--completion-at-point nil t)
  (add-hook 'eldoc-documentation-functions #'org-graph--eldoc nil t))

(add-hook 'org-mode-hook #'org-graph--setup-org-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-defblock registration

(defun org-graph--render-to-png (dot outfile)
  "Render DOT source to OUTFILE (PNG) via the dot command.
Returns OUTFILE on success, nil on failure (error logged to *Warnings*)."
  (-let [(exit . err-str) (org-graph--run-dot dot "png" outfile)]
    (if (zerop exit)
        outfile
      (warn "org-graph: dot failed rendering PNG:\n%s" (org-graph--annotate-dot-error err-str dot))
      nil)))

(defun org-graph--png-to-base64 (pngfile)
  "Return a base64 string of the binary contents of PNGFILE."
  (base64-encode-string
   (with-temp-buffer
     (set-buffer-multibyte nil)
     (insert-file-contents-literally pngfile)
     (buffer-string))
   t))

(defun org-graph--render-to-svg (dot outfile)
  "Render DOT source to OUTFILE (SVG) via the dot command.
Returns OUTFILE on success, nil on failure (error logged to *Warnings*)."
  (-let [(exit . err-str) (org-graph--run-dot dot "svg" outfile)]
    (if (zerop exit)
        outfile
      (warn "org-graph: dot failed rendering SVG:\n%s" (org-graph--annotate-dot-error err-str dot))
      nil)))

(defun org-graph--svg-contents (svgfile)
  "Return the <svg>…</svg> string from SVGFILE, stripping the XML preamble."
  (with-temp-buffer
    (insert-file-contents svgfile)
    (goto-char (point-min))
    (when (re-search-forward "<svg[ \t\n]" nil t)
      (goto-char (match-beginning 0))
      (buffer-substring (point) (point-max)))))

(org-defblock graph (main-arg "directed" file nil svg nil :edit-indirect-mode org-graph-edit-mode)
  "Render an Org list + tables as a Graphviz dot graph.

MAIN-ARG is \"directed\" (default, produces digraph), \"undirected\" (graph), or the graph's title, its label.
The header's other keys are graph attributes, such as :rankdir LR; :node.KEY and :edge.KEY set node and edge defaults;
and :directed nil makes an undirected graph.  Pass :file foo.png on the #+begin_graph line to fix the output filename;
otherwise the name is graph-HASH.png, HASH being that of the dot source,
so re-exporting an unchanged graph rewrites one file rather than adding another.

For HTML export: if :svg t is set, embeds an inline <svg> element (tooltips
are preserved as <title> attributes and appear on hover in browsers).
Otherwise, renders to an inline base64 PNG.
For other backends it emits a #+begin_src dot block.

Within the block body:

  #+graph: table  ↦ global graph[ ] attributes
  #+node:  table  ↦ default node[ ] attributes
  #+edge:  table  ↦ default edge[ ] attributes
  #+rank:  n1 n2  ↦ { rank=same; n1; n2 } for nodes without a common parent

Each list item declares a node.  Nesting encodes edges (parent → child); ordered children, 1. 2. 3., share a rank
in the order written.  An item's text may hold a <<target>>, its id; [[TARGET]] or [[TARGET][LABEL]] links, extra arcs;
and *bold*, /italic/, _underline_, +strike+, =code=, or ~pre~, which make its label an HTML-like one.
A two-column inline table on a list item supplies node attributes.

  | key        | val    |  ↦ a Graphviz attribute of the node; values are quoted for you.
  | :id        | alias  |  ↦ cross-reference name for this node.
  | :edge.KEY  | val    |  ↦ attribute of the arc from the parent; repeating a KEY starts a parallel arc.
  | :to        | target |  ↦ explicit outgoing edge; target \"rec.field\" reaches a record's field.
  | :to.KEY    | val    |  ↦ attribute of the :to edge just above.
  | :edge[T].KEY | val  |  ↦ attribute of this item's arc to T, from a :to row or a [[T]] link.
  | :eval      | T      |  ↦ splice the #+name: T template; explicit rows win.  Also :edge.:eval, :to.:eval.
  | :children-rank | same | ↦ constrain immediate children to the same rank;
                            also accepts min, max, source, sink.
  | shape | cluster |      ↦ make this node an inline subgraph cluster_ID { … }.
  | shape | record  |      ↦ children become fields; a child named {} flips the layout.

Example:

  #+begin_graph directed
  #+graph:
  | rankdir | LR |

  + A
    + B
      | :edge.label | calls |
      + C
        | :edge.label | returns |
  #+end_graph"
  (let* ((dot     (org--graph-raw->dot raw-contents main-arg org--handler-args))
         (outfile (or file
                      (format "graph-%s.png" (substring (md5 dot) 0 12)))))
    (pcase backend
      (`html
       (if svg
           ;; SVG path: inline <svg> so tooltip <title> elements are preserved
           (let ((svgfile (make-temp-file "org-graph-" nil ".svg")))
             (if (org-graph--render-to-svg dot svgfile)
                 (or (org-graph--svg-contents svgfile)
                     (format "<pre>%s</pre>" (s-replace "<" "&lt;" dot)))
               (format "<pre>%s</pre>" (s-replace "<" "&lt;" dot))))
         ;; PNG path (default)
         (let ((pngfile (make-temp-file "org-graph-" nil ".png")))
           (if (org-graph--render-to-png dot pngfile)
               (format "<img src=\"data:image/png;base64,%s\" alt=\"graph\" />"
                       (org-graph--png-to-base64 pngfile))
             ;; dot unavailable — emit the raw dot source in a <pre>
             (format "<pre>%s</pre>" (s-replace "<" "&lt;" dot))))))
      (_
       ;; `org-parse' lets Org export the src block, rather than emit its raw syntax into the output.
       (org-parse (format "#+begin_src dot :file %s\n%s\n#+end_src" outfile dot))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'org-graph-block)
;;; org-graph-block.el ends here
