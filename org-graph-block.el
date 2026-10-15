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
;;   #+begin_graph directed        ; main-arg: directed (default) | undirected
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
;;   #+rank: lambda combinatory    ; { rank=same; lambda; combinatory }
;;
;;   #+name: faint                 ; a style template, for :eval rows
;;   | style | dotted |
;;
;;   + lambda
;;     | :id            | lam     |  ; cross-ref alias
;;     | shape          | ellipse |  ; node attrs; values are quoted for you
;;     | :children-rank | same    |  ; { rank=same; <immediate children> }
;;     + church                     ; nesting = edge lambda→church
;;       | :edge.label | encodes |   ; arc attrs; a repeated key starts a parallel arc
;;       | label | "Church Encoding\nℕ, Bool, Pairs" |
;;     + lisp
;;       | :to       | church |      ; explicit non-tree edge; "rec.field" reaches a record field
;;       | :to.label | cross  |      ; its attrs
;;       | :to.:eval | faint  |      ; …or a template's
;;   #+end_graph
;;
;; Pressing C-c C-c anywhere inside the block renders a PNG immediately.
;; Use #+begin_graph directed :file foo.png to fix the output filename.

;;; Code:

(require 's)
(require 'dash)
(require 'cl-lib)
(require 'org)
(require 'org-special-block-extras)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Attribute database

(defcustom org-graph-attributes
  '((:name "label"
     :description "Text label attached to the object."
     :values nil
     :valid-on (nodes edges graphs clusters))
    (:name "shape"
     :description "Shape of the node."
     :values ("box" "ellipse" "circle" "diamond" "parallelogram"
              "hexagon" "octagon" "plaintext" "point" "triangle"
              "invtriangle" "trapezium" "invtrapezium" "doubleoctagon"
              "Mrecord" "record")
     :valid-on (nodes))
    (:name "rank"
     :description "Constrain immediate children to a common rank in the layout."
     :values ("same" "min" "max" "source" "sink")
     :valid-on (nodes))
    (:name "color"
     :description "Border/line color (name or #rrggbb)."
     :values nil
     :valid-on (nodes edges clusters graphs))
    (:name "fillcolor"
     :description "Background fill color (name or #rrggbb)."
     :values nil
     :valid-on (nodes clusters))
    (:name "style"
     :description "Drawing style."
     :values ("filled" "dashed" "dotted" "solid" "bold" "rounded"
              "filled,rounded" "invis")
     :valid-on (nodes edges clusters graphs))
    (:name "fontname"
     :description "Font family (e.g. Helvetica, Courier)."
     :values nil
     :valid-on (nodes edges graphs clusters))
    (:name "fontsize"
     :description "Font size in points."
     :values nil
     :valid-on (nodes edges graphs clusters))
    (:name "fontcolor"
     :description "Font color (name or #rrggbb)."
     :values nil
     :valid-on (nodes edges graphs clusters))
    (:name "width"
     :description "Minimum node width in inches."
     :values nil
     :valid-on (nodes))
    (:name "height"
     :description "Minimum node height in inches."
     :values nil
     :valid-on (nodes))
    (:name "penwidth"
     :description "Width of the drawing pen (border or edge line)."
     :values nil
     :valid-on (nodes edges))
    (:name "URL"
     :description "URL associated with the node (SVG/HTML output)."
     :values nil
     :valid-on (nodes))
    (:name "tooltip"
     :description "Tooltip string on mouse-over (SVG/HTML output)."
     :values nil
     :valid-on (nodes))
    (:name "href"
     :description "Hyperlink URL; synonym for URL in most output formats."
     :values nil
     :valid-on (nodes))
    (:name "arrowhead"
     :description "Arrow style at the head of the edge."
     :values ("normal" "vee" "open" "diamond" "dot" "none" "box"
              "odiamond" "ediamond" "inv" "invdot" "invodot" "crow")
     :valid-on (edges))
    (:name "arrowtail"
     :description "Arrow style at the tail of the edge."
     :values ("normal" "vee" "open" "diamond" "dot" "none" "box"
              "odiamond" "inv" "crow")
     :valid-on (edges))
    (:name "arrowsize"
     :description "Scaling factor for arrowhead size."
     :values nil
     :valid-on (edges))
    (:name "dir"
     :description "Direction of arrows on the edge."
     :values ("forward" "back" "both" "none")
     :valid-on (edges))
    (:name "constraint"
     :description "If false, edge does not constrain rank assignment."
     :values ("true" "false")
     :valid-on (edges))
    (:name "rankdir"
     :description "Direction of graph layout."
     :values ("TB" "BT" "LR" "RL")
     :valid-on (graphs))
    (:name "ranksep"
     :description "Separation between ranks (inches or \"equally\")."
     :values nil
     :valid-on (graphs))
    (:name "nodesep"
     :description "Minimum separation between nodes on the same rank (inches)."
     :values nil
     :valid-on (graphs))
    (:name "bgcolor"
     :description "Background color of the graph or cluster canvas."
     :values nil
     :valid-on (graphs clusters))
    (:name "labelloc"
     :description "Placement of graph/cluster label."
     :values ("t" "b" "c")
     :valid-on (graphs clusters))
    (:name "splines"
     :description "How edges are drawn."
     :values ("ortho" "curved" "line" "polyline" "none" "spline")
     :valid-on (graphs))
    (:name "pad"
     :description "Padding (inches) added around the graph drawing."
     :values nil
     :valid-on (graphs))
    (:name "concentrate"
     :description "Merge multi-edges into a single line."
     :values ("true" "false")
     :valid-on (graphs)))
  "Known Graphviz attributes for `org-graph--insert-attribute-row' completion.

Each entry is a plist with:
  :name        — attribute name string
  :description — one-line human description
  :values      — list of completion candidates, or nil for free-text entry
  :valid-on    — context symbols: any subset of (nodes edges graphs clusters)"
  :type '(repeat
          (plist :options
                 ((:name        string)
                  (:description string)
                  (:values      (choice (const nil) (repeat string)))
                  (:valid-on    (repeat (choice (const nodes)
                                                (const edges)
                                                (const graphs)
                                                (const clusters)))))))
  :group 'org-special-block-extras)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing helpers

(defconst org-graph-reserved-keys '(":id" ":to" ":eval" ":children-rank")
  "Table keys meaningful to the `graph' block itself, rather than to Graphviz; alongside the prefixes `:edge.' and `:to.'.")

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
  "An item's table ROWS ↦ (:id :attrs :edges :tos :children-rank), routing reserved keys away from node attributes.

  ((\":to\" \"b\") (\":to.style\" \"dotted\") (\"shape\" \"box\"))  ↦  (:attrs ((\"shape\" \"box\")) :tos ((\"b\" (\"style\" \"dotted\"))) …)

`:edge.ATTR' rows describe the arc from the parent; repeating an ATTR starts another, parallel, arc."
  (let (id attrs edges tos rank)
    (dolist (row (org--graph-expand-eval (--filter (cadr it) rows) name-tables))
      (-let [(key val) row]
        (cond
         ((equal key ":id") (setq id (org--graph-unquote val)))
         ((equal key ":children-rank") (setq rank val))
         ((equal key ":to") (push (list val) tos))
         ((s-starts-with-p ":to." key)
          (unless tos (user-error "graph: `| %s |' needs a preceding `| :to | TARGET |' row" key))
          (setcdr (car tos) (append (cdar tos) (list (list (substring key 4) val)))))
         ((s-starts-with-p ":edge." key)
          (let ((attr (substring key 6)))
            (when (or (null edges) (assoc attr (car edges))) (push nil edges))
            (setcar edges (append (car edges) (list (list attr val))))))
         ((member key '("EDGE" "to" "eval"))
          (user-error "graph: `| %s |' rows are now spelled `:edge.ATTR', `:to', and `:eval'" key))
         ((s-starts-with-p ":" key)
          (user-error "graph: unknown key `%s'; expected one of %s, or :edge.ATTR, :to.ATTR" key org-graph-reserved-keys))
         (t (push row attrs)))))
    (list :id id :attrs (nreverse attrs) :children-rank rank
          :edges (--map (org--graph-expand-eval it name-tables) (nreverse edges))
          :tos (--map (cons (car it) (org--graph-expand-eval (cdr it) name-tables)) (nreverse tos)))))

(defun org--graph-item-parts (item)
  "An `org-list-to-lisp' ITEM ↦ (NAME PREAMBLE CHILDREN): its first line, its table and directives, and its sub-items."
  (-let [(name-part body) (s-split-up-to "\n" (s-trim (car item)) 1)]
    (list (s-trim name-part) (org--graph-extract-item-preamble body) (-filter #'consp (cdr (cadr item))))))

(defun org--graph-item-id (item)
  "An `org-list-to-lisp' ITEM ↦ its node id: its | :id | row, else its text."
  (-let [(name pre) (org--graph-item-parts item)]
    (or (-some-> (assoc ":id" (plist-get pre :attr-rows)) cadr org--graph-unquote)
        (org--graph-unquote name))))

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

The order is: arcs PARENT → ITEM, ITEM's `:to' arcs, ITEM's declaration, then its children's statements."
  (-let* (((name pre children) (org--graph-item-parts item))
          (name     (org--graph-unquote name))
          (rows     (org--graph-item-rows (plist-get pre :attr-rows) (org--graph-ctx-name-tables ctx)))
          (id       (or (plist-get rows :id) name))
          (attrs    (plist-get rows :attrs))
          (shape    (cadr (assoc "shape" attrs)))
          (fresh    (not (gethash id (org--graph-ctx-nodes ctx))))
          (arcs     (append (when parent
                              (--map (list :edge parent id it) (or (plist-get rows :edges) '(nil))))
                            (--map (list :edge id (car it) (cdr it)) (plist-get rows :tos)))))
    (cond
     ((and fresh (member shape '("record" "Mrecord")))
      (org--graph-declare ctx id name nil)
      (puthash id t (org--graph-ctx-records ctx))
      (-let [(fields . stmts) (org--graph-parse-fields children id ctx)]
        (append arcs (list (list :record id :attrs attrs :fields fields)) stmts)))
     ((and fresh (equal shape "cluster"))
      (org--graph-declare ctx id name nil)
      (let ((stmts (--mapcat (org--graph-parse-item it nil ctx) children)))
        (-some--> (org--graph-first-node stmts) (puthash id it (org--graph-ctx-clusters ctx)))
        ;; The cluster comes first, so that its first node is declared inside it before any arc mentions that node.
        (cons (list :cluster id
                    :label (or (cadr (assoc "label" attrs)) name)
                    :attrs (--remove (member (car it) '("shape" "label")) attrs)
                    :scope (org--graph-expand-scope pre (org--graph-ctx-name-tables ctx))
                    :stmts stmts)
              arcs)))
     (t
      (append arcs
              (org--graph-declare ctx id name attrs)
              (--mapcat (org--graph-parse-item it id ctx) children)
              (-some--> (plist-get rows :children-rank)
                (list (list :rank it (-map #'org--graph-item-id children)))))))))

(defun org--graph-parse-fields (children id ctx)
  "A record ID's CHILDREN ↦ (FIELDS . STATEMENTS).
FIELDS are (:port P :label L :explicit P-or-nil), or (:group FIELDS) for a `{}' child, which flips the layout.
STATEMENTS are the fields' `:to' arcs, from ID:PORT, and their sub-items, parsed as ordinary top-level items."
  (let (fields stmts)
    (dolist (child children)
      (-let* (((name pre grandchildren) (org--graph-item-parts child))
              (rows (org--graph-item-rows (plist-get pre :attr-rows) (org--graph-ctx-name-tables ctx))))
        (if (equal name "{}")
            (-let [(inner . inner-stmts) (org--graph-parse-fields grandchildren id ctx)]
              (push (list :group inner) fields)
              (setq stmts (append stmts inner-stmts)))
          (let ((port (org--graph-dot-port (or (plist-get rows :id) name))))
            (push (list :port port :label name :explicit (plist-get rows :id)) fields)
            (setq stmts (append stmts
                                (--map (list :edge (format "%s.%s" id port) (car it) (cdr it)) (plist-get rows :tos))
                                (--mapcat (org--graph-parse-item it nil ctx) grandchildren)))))))
    (cons (nreverse fields) stmts)))

(defun org--graph-parse (raw-contents)
  "RAW-CONTENTS of a #+begin_graph block ↦ (PREAMBLE STATEMENTS CTX)."
  (let* ((raw      (org--graph-extract-preamble raw-contents))
         (preamble (org--graph-expand-scope raw (plist-get raw :name-tables)))
         (ctx      (org--graph-ctx-create :name-tables (plist-get preamble :name-tables)))
         (list     (org--parse-plain-list (s-trim (plist-get preamble :list-body)) "graph"))
         (stmts    (--mapcat (org--graph-parse-item it nil ctx) (-filter #'consp (cdr list)))))
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

(defun org--graph-raw->dot (raw-contents main-arg)
  "(RAW-CONTENTS, MAIN-ARG) ↦ the dot source of a #+begin_graph block: a digraph, or a graph when MAIN-ARG is \"undirected\"."
  (-let* (((preamble stmts ctx) (org--graph-parse raw-contents))
          (directed (not (equal main-arg "undirected"))))
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
           (dot        (org--graph-raw->dot raw main-arg)))
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
  (when (and arg (org-at-table-p) (org-special-block-at-point-p "graph"))
    (let* ((context (org-graph--table-context))
           (valid   (--filter (memq context (plist-get it :valid-on))
                              org-graph-attributes))
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
  (when (and arg (org-at-table-p) (org-special-block-at-point-p "graph"))
    (org-graph--insert-attribute-row arg)
    t))

(advice-add 'org-return :before-until #'org-graph--maybe-insert-attribute-row)

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

(org-defblock graph (main-arg "directed" file nil svg nil :edit-indirect-mode org-mode)
  "Render an Org list + tables as a Graphviz dot graph.

MAIN-ARG is \"directed\" (default, produces digraph) or \"undirected\" (graph).
Pass :file foo.png on the #+begin_graph line to fix the output filename;
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

Each list item declares a node.  Nesting encodes edges (parent → child).
A two-column inline table on a list item supplies node attributes.

  | key        | val    |  ↦ a Graphviz attribute of the node; values are quoted for you.
  | :id        | alias  |  ↦ cross-reference name for this node.
  | :edge.KEY  | val    |  ↦ attribute of the arc from the parent; repeating a KEY starts a parallel arc.
  | :to        | target |  ↦ explicit outgoing edge; target \"rec.field\" reaches a record's field.
  | :to.KEY    | val    |  ↦ attribute of the :to edge just above.
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
  (let* ((dot     (org--graph-raw->dot raw-contents main-arg))
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
