;;; org-special-block-extras-graph.el --- #+begin_graph: Org lists → Graphviz dot  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "27.1") (org "9.1") (org-special-block-extras "4.1.1"))
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
;;   + lambda
;;     | id      | lam     |       ; cross-ref alias
;;     | shape   | ellipse |       ; node attrs
;;     | rank    | same    |       ; { rank=same; <immediate children> }
;;     + church                    ; nesting = edge lambda→church
;;       | EDGE | label=encodes |  ; arc attrs (N rows = N parallel arcs)
;;       | id   | church     |
;;       | label | "Church Encoding\nℕ, Bool, Pairs" |
;;     + lisp
;;       | to | church | label=cross style=dotted |  ; explicit non-tree edge (3-col)
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

(defun org--graph-parse-table (s)
  "Parse a 2-or-3-col Org table string S into a list of row lists.
Each row is a list of trimmed non-empty cell strings.
Separator rows (cells containing only dashes/pluses) are skipped.
Returns a list of (key value) or (key value extra) lists."
  (when (and s (not (s-blank-str-p s)))
    (--keep
     (let ((cols (->> it
                      (s-split "|")
                      (-map #'s-trim)
                      (-remove #'s-blank-str-p))))
       (when (and cols
                  (not (s-matches-p "^[-+]+$" (car cols))))
         cols))
     (s-split "\n" (s-trim s)))))

(defun org--graph-parse-attrs (str)
  "Parse attr string STR like 'label=\"foo bar\" style=dashed' into a dot fragment.
Returns a space-joined string of key=value pairs, suitable for insertion
inside a dot [ … ] attribute list."
  (let ((result '())
        (rest (s-trim (or str ""))))
    (while (not (s-blank-str-p rest))
      (cond
       ;; key="quoted value"
       ((s-matches-p "^[a-zA-Z_][a-zA-Z_0-9]*=\"[^\"]*\"" rest)
        (-let [(_ k v tail)
               (s-match "^\\([a-zA-Z_][a-zA-Z_0-9]*\\)=\\(\"[^\"]*\"\\)\\(.*\\)" rest)]
          (push (format "%s=%s" k v) result)
          (setq rest (s-trim tail))))
       ;; key=unquoted-token
       ((s-matches-p "^[a-zA-Z_][a-zA-Z_0-9]*=[^ ]+" rest)
        (-let [(_ k v tail)
               (s-match "^\\([a-zA-Z_][a-zA-Z_0-9]*\\)=\\([^ ]+\\)\\(.*\\)" rest)]
          (push (format "%s=%s" k v) result)
          (setq rest (s-trim tail))))
       ;; skip unrecognised token
       (t (setq rest (s-trim (if (> (length rest) 1)
                                 (substring rest 1)
                               ""))))))
    (s-join " " (nreverse result))))

(defun org--graph-attrs->dot (rows)
  "Render a list of (key value) row-lists as a dot attribute string.
Returns \"key=value key=value …\" — no surrounding brackets."
  (when rows
    (s-join " "
            (--map (format "%s=%s" (car it) (cadr it)) rows))))

(defun org--graph-fetch-template-rows (names name-tables exclude-keys)
  "Return template rows for each name in NAMES, excluding any key in EXCLUDE-KEYS.
NAMES is a list of strings; NAME-TABLES is the block's template hash-table;
EXCLUDE-KEYS is a list of key strings that should not appear in the result."
  (--mapcat (--remove (member (car it) exclude-keys)
                      (or (gethash (s-trim it) name-tables) nil))
            names))

(defun org--graph-expand-eval-rows (rows name-tables)
  "Expand any \\='(\"eval\" NAME) rows in ROWS using NAME-TABLES.
Each eval row is replaced by the named template's rows, but only for
keys not already present in the explicit rows. Explicit rows always win
over template rows — the position of | eval | within the table does not matter."
  (let* ((eval-rows     (--filter (equal (car it) "eval") rows))
         (explicit-rows (--remove (equal (car it) "eval") rows))
         (template-rows (org--graph-fetch-template-rows
                         (--map (cadr it) eval-rows)
                         name-tables
                         (--map (car it) explicit-rows))))
    (append template-rows explicit-rows)))

(defun org--graph-expand-eval-in-attr-str (str name-tables)
  "Replace (eval NAME) tokens in STR with dot attr pairs from the named template.
Any explicit key=val tokens in STR override same-key pairs from the template.
Returns STR unchanged when it contains no (eval …) tokens."
  (if (or (null str) (not (s-contains-p "(eval " str)))
      str
    (let* ((evals        (s-match-strings-all "(eval \\([a-zA-Z_][a-zA-Z_0-9-]*\\))" str))
           (explicit-str (s-trim (replace-regexp-in-string
                                  "(eval [a-zA-Z_][a-zA-Z_0-9-]*)" "" str)))
           (explicit-keys (->> (s-split " " explicit-str t)
                               (--filter (s-matches-p "[a-zA-Z_][a-zA-Z_0-9]*=" it))
                               (--map (car (s-split "=" it)))))
           (template-str  (org--graph-attrs->dot
                           (org--graph-fetch-template-rows
                            (--map (cadr it) evals)
                            name-tables
                            explicit-keys))))
      (s-trim (s-join " " (-remove #'s-blank-str-p
                                   (list template-str explicit-str)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Preamble extraction

(defun org--graph-extract-preamble (raw)
  "Scan RAW block body and separate preamble directives from the Org list body.

Preamble directives may appear either bare at the top level or wrapped
in a :SETTINGS: … :END: drawer, which Org's native folding can collapse.
Both forms are equivalent; the drawer is purely for visual organisation.

Returns a plist:
  :graph-rows  — list of table row-lists for #+graph:
  :node-rows   — list of table row-lists for #+node:
  :edge-rows   — list of table row-lists for #+edge:
  :ranks       — list of strings, each a space-sep group of node IDs
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
          (let ((cols (->> trimmed
                           (s-split "|")
                           (-map #'s-trim)
                           (-remove #'s-blank-str-p))))
            (when (and cols (not (s-matches-p "^[-+]+$" (car cols))))
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
;;; Item-level preamble (for cluster nodes)

(defun org--graph-extract-item-preamble (table-part)
  "Parse TABLE-PART (the text after a list item's first line) for both
node-attribute rows and embedded preamble directives.

Unlike the top-level `org--graph-extract-preamble', this operates on
the raw string that `org-list-to-lisp' preserves inside a list item —
which may contain #+node:, #+edge:, #+graph:, and #+rank: directives
interleaved with ordinary | key | value | attribute rows.

Returns a plist:
  :attr-rows   — | rows that are genuine node attributes (id, shape, …)
  :node-rows   — rows collected under #+node:
  :edge-rows   — rows collected under #+edge:
  :graph-rows  — rows collected under #+graph:
  :ranks       — list of space-sep id-group strings from #+rank: lines"
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
          (let ((cols (->> trimmed
                           (s-split "|")
                           (-map #'s-trim)
                           (-remove #'s-blank-str-p))))
            (when (and cols (not (s-matches-p "^[-+]+$" (car cols))))
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
;;; Record-node helpers

(defun org--graph-record-shape-p (node-attrs)
  "Return non-nil when NODE-ATTRS declare shape=record or shape=Mrecord."
  (let ((shape-row (--first (equal (car it) "shape") node-attrs)))
    (and shape-row (member (cadr shape-row) '("record" "Mrecord")))))

(defun org--graph-cluster-shape-p (node-attrs)
  "Return non-nil when NODE-ATTRS declare shape=cluster."
  (let ((shape-row (--first (equal (car it) "shape") node-attrs)))
    (and shape-row (equal (cadr shape-row) "cluster"))))

(defun org--graph-build-port-index (parsed)
  "Pre-pass: return a hash-table mapping port-id → record-node-id.

PARSED is the org-list-to-lisp output.  We walk every top-level item;
when an item carries shape=record/Mrecord its immediate children each
contribute their port (id row value, else field name) → parent node-id.
Children named \"{}\" are layout-flip groups — we descend into them to
register any ports they contain under the same record node-id."
  (let ((index (make-hash-table :test #'equal)))
    (cl-labels
        ;; Register all ports reachable from FIELD-CHILDREN under OWNER-ID,
        ;; descending into {} groups transparently.
        ((index-fields (field-children owner-id)
           (dolist (child field-children)
             (when (and child (not (symbolp child)) (not (symbolp (car child))))
               (-let* (((c-raw c-grandchildren) child)
                       ((c-name c-table) (s-split-up-to "\n" (s-trim c-raw) 1))
                       (field-name (s-trim c-name))
                       (c-rows     (org--graph-parse-table c-table))
                       (c-id       (--first (equal (car it) "id") c-rows)))
                 (if (equal field-name "{}")
                     ;; {} group: descend without registering a port for the group itself
                     (index-fields c-grandchildren owner-id)
                   (let ((port (org--graph-dot-port (or (cadr c-id) field-name))))
                     (puthash port owner-id index)))))))
         (walk (lst)
           (when (and lst (not (symbolp lst)) (not (symbolp (car lst))))
             (-let* (((raw-text children) lst)
                     ((name-part table-part) (s-split-up-to "\n" (s-trim raw-text) 1))
                     (node-name  (s-trim name-part))
                     (table-rows (org--graph-parse-table table-part))
                     (id-row     (--first (equal (car it) "id") table-rows))
                     (node-attrs (--filter (not (member (car it) '("id" "to" "EDGE" "cluster")))
                                           table-rows))
                     (node-id    (or (cadr id-row) node-name)))
               (when (org--graph-record-shape-p node-attrs)
                 (index-fields children node-id))
               ;; recurse regardless — grandchildren may themselves be records
               (dolist (child children) (walk child))))))
      (dolist (item (cdr parsed)) (walk item)))
    index))

(defun org--graph-dot-port (port)
  "Return PORT as a valid dot port identifier.
Port names appear in two contexts with different quoting rules:
  • Inside a record label string: <portname> — must be a bare token,
    no quotes allowed inside the angle brackets.
  • In edge statements: node:port — follows normal dot ID quoting.
We normalise by replacing spaces with underscores so the same string
is safe in both contexts without any quoting."
  (s-replace " " "_" port))

(defun org--graph-resolve-port-target (target port-index)
  "Resolve TARGET to \"node:port\" form if TARGET is a known port in PORT-INDEX.
If TARGET is already in \"node:port\" form, or not in the index, return as-is."
  (if (s-contains-p ":" target)
      target
    (let* ((norm-port (org--graph-dot-port target))
           (owner     (gethash norm-port port-index)))
      (if owner (format "%s:%s" owner norm-port) target))))

(defun org--graph-record-label-fragment (children node-id seen-nodes cluster-membership port-index)
  "Recursively build a Graphviz record label fragment from CHILDREN.

Returns a plist:
  :label        — string of `|'-separated field entries, with nested
                  `{ … }' groups for `{}' layout-flip children.
  :edges        — list of dot edge statement strings for `to' rows.
  :grandchildren — list of dot statement strings for grandchildren of
                   real fields (those recurse as ordinary nodes).

NODE-ID is the enclosing record node's dot id, the source for `to' edges.
A child named \"{}\" contributes `{fragment}' to the label and has no
port or edges of its own; its children are folded in recursively."
  (let (parts edges grandchild-stmts)
    (dolist (child children)
      (when (and child (not (symbolp child)) (not (symbolp (car child))))
        (-let* (((raw-text grandchildren) child)
                ((name-part table-part) (s-split-up-to "\n" (s-trim raw-text) 1))
                (field-name (s-trim name-part))
                (table-rows (org--graph-parse-table table-part))
                (id-row     (--first (equal (car it) "id") table-rows))
                (to-rows    (--filter (equal (car it) "to") table-rows)))
          (if (equal field-name "{}")
              ;; Layout-flip group: recurse, wrap fragment in { }
              (let ((inner (org--graph-record-label-fragment
                            grandchildren node-id seen-nodes cluster-membership port-index)))
                (push (format "{%s}" (plist-get inner :label)) parts)
                (setq edges           (append edges           (plist-get inner :edges)))
                (setq grandchild-stmts (append grandchild-stmts (plist-get inner :grandchildren))))
            ;; Ordinary field: <port> prefix only when an explicit | id | row exists
            (let* ((explicit-port (when id-row (org--graph-dot-port (cadr id-row))))
                   (implicit-port (org--graph-dot-port field-name))
                   (port          (or explicit-port implicit-port))
                   (label         (s-replace " " "\\ " field-name)))
              (push (if explicit-port
                        (format "<%s> %s" port label)
                      label)
                    parts)
              ;; `to' rows emit port-anchored cross-edges
              (dolist (to-row to-rows)
                (let* ((raw-target (cadr to-row))
                       (target     (org--graph-resolve-port-target raw-target port-index))
                       (attr-str   (org--graph-parse-attrs (nth 2 to-row))))
                  (push (if (s-blank-str-p attr-str)
                            (format "%s:%s -> %s;" node-id port target)
                          (format "%s:%s -> %s [ %s ];" node-id port target attr-str))
                        edges)))
              ;; Grandchildren of real fields recurse as ordinary nodes
              (dolist (gc grandchildren)
                (setq grandchild-stmts
                      (append grandchild-stmts
                              (org--graph-list->dot gc nil seen-nodes cluster-membership port-index)))))))))
    (list :label        (s-join "|" (nreverse parts))
          :edges        (nreverse edges)
          :grandchildren grandchild-stmts)))

(defun org--graph-record->dot (node-id shape other-attrs children seen-nodes cluster-membership port-index)
  "Emit dot statements for a record/Mrecord node and its field children.

NODE-ID      — already-quoted dot id for the record node.
SHAPE        — \"record\" or \"Mrecord\".
OTHER-ATTRS  — node attr rows with the shape row removed.
CHILDREN     — raw org-list-to-lisp children of the record item.
SEEN-NODES, CLUSTER-MEMBERSHIP — forwarded hash-tables.
PORT-INDEX   — hash-table of port-id → record-node-id for target resolution.

Immediate children become record fields.  A child named \"{}\" is a
layout-flip group — its children are wrapped in { } in the label string
and no port or edge is emitted for the group node itself.  Deeper
nesting flips orientation again, mirroring the Graphviz record DSL."
  (let* ((result   (org--graph-record-label-fragment
                    children node-id seen-nodes cluster-membership port-index))
         (label    (concat "\"" (plist-get result :label) "\""))
         (all-attrs (append (list (list "shape" shape) (list "label" label))
                            other-attrs))
         (attr-str  (org--graph-attrs->dot all-attrs)))
    (puthash node-id t seen-nodes)
    (append
     (list (format "%s [ %s ];" node-id attr-str))
     (plist-get result :edges)
     (plist-get result :grandchildren))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; List walker

(defun org--graph-dot-id (name)
  "Return NAME as a valid dot identifier, quoting if necessary.
Already-quoted strings (starting and ending with \") are returned as-is.
Pure identifiers (letters, digits, underscore) and bare numbers are also
returned as-is.  Everything else is wrapped in double quotes with any
interior double-quotes escaped."
  (cond
   ((and (s-starts-with-p "\"" name) (s-ends-with-p "\"" name)) name)
   ((s-matches-p "^[a-zA-Z_][a-zA-Z_0-9]*$" name) name)
   ((s-matches-p "^-?[0-9]+\\.?[0-9]*$" name) name)
   (t (format "\"%s\"" (s-replace "\"" "\\\"" name)))))

(defun org--graph-list->dot (lst parent-id seen-nodes cluster-membership port-index name-tables)
  "Walk one org-list-to-lisp item LST and return a list of dot statement strings.

PARENT-ID is the dot ID of the parent node (nil for root items).
SEEN-NODES is a hash-table of node-id → t for already-declared nodes.
CLUSTER-MEMBERSHIP is a hash-table we populate: node-id → cluster-id.
PORT-INDEX is a hash-table of port-id → record-node-id (read-only here).

Emission order per node:
  1. Incoming nesting-edge(s) from parent (one per EDGE row, or one plain arc)
  2. Explicit `to` cross-edges from this node
  3. Node declaration (first occurrence only)
  4. Recurse into children"
  (cond
   ((symbolp lst) nil)
   ((symbolp (car lst)) (org--graph-list->dot (cadr lst) parent-id seen-nodes cluster-membership port-index name-tables))
   (t
    (-let* (((raw-text children) lst)
            ((name-part table-part) (s-split-up-to "\n" (s-trim raw-text) 1))
            (node-name   (s-trim name-part))
            (table-rows  (org--graph-expand-eval-rows
                          (org--graph-parse-table table-part)
                          name-tables))
            (id-row      (--first (equal (car it) "id")    table-rows))
            (to-rows     (--filter (equal (car it) "to")   table-rows))
            (edge-rows   (--filter (equal (car it) "EDGE") table-rows))
            (rank-row    (--first (equal (car it) "rank")  table-rows))
            (node-attrs  (--filter (not (member (car it) '("id" "to" "EDGE" "rank")))
                                   table-rows))
            (node-id     (org--graph-dot-id (or (cadr id-row) node-name)))
            (first-time  (not (gethash node-id seen-nodes))))

      (if (and first-time (org--graph-record-shape-p node-attrs))
          ;; Record node: immediate children become fields, not edges
          (let* ((shape-row   (--first (equal (car it) "shape") node-attrs))
                 (shape       (cadr shape-row))
                 (other-attrs (--remove (equal (car it) "shape") node-attrs)))
            (append
             ;; Nesting-edge(s) from parent still apply
             (when parent-id
               (if edge-rows
                   (--map (let ((attr-str (org--graph-parse-attrs
                                           (org--graph-expand-eval-in-attr-str (cadr it) name-tables))))
                            (if (s-blank-str-p attr-str)
                                (format "%s -> %s;" parent-id node-id)
                              (format "%s -> %s [ %s ];" parent-id node-id attr-str)))
                          edge-rows)
                 (list (format "%s -> %s;" parent-id node-id))))
             (org--graph-record->dot node-id shape other-attrs children
                                     seen-nodes cluster-membership port-index)))

        (if (and first-time (org--graph-cluster-shape-p node-attrs))
            ;; Cluster node: becomes an inline cluster_ID { } block.
            ;; #+node:, #+edge:, #+graph:, #+rank: lines embedded in the item
            ;; body are extracted and emitted as scoped directives inside the
            ;; subgraph block; child-explicit attrs override these defaults.
            (let* ((item-pre    (org--graph-extract-item-preamble table-part))
                   (all-rows    (plist-get item-pre :attr-rows))
                   (id-row      (--first (equal (car it) "id")    all-rows))
                   (node-id     (org--graph-dot-id (or (cadr id-row) node-name)))
                   (cluster-id  (org--graph-dot-port (s-replace "\"" "" node-id)))
                   (label-row   (--first (equal (car it) "label") all-rows))
                   (label       (if label-row
                                    (cadr label-row)
                                  (format "\"%s\""
                                          (s-replace "\"" "" node-name))))
                   (other-attrs (--filter (not (member (car it) '("id" "shape" "label")))
                                          all-rows))
                   (attr-str    (org--graph-attrs->dot other-attrs))
                   ;; Scoped directive statements inside the subgraph
                   (scope-stmts (append
                                 (when (plist-get item-pre :graph-rows)
                                   (list (format "graph [ %s ]"
                                                 (org--graph-attrs->dot (plist-get item-pre :graph-rows)))))
                                 (when (plist-get item-pre :node-rows)
                                   (list (format "node [ %s ]"
                                                 (org--graph-attrs->dot (plist-get item-pre :node-rows)))))
                                 (when (plist-get item-pre :edge-rows)
                                   (list (format "edge [ %s ]"
                                                 (org--graph-attrs->dot (plist-get item-pre :edge-rows)))))
                                 (--map (format "{ rank=same; %s }"
                                                (s-join "; " (s-split "[ \t]+" (s-trim it) t)))
                                        (plist-get item-pre :ranks))))
                   ;; Walk children as ordinary nodes, registering cluster membership
                   (child-stmts (--mapcat
                                 (progn
                                   ;; Pre-register membership so child walk sees it
                                   (when (and it (not (symbolp it)) (not (symbolp (car it))))
                                     (-let* (((c-raw _) it)
                                             ((c-name c-table) (s-split-up-to "\n" (s-trim c-raw) 1))
                                             (c-item-pre (org--graph-extract-item-preamble c-table))
                                             (c-rows     (plist-get c-item-pre :attr-rows))
                                             (c-id-row   (--first (equal (car it) "id") c-rows))
                                             (c-id       (org--graph-dot-id (or (cadr c-id-row) (s-trim c-name)))))
                                       (puthash c-id cluster-id cluster-membership)))
                                   (org--graph-list->dot it nil seen-nodes cluster-membership port-index name-tables))
                                 children)))
              (puthash node-id t seen-nodes)
              (list (format "subgraph cluster_%s {\n    label=%s\n    %s\n  }"
                            cluster-id
                            label
                            (s-join "\n    "
                                    (append
                                     (when (not (s-blank-str-p attr-str)) (list attr-str))
                                     scope-stmts
                                     child-stmts)))))

        (append
         ;; 1. Nesting-edge(s): parent → this node
         (when parent-id
           (if edge-rows
               (--map (let ((attr-str (org--graph-parse-attrs
                                       (org--graph-expand-eval-in-attr-str (cadr it) name-tables))))
                        (if (s-blank-str-p attr-str)
                            (format "%s -> %s;" parent-id node-id)
                          (format "%s -> %s [ %s ];" parent-id node-id attr-str)))
                      edge-rows)
             (list (format "%s -> %s;" parent-id node-id))))

         ;; 2. Explicit `to` cross-edges; resolve bare port-ids to node:port form
         (--map (let* ((target   (org--graph-resolve-port-target (cadr it) port-index))
                       (attr-str (org--graph-parse-attrs
                                  (org--graph-expand-eval-in-attr-str (nth 2 it) name-tables))))
                  (if (s-blank-str-p attr-str)
                      (format "%s -> %s;" node-id target)
                    (format "%s -> %s [ %s ];" node-id target attr-str)))
                to-rows)

         ;; 3. Node declaration (first occurrence only)
         ;; When no explicit `label` row exists, inject label=node-name so the
         ;; rendered node displays the item text regardless of its dot id.
         (when first-time
           (puthash node-id t seen-nodes)
           (let* ((effective-attrs
                   (if (--any? (equal (car it) "label") node-attrs)
                       node-attrs
                     (append node-attrs (list (list "label" (org--graph-dot-id node-name))))))
                  (attr-str (org--graph-attrs->dot effective-attrs)))
             (list (if (s-blank-str-p attr-str)
                       (format "%s;" node-id)
                     (format "%s [ %s ];" node-id attr-str)))))

         ;; 4. Recurse into children
         (let ((child-stmts
                (--mapcat (org--graph-list->dot it node-id seen-nodes cluster-membership port-index name-tables)
                          children)))
           (if rank-row
               ;; Collect immediate child ids and emit a rank constraint block
               (let* ((rank-val   (cadr rank-row))
                      (child-ids  (--keep
                                   (when (and it (not (symbolp it)) (not (symbolp (car it))))
                                     (-let* (((c-raw _) it)
                                             ((c-name c-table) (s-split-up-to "\n" (s-trim c-raw) 1))
                                             (c-rows   (org--graph-parse-table c-table))
                                             (c-id-row (--first (equal (car it) "id") c-rows)))
                                       (org--graph-dot-id (or (cadr c-id-row) (s-trim c-name)))))
                                   children))
                      (rank-stmt  (format "{ rank=%s; %s }"
                                          rank-val
                                          (s-join "; " child-ids))))
                 (append child-stmts (list rank-stmt)))
             child-stmts)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dot assembly

(defun org--graph-raw->dot (raw-contents main-arg)
  "Convert RAW-CONTENTS of a #+begin_graph block into a dot source string.
MAIN-ARG is \"directed\" (default) or \"undirected\"."
  (let* ((preamble        (org--graph-extract-preamble raw-contents))
         (graph-rows      (plist-get preamble :graph-rows))
         (node-rows       (plist-get preamble :node-rows))
         (edge-rows       (plist-get preamble :edge-rows))
         (ranks           (plist-get preamble :ranks))
         (name-tables     (plist-get preamble :name-tables))
         (list-body       (plist-get preamble :list-body))
         (digraph?        (not (equal main-arg "undirected")))
         (graph-type      (if digraph? "digraph" "graph"))
         (seen            (make-hash-table :test #'equal))
         (cluster-mem     (make-hash-table :test #'equal))
         ;; Parse the Org list
         (parsed          (condition-case err
                              (with-temp-buffer
                                (insert (s-trim list-body))
                                (goto-char (point-min))
                                (org-list-to-lisp))
                            (error
                             (user-error "#+begin_graph: invalid Org list.\n%s"
                                         (error-message-string err)))))
         ;; Pre-pass: build port-id → record-node-id index for field `to` resolution
         (port-index      (org--graph-build-port-index parsed))
         ;; Walk every top-level item
         (all-stmts       (--mapcat
                           (org--graph-list->dot it nil seen cluster-mem port-index name-tables)
                           (cdr parsed))))

    ;; Replace "->" with "--" for undirected graphs
    (when (not digraph?)
      (setq all-stmts
            (--map (s-replace "->" "--" it) all-stmts)))

    (let* ((rank-stmts
            (--map (format "{ rank=same; %s }"
                           (s-join "; " (s-split "[ \t]+" (s-trim it) t)))
                   ranks))
           (lines
            (append
             (when graph-rows
               (list (format "graph [ %s ]" (org--graph-attrs->dot graph-rows))))
             (when node-rows
               (list (format "node [ %s ]" (org--graph-attrs->dot node-rows))))
             (when edge-rows
               (list (format "edge [ %s ]" (org--graph-attrs->dot edge-rows))))
             rank-stmts
             all-stmts)))
      (format "%s {\n  %s\n}" graph-type (s-join "\n  " lines)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C-c C-c integration

(defun org-graph--insert-results (end-point outfile)
  "Insert or replace a #+RESULTS: block after END-POINT with a link to OUTFILE.
If the line immediately after END-POINT is already a #+RESULTS: block,
replace it; otherwise insert a new one."
  (save-excursion
    (goto-char end-point)
    (forward-line 1)
    (if (looking-at "[ \t]*#\\+RESULTS:")
        ;; Replace existing #+RESULTS: block (one header line + one link line)
        (progn
          (delete-region (point)
                         (progn (forward-line 2) (point)))
          (insert (format "#+RESULTS:\n[[file:%s]]\n" outfile)))
      (insert (format "#+RESULTS:\n[[file:%s]]\n" outfile)))))

(defun org-graph--annotate-dot-error (err-str dot)
  "Enrich ERR-STR from dot with offending lines from DOT and debug hints.
Collects every 'line N' reference in ERR-STR, extracts those lines, and
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
    (re-search-backward "^[ \t]*#\\+begin_graph\\b" nil t)
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
            (save-excursion
              (goto-char end-pt)
              (forward-line 1)
              (if (looking-at "[ \t]*#\\+RESULTS:")
                  (progn
                    (delete-region (point) (progn (forward-line 2) (point)))
                    (insert (format "#+RESULTS:\n#+begin_src dot\n%s\n#+end_src\n" dot)))
                (insert (format "#+RESULTS:\n#+begin_src dot\n%s\n#+end_src\n" dot))))
            (message "Graph dot source → *org-graph dot*"))
        (let* ((outfile (or (plist-get kwdargs :file)
                            (expand-file-name
                             (format "graph-%s.png" (substring (md5 dot) 0 12))
                             temporary-file-directory)))
               (dotfile (make-temp-file "org-graph-" nil ".dot")))
          (write-region dot nil dotfile)
          (let* ((err-file (make-temp-file "org-graph-err-"))
                 (exit     (call-process "dot" nil (list nil err-file) nil
                                         "-v" "-Tpng" dotfile "-o" outfile))
                 (err-str  (with-temp-buffer
                             (insert-file-contents err-file)
                             (s-trim (buffer-string)))))
            (delete-file err-file)
            (if (zerop exit)
                (progn
                  (org-graph--insert-results end-pt outfile)
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
  (let* ((dotfile  (make-temp-file "org-graph-" nil ".dot"))
         (err-file (make-temp-file "org-graph-err-")))
    (write-region dot nil dotfile)
    (let* ((exit    (call-process "dot" nil (list nil err-file) nil
                                  "-Tpng" dotfile "-o" outfile))
           (err-str (with-temp-buffer
                      (insert-file-contents err-file)
                      (s-trim (buffer-string)))))
      (delete-file err-file)
      (if (zerop exit)
          outfile
        (warn "org-graph: dot failed rendering PNG:\n%s"
              (org-graph--annotate-dot-error err-str dot))
        nil))))

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
  (let* ((dotfile  (make-temp-file "org-graph-" nil ".dot"))
         (err-file (make-temp-file "org-graph-err-")))
    (write-region dot nil dotfile)
    (let* ((exit    (call-process "dot" nil (list nil err-file) nil
                                  "-Tsvg" dotfile "-o" outfile))
           (err-str (with-temp-buffer
                      (insert-file-contents err-file)
                      (s-trim (buffer-string)))))
      (delete-file err-file)
      (if (zerop exit)
          outfile
        (warn "org-graph: dot failed rendering SVG:\n%s"
              (org-graph--annotate-dot-error err-str dot))
        nil))))

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
otherwise an auto-generated timestamped name is used.

For HTML export: if :svg t is set, embeds an inline <svg> element (tooltips
are preserved as <title> attributes and appear on hover in browsers).
Otherwise, renders to an inline base64 PNG.
For other backends it emits a #+begin_src dot block.

Within the block body:

  #+graph: table  — global graph[ ] attributes
  #+node:  table  — default node[ ] attributes
  #+edge:  table  — default edge[ ] attributes
  #+rank:  n1 n2  — { rank=same; n1; n2 } for nodes without a common parent

Each list item declares a node.  Nesting encodes edges (parent → child).
A two-column inline table on a list item supplies node attributes.

  | EDGE | attr-string |  — arc attrs for the incoming (parent→this) edge;
                            N such rows = N parallel arcs.
  | id   | alias       |  — cross-reference name for this node.
  | to   | target | attrs | — explicit outgoing edge (3 columns; attrs optional).
  | shape | cluster |    — make this node an inline subgraph cluster_ID { … }.
  | rank  | same    |    — constrain immediate children to the same rank;
                            also accepts min, max, source, sink.
  | key  | val  |         — any other key styles the node itself.

Example:

  #+begin_graph directed
  #+graph:
  | rankdir | LR |

  + A
    + B
      | EDGE | label=calls |
      + C
        | EDGE | label=returns |
  #+end_graph"
  (let* ((dot     (org--graph-raw->dot raw-contents main-arg))
         (outfile (or file
                      (format "graph-%s.png"
                              (format-time-string "%Y%m%d-%H%M%S")))))
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
;;; org-special-block-extras-graph.el ends here
