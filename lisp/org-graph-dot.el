;;; org-graph-dot.el --- #+begin_graph: graph statements ⟶ Graphviz dot  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Musa Al-hassy

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Part of org-graph-block.el: printing parsed statements as dot, and the whole pipeline,
;; `org--graph-raw->dot'.

;;; Code:

(require 'org-graph-parse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Printing: graph statements ⟶ dot

(defvar org--graph-hrefs nil
  "Non-nil while rendering a graph for its buffer, rather than for export: then each node and cluster sans URL gets
href=\"org-graph:LINE\", LINE being its item's in the body, so that a click on its image-map area leads back there.")

(defun org--graph-href (id attrs ctx)
  "(ID, ATTRS) ↦ ATTRS, plus an href to ID's item when `org--graph-hrefs' is set and ATTRS has no URL of its own.
  (\"Lisp\", ((\"shape\" \"box\")))  ↦  ((\"shape\" \"box\") (\"href\" \"org-graph:3\")), Lisp's item being body line 3"
  (-if-let (line (and org--graph-hrefs (not (assoc "URL" attrs)) (gethash id (org--graph-ctx-lines ctx))))
      (append attrs `(("href" ,(format "org-graph:%d" line))))
    attrs))

(defun org--graph-endpoint (raw ctx)
  "RAW ↦ (NODE . PORT): \"rec::p q\" ↦ (\"rec\" . \"p_q\") when rec is a record in CTX, as Org's [[file::search]] reaches
into a file; otherwise RAW names a node, sans PORT, such as \"std::vector\"."
  (let ((raw (org--graph-unquote raw)))
    (if (and (string-match "\\`\\(.+?\\)::\\(.+\\)\\'" raw) (gethash (match-string 1 raw) (org--graph-ctx-records ctx)))
        (cons (match-string 1 raw) (org--graph-dot-port (match-string 2 raw)))
      (list raw))))

(defun org--graph-all-stmts (stmts)
  "STMTS, each cluster followed by its own statements, recursively."
  (--mapcat (cons it (pcase it
                       (`(:cluster ,_ . ,props) (org--graph-all-stmts (plist-get props :stmts)))
                       (`(:scope ,_ ,stmts)     (org--graph-all-stmts stmts))))
            stmts))

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
  "SCOPE's :graph-rows, :node-rows, and :edge-rows ↦ the dot directives for its scope."
  (-keep (-lambda ((kind . key))
           (-when-let (rows (plist-get scope key))
             (concat kind (org--graph-attrs->dot (if (equal kind "edge") rows (org--graph-filled rows))))))
         '(("graph" . :graph-rows) ("node" . :node-rows) ("edge" . :edge-rows))))

(defvar org--graph-box-members nil
  "While a container prints, the nodes inside it, nested boxes' included; else nil.")

(defvar org--graph-box-edge-rows nil
  "While a container prints, the #+edge: rows of the boxes and scopes enclosing the statement printing, outermost
first; an arc printed outside them takes these rows along.")

(defvar org--graph-box-node-rows nil
  "While a container prints, the #+node: rows of the boxes and scopes enclosing the statement printing, outermost
first; a member that an earlier arc already created takes these rows into its own line.")

(defvar org--graph-mentioned nil
  "While a graph prints, a table of the nodes its printed lines have mentioned so far, each to a list of the
#+node: rows in force at its first mention.")

(defvar org--graph-hoisted nil
  "While a container prints, the arcs leaving it, latest first, to be printed after it.")

(defun org--graph-replace-rows (rows over)
  "Returns ROWS with OVER's rows replacing those of the same key, as an inner edge [ … ] line replaces an outer's;
unlike `org--graph-override', styles do not join, e.g. ((style dashed) (color red)) and ((style bold)) give
((color red) (style bold))."
  (append (--remove (assoc (car it) over) rows) over))

(defun org--graph-endpoint-node (raw ctx)
  "Returns the node an arc's endpoint RAW is drawn at, in CTX: a box's first node, else RAW's node, sans its port."
  (let ((node (car (org--graph-endpoint raw ctx))))
    (gethash node (org--graph-ctx-clusters ctx) node)))

(defun org--graph-box-members (id stmts ctx)
  "Returns the nodes inside box ID, whose statements are STMTS, in CTX: those STMTS declare, nested boxes' included,
and those a :container_ID: tag gathers."
  (append (--mapcat (pcase it
                      (`(:node ,node) (list node))
                      (`(:legend ,node . ,_) (list node (concat node "_end"))))
                    (org--graph-all-stmts stmts))
          (gethash id (org--graph-ctx-tag-clusters ctx))))

(defun org--graph-stmt->dot (stmt op ctx)
  "STMT, a parsed graph statement, ↦ its dot lines; OP is the edge operator, \"->\" or \"--\"."
  (pcase stmt
    ;; Graphviz gives a node the defaults in force where it is first mentioned; an arc mentioning a box's member
    ;; before the box does leaves it the graph's defaults, so the member's own line restates the box's.
    (`(:node ,id)
     (-let [(name . attrs) (gethash id (org--graph-ctx-nodes ctx))]
       (when org--graph-mentioned
         (let ((first (gethash id org--graph-mentioned)))
           (when (and first (not (equal (car first) org--graph-box-node-rows)))
             (setq attrs (org--graph-replace-rows org--graph-box-node-rows attrs))))
         (unless (gethash id org--graph-mentioned)
           (puthash id (list org--graph-box-node-rows) org--graph-mentioned)))
       (list (format "%s%s;" (org--graph-dot-id id)
                     (org--graph-attrs->dot
                      (org--graph-href id (org--graph-person (org--graph-filled (if (assoc "label" attrs) attrs (append attrs `(("label" ,name))))
                                                                           'node))
                                       ctx))))))
    ;; Graphviz draws a node in the first box mentioning it, so an arc leaving the box is printed outside it.
    (`(:edge ,src ,dst ,attrs)
     (if (and org--graph-box-members
              (not (--all? (member (org--graph-endpoint-node it ctx) org--graph-box-members) (list src dst))))
         (ignore (push `(:edge ,src ,dst ,(org--graph-replace-rows org--graph-box-edge-rows attrs))
                       org--graph-hoisted))
       (when org--graph-mentioned
         (dolist (end (list src dst))
           (let ((node (org--graph-endpoint-node end ctx)))
             (unless (gethash node org--graph-mentioned)
               (puthash node (list org--graph-box-node-rows) org--graph-mentioned)))))
       (list (org--graph-edge->dot src dst attrs op ctx))))
    ;; A template that styles arcs is shown as a captioned arc between invisible points; any other, as a captioned node.
    ;; The arc runs along the graph's direction, as a same-rank one may lose its caption; e.g., under splines=curved.
    (`(:legend ,id ,rows ,label ,templates)
     ;; Sans `constraint', which shapes the layout rather than the arc, and would let the sample drift out of line.
     ;; A backward arc, dir=back, draws its arrow at its start; its sample points it forward instead, as every other
     ;; sample does, so a legend's arrows all point one way.
     (let* ((rows (--remove (equal (car it) "constraint") rows))
            (rows (if (not (equal (cadr (assoc "dir" rows)) "back")) rows
                    (append (--remove (member (car it) '("dir" "arrowhead" "arrowtail")) rows)
                            `(("arrowhead" ,(or (cadr (assoc "arrowtail" rows)) "normal"))))))
            (rows (org--graph-override rows `(("label" ,label)))))
       (if (--some (gethash it (org--graph-ctx-arc-templates ctx)) templates)
           (let ((end (concat id "_end")))
             (list (format "%s [ shape=point style=invis ];" id)
                   (format "%s [ shape=point style=invis ];" end)
                   (format "%s %s %s%s;" id op end (org--graph-attrs->dot rows))))
         (list (format "%s%s;" id (org--graph-attrs->dot (org--graph-filled rows 'node)))))))
    ((or `(:legend-row) `(:legend-top)) nil)
    (`(:rank ,kind ,ids)
     (list (format "{ rank=%s; %s }" kind (s-join "; " (-map #'org--graph-dot-id ids)))))
    (`(:scope ,scope ,stmts)
     (let ((org--graph-filled-by-default (org--graph-filling-kinds scope))
           (org--graph-box-edge-rows (org--graph-replace-rows org--graph-box-edge-rows (plist-get scope :edge-rows)))
           (org--graph-box-node-rows (org--graph-replace-rows org--graph-box-node-rows (plist-get scope :node-rows))))
       (list (format "subgraph {\n    %s\n  }"
                     (s-join "\n    " (append (org--graph-scope->dot scope)
                                              (--mapcat (org--graph-stmt->dot it op ctx) stmts)))))))
    (`(:cluster ,id . ,props)
     ;; A theme's container fills shade each depth apart, outermost first; the container's own fillcolor wins.
     (let (subgraph hoisted)
       (let* ((fill (and org--graph-container-fills
                         (nth (mod org--graph-container-depth (length org--graph-container-fills))
                              org--graph-container-fills)))
              (attrs (org--graph-filled (if fill (org--graph-override `(("fillcolor" ,fill)) (plist-get props :attrs))
                                          (plist-get props :attrs))
                                        'graph))
              (org--graph-container-depth (1+ org--graph-container-depth))
              (org--graph-filled-by-default (org--graph-filling-kinds (plist-get props :scope)))
              (org--graph-box-members (org--graph-box-members id (plist-get props :stmts) ctx))
              (org--graph-box-edge-rows (org--graph-replace-rows org--graph-box-edge-rows
                                                                  (plist-get (plist-get props :scope) :edge-rows)))
              (org--graph-box-node-rows (org--graph-replace-rows org--graph-box-node-rows
                                                                  (plist-get (plist-get props :scope) :node-rows)))
              (org--graph-hoisted nil))
         (setq subgraph (format "subgraph %s {\n    %s\n  }" (org--graph-cluster-id id)
                                (s-join "\n    "
                                        (append (list (concat "label=" (org--graph-dot-id (plist-get props :label))))
                                                (-some--> (org--graph-href id attrs ctx)
                                                  (list (org--graph-attr-pairs it)))
                                                (org--graph-scope->dot (plist-get props :scope))
                                                (--mapcat (org--graph-stmt->dot it op ctx) (plist-get props :stmts)))))
               hoisted org--graph-hoisted))
       ;; Printed in the enclosing box, an arc leaving it too leaves that box in turn.
       (cons subgraph (--mapcat (org--graph-stmt->dot it op ctx) (reverse hoisted)))))))

(defun org--graph-note-ranks (ctx stmts)
  "Record in CTX each node's rank group, from STMTS' ranks, clusters' included.
  + church :@pioneers:  + turing :@pioneers:  ↦  church ↦ 0, turing ↦ 0"
  (let ((groups (--keep (pcase it (`(:rank ,_ ,ids) ids)) (org--graph-all-stmts stmts))))
    (--each-indexed groups (dolist (id it) (puthash id it-index (org--graph-ctx-ranks ctx))))))

(defun org--graph-paint-by-depth (ctx stmts colors)
  "Fills each node CTX declares by its depth in STMTS' arcs, from COLORS, cycling, save one with a fillcolor of its
own; so a theme's :node-colors color a mindmap's rings.  A node no arc enters is a root, of depth 0; else its depth
is one more than its shallowest parent's."
  (let* ((nodes (org--graph-ctx-nodes ctx))
         (arcs (--keep (pcase it (`(:edge ,src ,dst ,_) (cons (car (org--graph-endpoint src ctx))
                                                             (car (org--graph-endpoint dst ctx)))))
                       (org--graph-all-stmts stmts)))
         (depths (make-hash-table :test #'equal))
         (frontier (--remove (rassoc it arcs) (hash-table-keys nodes))))
    (cl-loop for depth from 0 while frontier
             do (dolist (id frontier) (puthash id depth depths))
                (setq frontier (-uniq (--keep (and (member (car it) frontier) (not (gethash (cdr it) depths)) (cdr it))
                                              arcs))))
    (maphash (lambda (id entry)
               (-when-let (depth (gethash id depths))
                 (unless (assoc "fillcolor" (cdr entry))
                   (puthash id (append entry `(("fillcolor" ,(nth (mod depth (length colors)) colors)))) nodes))))
             nodes)))

(defun org--graph-legend-rows (stmts)
  "Returns the entries of each legend row in STMTS, clusters' included, in order: those of each statement list
holding (:legend-row)."
  (append (when (member '(:legend-row) stmts) (list (--filter (eq (car-safe it) :legend) stmts)))
          (--mapcat (pcase it
                      (`(:cluster ,_ . ,props) (org--graph-legend-rows (plist-get props :stmts)))
                      (`(:scope ,_ ,inner) (org--graph-legend-rows inner)))
                    stmts)))

(defun org--graph-legend-chain (stmts op ctx &optional nested)
  "Returns invisible arcs, by OP, chaining each run of legend entries in STMTS ---and in each cluster's, its own---
from one entry's end to the next's start, so a legend stacks as a column rather than spreading as a row.
An entry sampling an arc template ends at its sample's far point, legend_N_end; any other, at itself.
A legend row, see `org--graph-legend-rows', instead sets its entries' ends on one rank; the first row lies beneath
every node that no arc leaves, and each later row beneath the one before; Graphviz's rank=max would not move a box's
nodes.  Under (:legend-top), nothing holds the rows down, so the first rises to the graph's top rank; arcs from each
node that nothing enters would hold them up there too, but they drag the whole layout about.  NESTED says STMTS are a cluster's, whose rows the outermost call has chained already."
  (let* ((end (lambda (stmt) (pcase-let ((`(:legend ,id ,_ ,_ ,templates) stmt))
                               (if (--some (gethash it (org--graph-ctx-arc-templates ctx)) templates) (concat id "_end") id))))
         (row (member '(:legend-row) stmts))
         (chain (unless nested
                  (let* ((every (org--graph-all-stmts stmts))
                         (sources (--keep (pcase it (`(:edge ,src . ,_) (org--graph-endpoint-node src ctx))) every))
                         (nodes (--keep (pcase it (`(:node ,id) id)) every))
                         (top (member '(:legend-top) every))
                         (above (unless top (--filter (not (member it sources)) nodes)))
                         lines)
                    (dolist (entries (--filter it (org--graph-legend-rows stmts)))
                      (push (format "{ rank=same; %s }" (s-join "; " (-map end entries))) lines)
                      (dolist (node above)
                        (push (format "%s %s %s [ style=invis weight=0 ];" (org--graph-dot-id node) op (cadr (car entries)))
                              lines))
                      (setq above (list (funcall end (car entries)))))
                    lines))))
    (cl-loop for (a b) on stmts
             do (when (and (not row) (eq (car-safe a) :legend) (eq (car-safe b) :legend))
                  (push (format "%s %s %s [ style=invis ];" (funcall end a) op (cadr b)) chain))
                (pcase a
                  (`(:cluster ,_ . ,props) (setq chain (append (reverse (org--graph-legend-chain (plist-get props :stmts) op ctx t))
                                                               chain)))
                  (`(:scope ,_ ,inner) (setq chain (append (reverse (org--graph-legend-chain inner op ctx t)) chain)))))
    (nreverse chain)))

(defun org--graph-tag-clusters->dot (ctx)
  "Returns a subgraph per box that :container_TITLE: tags gather in CTX, naming its nodes; one of the same title
that a `+ TITLE :container:' item declares is the same subgraph, which Graphviz merges, so the tagged nodes join it.
  + Jesus :container_Christianity:  ⇒  subgraph cluster_Christianity { label=Christianity; Jesus; }"
  (let (subgraphs)
    (maphash (lambda (title ids)
               (push (format "subgraph %s {\n    label=%s;\n    %s\n  }" (org--graph-cluster-id title)
                             (org--graph-dot-id title) (s-join " " (--map (concat (org--graph-dot-id it) ";") (reverse ids))))
                     subgraphs))
             (org--graph-ctx-tag-clusters ctx))
    (nreverse subgraphs)))

(defun org--graph-boxed-ranks-p (ctx stmts)
  "Returns whether a top-level rank statement of STMTS, from `:@X:' tags, names a node inside a box, whether a
container's item or a :container_TITLE: tag puts it there; Graphviz would otherwise pull the node out of its box.
So too for a legend row, whose rank=max names its box's entries.
  + Box :container:  + a :@pals:  …  + b :@pals:  ⇒  t"
  (let ((boxed (-uniq (append (--mapcat (pcase it
                                          (`(:cluster ,_ . ,props)
                                           (--keep (pcase it (`(:node ,id) id))
                                                   (org--graph-all-stmts (plist-get props :stmts)))))
                                        (org--graph-all-stmts stmts))
                              (-flatten (hash-table-values (org--graph-ctx-tag-clusters ctx)))))))
    (or (--some (pcase it (`(:rank ,_ ,ids) (-intersection ids boxed))) stmts)
        ;; A legend row's rank=max sits in its box, so it too needs newrank.
        (member '(:legend-row) (org--graph-all-stmts stmts)))))

(defvar org--graph-node-defaults nil
  "While a graph prints, its graph-wide node defaults: the rows of its node [ … ] line, a theme's included.")

(defun org--graph-person-icon (color)
  "Returns the file of a stick figure drawn in COLOR, written once, to a directory of the system's temporary files;
for Graphviz reads an image only from a file.  The SVG opens with an XML declaration, sans which Graphviz misses it."
  (let* ((dir (expand-file-name "org-graph-icons" temporary-file-directory))
         (file (expand-file-name (format "person-%s.svg" (replace-regexp-in-string "[^[:alnum:]]" "" color)) dir)))
    (unless (file-exists-p file)
      (make-directory dir t)
      (with-temp-file file
        (insert "<?xml version=\"1.0\"?>\n"
                "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"30\" height=\"45\" viewBox=\"0 0 36 54\">\n"
                (format "  <g fill=\"none\" stroke=\"%s\" stroke-width=\"3\" stroke-linecap=\"round\">\n" color)
                "    <circle cx=\"18\" cy=\"9\" r=\"7\"/><path d=\"M18 16v18M4 23h28M18 34 7 52M18 34l11 18\"/>\n"
                "  </g>\n</svg>\n")))
    file))

(defun org--graph-person (attrs)
  "Returns ATTRS, a node's, drawn as a person when their shape is person, which Graphviz lacks: a stick figure in the
node's color, else its defaults', above its label, in an HTML-like table; else ATTRS as they are.
  ((\"shape\" \"person\") (\"label\" \"Alice\"))  ↦  ((\"shape\" \"none\") (\"label\" \"<<TABLE …><IMG SRC=…/>…Alice…>\"))"
  (if (not (equal (cadr (assoc "shape" attrs)) "person"))
      attrs
    (let* ((color (or (cadr (assoc "color" attrs)) (cadr (assoc "color" org--graph-node-defaults)) "black"))
           (label (or (cadr (assoc "label" attrs)) ""))
           (body (if (string-match "\\`<\\(.*\\)>\\'" label) (match-string 1 label) (org--graph-html-escape label))))
      (append (--remove (member (car it) '("shape" "label" "style")) attrs)
              `(("shape" "none") ("style" "solid") ("margin" "0")
                ("label" ,(format (concat "<<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"0\" CELLPADDING=\"1\">"
                                          "<TR><TD><IMG SRC=\"%s\"/></TD></TR><TR><TD>%s</TD></TR></TABLE>>")
                                  (org--graph-person-icon color) body)))))))

(defvar org--graph-container-fills nil
  "While a themed graph prints, its theme's :container-colors, one per depth of nesting, outermost first; else nil.")

(defvar org--graph-container-depth 0
  "While a graph prints, how many containers enclose the statement printing.")

(defcustom org-graph-themes
  '((creamsicle
     :graph "bgcolor=white style=filled fillcolor=\"#FABA8A\" color=\"#311602\" penwidth=2 fontname=Helvetica fontsize=20 fontcolor=\"#170206\""
     :node "shape=box style=filled fillcolor=\"#FFE0C7\" color=\"#311602\" penwidth=2 fontname=\"Helvetica Bold\" fontcolor=\"#170206\" margin=\"0.3,0.15\""
     :edge "color=\"#311602\" penwidth=2 fontname=\"Helvetica Oblique\" fontcolor=\"#535152\" arrowsize=0.8"
     :container-colors "#FABA8A #FFE0C7 #FFF6EF #FFFFFF")
    (mindmap
     :graph "layout=twopi overlap=false ranksep=1.0"
     :node "shape=box style=\"rounded,filled\" color=gray40"
     :edge "color=gray50 dir=none"
     :node-colors "#c9a7f5 #f78fc4 #8be9fd #69f07a #ffb86c #ff7b7b #a8e6a3"))
  "The (NAME :graph PAIRS :node PAIRS :edge PAIRS :node-colors COLORS :container-colors COLORS) of each theme a graph
may wear, by `:theme NAME' or `org-graph-theme'.
Each PAIRS is a #+graph:, #+node:, or #+edge: line's KEY=VALUE pairs; the graph's own attributes win over them all.
The :graph pairs style containers too, since a Graphviz cluster inherits the graph's attributes; the root graph
draws only its bgcolor.  Each COLORS, apart by spaces, fills by depth, cycling: :container-colors, containers, the
outermost the first, a container within it the second; :node-colors, nodes, a root the first, its children the
second.  A node's, or container's, own fillcolor wins.  `creamsicle' follows D2's theme 101, Orange Creamsicle: cream
boxes, orange containers.  `mindmap' radiates from the first item, by twopi, its rings colored by depth.
A theme is data, so a new one is a `push' away:
  (push \='(ink :node \"shape=box\" :edge \"color=gray30\") org-graph-themes)"
  :type '(alist :key-type symbol :value-type (plist :key-type (choice (const :graph) (const :node) (const :edge)
                                                                     (const :node-colors) (const :container-colors))
                                                    :value-type string))
  :group 'org-special-block-extras)

(defcustom org-graph-theme 'creamsicle
  "The name of the theme, in `org-graph-themes', that every graph wears, sans its own `:theme'; nil for Graphviz's look."
  :type '(choice (const :tag "Graphviz's own look" nil) symbol)
  :group 'org-special-block-extras)

(defun org--graph-theme-rows (kwdargs)
  "Returns the (:graph-rows :node-rows :edge-rows :node-colors :container-colors) of the theme KWDARGS' :theme names, else
`org-graph-theme''s; nil when that is nil, or none.  An unknown theme is a user error."
  (let ((name (format "%s" (if (plist-member kwdargs :theme) (plist-get kwdargs :theme) org-graph-theme))))
    (unless (member name '("nil" "none"))
      (let ((theme (or (alist-get (intern name) org-graph-themes)
                       (user-error "graph: no theme named %s; see `org-graph-themes'" name))))
        (list :graph-rows (org--graph-keyword-rows 'graph (or (plist-get theme :graph) ""))
              :node-rows (org--graph-keyword-rows 'node (or (plist-get theme :node) ""))
              :edge-rows (org--graph-keyword-rows 'edge (or (plist-get theme :edge) ""))
              :node-colors (split-string (or (plist-get theme :node-colors) ""))
              :container-colors (split-string (or (plist-get theme :container-colors) "")))))))

(defun org--graph-raw->dot (raw-contents main-arg &optional kwdargs)
  "(RAW-CONTENTS, MAIN-ARG, KWDARGS) ↦ the dot source of a #+begin_graph block: a digraph, whose #+edge: :undirected:
makes its arcs arrowless.

`:wrap', `:notes', and `:unknown-tags' are as `org--graph-parse' reads them; `:unflatten N' staggers leaves, see
`org-graph--unflatten'; and `:theme NAME' styles the graph by `org-graph-themes', beneath everything else.  Under
layout=twopi, sans a root, the first item is the root, so an outline radiates from its top.
MAIN-ARG is the graph's title, its label; other KWDARGS are graph attributes, or node and edge defaults when spelled `:node.KEY' and `:edge.KEY'.
Header attributes come before the block's own #+graph:, #+node:, and #+edge: tables, so those tables win."
  (-let* (((preamble stmts ctx) (org--graph-parse raw-contents kwdargs))
          ;; Under `:notes inline', the graph's own notes sit beneath its title.
          ;; The title is labelled as an item's text is, its markup, colors, and maths so too.
          (title    (org--graph-with-body (unless (s-blank-str-p main-arg) (org--graph-title-label main-arg))
                                          (plist-get preamble :body)))
          (header   (org--graph-header-rows kwdargs))
          (fills    nil))
    (setq header (plist-put header :graph-rows (append (when title `(("label" ,title))) (plist-get header :graph-rows))))
    (-when-let (theme (org--graph-theme-rows kwdargs))
      (setq fills (plist-get theme :container-colors))
      (-some->> (plist-get theme :node-colors) (org--graph-paint-by-depth ctx stmts))
      (dolist (key '(:graph-rows :node-rows :edge-rows))
        (setq header (plist-put header key (org--graph-override (plist-get theme key) (plist-get header key))))))
    (dolist (key '(:graph-rows :node-rows :edge-rows))
      (setq preamble (plist-put preamble key (org--graph-override (plist-get header key) (plist-get preamble key)))))
    (when (equal "curved" (cadr (assoc "splines" (plist-get preamble :graph-rows))))
      (org--graph-note-ranks ctx stmts))
    ;; An outline radiates from its top: twopi's root, else, is whichever node it finds most central.
    (-when-let (root (and (equal "twopi" (cadr (assoc "layout" (plist-get preamble :graph-rows))))
                          (not (assoc "root" (plist-get preamble :graph-rows)))
                          (org--graph-first-node stmts)))
      (setq preamble (plist-put preamble :graph-rows (append (plist-get preamble :graph-rows) `(("root" ,root))))))
    (let* ((org--graph-container-fills fills)
           (org--graph-mentioned (make-hash-table :test #'equal))
           (org--graph-filled-by-default (org--graph-filling-kinds preamble))
           (org--graph-node-defaults (plist-get preamble :node-rows))
           (body (append (--mapcat (org--graph-stmt->dot it "->" ctx) stmts)
                        (org--graph-tag-clusters->dot ctx)
                        ;; A legend is a column: top-down, its entries are chained; left-right, they share a rank already.
                        (unless (member (cadr (assoc "rankdir" (plist-get preamble :graph-rows))) '("LR" "RL"))
                          (org--graph-legend-chain stmts "->" ctx)))))
      (when (org--graph-ctx-compound ctx)
        (setq preamble (plist-put preamble :graph-rows (append (plist-get preamble :graph-rows) '(("compound" "true"))))))
      ;; One global ranking, so an `:@X:' group may align nodes in boxes, and leave them there.
      (when (and (org--graph-boxed-ranks-p ctx stmts) (not (assoc "newrank" (plist-get preamble :graph-rows))))
        (setq preamble (plist-put preamble :graph-rows (append (plist-get preamble :graph-rows) '(("newrank" "true"))))))
      (org-graph--unflatten (format "digraph {\n  %s\n}"
                                    (s-join "\n  " (append (org--graph-scope->dot preamble) body)))
                            (plist-get kwdargs :unflatten)))))

(defun org-graph--unflatten (dot n)
  "(DOT, N) ↦ DOT as Graphviz's `unflatten -l N' rewrites it: a node's leaves, otherwise one wide row, staggered over
N ranks; DOT itself sans N.  A tree whose root has a dozen children is then a block rather than a ribbon."
  (if (not n)
      dot
    (with-temp-buffer
      (insert dot)
      (let ((exit (condition-case nil
                      (call-process-region (point-min) (point-max) "unflatten" t '(t nil) nil "-l" (format "%s" n))
                    (file-missing nil))))
        (unless (eql exit 0)
          (user-error "graph: `:unflatten %s' needs Graphviz's unflatten; is Graphviz installed? (M-x shell-command: \
unflatten -?)" n)))
      (buffer-string))))

(provide 'org-graph-dot)
;;; org-graph-dot.el ends here
