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
                     (org--graph-attrs->dot
                      (org--graph-href id (if (assoc "label" attrs) attrs (append attrs `(("label" ,name)))) ctx))))))
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
    (`(:scope ,scope ,stmts)
     (list (format "subgraph {\n    %s\n  }"
                   (s-join "\n    " (append (org--graph-scope->dot scope)
                                            (--mapcat (org--graph-stmt->dot it op ctx) stmts))))))
    (`(:cluster ,id . ,props)
     (list (format "subgraph %s {\n    %s\n  }" (org--graph-cluster-id id)
                   (s-join "\n    "
                           (append (list (concat "label=" (org--graph-dot-id (plist-get props :label))))
                                   (-some--> (org--graph-href id (plist-get props :attrs) ctx)
                                     (list (org--graph-attr-pairs it)))
                                   (org--graph-scope->dot (plist-get props :scope))
                                   (--mapcat (org--graph-stmt->dot it op ctx) (plist-get props :stmts)))))))))

(defun org--graph-note-ranks (ctx preamble stmts)
  "Record in CTX each node's rank group, from PREAMBLE's #+rank: lines and STMTS' ranks, clusters' included.
  #+rank: church turing  ↦  church ↦ 0, turing ↦ 0"
  (let ((groups (append (-map #'org--graph-rank-ids (plist-get preamble :ranks))
                        (--mapcat (pcase it
                                    (`(:rank ,_ ,ids) (list ids))
                                    (`(:cluster ,_ . ,props)
                                     (-map #'org--graph-rank-ids (plist-get (plist-get props :scope) :ranks)))
                                    (`(:scope ,scope ,_) (-map #'org--graph-rank-ids (plist-get scope :ranks))))
                                  (org--graph-all-stmts stmts)))))
    (--each-indexed groups (dolist (id it) (puthash id it-index (org--graph-ctx-ranks ctx))))))

(defcustom org-graph-mindmap-palette '("#c9a7f5" "#f78fc4" "#8be9fd" "#69f07a" "#ffb86c" "#ff7b7b" "#a8e6a3")
  "The fill colours of a `:mindmap t' graph's nodes, by depth: the root the first, its children the second, …; the
palette repeats past its end.  A node's own fillcolor wins."
  :type '(repeat color)
  :group 'org-special-block-extras)

(defun org--graph-mindmap-paint (ctx stmts)
  "Fills each node CTX declares by its depth in STMTS' arcs, from `org-graph-mindmap-palette', save one with a fillcolor
of its own.  A node no arc enters is a root, of depth 0; else its depth is one more than its shallowest parent's."
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
                   (puthash id (append entry `(("fillcolor" ,(nth (mod depth (length org-graph-mindmap-palette))
                                                                   org-graph-mindmap-palette))))
                            nodes))))
             nodes)))

(defun org--graph-legend-chain (stmts op ctx)
  "Returns invisible arcs, by OP, chaining each run of legend entries in STMTS ---and in each cluster's, its own---
from one entry's end to the next's start, so a legend stacks as a column rather than spreading as a row.
An entry sampling an arc template ends at its sample's far point, legend_N_end; any other, at itself."
  (let ((end (lambda (stmt) (pcase-let ((`(:legend ,id ,_ ,_ ,templates) stmt))
                              (if (--some (gethash it (org--graph-ctx-arc-templates ctx)) templates) (concat id "_end") id))))
        chain)
    (cl-loop for (a b) on stmts
             do (when (and (eq (car-safe a) :legend) (eq (car-safe b) :legend))
                  (push (format "%s %s %s [ style=invis ];" (funcall end a) op (cadr b)) chain))
                (pcase a
                  (`(:cluster ,_ . ,props) (setq chain (append (reverse (org--graph-legend-chain (plist-get props :stmts) op ctx))
                                                               chain)))
                  (`(:scope ,_ ,inner) (setq chain (append (reverse (org--graph-legend-chain inner op ctx)) chain)))))
    (nreverse chain)))

(defun org--graph-tag-clusters->dot (ctx)
  "Returns a subgraph per cluster that :cluster_TITLE: tags gather in CTX, naming its nodes; one of the same title
that a #+begin_cluster declares is the same subgraph, which Graphviz merges, so the tagged nodes join it.
  + Jesus :cluster_Christianity:  ⇒  subgraph cluster_Christianity { label=Christianity; Jesus; }"
  (let (subgraphs)
    (maphash (lambda (title ids)
               (push (format "subgraph %s {\n    label=%s;\n    %s\n  }" (org--graph-cluster-id title)
                             (org--graph-dot-id title) (s-join " " (--map (concat (org--graph-dot-id it) ";") (reverse ids))))
                     subgraphs))
             (org--graph-ctx-tag-clusters ctx))
    (nreverse subgraphs)))

(defun org--graph-raw->dot (raw-contents main-arg &optional kwdargs)
  "(RAW-CONTENTS, MAIN-ARG, KWDARGS) ↦ the dot source of a #+begin_graph block.

It is a digraph, unless KWDARGS has `:directed nil', or `:mindmap t': an undirected graph, radiating from its first
item, by Graphviz's twopi layout, its nodes rounded boxes filled by depth, see `org--graph-mindmap-paint'.  `:wrap', `:notes', and `:unknown-tags' are as
`org--graph-parse' reads them; and `:unflatten N' staggers leaves, see `org-graph--unflatten'.
MAIN-ARG is the graph's title, its label; other KWDARGS are graph attributes, or node and edge defaults when spelled `:node.KEY' and `:edge.KEY'.
Header attributes come before the block's own #+graph:, #+node:, and #+edge: tables, so those tables win."
  (-let* (((preamble stmts ctx) (org--graph-parse raw-contents kwdargs))
          (mindmap  (plist-get kwdargs :mindmap))
          (directed (and (not mindmap) (or (not (plist-member kwdargs :directed)) (plist-get kwdargs :directed))))
          ;; Under `:notes inline', the graph's own notes sit beneath its title.
          ;; The title is labelled as an item's text is, its markup, colours, and maths so too.
          (title    (org--graph-with-body (unless (s-blank-str-p main-arg) (org--graph-title-label main-arg))
                                          (plist-get preamble :body)))
          (header   (org--graph-header-rows kwdargs)))
    (setq header (plist-put header :graph-rows (append (when title `(("label" ,title))) (plist-get header :graph-rows))))
    ;; An outline is a mindmap already: its first item at the centre, the rest radiating out; the header's own rows win.
    (when mindmap
      (setq header (plist-put header :graph-rows
                              (org--graph-override `(("layout" "twopi") ("overlap" "false") ("ranksep" "1.0")
                                                     ,@(-some--> (org--graph-first-node stmts) `(("root" ,it))))
                                                   (plist-get header :graph-rows))))
      (setq header (plist-put header :node-rows
                              (org--graph-override '(("shape" "box") ("style" "rounded,filled") ("color" "gray40"))
                                                   (plist-get header :node-rows))))
      (setq header (plist-put header :edge-rows
                              (org--graph-override '(("color" "gray50")) (plist-get header :edge-rows))))
      (org--graph-mindmap-paint ctx stmts))
    (dolist (key '(:graph-rows :node-rows :edge-rows))
      (setq preamble (plist-put preamble key (org--graph-override (plist-get header key) (plist-get preamble key)))))
    (when (equal "curved" (cadr (assoc "splines" (plist-get preamble :graph-rows))))
      (org--graph-note-ranks ctx preamble stmts))
    (let ((body (append (--mapcat (org--graph-stmt->dot it (if directed "->" "--") ctx) stmts)
                        (org--graph-tag-clusters->dot ctx)
                        ;; A legend is a column: top-down, its entries are chained; left-right, they share a rank already.
                        (unless (member (cadr (assoc "rankdir" (plist-get preamble :graph-rows))) '("LR" "RL"))
                          (org--graph-legend-chain stmts (if directed "->" "--") ctx)))))
      (when (org--graph-ctx-compound ctx)
        (setq preamble (plist-put preamble :graph-rows (append (plist-get preamble :graph-rows) '(("compound" "true"))))))
      (org-graph--unflatten (format "%s {\n  %s\n}" (if directed "digraph" "graph")
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
