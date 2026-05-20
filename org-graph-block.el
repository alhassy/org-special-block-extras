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
;;   #+cluster: emacs_concepts "Emacs Concepts"
;;   | color | #313244 |
;;   | style | dashed  |
;;
;;   + lambda
;;     | id      | lam     |       ; cross-ref alias
;;     | shape   | ellipse |       ; node attrs
;;     | cluster | cat     |       ; assign to a cluster
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Preamble extraction

(defun org--graph-extract-preamble (raw)
  "Scan RAW block body and separate preamble directives from the Org list body.

Returns a plist:
  :graph-rows  — list of table row-lists for #+graph:
  :node-rows   — list of table row-lists for #+node:
  :edge-rows   — list of table row-lists for #+edge:
  :ranks       — list of strings, each a space-sep group of node IDs
  :clusters    — alist of (id . (:label LABEL :rows ROWS))
  :list-body   — string: the lines that form the Org plain list"
  (let (graph-rows node-rows edge-rows ranks clusters
        current-cluster-id list-lines
        (mode nil))
    (dolist (line (s-split "\n" raw))
      (let ((trimmed (s-trim line)))
        (cond
         ;; #+graph: / #+node: / #+edge: switch table-collection mode
         ((s-matches-p "^#\\+graph:" trimmed) (setq mode 'graph current-cluster-id nil))
         ((s-matches-p "^#\\+node:"  trimmed) (setq mode 'node  current-cluster-id nil))
         ((s-matches-p "^#\\+edge:"  trimmed) (setq mode 'edge  current-cluster-id nil))
         ;; #+rank: id1 id2 …
         ((s-matches-p "^#\\+rank:" trimmed)
          (setq mode nil current-cluster-id nil)
          (let ((ids (s-trim (substring trimmed (length "#+rank:")))))
            (unless (s-blank-str-p ids)
              (push ids ranks))))
         ;; #+cluster: id "Optional Label"
         ((s-matches-p "^#\\+cluster:" trimmed)
          (setq mode 'cluster)
          (-let [(_ id label)
                 (s-match "^#\\+cluster:[ \t]+\\([^ \t\"]+\\)\\(?:[ \t]+\\(\"[^\"]*\"\\|[^ \t]+\\)\\)?"
                           trimmed)]
            (setq current-cluster-id id)
            (push (cons id (list :label (or label id) :rows nil)) clusters)))
         ;; Table rows dispatched to current mode
         ((and mode (s-matches-p "^|" trimmed))
          (let ((cols (->> trimmed
                           (s-split "|")
                           (-map #'s-trim)
                           (-remove #'s-blank-str-p))))
            (when (and cols (not (s-matches-p "^[-+]+$" (car cols))))
              (pcase mode
                ('graph (push cols graph-rows))
                ('node  (push cols node-rows))
                ('edge  (push cols edge-rows))
                ('cluster
                 (when current-cluster-id
                   (let ((entry (assoc current-cluster-id clusters)))
                     (when entry
                       (plist-put (cdr entry) :rows
                                  (append (plist-get (cdr entry) :rows)
                                          (list cols)))))))))))
         ;; Everything else = list body (reset mode on non-table non-directive lines)
         (t
          (unless (s-blank-str-p trimmed)
            (setq mode nil current-cluster-id nil))
          (push line list-lines)))))
    (list :graph-rows  (nreverse graph-rows)
          :node-rows   (nreverse node-rows)
          :edge-rows   (nreverse edge-rows)
          :ranks       (nreverse ranks)
          :clusters    (nreverse clusters)
          :list-body   (s-join "\n" (nreverse list-lines)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; List walker

(defun org--graph-list->dot (lst parent-id seen-nodes cluster-membership)
  "Walk one org-list-to-lisp item LST and return a list of dot statement strings.

PARENT-ID is the dot ID of the parent node (nil for root items).
SEEN-NODES is a hash-table of node-id → t for already-declared nodes.
CLUSTER-MEMBERSHIP is a hash-table we populate: node-id → cluster-id.

Emission order per node:
  1. Incoming nesting-edge(s) from parent (one per EDGE row, or one plain arc)
  2. Explicit `to` cross-edges from this node
  3. Node declaration (first occurrence only)
  4. Recurse into children"
  (cond
   ((symbolp lst) nil)
   ((symbolp (car lst)) (org--graph-list->dot (cadr lst) parent-id seen-nodes cluster-membership))
   (t
    (-let* (((raw-text children) lst)
            ((name-part table-part) (s-split-up-to "\n" (s-trim raw-text) 1))
            (node-name   (s-trim name-part))
            (table-rows  (org--graph-parse-table table-part))
            (id-row      (--first (equal (car it) "id")    table-rows))
            (to-rows     (--filter (equal (car it) "to")   table-rows))
            (edge-rows   (--filter (equal (car it) "EDGE") table-rows))
            (cluster-row (--first (equal (car it) "cluster") table-rows))
            (node-attrs  (--filter (not (member (car it) '("id" "to" "EDGE" "cluster")))
                                   table-rows))
            (node-id     (or (cadr id-row) node-name))
            (first-time  (not (gethash node-id seen-nodes))))

      ;; Track cluster membership
      (when cluster-row
        (puthash node-id (cadr cluster-row) cluster-membership))

      (append
       ;; 1. Nesting-edge(s): parent → this node
       (when parent-id
         (if edge-rows
             (--map (let ((attr-str (org--graph-parse-attrs (cadr it))))
                      (if (s-blank-str-p attr-str)
                          (format "%s -> %s;" parent-id node-id)
                        (format "%s -> %s [ %s ];" parent-id node-id attr-str)))
                    edge-rows)
           (list (format "%s -> %s;" parent-id node-id))))

       ;; 2. Explicit `to` cross-edges
       (--map (let* ((target   (cadr it))
                     (attr-str (org--graph-parse-attrs (nth 2 it))))
                (if (s-blank-str-p attr-str)
                    (format "%s -> %s;" node-id target)
                  (format "%s -> %s [ %s ];" node-id target attr-str)))
              to-rows)

       ;; 3. Node declaration (first occurrence only)
       (when first-time
         (puthash node-id t seen-nodes)
         (let ((attr-str (org--graph-attrs->dot node-attrs)))
           (list (if (s-blank-str-p attr-str)
                     (format "%s;" node-id)
                   (format "%s [ %s ];" node-id attr-str)))))

       ;; 4. Recurse into children
       (--mapcat (org--graph-list->dot it node-id seen-nodes cluster-membership)
                 children))))))

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
         (clusters        (plist-get preamble :clusters))
         (list-body       (plist-get preamble :list-body))
         (digraph?        (not (equal main-arg "undirected")))
         (graph-type      (if digraph? "digraph" "graph"))
         (arrow           (if digraph? "->" "--"))
         (seen            (make-hash-table :test #'equal))
         (cluster-mem     (make-hash-table :test #'equal))
         ;; Parse the Org list
         (parsed          (condition-case err
                              (with-temp-buffer
                                (insert list-body)
                                (goto-char (point-min))
                                (org-list-to-lisp))
                            (error
                             (user-error "#+begin_graph: invalid Org list.\n%s"
                                         (error-message-string err)))))
         ;; Walk every top-level item
         (all-stmts       (--mapcat
                           (org--graph-list->dot it nil seen cluster-mem)
                           (cdr parsed))))

    ;; Replace "->" with "--" for undirected graphs
    (when (not digraph?)
      (setq all-stmts
            (--map (s-replace "->" "--" it) all-stmts)))

    ;; Build cluster subgraphs: pull member nodes into their subgraph block
    (let* ((cluster-stmts
            (--map
             (-let* (((id . props) it)
                     (label     (plist-get props :label))
                     (attr-rows (plist-get props :rows))
                     (members   (--keep (when (equal (gethash it cluster-mem) id) it)
                                        (hash-table-keys seen)))
                     (attr-str  (org--graph-attrs->dot attr-rows)))
               (format "subgraph cluster_%s {\n    label=%s\n    %s\n    %s\n  }"
                       id label
                       (if (s-blank-str-p attr-str) "" attr-str)
                       (s-join "\n    " members)))
             clusters))
           (rank-stmts
            (--map
             (format "{ rank=same; %s }"
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
             cluster-stmts
             all-stmts)))
      (format "%s {\n  %s\n}" graph-type (s-join "\n  " lines)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C-c C-c integration

(defun org-graph--point-in-graph-block-p ()
  "Return non-nil if point is inside a #+begin_graph … #+end_graph block."
  (save-excursion
    (let ((pos (point))
          (found nil))
      (goto-char (point-min))
      (while (and (not found)
                  (re-search-forward "^[ \t]*#\\+begin_graph\\b" nil t))
        (let ((block-start (match-beginning 0)))
          (when (re-search-forward "^[ \t]*#\\+end_graph\\b" nil t)
            (when (and (>= pos block-start) (<= pos (match-end 0)))
              (setq found t)))))
      found)))

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

(defun org-graph--cc-handler ()
  "Render the #+begin_graph block at point to a PNG file.
Inserts (or replaces) a #+RESULTS: [[file:...]] link after the block.
Returns t (claiming C-c C-c) when point is inside a graph block, nil otherwise."
  (when (and (fboundp 'org-special-block-after-point)
             (org-graph--point-in-graph-block-p))
    (save-excursion
      ;; Navigate to #+begin_graph and parse the block
      (re-search-backward "^[ \t]*#\\+begin_graph\\b" nil t)
      (let* ((block    (org-special-block-after-point "graph"))
             (kwdargs  (org-special-block-kwdargs block))
             (raw      (org-special-block-contents block))
             (main-arg (or (org-special-block-main-arg block) "directed"))
             (end-pt   (org-special-block-end-point block))
             (outfile  (or (plist-get kwdargs :file)
                           (format "graph-%s.png"
                                   (format-time-string "%Y%m%d-%H%M%S"))))
             (dot      (org--graph-raw->dot raw main-arg))
             (dotfile  (make-temp-file "org-graph-" nil ".dot")))
        (write-region dot nil dotfile)
        (if (zerop (shell-command (format "dot -Tpng %s -o %s" dotfile outfile)))
            (progn
              (org-graph--insert-results end-pt outfile)
              ;; Refresh inline images so the result appears immediately
              (when (fboundp 'org-display-inline-images)
                (org-display-inline-images))
              (message "Graph rendered → %s" (expand-file-name outfile)))
          (user-error "dot failed — is Graphviz installed? (M-x shell-command: dot -V)"))
        t))))

(add-hook 'org-ctrl-c-ctrl-c-hook #'org-graph--cc-handler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-defblock registration

(defun org-graph--render-to-png (dot outfile)
  "Render DOT source to OUTFILE (PNG) via the dot command.
Returns OUTFILE on success, nil on failure."
  (let ((dotfile (make-temp-file "org-graph-" nil ".dot")))
    (write-region dot nil dotfile)
    (when (zerop (shell-command
                  (format "dot -Tpng %s -o %s" dotfile outfile)))
      outfile)))

(defun org-graph--png-to-base64 (pngfile)
  "Return a base64 string of the binary contents of PNGFILE."
  (base64-encode-string
   (with-temp-buffer
     (set-buffer-multibyte nil)
     (insert-file-contents-literally pngfile)
     (buffer-string))
   t))

(org-defblock graph (main-arg "directed" file nil)
  "Render an Org list + tables as a Graphviz dot graph.

MAIN-ARG is \"directed\" (default, produces digraph) or \"undirected\" (graph).
Pass :file foo.png on the #+begin_graph line to fix the output filename;
otherwise an auto-generated timestamped name is used.

For HTML export the block renders to an inline base64 PNG.
For other backends it emits a #+begin_src dot block.

Within the block body:

  #+graph: table  — global graph[ ] attributes
  #+node:  table  — default node[ ] attributes
  #+edge:  table  — default edge[ ] attributes
  #+rank:  n1 n2  — { rank=same; n1; n2 }
  #+cluster: id \"Label\" + table — subgraph cluster_id { … }

Each list item declares a node.  Nesting encodes edges (parent → child).
A two-column inline table on a list item supplies node attributes.

  | EDGE | attr-string |  — arc attrs for the incoming (parent→this) edge;
                            N such rows = N parallel arcs.
  | id   | alias       |  — cross-reference name for this node.
  | to   | target | attrs | — explicit outgoing edge (3 columns; attrs optional).
  | cluster | name |      — assign node to a named cluster.
  | key  | value  |       — any other key styles the node itself.

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
       (let ((pngfile (make-temp-file "org-graph-" nil ".png")))
         (if (org-graph--render-to-png dot pngfile)
             (format "<img src=\"data:image/png;base64,%s\" alt=\"graph\" />"
                     (org-graph--png-to-base64 pngfile))
           ;; dot unavailable — emit the raw dot source in a <pre>
           (format "<pre>%s</pre>" (s-replace "<" "&lt;" dot)))))
      (_
       (format "#+begin_src dot :file %s\n%s\n#+end_src" outfile dot)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'org-special-block-extras-graph)
;;; org-special-block-extras-graph.el ends here
