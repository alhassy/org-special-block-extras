;;; org-graph-preamble-list-body-tests.el --- Preamble + list-body edge cases  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; Preamble + list-body edge cases

(define-graph-test "regression: blank line between last preamble directive and first list item"
  ;; A blank line between a preamble directive and the first list item caused
  ;; org-list-to-lisp to be called on a buffer starting with a newline,
  ;; crashing with (max) / 0 arguments.  s-trim in org--graph-raw->dot fixes this.
  :input "#+graph:\n| rankdir | TB |\n\n+ G\n  | shape | cluster |\n  | label | \"G\" |\n  | color | red |\n  + A\n"
  :output "digraph {
  graph [ rankdir=TB ]
  subgraph cluster_G {
    label=\"G\"
    color=red
    A [ label=A ];
  }
}")
