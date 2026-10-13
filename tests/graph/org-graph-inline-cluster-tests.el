;;; org-graph-inline-cluster-tests.el --- Inline cluster (shape=cluster)  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; Inline cluster (shape=cluster)

(define-graph-test "shape=cluster emits inline cluster block with attrs"
  :input "+ G\n  | id | grp |\n  | shape | cluster |\n  | color | red |\n  + A\n  + B\n"
  :output "digraph {
  subgraph cluster_grp {
    label=\"G\"
    color=red
    A [ label=A ];
    B [ label=B ];
  }
}")

(define-graph-test "shape=cluster: node name used as label, spaces normalised in cluster id"
  :input "+ My Group\n  | shape | cluster |\n  + X\n"
  :output "digraph {
  subgraph cluster_My_Group {
    label=\"My Group\"
    X [ label=X ];
  }
}")

(define-graph-test "shape=cluster: explicit label row overrides node name"
  :input "+ grp\n  | shape | cluster |\n  | label | \"The Group\" |\n  + A\n"
  :output "digraph {
  subgraph cluster_grp {
    label=\"The Group\"
    A [ label=A ];
  }
}")

(define-graph-test "shape=cluster: children carry their own attrs"
  :input "+ Legend\n  | id | legend |\n  | shape | cluster |\n  | style | filled |\n  + leg_solid\n    | id | leg_solid |\n    | label | \"solid = direct lineage\" |\n    | shape | plaintext |\n  + leg_dashed\n    | id | leg_dashed |\n    | label | \"dashed = inspiration\" |\n    | shape | plaintext |\n"
  :output "digraph {
  subgraph cluster_legend {
    label=\"Legend\"
    style=filled
    leg_solid [ label=\"solid = direct lineage\" shape=plaintext ];
    leg_dashed [ label=\"dashed = inspiration\" shape=plaintext ];
  }
}")
