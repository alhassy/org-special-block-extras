;;; org-graph-inline-cluster-tests.el --- Inline cluster (shape=cluster)  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; Inline cluster (shape=cluster)

(define-graph-test "shape=cluster emits inline cluster block with attrs"
  :input "
+ G
  | :id   | grp     |
  | shape | cluster |
  | color | red     |
  + A
  + B"
  :output "digraph {
  subgraph cluster_grp {
    label=G
    color=red
    A [ label=A ];
    B [ label=B ];
  }
}")

(define-graph-test "shape=cluster: node name used as label, spaces normalised in cluster id"
  :input "
+ My Group
  | shape | cluster |
  + X"
  :output "digraph {
  subgraph cluster_My_Group {
    label=\"My Group\"
    X [ label=X ];
  }
}")

(define-graph-test "shape=cluster: explicit label row overrides node name"
  :input "
+ grp
  | shape | cluster       |
  | label | \"The Group\" |
  + A"
  :output "digraph {
  subgraph cluster_grp {
    label=\"The Group\"
    A [ label=A ];
  }
}")

(define-graph-test "shape=cluster: children carry their own attrs"
  :input "
+ Legend
  | :id   | legend  |
  | shape | cluster |
  | style | filled  |
  + leg_solid
    | :id   | leg_solid                  |
    | label | \"solid = direct lineage\" |
    | shape | plaintext                  |
  + leg_dashed
    | :id   | leg_dashed               |
    | label | \"dashed = inspiration\" |
    | shape | plaintext                |"
  :output "digraph {
  subgraph cluster_legend {
    label=Legend
    style=filled
    leg_solid [ label=\"solid = direct lineage\" shape=plaintext ];
    leg_dashed [ label=\"dashed = inspiration\" shape=plaintext ];
  }
}")
