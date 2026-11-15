;;; org-graph-inline-cluster-tests.el --- Inline cluster (shape=cluster)  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; Inline cluster (shape=cluster)

(define-graph-test "shape=cluster emits inline cluster block with attrs"
  :input "
+ <<grp>> G
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
+ <<legend>> Legend
  | shape | cluster |
  | style | filled  |
  + leg_solid
    | label | \"solid = direct lineage\" |
    | shape | plaintext                  |
  + leg_dashed
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

;;; Arcs leaving a box

(define-graph-test "An arc leaving a box is printed after it, so its target stays out of the box"
  :input "
+ Battles :container:
  + Conquest
    + [[sufyan]]
+ Antagonists :container:
  + <<sufyan>> Abu Sufyan"
  :output "digraph {
  subgraph cluster_Battles {
    label=Battles
    Conquest [ label=Conquest ];
  }
  Conquest -> sufyan;
  subgraph cluster_Antagonists {
    label=Antagonists
    sufyan [ label=\"Abu Sufyan\" ];
  }
}")

(define-graph-test "An arc leaving nested boxes takes their #+edge: rows along, beneath its own"
  :input "
+ Outer :container:
  #+edge: color=red style=dashed
  + Inner :container:
    #+edge: color=blue
    + A
      + B
      + [[C]]
        | :edge.style | bold |
+ C"
  :output "digraph {
  subgraph cluster_Outer {
    label=Outer
    edge [ color=red style=dashed ]
    subgraph cluster_Inner {
    label=Inner
    edge [ color=blue ]
    A [ label=A ];
    A -> B;
    B [ label=B ];
  }
  }
  A -> C [ color=blue style=bold ];
  C [ label=C ];
}")
