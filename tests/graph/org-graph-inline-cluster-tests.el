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

(define-graph-test "A member an earlier arc mentions restates its box's #+node: rows, which Graphviz would not apply"
  :input "
+ Abu Talib
  + [[ali]]
+ Imams :container:
  #+node: fillcolor=green
  + <<ali>> Ali
    + Hasan"
  :output "digraph {
  \"Abu Talib\" [ label=\"Abu Talib\" ];
  \"Abu Talib\" -> ali;
  subgraph cluster_Imams {
    label=Imams
    node [ fillcolor=green style=filled ]
    ali [ fillcolor=green label=Ali ];
    ali -> Hasan;
    Hasan [ label=Hasan ];
  }
}")

(define-graph-test "An ordered legend is a row along the graph's foot: its entries' ends share a rank, beneath every sink"
  :input "
#+name: red color=red
#+name: blue fillcolor=blue
+ A
  + [[B][:red:]]
+ B
+ Legend :container:
  1. reds :red:
  2. blues :blue:"
  :output "digraph {
  graph [ newrank=true ]
  A [ label=A ];
  A -> B [ color=red ];
  B [ label=B ];
  subgraph cluster_Legend {
    label=Legend
    legend_1 [ shape=point style=invis ];
    legend_1_end [ shape=point style=invis ];
    legend_1 -> legend_1_end [ color=red label=reds ];
    legend_2 [ fillcolor=blue label=blues style=filled ];
  }
  { rank=same; legend_1_end; legend_2 }
  B -> legend_1 [ style=invis weight=0 ];
}")

(define-graph-test "A legend's untitled boxes, each ordered, are rows stacked beneath one another"
  :input "
#+name: blue fillcolor=blue
#+name: green fillcolor=green
+ A
+ Legend :container:
  + :container:
    1. blues :blue:
  + :container:
    1. greens :green:"
  :output "digraph {
  graph [ newrank=true ]
  A [ label=A ];
  subgraph cluster_Legend {
    label=Legend
    subgraph cluster_anonymous_6 {
    label=\"\"
    legend_1 [ fillcolor=blue label=blues style=filled ];
  }
    subgraph cluster_anonymous_8 {
    label=\"\"
    legend_2 [ fillcolor=green label=greens style=filled ];
  }
  }
  { rank=same; legend_1 }
  A -> legend_1 [ style=invis weight=0 ];
  { rank=same; legend_2 }
  legend_1 -> legend_2 [ style=invis weight=0 ];
}")

(define-graph-test "A legend samples a backward arc, dir=back, pointing forward, its arrow the arc's arrowtail"
  :input "
#+name: killed color=red dir=back arrowhead=dot
+ A
  + [[B][:killed:]]
+ B
+ Legend :container:
  - killed :killed:"
  :output "digraph {
  A [ label=A ];
  A -> B [ color=red dir=back arrowhead=dot ];
  B [ label=B ];
  subgraph cluster_Legend {
    label=Legend
    legend_1 [ shape=point style=invis ];
    legend_1_end [ shape=point style=invis ];
    legend_1 -> legend_1_end [ color=red arrowhead=normal label=killed ];
  }
}")

(define-graph-test "A legend tagged :@start: sits atop the graph: nothing holds its rows down beneath the sinks"
  :input "
#+name: blue fillcolor=blue
+ A
  + B
+ Legend :container:@start:
  1. blues :blue:"
  :output "digraph {
  graph [ newrank=true ]
  A [ label=A ];
  A -> B;
  B [ label=B ];
  subgraph cluster_Legend {
    label=Legend
    legend_1 [ fillcolor=blue label=blues style=filled ];
  }
  { rank=same; legend_1 }
}")
