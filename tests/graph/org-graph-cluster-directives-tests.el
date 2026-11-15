;;; org-graph-cluster-directives-tests.el --- Cluster-scoped directives (#+node: / #+edge: / #+graph: inside cluster item)  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; Cluster-scoped directives (#+node: / #+edge: / #+graph: inside cluster item)

(define-graph-test "cluster: #+node: inside item sets default node attrs for children"
  :input "
+ G
  | shape | cluster |
  #+node:
  | shape     | plaintext   |
  | fontcolor | \"#6c7086\" |
  + Solid
  + Dashed"
  :output "digraph {
  subgraph cluster_G {
    label=G
    node [ shape=plaintext fontcolor=\"#6c7086\" ]
    Solid [ label=Solid ];
    Dashed [ label=Dashed ];
  }
}")

(define-graph-test "cluster: #+edge: inside item sets default edge attrs for children"
  :input "
+ G
  | shape | cluster |
  #+edge:
  | style | dashed |
  + A
  + B"
  :output "digraph {
  subgraph cluster_G {
    label=G
    edge [ style=dashed ]
    A [ label=A ];
    B [ label=B ];
  }
}")

(define-graph-test "cluster: #+graph: inside item sets subgraph-level attrs"
  :input "
+ G
  | shape | cluster |
  #+graph:
  | bgcolor | \"#181825\" |
  + A"
  :output "digraph {
  subgraph cluster_G {
    label=G
    graph [ bgcolor=\"#181825\" ]
    A [ label=A ];
  }
}")

(define-graph-test "cluster: its members' :@X: tags align them, under newrank=true, lest Graphviz pull them out of the box"
  :input "
+ G
  | shape | cluster |
  + A :@pair:
  + B :@pair:"
  :output "digraph {
  graph [ newrank=true ]
  subgraph cluster_G {
    label=G
    A [ label=A ];
    B [ label=B ];
  }
  { rank=same; A; B }
}")

(define-graph-test "cluster: #+node: and #+edge: together inside item"
  :input "
+ G
  | shape | cluster |
  | color | red     |
  #+node:
  | shape | plaintext |
  #+edge:
  | style | dotted |
  + X
  + Y"
  :output "digraph {
  subgraph cluster_G {
    label=G
    color=red
    node [ shape=plaintext ]
    edge [ style=dotted ]
    X [ label=X ];
    Y [ label=Y ];
  }
}")

(define-graph-test "cluster: child explicit attrs override #+node: defaults"
  ;; X carries its own shape=record, which must win over the cluster's #+node: shape=plaintext
  :input "
+ G
  | shape | cluster |
  #+node:
  | shape | plaintext |
  + X
    | shape | ellipse |
  + Y"
  :output "digraph {
  subgraph cluster_G {
    label=G
    node [ shape=plaintext ]
    X [ shape=ellipse label=X ];
    Y [ label=Y ];
  }
}")

(deftest "a :container_TITLE: tag's member, aligned with a node outside its box, sets newrank=true too; the graph's own wins"
  (should (s-contains-p "graph [ newrank=true ]" (org--graph-raw->dot "
+ a :container_Box:@pals:
+ b :@pals:" nil)))
  (should (s-contains-p "graph [ newrank=false ]" (org--graph-raw->dot "
#+graph: newrank=false
+ a :container_Box:@pals:
+ b :@pals:" nil))))
