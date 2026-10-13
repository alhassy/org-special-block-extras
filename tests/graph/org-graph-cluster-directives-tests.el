;;; org-graph-cluster-directives-tests.el --- Cluster-scoped directives (#+node: / #+edge: / #+graph: / #+rank: inside cluster item)  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; Cluster-scoped directives (#+node: / #+edge: / #+graph: / #+rank: inside cluster item)

(define-graph-test "cluster: #+node: inside item sets default node attrs for children"
  :input "+ G\n  | shape | cluster |\n  #+node:\n  | shape | plaintext |\n  | fontcolor | \"#6c7086\" |\n  + Solid\n  + Dashed\n"
  :output "digraph {
  subgraph cluster_G {
    label=\"G\"
    node [ shape=plaintext fontcolor=\"#6c7086\" ]
    Solid [ label=Solid ];
    Dashed [ label=Dashed ];
  }
}")

(define-graph-test "cluster: #+edge: inside item sets default edge attrs for children"
  :input "+ G\n  | shape | cluster |\n  #+edge:\n  | style | dashed |\n  + A\n  + B\n"
  :output "digraph {
  subgraph cluster_G {
    label=\"G\"
    edge [ style=dashed ]
    A [ label=A ];
    B [ label=B ];
  }
}")

(define-graph-test "cluster: #+graph: inside item sets subgraph-level attrs"
  :input "+ G\n  | shape | cluster |\n  #+graph:\n  | bgcolor | \"#181825\" |\n  + A\n"
  :output "digraph {
  subgraph cluster_G {
    label=\"G\"
    graph [ bgcolor=\"#181825\" ]
    A [ label=A ];
  }
}")

(define-graph-test "cluster: #+rank: inside item emits rank constraint within subgraph"
  :input "+ G\n  | shape | cluster |\n  #+rank: A B\n  + A\n  + B\n"
  :output "digraph {
  subgraph cluster_G {
    label=\"G\"
    { rank=same; A; B }
    A [ label=A ];
    B [ label=B ];
  }
}")

(define-graph-test "cluster: #+node: and #+edge: together inside item"
  :input "+ G\n  | shape | cluster |\n  | color | red |\n  #+node:\n  | shape | plaintext |\n  #+edge:\n  | style | dotted |\n  + X\n  + Y\n"
  :output "digraph {
  subgraph cluster_G {
    label=\"G\"
    color=red
    node [ shape=plaintext ]
    edge [ style=dotted ]
    X [ label=X ];
    Y [ label=Y ];
  }
}")

(define-graph-test "cluster: child explicit attrs override #+node: defaults"
  ;; X carries its own shape=record, which must win over the cluster's #+node: shape=plaintext
  :input "+ G\n  | shape | cluster |\n  #+node:\n  | shape | plaintext |\n  + X\n    | shape | ellipse |\n  + Y\n"
  :output "digraph {
  subgraph cluster_G {
    label=\"G\"
    node [ shape=plaintext ]
    X [ shape=ellipse label=X ];
    Y [ label=Y ];
  }
}")
