;;; org-graph-raw-to-dot-tests.el --- org--graph-raw->dot — snapshot relation tests  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; org--graph-raw->dot — snapshot relation tests

(define-graph-test "single node"
  :input "
+ A"
  :output "digraph {
  A [ label=A ];
}")

(deftest "`org--graph-raw->dot' undirected graph uses -- arrows and graph keyword"
  (should (equal "graph {\n  A [ label=A ];\n  A -- B;\n  B [ label=B ];\n}"
                 (org--graph-raw->dot "+ A\n  + B\n" "undirected"))))

(define-graph-test "simple nesting A→B"
  :input "
+ A
  + B"
  :output "digraph {
  A [ label=A ];
  A -> B;
  B [ label=B ];
}")

(define-graph-test "A→B→C chain"
  :input "
+ A
  + B
    + C"
  :output "digraph {
  A [ label=A ];
  A -> B;
  B [ label=B ];
  B -> C;
  C [ label=C ];
}")

(define-graph-test ":edge. row attaches label"
  :input "
+ A
  + B
    | :edge.label | calls |"
  :output "digraph {
  A [ label=A ];
  A -> B [ label=calls ];
  B [ label=B ];
}")

(define-graph-test "repeating an :edge. key starts a second, parallel, arc"
  :input "
+ A
  + B
    | :edge.label | first  |
    | :edge.label | second |"
  :output "digraph {
  A [ label=A ];
  A -> B [ label=first ];
  A -> B [ label=second ];
  B [ label=B ];
}")

(define-graph-test "id row makes cross-reference alias"
  :input "
+ lambda
  | :id | lam |
  + church
    | :edge.label | encodes |"
  :output "digraph {
  lam [ label=lambda ];
  lam -> church [ label=encodes ];
  church [ label=church ];
}")

(define-graph-test "second occurrence emits edge only, not a second node decl"
  :input "
+ A
  + B
+ C
  + B
    | :edge.label | again |"
  :output "digraph {
  A [ label=A ];
  A -> B;
  B [ label=B ];
  C [ label=C ];
  C -> B [ label=again ];
}")

(deftest "`org--graph-raw->dot' explicit to row produces cross-edge"
  (should (equal "digraph {\n  a [ label=A ];\n  B -> a [ label=back ];\n  B [ label=B ];\n}"
                 (org--graph-raw->dot
                  (concat "+ A\n  | :id | a |\n"
                          "+ B\n  | :to | a |\n  | :to.label | back |\n")
                  "directed"))))

(define-graph-test "#+rank: emits rank=same block for nodes without a common parent"
  :input "
#+rank: A B
+ A
+ B"
  :output "digraph {
  { rank=same; A; B }
  A [ label=A ];
  B [ label=B ];
}")

(define-graph-test "rank=same on a node constrains its immediate children"
  :input "
+ root
  | :children-rank | same |
  + A
  + B
  + C"
  :output "digraph {
  root [ label=root ];
  root -> A;
  A [ label=A ];
  root -> B;
  B [ label=B ];
  root -> C;
  C [ label=C ];
  { rank=same; A; B; C }
}")

(define-graph-test "rank=source on a node constrains its immediate children"
  :input "
+ entry
  | :children-rank | source |
  + X
  + Y"
  :output "digraph {
  entry [ label=entry ];
  entry -> X;
  X [ label=X ];
  entry -> Y;
  Y [ label=Y ];
  { rank=source; X; Y }
}")

(define-graph-test "rank row respects child id aliases"
  :input "
+ root
  | :children-rank | same |
  + alpha
    | :id | a |
  + beta
    | :id | b |"
  :output "digraph {
  root [ label=root ];
  root -> a;
  a [ label=alpha ];
  root -> b;
  b [ label=beta ];
  { rank=same; a; b }
}")

(define-graph-test "#+graph: table sets graph-level attrs"
  :input "
#+graph:
| rankdir | TB |
+ A"
  :output "digraph {
  graph [ rankdir=TB ]
  A [ label=A ];
}")

(define-graph-test "#+node: table sets default node attrs"
  :input "
#+node:
| shape | box |
+ A"
  :output "digraph {
  node [ shape=box ]
  A [ label=A ];
}")

(define-graph-test "node attrs styled inline"
  :input "
+ A
  | shape | ellipse |
  | color | red     |"
  :output "digraph {
  A [ shape=ellipse color=red label=A ];
}")
