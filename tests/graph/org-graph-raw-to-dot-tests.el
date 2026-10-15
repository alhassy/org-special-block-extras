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
  (should (equal "graph {
  A [ label=A ];
  A -- B;
  B [ label=B ];
}"
                 (org--graph-raw->dot "
+ A
  + B" nil (quote (:directed nil))))))

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

(deftest "repeating an :edge. key is a user error, since a parallel arc is a second child item"
  (should-error (org--graph-raw->dot "
+ A
  + B
    | :edge.label | first  |
    | :edge.label | second |" nil) :type 'user-error))

(deftest "two templates setting one :edge. key are no repeat: the earlier template wins, as with Emacs faces"
  (should (s-contains-p "A -> B [ color=red ];" (org--graph-raw->dot "
#+name: hot
| :edge.color | red |
#+name: cold
| :edge.color | blue |
+ A
  + B :hot:cold:" nil))))

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

(deftest "`org--graph-raw->dot' a lone-link child draws a cross-edge to a node declared elsewhere"
  (should (equal "digraph {
  a [ label=A ];
  B [ label=B ];
  B -> a [ label=back ];
}"
                 (org--graph-raw->dot
                  "
+ A
  | :id | a |
+ B
  + back :: [[a]]"
                  nil))))

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
