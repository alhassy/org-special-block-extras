;;; org-graph-raw-to-dot-tests.el --- org--graph-raw->dot — snapshot relation tests  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; org--graph-raw->dot — snapshot relation tests

(define-graph-test "single node"
  :input "+ A\n"
  :output "digraph {
  A [ label=A ];
}")

(deftest "`org--graph-raw->dot' undirected graph uses -- arrows and graph keyword"
  (should (equal "graph {\n  A [ label=A ];\n  A -- B;\n  B [ label=B ];\n}"
                 (org--graph-raw->dot "+ A\n  + B\n" "undirected"))))

(define-graph-test "simple nesting A→B"
  :input "+ A\n  + B\n"
  :output "digraph {
  A [ label=A ];
  A -> B;
  B [ label=B ];
}")

(define-graph-test "A→B→C chain"
  :input "+ A\n  + B\n    + C\n"
  :output "digraph {
  A [ label=A ];
  A -> B;
  B [ label=B ];
  B -> C;
  C [ label=C ];
}")

(define-graph-test "EDGE row attaches label"
  :input "+ A\n  + B\n    | EDGE | label=calls |\n"
  :output "digraph {
  A [ label=A ];
  A -> B [ label=calls ];
  B [ label=B ];
}")

(define-graph-test "two EDGE rows produce two parallel arcs"
  :input "+ A\n  + B\n    | EDGE | label=first |\n    | EDGE | label=second |\n"
  :output "digraph {
  A [ label=A ];
  A -> B [ label=first ];
  A -> B [ label=second ];
  B [ label=B ];
}")

(define-graph-test "id row makes cross-reference alias"
  :input "+ lambda\n  | id | lam |\n  + church\n    | EDGE | label=encodes |\n"
  :output "digraph {
  lam [ label=lambda ];
  lam -> church [ label=encodes ];
  church [ label=church ];
}")

(define-graph-test "second occurrence emits edge only, not a second node decl"
  :input "+ A\n  + B\n+ C\n  + B\n    | EDGE | label=again |\n"
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
                  (concat "+ A\n  | id | a |\n"
                          "+ B\n  | to | a | label=back |\n")
                  "directed"))))

(define-graph-test "#+rank: emits rank=same block for nodes without a common parent"
  :input "#+rank: A B\n+ A\n+ B\n"
  :output "digraph {
  { rank=same; A; B }
  A [ label=A ];
  B [ label=B ];
}")

(define-graph-test "rank=same on a node constrains its immediate children"
  :input "+ root\n  | rank | same |\n  + A\n  + B\n  + C\n"
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
  :input "+ entry\n  | rank | source |\n  + X\n  + Y\n"
  :output "digraph {
  entry [ label=entry ];
  entry -> X;
  X [ label=X ];
  entry -> Y;
  Y [ label=Y ];
  { rank=source; X; Y }
}")

(define-graph-test "rank row respects child id aliases"
  :input "+ root\n  | rank | same |\n  + alpha\n    | id | a |\n  + beta\n    | id | b |\n"
  :output "digraph {
  root [ label=root ];
  root -> a;
  a [ label=alpha ];
  root -> b;
  b [ label=beta ];
  { rank=same; a; b }
}")

(define-graph-test "#+graph: table sets graph-level attrs"
  :input "#+graph:\n| rankdir | TB |\n+ A\n"
  :output "digraph {
  graph [ rankdir=TB ]
  A [ label=A ];
}")

(define-graph-test "#+node: table sets default node attrs"
  :input "#+node:\n| shape | box |\n+ A\n"
  :output "digraph {
  node [ shape=box ]
  A [ label=A ];
}")

(define-graph-test "node attrs styled inline"
  :input "+ A\n  | shape | ellipse |\n  | color | red |\n"
  :output "digraph {
  A [ shape=ellipse color=red label=A ];
}")
