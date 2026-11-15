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

(deftest "a reserved :KEY reaching dot is an error naming the plain key it wants, rather than dot's own syntax error" [graph]
  (should (string-match-p "‘:edge.dir’ is no Graphviz attribute here.*wants ‘dir’"
                          (cadr (should-error (org--graph-raw->dot "
#+name: reply
| :edge.dir | back |
+ a
  + hi :reply: :: [[b]]
+ <<b>> b" nil) :type 'user-error)))))

;;; Built-in templates: UML arcs, state-machine nodes, ER cardinalities; and :mindmap

(deftest "a reference's UML tag draws its arc as UML does: a hollow triangle, a dashed one, diamonds, a dashed vee" [graph]
  (let ((dot (org--graph-raw->dot "
+ Dog
  + [[Animal]] :inherits:
  + [[Pet]] :implements:
  + [[Tail]] :composes:
  + [[Owner]] :aggregates:
  + [[Bone]] :depends:
+ <<Animal>> Animal
+ <<Pet>> Pet
+ <<Tail>> Tail
+ <<Owner>> Owner
+ <<Bone>> Bone" nil)))
    (should (string-search "Dog -> Animal [ arrowhead=empty ];" dot))
    (should (string-search "Dog -> Pet [ arrowhead=empty style=dashed ];" dot))
    (should (string-search "Dog -> Tail [ dir=both arrowtail=diamond arrowhead=none ];" dot))
    (should (string-search "Dog -> Owner [ dir=both arrowtail=odiamond arrowhead=none ];" dot))
    (should (string-search "Dog -> Bone [ arrowhead=vee style=dashed ];" dot))))

(deftest "a state machine's tags draw a start dot, rounded states, a choice diamond, and a ringed final dot" [graph]
  (let ((dot (org--graph-raw->dot "
+ <<start>> :start:
  + Idle :state:
    + coin :: <<check>> :choice:
      + done :: <<end>> :final:" nil)))
    (should (string-search "start [ shape=point width=0.2 label=\"\" ];" dot))
    (should (string-search "Idle [ shape=box style=rounded label=Idle ];" dot))
    (should (string-search "check [ shape=diamond width=0.3 height=0.3 fixedsize=true label=\"\" ];" dot))
    (should (string-search "end [ shape=point width=0.2 peripheries=2 label=\"\" ];" dot))))

(deftest "an ER cardinality tag, on a description term, draws crow's feet and bars, read from parent to child" [graph]
  (let ((dot (org--graph-raw->dot "
+ Customer
  + places :one-to-many: :: Order
  + has :one-to-one: :: Account" nil)))
    (should (string-search "Customer -> Order [ label=places dir=both arrowtail=teetee arrowhead=crowtee ];" dot))
    (should (string-search "Customer -> Account [ label=has dir=both arrowtail=teetee arrowhead=teetee ];" dot))))

(deftest "a graph's own template of a built-in's name wins" [graph]
  (should (string-search "Dog -> Animal [ color=red ];"
                         (org--graph-raw->dot "
#+name: inherits
| color | red |
+ Dog
  + [[Animal]] :inherits:
+ <<Animal>> Animal" nil))))

(deftest "a :mindmap t graph is undirected, by twopi, rooted at its first item, its nodes rounded and filled; the header wins" [graph]
  (let ((dot (org--graph-raw->dot "
+ Org mode
  + Tables
  + Babel" nil '(:mindmap t :ranksep 2))))
    (should (string-prefix-p "graph {" dot))
    (should (string-search "Org -- Tables" (s-replace "\"Org mode\"" "Org" dot)))
    (should (string-search "layout=twopi overlap=false root=\"Org mode\" ranksep=2" dot))
    (should (string-search "node [ shape=box style=\"rounded,filled\" color=gray40 ]" dot))))

(deftest "a :mindmap t graph fills each node by its depth, from `org-graph-mindmap-palette'; a node's own fillcolor wins" [graph]
  (let* ((org-graph-mindmap-palette '("red" "green" "blue"))
         (dot (org--graph-raw->dot "
+ Root
  + Child
    + Grandchild
      + Great
  + Own
    | fillcolor | gold |" nil '(:mindmap t))))
    (should (string-match-p "Root \\[ fillcolor=red label=Root \\]" dot))
    (should (string-match-p "Child \\[ fillcolor=green label=Child \\]" dot))
    (should (string-match-p "Grandchild \\[ fillcolor=blue label=Grandchild \\]" dot))
    (should (string-match-p "Great \\[ fillcolor=red label=Great \\]" dot))
    (should (string-match-p "Own \\[ fillcolor=gold label=Own \\]" dot))
    (should (string-search "node [ shape=box style=\"rounded,filled\" color=gray40 ]" dot))))
