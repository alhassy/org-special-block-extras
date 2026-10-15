;;; org-graph-org-syntax-tests.el --- Org's own syntax in a graph: targets, links, markup, ordered lists, the header  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; <<targets>> name nodes, [[links]] draw arcs

(define-graph-test "a <<target>> is the node's id, and a [[link]] to it is an arc"
  :input "
+ <<lam>> lambda
  + church
+ lisp [[lam]]"
  :output "digraph {
  lam [ label=lambda ];
  lam -> church;
  church [ label=church ];
  lisp -> lam;
  lisp [ label=lisp ];
}")

(define-graph-test "a [[TARGET][LABEL]] link is an arc labelled LABEL; an item may have several links"
  :input "
+ lisp [[church][encodes]] [[haskell]]
+ church
+ haskell"
  :output "digraph {
  lisp -> church [ label=encodes ];
  lisp -> haskell;
  lisp [ label=lisp ];
  church [ label=church ];
  haskell [ label=haskell ];
}")

(define-graph-test "an item that is a lone [[TARGET][LABEL]] link is the node TARGET, its parent's arc labelled LABEL"
  :input "
+ lambda
  + [[church][encodes]]"
  :output "digraph {
  lambda [ label=lambda ];
  lambda -> church [ label=encodes ];
  church [ label=church ];
}")

(define-graph-test "`:edge[TARGET].KEY' rows describe the arc to TARGET, from a link or a `:to' row, overriding a link's label"
  :input "
+ lisp [[church][encodes]]
  | :to                  | haskell  |
  | :edge[church].label  | inspired |
  | :edge[church].style  | dotted   |
  | :edge[haskell].color | red      |"
  :output "digraph {
  lisp -> haskell [ color=red ];
  lisp -> church [ label=inspired style=dotted ];
  lisp [ label=lisp ];
}")

(deftest "an `:edge[TARGET].KEY' row with no arc to TARGET is a user error"
  (should-error (org--graph-raw->dot "
+ a [[b]]
  | :edge[c].style | dotted |" "directed") :type 'user-error))

(deftest "a link that is not to a node, such as a URL, is text rather than an arc"
  (should (equal "digraph {
  \"see site\" [ label=\"see site\" ];
}"
                 (org--graph-raw->dot "
+ see [[https://example.org][site]]" "directed"))))

;;; Markup becomes an HTML-like label

(define-graph-test "all six Org markers become HTML-like label tags, while the node's id is the plain text"
  :input "
+ *bold* /it/ _u_ +s+ =v= ~c~"
  :output "digraph {
  \"bold it u s v c\" [ label=<<B>bold</B> <I>it</I> <U>u</U> <S>s</S> <FONT FACE=\"monospace\">v</FONT> <FONT FACE=\"monospace\">c</FONT>> ];
}")

(define-graph-test "text in a marked-up label is HTML-escaped, both inside and outside the markup"
  :input "
+ a & =x<y="
  :output "digraph {
  \"a & x<y\" [ label=<a &amp; <FONT FACE=\"monospace\">x&lt;y</FONT>> ];
}")

(define-graph-test "names such as a_b, C++, and org-graph.el are not mistaken for markup"
  :input "
+ a_b
+ C++
+ org-graph.el"
  :output "digraph {
  a_b [ label=a_b ];
  \"C++\" [ label=\"C++\" ];
  \"org-graph.el\" [ label=\"org-graph.el\" ];
}")

(define-graph-test "a record's fields drop their markup, take a <<target>> as their port, and have [[links]] as arcs"
  :input "
+ table
  | shape | record |
  + <<p>> *left*
  + right [[other]]
+ other"
  :output "digraph {
  table [ shape=record label=\"<p> left|<right> right\" ];
  table:right -> other;
  other [ label=other ];
}")

;;; Ordered lists keep their order, on one rank

(define-graph-test "ordered children sit on one rank, in the order written"
  :input "
+ lambda
  1. church
  2. lisp"
  :output "digraph {
  lambda [ ordering=out label=lambda ];
  lambda -> church;
  church [ label=church ];
  lambda -> lisp;
  lisp [ label=lisp ];
  { rank=same; church; lisp }
}")

(define-graph-test "an ordered top-level list, or one inside a cluster, also shares a rank"
  :input "
1. a
2. b
3. box
   | shape | cluster |
   1. c
   2. d"
  :output "digraph {
  a [ label=a ];
  b [ label=b ];
  subgraph cluster_box {
    label=box
    c [ label=c ];
    d [ label=d ];
    { rank=same; c; d }
  }
  { rank=same; a; b; box }
}")

(define-graph-test "an explicit `:children-rank' beats an ordered list's rank=same, and a lone ordered child is left alone"
  :input "
+ p
  | :children-rank | min |
  1. a
  2. b
+ q
  1. only"
  :output "digraph {
  p [ ordering=out label=p ];
  p -> a;
  a [ label=a ];
  p -> b;
  b [ label=b ];
  { rank=min; a; b }
  q [ label=q ];
  q -> only;
  only [ label=only ];
}")

;;; The #+begin_graph line

(deftest "the header's title labels the graph, its keys are graph attributes, and :node.KEY / :edge.KEY set defaults"
  (should (equal "graph {
  graph [ label=\"Lambda calculus\" rankdir=LR ]
  node [ shape=box ]
  edge [ color=\"#f00\" ]
  a [ label=a ];
}"
                 (org--graph-raw->dot "
+ a" "Lambda calculus"
                                      '(:directed nil :rankdir LR :node.shape box :edge.color "#f00"
                                        :file "x.png" :svg t :C-c_C-c dot :exports svg)))))

(deftest "the block's own #+graph: and #+node: tables override the header"
  (should (equal "digraph {
  graph [ rankdir=TB ]
  node [ shape=ellipse ]
  a [ label=a ];
}"
                 (org--graph-raw->dot "
#+graph:
| rankdir | TB |
#+node:
| shape | ellipse |
+ a" "directed"
                                      '(:rankdir LR :node.shape box)))))

(deftest "`:directed t' beats an \"undirected\" main argument"
  (should (s-starts-with-p "digraph" (org--graph-raw->dot "
+ a
  + b" "undirected" '(:directed t)))))

(deftest "exporting reads the header line: a title and attributes reach the dot source"
  ;; Without Graphviz, the HTML export falls back to the dot source in a <pre>.
  (cl-letf (((symbol-function 'org-graph--render-to-png) (lambda (&rest _) nil)))
    (let ((html (org-export-string "#+begin_graph Lambda calculus :rankdir LR :node.shape box
+ a
#+end_graph" 'html)))
      (should (s-contains-p "graph [ label=\"Lambda calculus\" rankdir=LR ]" html))
      (should (s-contains-p "node [ shape=box ]" html))
      (should (s-contains-p "a [ label=a ];" html)))))

(provide 'org-graph-org-syntax-tests)
