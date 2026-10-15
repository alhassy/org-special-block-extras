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

(define-graph-test "an item that is a lone [[TARGET][LABEL]] link refers to TARGET, its parent's arc labelled LABEL"
  :input "
+ <<church>> Church
+ lambda
  + [[church][encodes]]"
  :output "digraph {
  church [ label=Church ];
  lambda [ label=lambda ];
  lambda -> church [ label=encodes ];
}")

(define-graph-test "`:edge[TARGET].KEY' rows describe the arc to TARGET, from a link, overriding the link's label"
  :input "
+ lisp [[church][encodes]] [[haskell]]
  | :edge[church].label  | inspired |
  | :edge[church].style  | dotted   |
  | :edge[haskell].color | red      |"
  :output "digraph {
  lisp -> church [ label=inspired style=dotted ];
  lisp -> haskell [ color=red ];
  lisp [ label=lisp ];
}")

(define-graph-test "an item's text is its first paragraph, so its [[links]] may wrap onto continuation lines"
  :input "
+ <<lam>> λ-calculus
+ <<church>> Church
+ Lisp [[lam][inspired by]]
  [[church]]
  | :edge[church].style | dotted |"
  :output "digraph {
  lam [ label=\"λ-calculus\" ];
  church [ label=Church ];
  Lisp -> lam [ label=\"inspired by\" ];
  Lisp -> church [ style=dotted ];
  Lisp [ label=Lisp ];
}")

(define-graph-test "a parent's arc to a lone-link child is styled by the child's `:edge.KEY' rows; two such children, two styled arcs"
  :input "
+ <<lam>> λ-calculus
+ Lisp
  + inspired by :: [[lam]]
    | :edge.style | dotted |
  + borrows :: [[lam]]
    | :edge.color | red |"
  :output "digraph {
  lam [ label=\"λ-calculus\" ];
  Lisp [ label=Lisp ];
  Lisp -> lam [ label=\"inspired by\" style=dotted ];
  Lisp -> lam [ label=borrows color=red ];
}")

(deftest "an `:edge[TARGET].KEY' row with no arc to TARGET is a user error"
  (should-error (org--graph-raw->dot "
+ a [[b]]
  | :edge[c].style | dotted |" nil) :type 'user-error))

(define-graph-test "a web or file link is the node's URL, its text staying in the label; an image link is its image"
  :input "
+ see [[https://example.org][site]]
+ notes [[file:notes.org]]
+ <<emacs>> Emacs [[file:emacs.png]]
+ [[file:gnu.png]]"
  :output "digraph {
  \"see site\" [ URL=\"https://example.org\" label=\"see site\" ];
  \"notes notes.org\" [ URL=\"notes.org\" label=\"notes notes.org\" ];
  emacs [ image=\"emacs.png\" label=Emacs ];
  \"gnu.png\" [ image=\"gnu.png\" label=\"\" ];
}")

(deftest "a node has one URL, so an item with two web links is a user error"
  (should-error (org--graph-raw->dot "
+ [[https://a.org][a]] [[https://b.org][b]]" nil) :type 'user-error))

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
+ a" nil
                                      '(:rankdir LR :node.shape box)))))

(deftest "the header line reads as #+attr_html: does: a hex colour, or a word Lisp cannot read, needs no quotes"
  ;; Without Graphviz, the HTML export falls back to the dot source in a <pre>.
  (cl-letf (((symbol-function 'org-graph--render-to-png) (lambda (&rest _) nil)))
    (should (s-contains-p "graph [ bgcolor=\"#6c7086\" fontname=\"Helvetica,Arial\" ]"
                          (org-export-string "#+begin_graph :bgcolor #6c7086 :fontname Helvetica,Arial
+ a
#+end_graph" 'html)))))

(deftest "exporting reads the header line: a title and attributes reach the dot source"
  ;; Without Graphviz, the HTML export falls back to the dot source in a <pre>.
  (cl-letf (((symbol-function 'org-graph--render-to-png) (lambda (&rest _) nil)))
    (let ((html (org-export-string "#+begin_graph Lambda calculus :rankdir LR :node.shape box
+ a
#+end_graph" 'html)))
      (should (s-contains-p "graph [ label=\"Lambda calculus\" rankdir=LR ]" html))
      (should (s-contains-p "node [ shape=box ]" html))
      (should (s-contains-p "a [ label=a ];" html)))))

;;; Description lists label arcs

(define-graph-test "`+ TAG :: CHILD' labels the parent's arc TAG; a repeated child is a parallel arc, the tag may be marked up, and CHILD may be a [[link]]"
  :input "
+ <<lam>> λ-calculus
+ client
  + request :: server
  + /retry/ :: server
+ Lisp
  + inspired by :: [[lam]]"
  :output "digraph {
  lam [ label=\"λ-calculus\" ];
  client [ label=client ];
  client -> server [ label=request ];
  server [ label=server ];
  client -> server [ label=<<I>retry</I>> ];
  Lisp [ label=Lisp ];
  Lisp -> lam [ label=\"inspired by\" ];
}")

(define-graph-test "an explicit `:edge.label' row beats a description tag; `std::vector' is no description"
  :input "
+ a
  + x :: std::vector
    | :edge.label | y |"
  :output "digraph {
  a [ label=a ];
  a -> \"std::vector\" [ label=y ];
  \"std::vector\" [ label=\"std::vector\" ];
}")

(deftest "a description tag on a parentless item is a user error"
  (should-error (org--graph-raw->dot "
+ x :: a" nil) :type 'user-error))

;;; :tags: splice templates

(define-graph-test "`:t₁:t₂:' splices templates t₁ and t₂, explicit rows winning; colons elsewhere, or in =verbatim=, are text"
  :input "
#+name: wife
| color | pink |
| shape | box  |
#+name: faint
| style | dotted |

+ Khadija :wife:faint:
  | color | red |
+ Ratio 1:2
+ Meeting at 12:30 :faint:
+ Vim's =:wq:="
  :output "digraph {
  Khadija [ shape=box style=dotted color=red label=Khadija ];
  \"Ratio 1:2\" [ label=\"Ratio 1:2\" ];
  \"Meeting at 12:30\" [ style=dotted label=\"Meeting at 12:30\" ];
  \"Vim's :wq:\" [ label=<Vim's <FONT FACE=\"monospace\">:wq:</FONT>> ];
}")

(deftest "a tag naming no #+name: template is a user error, rather than silently vanishing"
  (should-error (org--graph-raw->dot "
+ a :typo:" nil) :type 'user-error))

(deftest "an item's trailing tags get Org's `org-tag' face in a graph, inside clusters too, but not elsewhere"
  (with-temp-buffer
    (org-mode)
    (insert "
+ outside :wife:
#+begin_graph
+ Emacs :wife:
  #+begin_cluster Packages
  + Org :a:b:
  #+end_cluster
#+end_graph")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "outside :")
    (should-not (memq 'org-tag (ensure-list (get-text-property (point) 'face))))
    (search-forward "Emacs :")
    (should (memq 'org-tag (ensure-list (get-text-property (point) 'face))))
    (search-forward "Org :")
    (should (memq 'org-tag (ensure-list (get-text-property (point) 'face))))))

(define-graph-test "a reference's tags style its arc, as one arc rather than parallel ones; explicit `:edge.KEY' rows win"
  :input "
#+name: married
| color | \"#cc3399\" |
| dir   | both        |
#+name: faint
| style | dotted |

+ Muhammad
+ Khadija
  + [[Muhammad]] :married:faint:
    | :edge.color | red |"
  :output "digraph {
  Muhammad [ label=Muhammad ];
  Khadija [ label=Khadija ];
  Khadija -> Muhammad [ dir=both style=dotted color=red ];
}")

(define-graph-test "a description term's tags style the arc it labels, the item's own tags its node; a term may be tags alone"
  :input "
#+name: married
| color | \"#cc3399\" |
#+name: wife
| shape | box |

+ Muhammad
  + wed :married: :: Khadija :wife:
  + :married: :: Sawda"
  :output "digraph {
  Muhammad [ label=Muhammad ];
  Muhammad -> Khadija [ label=wed color=\"#cc3399\" ];
  Khadija [ shape=box label=Khadija ];
  Muhammad -> Sawda [ color=\"#cc3399\" ];
  Sawda [ label=Sawda ];
}")

(define-graph-test "tags on #+node: and #+edge: lines inherit templates, and on a #+name: line build one template from others"
  :input "
#+name: rounded
| style | rounded |
#+name: pink
| color | pink |
#+name: wife :rounded:pink:
| shape | box |

#+node: :wife:
| fontname | Georgia |
#+edge: :pink:
+ Khadija"
  :output "digraph {
  node [ style=rounded color=pink shape=box fontname=Georgia ]
  edge [ color=pink ]
  Khadija [ label=Khadija ];
}")

(deftest "tags in a description term, and on keyword lines, get the `org-tag' face too"
  (with-temp-buffer
    (org-mode)
    (insert "
#+begin_graph
#+name: wife :base:
#+node: :wife:
+ Muhammad
  + wed :married: :: Khadija :wife:
#+end_graph")
    (font-lock-ensure)
    (dolist (at '("wife :" "#+node: :" "wed :" "Khadija :"))
      (goto-char (point-min))
      (search-forward at)
      (should (memq 'org-tag (ensure-list (get-text-property (point) 'face)))))))

;;; Footnotes are tooltips

(define-graph-test "an inline footnote is the node's tooltip, vanishing from its label; an explicit tooltip row wins"
  :input "
+ Lisp [fn::McCarthy, 1958]
+ Org [fn::Dominik]
  | tooltip | Carsten |"
  :output "digraph {
  Lisp [ tooltip=\"McCarthy, 1958\" label=Lisp ];
  Org [ tooltip=Carsten label=Org ];
}")

;;; #+rank: takes links

(define-graph-test "`#+rank:' takes [[links]] as well as bare ids, so multi-word ids need no quotes"
  :input "
#+rank: a [[Org mode]] [[b][the b]]
+ a
+ Org mode
+ b"
  :output "digraph {
  { rank=same; \"Org mode\"; b; a }
  a [ label=a ];
  \"Org mode\" [ label=\"Org mode\" ];
  b [ label=b ];
}")

;;; #+begin_cluster

(define-graph-test "`#+begin_cluster TITLE :KEY V' is the `| shape | cluster |' item: nested, anonymous, and colons in titles all work"
  :input "
+ Emacs
  #+begin_cluster Packages :bgcolor lightgrey :node.shape box
  + Org
    #+begin_cluster
    + agenda
    #+end_cluster
  #+end_cluster
#+begin_cluster Time 12:30
+ lunch
#+end_cluster"
  :output "digraph {
  graph [ compound=true ]
  Emacs [ label=Emacs ];
  subgraph cluster_Packages {
    label=Packages
    bgcolor=lightgrey
    node [ shape=box ]
    Org [ label=Org ];
    subgraph cluster_anonymous_1 {
    label=\"\"
    agenda [ label=agenda ];
  }
    Org -> agenda [ lhead=cluster_anonymous_1 ];
  }
  Emacs -> Org [ lhead=cluster_Packages ];
  subgraph \"cluster_Time_12:30\" {
    label=\"Time 12:30\"
    lunch [ label=lunch ];
  }
}")

(deftest "a `#+begin_cluster' with no `#+end_cluster' is a user error"
  (should-error (org--graph-raw->dot "
#+begin_cluster P
+ a" nil) :type 'user-error))

(deftest "inside a graph's #+begin_cluster, we are still in the graph; a lone #+begin_cluster is none of ours"
  (with-temp-buffer
    (org-mode)
    (insert "
#+begin_graph
#+begin_cluster P
+ a
#+end_cluster
#+end_graph
#+begin_cluster Q
+ b
#+end_cluster")
    (goto-char (point-min))
    (search-forward "+ a")
    (should (org-graph--in-graph-p))
    (search-forward "+ b")
    (should-not (org-graph--in-graph-p))))

;;; \\ breaks a label's line

(define-graph-test "`\\\\' breaks the label's line, at a line's end as in Org, or mid-line; the id joins the lines with a space"
  :input "
+ Emacs
  + provides :: <<closures>> Closures \\\\ (lexical scope)
  + *Lisp* \\\\
    McCarthy, 1958"
  :output "digraph {
  Emacs [ label=Emacs ];
  Emacs -> closures [ label=provides ];
  closures [ label=<Closures<BR/>(lexical scope)> ];
  Emacs -> \"Lisp McCarthy, 1958\";
  \"Lisp McCarthy, 1958\" [ label=<<B>Lisp</B><BR/>McCarthy, 1958> ];
}")

;;; Tables and directives after an item's sub-items

(define-graph-test "tables and directives after an item's sub-items still belong to it, as in Org"
  :input "
+ Prophets
  | shape | cluster |
  + Hud
  + Salih
  # Both were sent to Arabian tribes.
  #+rank: Hud Salih
  | style | dashed |"
  :output "digraph {
  subgraph cluster_Prophets {
    label=Prophets
    style=dashed
    { rank=same; Hud; Salih }
    Hud [ label=Hud ];
    Salih [ label=Salih ];
  }
}")

;;; Checkboxes

(define-graph-test "a checkbox applies its template: [X] ↦ checked, [-] ↦ partial, [ ] ↦ unchecked; one the graph defines wins"
  :input "
#+name: unchecked
| style | dashed |
#+name: partial
| color | gold |
+ [-] compile
  + [X] parse
  + [ ] typecheck
  + plain"
  :output "digraph {
  compile [ color=gold label=compile ];
  compile -> parse;
  parse [ color=forestgreen fontcolor=forestgreen label=parse ];
  compile -> typecheck;
  typecheck [ style=dashed label=typecheck ];
  compile -> plain;
  plain [ label=plain ];
}")

(define-graph-test "a checkbox whose template exists nowhere styles nothing, rather than being an error"
  :input "
+ [ ] typecheck"
  :output "digraph {
  typecheck [ label=typecheck ];
}")

(deftest "C-c C-c on a checkbox toggles it, as Org's would, then renders the graph"
  (with-temp-buffer
    (org-mode)
    (insert "
#+begin_graph :C-c_C-c dot
+ [ ] parse
#+end_graph")
    (search-backward "parse")
    (let (dot)
      (cl-letf (((symbol-function 'org-graph--show-dot) (lambda (d) (setq dot d))))
        (org-graph--cc-handler))
      (should (s-contains-p "+ [X] parse" (buffer-string)))
      (should (s-contains-p "parse [ color=forestgreen" dot)))))

;;; Macros

(deftest "{{{macros}}} expand by the buffer's #+macro: lines, so a palette is named once"
  (with-temp-buffer
    (org-mode)
    (insert "
#+macro: gold \"#f9e2af\"
#+begin_graph :C-c_C-c dot
+ a
  | color | {{{gold}}} |
#+end_graph")
    (search-backward "+ a")
    (let (dot)
      (cl-letf (((symbol-function 'org-graph--show-dot) (lambda (d) (setq dot d))))
        (org-graph--cc-handler))
      (should (s-contains-p "a [ color=\"#f9e2af\" label=a ];" dot)))))

(provide 'org-graph-org-syntax-tests)
