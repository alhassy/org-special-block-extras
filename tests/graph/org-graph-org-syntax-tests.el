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
  \"bold it u s v c\" [ label=<<B>bold</B> <I>it</I> <U>u</U> <S>s</S> <FONT
FACE=\"monospace\">v</FONT> <FONT FACE=\"monospace\">c</FONT>> ];
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
  (cl-letf (((symbol-function 'org-graph--render) (lambda (&rest _) nil)))
    (should (s-contains-p "graph [ bgcolor=\"#6c7086\" fontname=\"Helvetica,Arial\" ]"
                          (org-export-string "#+begin_graph :bgcolor #6c7086 :fontname Helvetica,Arial
+ a
#+end_graph" 'html)))))

(deftest "exporting reads the header line: a title and attributes reach the dot source"
  ;; Without Graphviz, the HTML export falls back to the dot source in a <pre>.
  (cl-letf (((symbol-function 'org-graph--render) (lambda (&rest _) nil)))
    (let ((html (org-export-string "#+begin_graph Lambda calculus :rankdir LR :node.shape box
+ a
#+end_graph" 'html)))
      (should (s-contains-p "graph [ label=\"Lambda calculus\" rankdir=LR ]" html))
      (should (s-contains-p "node [ shape=box ]" html))
      (should (s-contains-p "a [ label=a ];" html)))))

(deftest "`:wrap N' fills node and arc labels to N characters a line; an HTML-like label breaks with <BR/>, a `\\\\' label stays"
  (should (equal "digraph {
  \"Structure and Interpretation of Computer Programs\" [ label=\"Structure and\\nInterpretation\\nof Computer\\nPrograms\" ];
  \"Structure and Interpretation of Computer Programs\" -> \"Scheme, a small Lisp dialect\" [ label=\"written in\" ];
  \"Scheme, a small Lisp dialect\" [ label=<<FONT FACE=\"monospace\">Scheme</FONT>, a<BR/><I>small</I> Lisp<BR/>dialect> ];
  \"Structure and Interpretation of Computer Programs\" -> \"Lisp the language\";
  \"Lisp the language\" [ label=<Lisp<BR/>the language> ];
}"
                 (org--graph-raw->dot "
+ Structure and Interpretation of Computer Programs
  + written in :: =Scheme=, a /small/ Lisp dialect
  + Lisp \\\\ the language" nil '(:wrap 14)))))

(deftest "`:wrap' reads a string as a number, as a :graph: section's :WRAP: property gives it"
  (should (s-contains-p "label=\"Structure\\nand\""
                        (org--graph-raw->dot "
+ Structure and" nil '(:wrap "10")))))

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

(org-deftest "an item's trailing tags get Org's `org-tag' face in a graph, inside clusters too, but not elsewhere"
  :text "
+ outside :wife:
#+begin_graph
+ Emacs :wife:
  #+begin_cluster Packages
  + Org :a:b:
  #+end_cluster
#+end_graph"
  (font-lock-ensure)
  (goto-char (point-min))
  (search-forward "outside :")
  (should-not (memq 'org-tag (ensure-list (get-text-property (point) 'face))))
  (search-forward "Emacs :")
  (should (memq 'org-tag (ensure-list (get-text-property (point) 'face))))
  (search-forward "Org :")
  (should (memq 'org-tag (ensure-list (get-text-property (point) 'face)))))

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

(org-deftest "tags in a description term, on keyword lines, and after a record cell's link get the `org-tag' face too"
  :text "
#+begin_graph
#+name: wife :base:
#+node: :wife:
+ Muhammad
  + wed :married: :: Khadija :wife:
+ s
  #+record:
  | <<p>> [[Muhammad]] :faint: |
#+end_graph"
  (font-lock-ensure)
  (dolist (at '("wife :" "#+node: :" "wed :" "Khadija :" "[[Muhammad]] :"))
    (goto-char (point-min))
    (search-forward at)
    (should (memq 'org-tag (ensure-list (get-text-property (point) 'face))))))

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

;;; Notes are tooltips: #+begin_note

(define-graph-test "a #+begin_note is its item's tooltip, a paragraph a line; with :edge, the arc's; under a reference, the arc's"
  :input "
+ Lisp
  #+begin_note
  McCarthy's /list processor/,
  of 1958.

  Lists \\\\ all the way down.
  #+end_note
  + Scheme
    #+begin_note :edge
    Steele and Sussman /refined/ it.
    #+end_note
  + Clojure
    + [[Scheme]]
      #+begin_note
      Rich Hickey cites it.
      #+end_note"
  :output "digraph {
  Lisp [ tooltip=\"McCarthy's list processor, of 1958.\\nLists\\nall the way down.\" label=Lisp ];
  Lisp -> Scheme [ tooltip=\"Steele and Sussman refined it.\" ];
  Scheme [ label=Scheme ];
  Lisp -> Clojure;
  Clojure [ label=Clojure ];
  Clojure -> Scheme [ tooltip=\"Rich Hickey cites it.\" ];
}")

(define-graph-test "a #+begin_note outside every item is the graph's tooltip; in a #+begin_cluster, the box's"
  :input "
#+begin_note
Languages, by influence.
#+end_note
#+begin_cluster JVM
#+begin_note
Run on the Java virtual machine.
#+end_note
+ Clojure
+ Scala
#+end_cluster"
  :output "digraph {
  graph [ tooltip=\"Languages, by influence.\" ]
  subgraph cluster_JVM {
    label=JVM
    tooltip=\"Run on the Java virtual machine.\"
    Clojure [ label=Clojure ];
    Scala [ label=Scala ];
  }
}")

(deftest "a #+begin_note follows a footnote, line by line, in its node's tooltip"
  (should (s-contains-p "Lisp [ tooltip=\"1958\\nBy McCarthy.\" label=Lisp ];"
                        (org--graph-raw->dot "
+ Lisp [fn::1958]
  #+begin_note
  By McCarthy.
  #+end_note" nil))))

(deftest "a `#+begin_note :edge' with no arc to describe, or a note with other arguments, is a user error"
  (should-error (org--graph-raw->dot "
+ Lisp
  #+begin_note :edge
  No parent.
  #+end_note" nil) :type 'user-error)
  (should-error (org--graph-raw->dot "
+ Lisp
  #+begin_note Title
  Callouts have titles; tooltips do not.
  #+end_note" nil) :type 'user-error))

(deftest "exporting a graph leaves its #+begin_note blocks to it, rather than rewriting them as callouts first"
  ;; Without Graphviz, the HTML export falls back to the dot source in a <pre>.
  (cl-letf (((symbol-function 'org-graph--render) (lambda (&rest _) nil)))
    (should (s-contains-p "Lisp [ tooltip=\"By McCarthy.\" label=Lisp ];"
                          (org-export-string "#+begin_graph
+ Lisp
  #+begin_note
  By McCarthy.
  #+end_note
#+end_graph" 'html)))))

(org-deftest "C-c C-c within a graph's #+begin_note renders the graph"
  :text "
#+begin_graph
+ Lisp
  #+begin_note
  By McCarthy.
  #+end_note
#+end_graph"
  :at "By McCarthy"
  (let (rendered)
    (cl-letf (((symbol-function 'org-graph--cc-handler) (lambda () (setq rendered t))))
      (org--block-cc-dispatcher)
      (should rendered))))

;;; An item's later paragraphs are notes; `:notes inline' shows notes in labels

(define-graph-test "an item's paragraphs past its first are notes, as #+begin_note blocks are; prose at the top, the graph's"
  :input "
Languages, by influence.
+ Lisp

  McCarthy's list processor.

  Code /is/ data \\\\ and data is code.
  + [[Lisp]]

    Eval calls apply calls eval."
  :output "digraph {
  graph [ tooltip=\"Languages, by influence.\" ]
  Lisp [ tooltip=\"McCarthy's list processor.\\nCode is data\\nand data is code.\" label=Lisp ];
  Lisp -> Lisp [ tooltip=\"Eval calls apply calls eval.\" ];
}")

(deftest "`:notes inline' shows notes beneath their labels, markup kept and filled to :wrap; the graph's, beneath its title"
  (should (equal "digraph {
  graph [ label=<Lisp's family<BR/><FONT POINT-SIZE=\"10\">Languages, by<BR ALIGN=\"LEFT\"/>influence.<BR ALIGN=\"LEFT\"/></FONT>> ]
  Lisp [ tooltip=1958 label=<Lisp<BR/><FONT POINT-SIZE=\"10\">McCarthy's<BR ALIGN=\"LEFT\"/><I>list<BR ALIGN=\"LEFT\"/>processor</I>.<BR ALIGN=\"LEFT\"/></FONT>> ];
  Lisp -> Scheme [ label=<distils<BR/><FONT POINT-SIZE=\"10\">Steele and<BR ALIGN=\"LEFT\"/>Sussman.<BR ALIGN=\"LEFT\"/></FONT>> ];
  Scheme [ label=Scheme ];
}"
                 (org--graph-raw->dot "
Languages, by influence.
+ Lisp [fn::1958]

  McCarthy's /list processor/.
  + distils :: Scheme
    #+begin_note :edge
    Steele and Sussman.
    #+end_note" "Lisp's family" '(:notes inline :wrap 14)))))

(deftest "`:notes' is tooltip or inline; anything else is a user error"
  (should-error (org--graph-raw->dot "
+ a" nil '(:notes hover)) :type 'user-error))

;;; #+node:, #+edge:, and #+rank: under an item reach its whole subtree

(define-graph-test "an item's #+node:, #+edge:, and #+rank: lines style its descendants, inherited as Org's properties are"
  :input "
+ Weather
  #+node:
  | shape | box |
  #+edge:
  | style | dotted |
  #+rank: Rain Sun
  + Rain
    + Umbrella
  + Sun
+ Climate"
  :output "digraph {
  Weather [ label=Weather ];
  subgraph {
    node [ shape=box ]
    edge [ style=dotted ]
    { rank=same; Rain; Sun }
    Weather -> Rain;
    Rain [ label=Rain ];
    Rain -> Umbrella;
    Umbrella [ label=Umbrella ];
    Weather -> Sun;
    Sun [ label=Sun ];
  }
  Climate [ label=Climate ];
}")

;;; `:unknown-tags random-templates': prototype now, name templates later

(deftest "under `:unknown-tags random-templates', each unknown tag gets a made-up template, the same at every use, legends' too"
  (should (equal "digraph {
  legend_1 [ color=\"#d20f39\" fontcolor=\"#d20f39\" shape=box fontname=Helvetica label=\"a calculus\" ];
  legend_2 [ color=\"#1e66f5\" fontcolor=\"#1e66f5\" shape=ellipse fontname=Times label=\"a language\" ];
  \"λ-calculus\" [ color=\"#d20f39\" fontcolor=\"#d20f39\" shape=box fontname=Helvetica label=\"λ-calculus\" ];
  \"λ-calculus\" -> Lisp;
  Lisp [ color=\"#1e66f5\" fontcolor=\"#1e66f5\" shape=ellipse fontname=Times label=Lisp ];
}"
                 (org--graph-raw->dot "
- :theory: :: a calculus
- :language: :: a language
+ λ-calculus :theory:
  + Lisp :language:" nil '(:unknown-tags random-templates)))))

(deftest "by default an unknown tag is a user error, pointing at `:unknown-tags random-templates'"
  (should (string-match-p "random-templates"
                          (cadr (should-error (org--graph-raw->dot "
+ Lisp :language:" nil) :type 'user-error)))))

;;; `:unflatten N' staggers a wide row of leaves

(deftest "`:unflatten N' pipes the dot through Graphviz's unflatten, so a node's leaves stagger over N ranks"
  (skip-unless (executable-find "unflatten"))
  (let ((dot (org--graph-raw->dot "
+ Lisp
  + Scheme
  + Clojure
  + Emacs Lisp" nil '(:unflatten 2))))
    (should (s-contains-p "Lisp -> Scheme\t[minlen=1]" dot))
    (should (s-contains-p "Lisp -> Clojure\t[minlen=2]" dot))))

;;; Section links: [[*Heading]]

(define-graph-test "a [[*Heading]] link is its node's URL, not an arc; bare, it shows the heading's title"
  :input "
+ [[*Closures][Closure]]
  + [[*Lexical scope]]
+ [[https://lisp-lang.org][Lisp]]
  + [[Closure][captures]]"
  :output "digraph {
  Closure [ URL=\"*Closures\" label=Closure ];
  Closure -> \"Lexical scope\";
  \"Lexical scope\" [ URL=\"*Lexical scope\" label=\"Lexical scope\" ];
  Lisp [ URL=\"https://lisp-lang.org\" label=Lisp ];
  Lisp -> Closure [ label=captures ];
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

(org-deftest "inside a graph's #+begin_cluster, we are still in the graph; a lone #+begin_cluster is none of ours"
  :text "
#+begin_graph
#+begin_cluster P
+ a
#+end_cluster
#+end_graph
#+begin_cluster Q
+ b
#+end_cluster"
  :at "+ a"
  (should (org-graph--in-graph-p))
  (search-forward "+ b")
  (should-not (org-graph--in-graph-p)))

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

(org-deftest "C-c C-c on a checkbox toggles it, as Org's would, then renders the graph"
  :text "
#+begin_graph :C-c_C-c dot
+ [ ] parse
#+end_graph"
  :at "parse"
  (let (dot)
    (cl-letf (((symbol-function 'org-graph--show-dot) (lambda (d) (setq dot d))))
      (org-graph--cc-handler))
    (should (s-contains-p "+ [X] parse" (buffer-string)))
    (should (s-contains-p "parse [ color=forestgreen" dot))))

;;; Macros

(org-deftest "{{{macros}}} expand by the buffer's #+macro: lines, so a palette is named once"
  :text "
#+macro: gold \"#f9e2af\"
#+begin_graph :C-c_C-c dot
+ a
  | color | {{{gold}}} |
#+end_graph"
  :at "+ a"
  (let (dot)
    (cl-letf (((symbol-function 'org-graph--show-dot) (lambda (d) (setq dot d))))
      (org-graph--cc-handler))
    (should (s-contains-p "a [ color=\"#f9e2af\" label=a ];" dot))))

;;; One line break: `\\'

(define-graph-test "`\\\\' breaks a line everywhere a label is written: a description term, a table value, a record cell"
  :input "
+ Monad
  + every monad \\\\ is an adjunction :: Adjunction
  + Turing
    | :edge.label | Church– \\\\ Turing thesis |
    | :edge.color | <<B>red</B>> |
+ s
  #+record:
  | hello \\\\ world | <<p>> b |"
  :output "digraph {
  Monad [ label=Monad ];
  Monad -> Adjunction [ label=<every monad<BR/>is an adjunction> ];
  Adjunction [ label=Adjunction ];
  Monad -> Turing [ label=\"Church–\\nTuring thesis\" color=<<B>red</B>> ];
  Turing [ label=Turing ];
  s [ shape=plain label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\"
CELLSPACING=\"0\"><TR><TD>hello<BR/>world</TD><TD PORT=\"p\">b</TD></TR></TABLE>> ];
}")

;;; Legends: a top-level `- :T: :: MEANING'

(define-graph-test "a legend entry samples its templates: as an arc when they style arcs in the graph, else as a node"
  :input "
#+name: married
| color | pink |
| dir   | both |
#+name: wife
| shape | box |
+ Muhammad
+ Khadija :wife:
  + [[Muhammad]] :married:
#+begin_cluster Legend
- :married: :: marriage
- :wife: :: /wife/
#+end_cluster"
  :output "digraph {
  Muhammad [ label=Muhammad ];
  Khadija [ shape=box label=Khadija ];
  Khadija -> Muhammad [ color=pink dir=both ];
  subgraph cluster_Legend {
    label=Legend
    legend_1 [ shape=point style=invis ];
    legend_1_end [ shape=point style=invis ];
    legend_1 -> legend_1_end [ color=pink dir=both label=marriage ];
    legend_2 [ shape=box label=<<I>wife</I>> ];
  }
}")

(define-graph-test "a template styles arcs when an #+edge: line's tags name it, too; its sample sans `constraint', which is layout"
  :input "
#+name: faint
| style      | dotted |
| constraint | false  |
#+edge: :faint:
- :faint: :: inferred
+ A
  + B"
  :output "digraph {
  edge [ style=dotted constraint=false ]
  legend_1 [ shape=point style=invis ];
  legend_1_end [ shape=point style=invis ];
  legend_1 -> legend_1_end [ style=dotted label=inferred ];
  A [ label=A ];
  A -> B;
  B [ label=B ];
}")

(deftest "a top-level `TERM ::' that is not just tags is an error pointing at the legend spelling"
  (should (s-contains-p "- :T: :: Khadija"
                        (cadr (should-error (org--graph-raw->dot "\n- wed :: Khadija" nil) :type 'user-error)))))

;;; Same-rank arcs under splines=curved

(define-graph-test "under splines=curved, a same-rank arc's label is an xlabel, which Graphviz still draws; others keep theirs"
  :input "
#+graph:
| splines | curved |
#+rank: church turing
+ λ
  + church
  + turing
    + Church \\\\ Turing thesis :: [[church]]
  + plain :: other"
  :output "digraph {
  graph [ splines=curved ]
  { rank=same; church; turing }
  \"λ\" [ label=\"λ\" ];
  \"λ\" -> church;
  church [ label=church ];
  \"λ\" -> turing;
  turing [ label=turing ];
  turing -> church [ xlabel=<Church<BR/>Turing thesis> ];
  \"λ\" -> other [ label=plain ];
  other [ label=other ];
}")

(define-graph-test "without splines=curved, a same-rank arc keeps its label"
  :input "
#+rank: church turing
+ church
+ turing
  + thesis :: [[church]]"
  :output "digraph {
  { rank=same; church; turing }
  church [ label=church ];
  turing [ label=turing ];
  turing -> church [ label=thesis ];
}")

(provide 'org-graph-org-syntax-tests)
