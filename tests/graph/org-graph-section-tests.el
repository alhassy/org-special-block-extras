;;; org-graph-section-tests.el --- A :graph: section: its subheadings are its nodes, its drawers their attributes  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; The equivalence: a block's body and a section's text and subsections draw the same dot

(deftest "a section's subheadings, drawers, notes, and ORDERED draw what a block's items, tables, notes, and 1. 2. do"
  (should (equal (org--graph-raw->dot "
+ expr
  | color | navy |
  + term :bold:

    A sum of factors.

    1. factor
    2. NUMBER
  + nests :: [[expr]]" nil)
                 (org--graph-raw->dot "
** expr
:PROPERTIES:
:COLOR: navy
:END:
*** term :bold:
:PROPERTIES:
:ORDERED: t
:END:
A sum of factors.
**** factor
**** NUMBER
*** nests :: [[expr]]" nil))))

(deftest "a section's body may hold a list, read as a block's is, its items children beside the subheadings"
  (should (equal (org--graph-raw->dot "
+ λ-calculus
  + inspired :: Lisp
    + subset of :: Emacs Lisp
    + dialect :: Scheme" nil)
                 (org--graph-raw->dot "
** λ-calculus
*** inspired :: Lisp
+ subset of :: Emacs Lisp
+ dialect :: Scheme" nil))))

(deftest "the root's own text is the graph's preamble: its #+node: rows, and its notes"
  (should (equal (org--graph-raw->dot "
#+node: shape=box
A grammar.
+ expr" nil)
                 (org--graph-raw->dot "
#+node: shape=box
A grammar.
** expr" nil))))

;;; Property drawers

(define-graph-test "a drawer's keys are Graphviz attributes regardless of case; EDGE.KEY styles the arc into the node"
  :input "
** expr
*** term
:PROPERTIES:
:SHAPE: box
:url: https://orgmode.org
:EDGE.STYLE: dashed
:END:"
  :output "digraph {
  expr [ label=expr ];
  expr -> term [ style=dashed ];
  term [ shape=box URL=\"https://orgmode.org\" label=term ];
}")

(define-graph-test "EDGE[T].KEY styles the node's arc to T, T keeping its case"
  :input "
** <<Expr>> expr
** term [[Expr][nests]]
:PROPERTIES:
:EDGE[Expr].COLOR: red
:END:"
  :output "digraph {
  Expr [ label=expr ];
  term -> Expr [ label=nests color=red ];
  term [ label=term ];
}")

(define-graph-test "NODE.KEY sets the defaults of the heading's whole subtree, as an item's #+node: does"
  :input "
** expr
:PROPERTIES:
:NODE.SHAPE: box
:END:
*** term
** NUMBER"
  :output "digraph {
  expr [ label=expr ];
  subgraph {
    node [ shape=box ]
    expr -> term;
    term [ label=term ];
  }
  NUMBER [ label=NUMBER ];
}")

(define-graph-test "Org's own properties stay Org's, bar STYLE, which Graphviz names too"
  :input "
** term
:PROPERTIES:
:CATEGORY: grammar
:CUSTOM_ID: term
:STYLE: dashed
:END:"
  :output "digraph {
  term [ style=dashed label=term ];
}")

(deftest "a drawer key that names no Graphviz attribute is a user error"
  (should-error (org--graph-raw->dot "
** term
:PROPERTIES:
:SHAPPE: box
:END:" nil) :type 'user-error))

;;; Which headings are nodes

(define-graph-test "commented, archived, and :noexport: headings are no nodes, nor are their subtrees"
  :input "
** expr
*** COMMENT term
**** factor
*** draft :noexport:
*** old :ARCHIVE:
*** NUMBER"
  :output "digraph {
  expr [ label=expr ];
  expr -> NUMBER;
  NUMBER [ label=NUMBER ];
}")

(deftest "a :graph: heading within a graph's subtree is a user error; :container: draws a box"
  (should-error (org--graph-raw->dot "
** expr
*** Inner :graph:" nil) :type 'user-error))

(define-graph-test "a #+RESULTS: element, C-c C-c's image, is no part of the graph"
  :input "
#+RESULTS:
[[file:grammar.png]]
** expr"
  :output "digraph {
  expr [ label=expr ];
}")

;;; TODO keywords are checkboxes

(define-graph-test "DONE applies the checked template, as [X] does; TODO, unchecked, which by default styles nothing"
  :input "
** parse
*** DONE lex
*** TODO typecheck"
  :output "digraph {
  parse [ label=parse ];
  parse -> lex;
  lex [ color=forestgreen fontcolor=forestgreen label=lex ];
  parse -> typecheck;
  typecheck [ label=typecheck ];
}")

(define-graph-test "a TODO keyword K applies the template k, when there is one"
  :input "
#+name: todo
| style | dashed |
** parse
*** TODO typecheck"
  :output "digraph {
  parse [ label=parse ];
  parse -> typecheck;
  typecheck [ style=dashed label=typecheck ];
}")

;;; Export

(deftest "a :graph: section exports its text and subsections as one graph, beneath its heading; the next section stays"
  (cl-letf (((symbol-function 'org-graph--render) (lambda (&rest _) nil)))
    (let ((html (org-export-string-as "
* Our grammar :graph:
:PROPERTIES:
:DIRECTION: right
:END:
** expr
*** term
* Afterwards
Prose." 'html t)))
      (should (s-contains-p "Our grammar" html))
      (should (s-contains-p "rankdir=LR" html))
      (should (s-contains-p "expr -> term" html))
      (should (s-contains-p "Afterwards" html))
      (should-not (s-contains-p "<h3" html)))))

(deftest "a plain :graph: heading titles its graph, as #+begin_graph TITLE does; :graph_T: retitles it"
  (cl-letf (((symbol-function 'org-graph--render) (lambda (&rest _) nil)))
    (should (s-contains-p "label=\"Our grammar\"" (org-export-string-as "
* Our grammar :graph:
** expr" 'html t)))
    (should (s-contains-p "label=Lineage" (org-export-string-as "
* Our grammar :graph_Lineage:
** expr" 'html t)))))

(deftest "a graph node tagged as another block, :details:, stays a node; the graph's tag runs first"
  (cl-letf (((symbol-function 'org-graph--render) (lambda (&rest _) nil)))
    (let ((html (org-export-string-as "
* Our grammar :graph:
** expr :details:" 'html t)))
      (should (s-contains-p "expr [" html))
      (should-not (s-contains-p "<details" html)))))

;;; C-c C-c

(org-deftest "C-c C-c in a :graph: section's body renders it, its #+RESULTS: beneath the root's drawer, replaced anew"
  :text "
* Our grammar :graph:
:PROPERTIES:
:DIRECTION: right
:END:
** expr
*** term
Prose about terms.
* Afterwards"
  :at "Prose about"
  (let (dots)
    (cl-letf (((symbol-function 'org-graph--run-dot) (lambda (dot &rest _) (push dot dots) '(0 . "")))
              ((symbol-function 'org-display-inline-images) #'ignore))
      (should (org-graph--section-cc))
      (should (org-graph--section-cc))
      (should (s-contains-p "expr -> term" (car dots)))
      (should (equal (car dots) (cadr dots)))
      (goto-char (point-min))
      (should (search-forward ":END:\n#+RESULTS:\n[[file:" nil t))
      (should (= 1 (s-count-matches "#\\+RESULTS:" (buffer-string)))))))

(org-deftest "C-c C-c on a :graph: section's heading stays Org's, setting tags"
  :text "
* Our grammar :graph:
** expr"
  :at "expr"
  (should-not (org-graph--section-cc)))

;;; Completion and eldoc in drawers

(org-deftest "C-M-i in a :graph: section's drawer completes attribute keys, EDGE.KEY ones too, then their values"
  :text "
* Our grammar :graph:
** expr
:PROPERTIES:
:SHA
:EDGE.STY
:SHAPE: bo
:END:"
  (dolist (probe '((":SHA" "SHAPE") (":EDGE.STY" "EDGE.STYLE") (":SHAPE: bo" "box")))
    (goto-char (point-min))
    (search-forward (car probe))
    (-let [(beg _ candidates) (org-graph--drawer-completion)]
      (should (member (cadr probe) (all-completions (buffer-substring beg (point)) candidates))))))

(org-deftest "outside a :graph: section, a drawer's keys are left to Org"
  :text "
* Notes
:PROPERTIES:
:SHA
:END:"
  :at ":SHA"
  (should-not (org-graph--drawer-completion))
  (should-not (org-graph--property-allowed-values "SHAPE")))

(org-deftest "C-c C-x p and S-→ offer Graphviz's values for a :graph: node's property"
  :text "
* Our grammar :graph:
** expr"
  :at "expr"
  (should (member "box" (org-property-get-allowed-values nil "SHAPE"))))

(org-deftest "eldoc explains the attribute a :graph: node's property sets"
  :text "
* Our grammar :graph:
** expr
:PROPERTIES:
:EDGE.STYLE: dashed
:END:"
  :at ":EDGE.ST"
  (let (shown)
    (org-graph--eldoc (lambda (doc &rest _) (setq shown doc)))
    (should (s-contains-p "style" (downcase shown)))))

(provide 'org-graph-section-tests)
