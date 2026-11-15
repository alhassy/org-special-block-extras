;;; org-graph-markup-links-tests.el --- Our links, Org's links, and $…$ maths in a graph's labels  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; Our links mean in a label what they mean in prose; see `org-markup-links'

(define-graph-test "a color link colors its node's label, as red:x colors x in prose"
  :input "
+ red:Church
  + [[blue:][Turing]] machines"
  :output "digraph {
  Church [ label=<<FONT COLOR=\"red\">Church</FONT>> ];
  Church -> \"Turing machines\";
  \"Turing machines\" [ label=<<FONT COLOR=\"blue\">Turing</FONT> machines> ];
}")

(define-graph-test "a tooltip: link is its node's tooltip, as a footnote is; its tip's markup stays out of the label"
  :input "
+ Lisp
  + [[tooltip:Church][who /invented/ the λ-calculus]]"
  :output "digraph {
  Lisp [ label=Lisp ];
  Lisp -> Church;
  Church [ tooltip=\"who invented the λ-calculus\" label=Church ];
}")

(define-graph-test "margin: is tooltip:, as the margin block is the tooltip block"
  :input "
+ [[margin:Church][who invented the λ-calculus]]"
  :output "digraph {
  Church [ tooltip=\"who invented the λ-calculus\" label=Church ];
}")

;;; Org's own links: a web or file link is the node's URL

(define-graph-test "a web link, and a file link, is its node's URL, its description the label"
  :input "
+ [[https://orgmode.org][Org]]
  + [[file:notes.org][notes]]"
  :output "digraph {
  Org [ URL=\"https://orgmode.org\" label=Org ];
  Org -> notes;
  notes [ URL=\"notes.org\" label=notes ];
}")

;;; $…$ maths: a label holds no maths, so its inside shows as Org's entities and sub/superscripts do

(define-graph-test "$…$ maths shows as its inside, \\alpha as α, a braced subscript as <SUB>"
  :input "
+ $\\lambda_{x}$ calculus
  + \\(\\beta\\) reduction"
  :output "digraph {
  \"λx calculus\" [ label=<λ<SUB>x</SUB> calculus> ];
  \"λx calculus\" -> \"β reduction\";
  \"β reduction\" [ label=\"β reduction\" ];
}")

(define-graph-test "our links within $…$ render as without: $red:\\alpha + \\beta$ is red:\\alpha + \\beta"
  :input "
+ $red:\\alpha + \\beta$"
  :output "digraph {
  \"α + β\" [ label=<<FONT COLOR=\"red\">α</FONT> + β> ];
}")

;;; The title is labelled as an item's text is

(deftest "a graph's title reads our links, Org's markup, and maths, as an item's text does"
  (should (string-search "graph [ label=<<B>Origins</B> of <FONT COLOR=\"red\">λ</FONT>> ]"
                         (org--graph-raw->dot "
+ a" "*Origins* of $red:\\lambda$"))))

(provide 'org-graph-markup-links-tests)
;;; org-graph-markup-links-tests.el ends here
