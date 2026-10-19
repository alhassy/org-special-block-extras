;;; org-math-block-tests.el --- Tests for the math block, its alignment at &, and eqref links  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

;;; Links

(deftest "in maths, COLOUR:ATOM colours a {group}, a \\command with its {arguments}, or a word; a group's own links too"
  (should (equal (org--math-markup "red:x + blue:{y + green:z}" 'latex)
                 "{\\color{red}x} + {\\color{blue}{y + {\\color{green}z}}}"))
  (should (equal (org--math-markup "teal:\\bigoplus_{violet:x = red:ab}" 'latex)
                 "{\\color{teal}\\bigoplus}_{{\\color{violet}x} = {\\color{red}ab}}"))
  (should (equal (org--math-markup "red:\\frac{a}{b} + blue:\\mathbb{N}" 'latex)
                 "{\\color{red}\\frac{a}{b}} + {\\color{blue}\\mathbb{N}}")))

(deftest "in maths, COLOUR:ATOM right after a $ is a link too, as in a hint's inline maths"
  (should (equal (org--math-markup "\\text{Distribute $red:{(1 - r)}$ over}" 'latex)
                 "\\text{Distribute ${\\color{red}{(1 - r)}}$ over}")))

(deftest "in maths, a COLOUR: sans atom is no link, and a word merely ending in a colour's name is no link either"
  (should (equal (org--math-markup "f : A \\to B, Fred:x" 'latex) "f : A \\to B, Fred:x")))

(deftest "in maths, [[TYPE:PATH][DESC]] is TYPE's LaTeX; an unknown TYPE shows its DESC alone"
  (should (equal (org--math-markup "[[red:][a + b]] [[color:#ff0066][c]] [[color:navy][d]]" 'latex)
                 "{\\color{red}a + b} {\\color[RGB]{255,0,102}c} {\\color{navy}d}"))
  (should (equal (org--math-markup "[[https://orgmode.org][\\mathrm{Org}]] [[eqref:Golden Rule]] [[file:x.png][x]]" 'latex)
                 "\\href{https://orgmode.org}{\\mathrm{Org}} \\eqref{Golden Rule} x")))

(defun org-math-tests--tips (math backend)
  "(MATH, BACKEND) ↦ (MATH in LaTeX . its tooltips' <template>s, oldest first), each gensym numbered from g1."
  (let ((seed 0) org--math-tips)
    (cl-letf (((symbol-function 'gensym) (lambda (&rest _) (format "g%s" (cl-incf seed)))))
      (cons (org--math-markup math backend) (reverse org--math-tips)))))

(deftest "in maths, a tooltip: is one of tooltips.js', as in prose, its tip a <template> beside the maths; in LaTeX, none"
  (should (equal (org-math-tests--tips "[[tooltip:violet:x][the /current/ element]]" 'html)
                 '("\\class{tooltip osbe-tip-g1}{{\\color{violet}x}}"
                   "<template id=\"osbe-tip-g1\"><p>\nthe <i>current</i> element</p></template>")))
  (should (equal (car (org-math-tests--tips "[[tooltip:violet:x][the current element]]" 'latex))
                 "{{\\color{violet}x}}^{\\textnormal{\\the\\numexpr\\value{footnote}+1\\relax}}")))

;;; Emphasis

(deftest "in maths, Org's emphasis is LaTeX's, when its markers hug their text"
  (should (equal (org--math-markup "*f(a)* + /g/ + _h_ + (+i+) + =j= + ~k~" 'latex)
                 "\\boldsymbol{f(a)} + \\mathit{g} + \\underline{h} + (\\cancel{i}) + \\texttt{j} + \\mathtt{k}")))

(deftest "in a hint's prose, Org's emphasis is maths about text, $\\textbf{…}$, which MathJax reads; in $…$, maths'"
  (should (equal (org--math-markup "\\text{by *induction*, /twice/, on ~n~ and $*x*$}" 'latex)
                 "\\text{by $\\textbf{induction}$, $\\textit{twice}$, on $\\texttt{n}$ and $\\boldsymbol{x}$}")))

(define-org-call-test "a math block's matrix table may compute its entries by #+TBLFM:, which is Org's, not maths"
  :call (org-block/math 'latex "
M =
| 1 | 2 | 3 |
| 4 | 5 | 9 |
#+TBLFM: $3=$1+$2" nil)
  :returns "#+begin_export latex
\\[
M =
\\begin{pmatrix} 1 & 2 & 3 \\\\ 4 & 5 & 9 \\end{pmatrix}
\\]
#+end_export")

(org-deftest "C-c C-c on a math block's matrix table computes its #+TBLFM: formulas, as for any Org table"
  :text "
#+begin_math
M =
| 1 | 2 |   |
| 4 | 5 |   |
#+TBLFM: $3=$1+$2
#+end_math"
  :at "#+TBLFM"
  (org-ctrl-c-ctrl-c)
  (should (string-match-p (regexp-quote "| 1 | 2 | 3 |") (buffer-string)))
  (should (string-match-p (regexp-quote "| 4 | 5 | 9 |") (buffer-string))))

(define-org-call-test "a math block's hint may hold an Org table, a matrix, as a line of maths amid its prose"
  :call (org-block/math 'latex "
M ={ as
     | 1 | 0 |
     | 0 | 1 |
   is the identity } I" nil :hints 'beside)
  :returns "#+begin_export latex
\\begin{align*}
M &\\quad = \\quad I && \\begin{array}[t]{@{}l@{}} \\text{as} \\\\ \\begin{pmatrix} 1 & 0 \\\\ 0 & 1 \\end{pmatrix} \\\\ \\text{is the identity} \\end{array}
\\end{align*}
#+end_export")

(deftest "in maths, markers that are arithmetic, sub- and superscripts are left be"
  (dolist (math '("a = b = c" "a*b*c" "x_i_j" "a^* b^*" "{a/b/c}" "1 + 2 + 3"))
    (should (equal (org--math-markup math 'latex) math))))

;;; The block

(define-org-call-test "a math block is display maths; in HTML, a < is escaped, lest it begin a tag"
  :call (org-block/math 'html "
red:0 < 1" nil)
  :returns "#+begin_export html
\\[
{\\color{red}0} &lt; 1
\\]
#+end_export")

(define-org-call-test "a math block's definitions are hidden in HTML, wherein MathJax reads them page-wide"
  :call (org-block/math 'html "
\\def\\N{blue:\\mathbb{N}}
0 \\in \\N" nil)
  :returns "#+begin_export html
<p style=\"display:none\">\\[\\def\\N{{\\color{blue}\\mathbb{N}}}\\]</p>
\\[
0 \\in \\N
\\]
#+end_export")

(define-org-call-test "a math block's definitions go to the LaTeX preamble; a block of only definitions shows nothing"
  :call (org-block/math 'latex "
\\newcommand{\\N}{\\mathbb{N}}" nil)
  :returns "#+latex_header: \\newcommand{\\N}{\\mathbb{N}}
#+begin_export latex

#+end_export")

;;; Alignment ---lines aligned at a top-level &

(deftest "a math block's top-level marks are its &, \\\\, and { outside every {group} and \\begin…\\end, each with its position"
  (should (equal (org--math-top-level-marks "a &= b \\\\ c &= d") '(("&" . 2) ("\\\\" . 7) ("&" . 12))))
  ;; An escaped \\& or \\{ is none; a {group}'s own opening brace is, but nothing within it.
  (should (equal (org--math-top-level-marks "\\text{Q\\&A} = \\{ x \\} \\cases{1 & n \\cr 0 & m}")
                 '(("{" . 5) ("{" . 28))))
  (should (equal (org--math-top-level-marks "f &= \\begin{cases} 1 & n = 0 \\\\ 0 & n > 0 \\end{cases}")
                 '(("&" . 2) ("{" . 46)))))

(define-org-call-test "a math block whose lines have an & is an align; its :tag-COLOUR: NAME a \\tag; its last \\ dropped"
  :call (org-block/math 'latex "x &= x + 0 \\\\
x + 0 &= 0 + x   :tag-blue: Symmetry" nil)
  :returns "#+begin_export latex
\\begin{align*}
x &= x + 0 \\\\
x + 0 &= 0 + x   \\tag{$\\color{blue}{\\text{Symmetry}}$} \\label{Symmetry}
\\end{align*}
#+end_export")

(define-org-call-test "an aligned math block that is :numbered t is LaTeX's align, sans a trailing \\ that would number an empty line"
  :call (org-block/math 'latex "a &= b \\\\
c &= d \\\\
" nil :numbered t)
  :returns "#+begin_export latex
\\begin{align}
a &= b \\\\
c &= d
\\end{align}
#+end_export")

(define-org-call-test "an aligned math block escapes & and < in HTML, which MathJax then reads as LaTeX"
  :call (org-block/math 'html "0 &< 1" nil)
  :returns "#+begin_export html
\\begin{align*}
0 &amp;&lt; 1
\\end{align*}
#+end_export")

(define-org-call-test "an aligned math block keeps its definitions and tables, as any math block does"
  :call (org-block/math 'latex "
\\def\\N{\\mathbb{N}}
M &=
| 1 & 0 |
| 0 & 1 |
" nil)
  :returns "#+latex_header: \\def\\N{\\mathbb{N}}
#+begin_export latex
\\begin{align*}
M &=
\\begin{pmatrix} 1 & 0 \\\\ 0 & 1 \\end{pmatrix}
\\end{align*}
#+end_export")

(define-org-export-test "an aligned math block reads our links, Org's emphasis, and :let shorthands"
  :to latex
  :input "
          #+begin_math :let (-> \"\\\\Rightarrow\" == \"\\\\equiv\")
          *p* -> q &== red:\\neg p \\lor q   :tag-blue: Implication
          #+end_math
          By [[eqref:Implication]], p -> p."
  :equals "\\begin{align*}
\\boldsymbol{p} \\Rightarrow q &\\equiv {\\color{red}\\neg} p \\lor q   \\tag{$\\color{blue}{\\text{Implication}}$} \\label{Implication}
\\end{align*}
By \\eqref{Implication}, p -> p.
")

;;; Cases ---whose & and \\ are their own, not the block's

(define-org-call-test "a math block's \\begin{cases} keeps its & and \\\\, so the block is one equation, neither aligned nor gathered"
  :call (org-block/math 'latex "
n! = \\begin{cases}
1 & n = 0 \\\\
n \\cdot (n - 1)! & n > 0
\\end{cases}" nil)
  :returns "#+begin_export latex
\\[
n! = \\begin{cases}
1 & n = 0 \\\\
n \\cdot (n - 1)! & n > 0
\\end{cases}
\\]
#+end_export")

(define-org-call-test "a math block's plain-TeX \\cases{…} keeps its &, so the block is one equation"
  :call (org-block/math 'latex "
\\lvert x \\rvert = \\cases{x & x \\geq 0 \\cr -x & x < 0}" nil :numbered t)
  :returns "#+begin_export latex
\\begin{equation}
\\lvert x \\rvert = \\cases{x & x \\geq 0 \\cr -x & x < 0}
\\end{equation}
#+end_export")

(define-org-call-test "a math block's lines may align at & around cases, whose own & and \\\\ are left be"
  :call (org-block/math 'latex "
f(n) &= \\begin{cases} 1 & n = 0 \\\\ 0 & n > 0 \\end{cases}   :tag-teal: Indicator
g(n) &= 1 - f(n)" nil)
  :returns "#+begin_export latex
\\begin{align*}
f(n) &= \\begin{cases} 1 & n = 0 \\\\ 0 & n > 0 \\end{cases}   \\tag{$\\color{teal}{\\text{Indicator}}$} \\label{Indicator} \\\\
g(n) &= 1 - f(n)
\\end{align*}
#+end_export")

(define-org-call-test "a math block's lines that end in \\\\ around cases are gathered, by their own \\\\ alone"
  :call (org-block/math 'latex "
a = \\begin{cases} 1 & p \\\\ 0 & q \\end{cases} \\\\
b = 2" nil)
  :returns "#+begin_export latex
\\begin{gather*}
a = \\begin{cases} 1 & p \\\\ 0 & q \\end{cases} \\\\
b = 2
\\end{gather*}
#+end_export")

;;; Atoms

(deftest "in maths, red:\\left(…\\right) colours the whole bracketing, as a group of \\left( alone is an error"
  (should (equal (org--math-markup "red:\\left( x \\right) + y" 'latex) "{\\color{red}\\left( x \\right)} + y"))
  (should (equal (org--math-markup "red:\\left( \\left[ x \\right] \\right) + y" 'latex)
                 "{\\color{red}\\left( \\left[ x \\right] \\right)} + y")))

(deftest "in maths, a sizer's atom holds its delimiter; and a COLOUR: sans word or group colours one character"
  (should (equal (org--math-markup "red:\\big( x blue:\\Big\\langle" 'latex)
                 "{\\color{red}\\big(} x {\\color{blue}\\Big\\langle}"))
  (should (equal (org--math-markup "a red:+ b red:→ c" 'latex) "a {\\color{red}+} b {\\color{red}→} c")))

;;; Footnotes

(deftest "in maths, ATOM[fn::TIP] hovers TIP over ATOM in HTML, a \\command{with}{arguments} being one atom"
  (should (equal (car (org-math-tests--tips "x[fn::the current element] + \\frac{a}{b}[fn::a ratio]" 'html))
                 "\\class{tooltip osbe-tip-g1}{x} + \\class{tooltip osbe-tip-g2}{\\frac{a}{b}}"))
  (should (equal (car (org-math-tests--tips "violet:x[fn::the current element]" 'html))
                 "{\\color{violet}\\class{tooltip osbe-tip-g1}{x}}"))
  ;; In LaTeX, as a prose tooltip is: a margin note, numbered at its ATOM; maths holds no \\marginpar, so it follows.
  (-let [(math . notes) (org-math-tests--tips "x[fn::the current element] + \\frac{a}{b}[fn::a ratio]" 'latex)]
    (should (equal math (concat "{x}^{\\textnormal{\\the\\numexpr\\value{footnote}+1\\relax}}"
                                " + {\\frac{a}{b}}^{\\textnormal{\\the\\numexpr\\value{footnote}+2\\relax}}")))
    (should (equal (length notes) 2))
    (should (string-prefix-p "\\refstepcounter{footnote}\\newsavebox" (car notes)))
    (should (string-match-p "\\marginpar.*" (car notes)))
    (should (string-match-p "the current element" (car notes)))))

(deftest "a page that merely mentions MathJax in prose still gets MathJax, which typesets its maths"
  (should (string-match-p "window.MathJax" (org-export-string-as "
MathJax typesets this.
#+begin_math
x = red:y
#+end_math" 'html))))

(deftest "a page whose only tooltips are in maths still gets tooltips.js, which shows them"
  ;; The maths' tooltips are noted in a temporary buffer, whereas the page's head is the export buffer's to note.
  (let ((org-special-block-add-html-extra t))
    (should (string-match-p "tooltip-box" (org-export-string-as "
#+begin_math
x[fn::the current element]
#+end_math" 'html)))))

(define-org-call-test "a math block's tooltips' <template>s follow its display, outside the maths MathJax reads"
  :call (cl-letf (((symbol-function 'gensym) (lambda (&rest _) "g1")))
          (org-block/math 'html "
x[fn::the current element] < 1" nil))
  :returns "#+begin_export html
\\[
\\class{tooltip osbe-tip-g1}{x} &lt; 1
\\]
<template id=\"osbe-tip-g1\"><p>
the current element</p></template>
#+end_export")

(deftest "in maths, ATOM[[tooltip:TIP]], sans description, is ATOM[fn::TIP]; a lone one is its text, sans tooltip"
  (should (equal (org-math-tests--tips "violet:x[[tooltip:The current element]] = a" 'html)
                 (org-math-tests--tips "violet:x[fn::The current element] = a" 'html)))
  (should (equal (org-math-tests--tips "{} + [[tooltip:Alone]]" 'html) '("{} + Alone"))))

(deftest "in maths, ATOM[fn:NAME] hovers NAME's definition over ATOM, as ATOM[fn::TIP] hovers TIP; one tip serves many"
  (with-temp-buffer
    (org-mode)
    (insert "\n[fn:elt] The /current/ element.\n")
    (should (equal (org-math-tests--tips "x[fn:elt] + y[fn:elt]" 'html)
                   (org-math-tests--tips "x[fn::The /current/ element.] + y[fn::The /current/ element.]" 'html)))
    (should-error (org--math-markup "x[fn:nowhere]" 'html) :type 'user-error)))

(deftest "in maths, a section link, [[*Heading]], [[#custom-id]], or [[id:ID]], links its description to that section"
  (should (equal (org--math-markup "[[*Folds][\\oplus]] + [[#fold][f]]" 'html) "\\href{*Folds}{\\oplus} + \\href{#fold}{f}"))
  (should (equal (org--math-markup "[[id:42][\\oplus]]" 'latex) "\\hyperref[id:42]{\\oplus}"))
  ;; Sans description, the heading shows, as text; Markdown's maths has no links.
  (should (equal (org--math-markup "[[*Folds]]" 'html) "\\href{*Folds}{\\text{Folds}}"))
  (should (equal (let ((org-export-current-backend 'md)) (org--math-markup "[[*Folds][f]]" 'html)) "f")))

(deftest "a math block's section links reach the section's anchor once exported, in HTML and LaTeX; an unknown one stays"
  (let ((org "
#+begin_math
[[*Folds][f]] + [[#fold][g]] + [[*Nowhere][h]]
#+end_math
* Folds
:PROPERTIES:
:CUSTOM_ID: fold
:END:"))
    (should (s-contains-p "\\href{#fold}{f} + \\href{#fold}{g} + \\href{*Nowhere}{h}" (org-export-string org 'html)))
    (let ((latex (org-export-string org 'latex)))
      (should (string-match "\\\\label{\\([^}]+\\)}" latex))
      (should (s-contains-p (format "\\hyperref[%s]{f} + \\hyperref[%1$s]{g} + \\hyperref[*Nowhere]{h}" (match-string 1 latex))
                            latex)))))

;;; Structure

(deftest "a math block's description items are cases: each term a value, each description its condition, prose as text"
  (should (equal (org--math-cases "
n! =
- 1 :: n = 0
- n · (n-1)! :: otherwise
+ 1")
                 "
n! =
\\begin{cases} 1 & n = 0 \\\\ n · (n-1)! & \\text{otherwise} \\end{cases}
+ 1")))

(define-org-call-test "a math block's cases are LaTeX's own, whose & aligns no lines of the block"
  :call (org-block/math 'latex "
\\lvert x \\rvert =
- x :: x ≥ 0
- -x :: otherwise" nil)
  :returns "#+begin_export latex
\\[
\\lvert x \\rvert =
\\begin{cases} x & x ≥ 0 \\\\ -x & \\text{otherwise} \\end{cases}
\\]
#+end_export")

(define-org-call-test "a math block's “# …” lines are Org comments, which TeX would read as a macro's parameter"
  :call (org-block/math 'latex "
# The identity, for one.
x = x" nil)
  :returns "#+begin_export latex
\\[
x = x
\\]
#+end_export")

(define-org-call-test "a math block's definition runs through the line that closes its braces; in HTML, it is escaped"
  :call (org-block/math 'html "
\\newcommand{\\lt}[2]{
  #1 < #2}
\\lt{a}{b}" nil)
  :returns "#+begin_export html
<p style=\"display:none\">\\[\\newcommand{\\lt}[2]{
  #1 &lt; #2}\\]</p>
\\[
\\lt{a}{b}
\\]
#+end_export")

(define-org-call-test "a math block's multiline definition is one #+latex_header: line per line"
  :call (org-block/math 'latex "
\\newcommand{\\pair}[2]{
  \\langle #1, #2 \\rangle}" nil)
  :returns "#+latex_header: \\newcommand{\\pair}[2]{
#+latex_header:   \\langle #1, #2 \\rangle}
#+begin_export latex

#+end_export")

(define-org-call-test "a math block's Org table is a matrix, sans its rules; :matrix names its environment"
  :call (org-block/math 'latex "
I =
| red:1 | 0 |
|-------+---|
| 0     | 1 |" nil :matrix "bmatrix")
  :returns "#+begin_export latex
\\[
I =
\\begin{bmatrix} {\\color{red}1} & 0 \\\\ 0 & 1 \\end{bmatrix}
\\]
#+end_export")

(define-org-call-test "a math block's lines that end in \\\\ are gathered; :numbered t numbers them"
  :call (org-block/math 'latex "
a = b \\\\
c = d" nil :numbered t)
  :returns "#+begin_export latex
\\begin{gather}
a = b \\\\
c = d
\\end{gather}
#+end_export")

(define-org-call-test "a math block's :tag-COLOUR: NAME is a coloured \\tag, labelled NAME; tagged lines are gathered"
  :call (org-block/math 'latex "
a = b   :tag-blue: One
c = d   :tag-red: Two" nil)
  :returns "#+begin_export latex
\\begin{gather*}
a = b   \\tag{$\\color{blue}{\\text{One}}$} \\label{One} \\\\
c = d   \\tag{$\\color{red}{\\text{Two}}$} \\label{Two}
\\end{gather*}
#+end_export")

(define-org-call-test "a math block that is :numbered t, of one line, is LaTeX's equation"
  :call (org-block/math 'latex "
e^{i\\pi} + 1 = 0" nil :numbered t)
  :returns "#+begin_export latex
\\begin{equation}
e^{i\\pi} + 1 = 0
\\end{equation}
#+end_export")

(org-deftest "following eqref:NAME visits the line tagged :tag-COLOUR: NAME, else a \\tag{…NAME…}"
  :text "
By [[eqref:Golden Rule]] and [[eqref:Leibniz]].
#+begin_math
p \\land q &\\equiv p \\equiv q \\equiv p \\lor q   :tag-orange: Golden Rule
#+end_math
\\begin{equation} x = y \\Rightarrow f(x) = f(y) \\tag{Leibniz} \\end{equation}"
  (funcall (org-link-get-parameter "eqref" :follow) "Golden Rule" nil)
  (should (looking-at-p "p \\\\land q"))
  (funcall (org-link-get-parameter "eqref" :follow) "Leibniz" nil)
  (should (looking-at-p "\\\\begin{equation}"))
  (should-error (funcall (org-link-get-parameter "eqref" :follow) "Nowhere" nil) :type 'user-error))

;;; Names ---Org's own #+name: labels the block

(define-org-export-test "a named math block that is :numbered t is labelled by its #+name:, which eqref: cites"
  :to latex
  :input "
          #+name: Euler
          #+begin_math :numbered t
          e^{i\\pi} + 1 = 0
          #+end_math
          By [[eqref:Euler]]."
  :equals "\\begin{equation}
e^{i\\pi} + 1 = 0 \\label{Euler}
\\end{equation}
By \\eqref{Euler}.
")

(define-org-export-test "a named math block sans numbers is tagged by its #+name:, so eqref: has a name to cite"
  :to latex
  :input "
          #+name: Euler
          #+begin_math
          e^{i\\pi} + 1 = 0
          #+end_math"
  :equals "\\[
e^{i\\pi} + 1 = 0 \\tag{Euler} \\label{Euler}
\\]
")

(org-deftest "following eqref:NAME visits the math block named NAME"
  :text "
By [[eqref:Euler]].
#+name: Euler
#+begin_math
e^{i\\pi} + 1 = 0
#+end_math"
  (funcall (org-link-get-parameter "eqref" :follow) "Euler" nil)
  (should (looking-at-p "#\\+name: Euler")))

;;; Hints ---REL{ HINT } aligns its line at REL, the hint in a column of its own

(define-org-call-test "a math block's “REL{ HINT }” aligns its line at REL, the HINT at the line's end, before its \\\\"
  :call (org-block/math 'latex "
(a + b)^2 ={ by expanding } a^2 + 2ab + b^2 \\\\
≥{ as squares are nonnegative } 2ab" nil :hints 'beside)
  :returns "#+begin_export latex
\\begin{align*}
(a + b)^2 &\\quad = \\quad a^2 + 2ab + b^2 && \\text{by expanding} \\\\
&\\quad ≥ \\quad 2ab && \\text{as squares are nonnegative}
\\end{align*}
#+end_export")

(define-org-call-test "a math block's line before a hint's ends there, since a hint begins a step; so it needs no \\\\"
  :call (org-block/math 'latex "
p \\land q ={ Golden Rule } p = q = p \\lor q
          ={ by symmetry } q \\land p \\\\
          ={ by me and my ideas } p \\land q" nil :hints 'beside)
  :returns "#+begin_export latex
\\begin{align*}
p \\land q &\\quad = \\quad p = q = p \\lor q && \\text{Golden Rule} \\\\
&\\quad = \\quad q \\land p && \\text{by symmetry} \\\\
&\\quad = \\quad p \\land q && \\text{by me and my ideas}
\\end{align*}
#+end_export")

(deftest "a math block's step runs from one hint to the next, wherever its newlines fall; as LaTeX reads them, they are spaces"
  (let ((squeeze (lambda (s) (replace-regexp-in-string "[ \t\n]+" " " s))))
    (should (equal (funcall squeeze (org-block/math 'latex "
p \\land q ={ Golden Rule } p = q = & p \\lor q
          ={ by symmetry } q \\land & p
          ={ by me and my ideas } & p \\land q" nil))
                   (funcall squeeze (org-block/math 'latex "
  p \\land q
={ Golden Rule }
  p = q = & p \\lor q
={ by symmetry }
  q \\land & p
={ by me and my ideas }
& p \\land q" nil))))))

(define-org-call-test "a math block's hint goes at its step's end, before the tag there; a step may begin a line's middle"
  :call (org-block/math 'latex "
\\def\\N{\\mathbb{N}}
p \\land q
={ Golden Rule }
p = q   :tag-blue: Step
={ by symmetry } q = p ={ by reflexivity } q = p" nil :hints 'beside)
  :returns "#+latex_header: \\def\\N{\\mathbb{N}}
#+begin_export latex
\\begin{align*}
p \\land q
&\\quad = \\quad
p = q && \\text{Golden Rule}   \\tag{$\\color{blue}{\\text{Step}}$} \\label{Step} \\\\
&\\quad = \\quad q = p && \\text{by symmetry} \\\\ &\\quad = \\quad q = p && \\text{by reflexivity}
\\end{align*}
#+end_export")

(define-org-call-test "a math block's definition right before a hint's line is left be, as no line of maths"
  :call (org-block/math 'latex "
\\def\\N{\\mathbb{N}}
x ={ by definition } \\N" nil :hints 'beside)
  :returns "#+latex_header: \\def\\N{\\mathbb{N}}
#+begin_export latex
\\begin{align*}
x &\\quad = \\quad \\N && \\text{by definition}
\\end{align*}
#+end_export")

(define-org-call-test "a math block's hint comes before its line's tag; a hint may hold {groups} of its own"
  :call (org-block/math 'latex "
x ={ by {IH} } y   :tag-blue: Step" nil :hints 'beside)
  :returns "#+begin_export latex
\\begin{align*}
x &\\quad = \\quad y && \\text{by {IH}}   \\tag{$\\color{blue}{\\text{Step}}$} \\label{Step}
\\end{align*}
#+end_export")

(define-org-export-test "a math block's hint follows a :let shorthand's relation, as in a calculational proof"
  :to latex
  :input "
          #+begin_math :let (== \"\\\\equiv\") :hints beside
          p =={ by definition } q
          #+end_math"
  :equals "\\begin{align*}
p &\\quad \\equiv \\quad q && \\text{by definition}
\\end{align*}
")

(define-org-call-test "a math block's hint may span lines, its { followed by a newline; it shows on those lines"
  :call (org-block/math 'latex "
\\zero + \\suc \\zero
  ={
Addition moves the $\\suc$-s from the left argument to the
  right.
}
  \\suc (\\zero + \\zero)" nil :hints 'beside)
  :returns "#+begin_export latex
\\begin{align*}
\\zero + \\suc \\zero
&\\quad = \\quad
\\suc (\\zero + \\zero) && \\begin{array}[t]{@{}l@{}} \\text{Addition moves the $\\suc$-s from the left
argument to the} \\\\ \\text{right.} \\end{array}
\\end{align*}
#+end_export")

(define-org-call-test "a math block's hint keeps its blank lines, as gaps between its paragraphs"
  :call (org-block/math 'latex "
x ={ First, this.

     Then, that. } y" nil :hints 'beside)
  :returns "#+begin_export latex
\\begin{align*}
x &\\quad = \\quad y && \\begin{array}[t]{@{}l@{}} \\text{First, this.} \\\\  \\\\ \\text{Then, that.}
\\end{array}
\\end{align*}
#+end_export")

(define-org-call-test "a math block's hint may hold a nested calculation, \\[ … \\], its own hints read alike"
  :call (org-block/math 'latex "
x ={ Why? Let me explain:
     \\[    x
        ={ hint 1 } x′
        ={ hint 2 } y \\]
     So there.
   } y" nil :hints 'beside)
  :returns "#+begin_export latex
\\begin{align*}
x &\\quad = \\quad y && \\begin{array}[t]{@{}l@{}} \\text{Why? Let me explain:} \\\\ \\begin{aligned}[t] x
&\\quad = \\quad x′ && \\text{hint 1} \\\\ &\\quad = \\quad y && \\text{hint 2} \\end{aligned} \\\\ \\text{So
there.} \\end{array}
\\end{align*}
#+end_export")

(define-org-call-test "a math block's nested calculation may nest a calculation in turn"
  :call (org-block/math 'latex "
a ={ \\[ a ={ \\[ a ={ inmost } b \\] } b \\] } b" nil :hints 'beside)
  :returns "#+begin_export latex
\\begin{align*}
a &\\quad = \\quad b && \\begin{aligned}[t] a &\\quad = \\quad b && \\begin{aligned}[t] a &\\quad = \\quad
b && \\text{inmost} \\end{aligned} \\end{aligned}
\\end{align*}
#+end_export")

(define-org-call-test "a math block :hints between sets each hint on a row of its own, between the expressions it relates"
  :call (org-block/math 'latex "
\\def\\N{\\mathbb{N}}
  x
={ why x is y } y   :tag-blue: Step
≤{ why y is below z,
   over lines } z" nil :hints 'between)
  :returns "#+latex_header: \\def\\N{\\mathbb{N}}
#+begin_export latex
\\begin{align*}
& x
\\\\ = & \\quad {\\color[RGB]{128,0,0} \\left[ \\text{why x is y} \\right.} \\notag \\\\ & y
\\tag{$\\color{blue}{\\text{Step}}$} \\label{Step} \\\\
≤ & \\quad {\\color[RGB]{128,0,0} \\left[ \\begin{array}{@{}l@{}} \\text{why y is below z,} \\\\
\\text{over lines} \\end{array} \\right.} \\notag \\\\ & z
\\end{align*}
#+end_export")

(deftest "a math block's hint in HTML is clickable, to fold it to …, its tooltip saying so; LaTeX's is no such thing"
  (let ((html (org-block/math 'html "x ={ h } y" nil :hints 'beside)))
    (should (string-match-p (regexp-quote "\\class{tooltip osbe-tip-hint osbe-hint}{\\class{osbe-hint-body}{\\text{h}}\\class{osbe-hint-fold}{[\\,\\cdots\\,]}}")
                            html)))
  ;; Between expressions, the fold is a closed bracket in the hint's maroon, not the open bracket of a tall hint.
  (should (string-match-p (regexp-quote "\\class{osbe-hint-fold}{{\\color[RGB]{128,0,0} [\\,\\cdots\\,]}}")
                          (org-block/math 'html "x ={ h } y" nil)))
  (should-not (string-match-p "class" (org-block/math 'latex "x ={ h } y" nil))))

(deftest "a page whose maths has hints carries their tip and the script by which a click folds them, in its head"
  (let* ((page (org-export-string-as "#+begin_math\nx ={ h } y\n#+end_math" 'html))
         (head (substring page 0 (string-match "</head>" page))))
    (should (string-match-p (regexp-quote "<template id=\"osbe-tip-hint\">") head))
    (should (string-match-p (regexp-quote "classList.toggle('osbe-folded')") head))
    (should (string-match-p "tooltip-box" head))
    ;; A hint is plainly a hint, so it wears no tooltip's dotted underline; as two classes, the rule outranks .tooltip.
    (should (string-match-p (regexp-quote ".tooltip.osbe-hint { border-bottom: none; }") head))
    ;; A shift-click toggles every hint of its proof, as C-TAB does in Emacs; the tip says so.
    (should (string-match-p "shiftKey" head))
    (should (string-match-p "shift-click" head))))

(deftest "a math block's hint colour is LaTeX's and MathJax's alike: #RRGGBB as RGB, since MathJax has no HTML model"
  (should (equal (org--math-color "#800000") "[RGB]{128,0,0}"))
  (should (equal (org--math-color "teal") "{teal}")))

(deftest "a math block's :hints sets them between its expressions, maroon and bracketed; or beside, plain; or none"
  (should (string-match-p (regexp-quote "= & \\quad {\\color[RGB]{128,0,0} \\left[ \\text{h} \\right.}")
                          (org-block/math 'latex "x ={ h } y" nil)))
  (should (string-match-p (regexp-quote "&& \\text{h}\n") (org-block/math 'latex "x ={ h } y" nil :hints 'beside)))
  ;; A list gives the layout a format of our own, its %s the hint; a colour is LaTeX's, within it.
  (should (string-match-p (regexp-quote "&& {\\color{teal} \\langle \\text{h} \\rangle}")
                          (org-block/math 'latex "x ={ h } y" nil :hints '(beside "{\\color{teal} \\langle %s \\rangle}"))))
  ;; none hides hints from the export; their steps remain, as a compact chain.
  (let ((hidden (org-block/math 'latex "x ={ h } y" nil :hints 'none)))
    (should-not (string-match-p "\\\\text{h}" hidden))
    (should (string-match-p (regexp-quote "x &\\quad = \\quad y\n") hidden))))

(deftest "a math block's :hints may be folded, so a page's reader clicks to see each; LaTeX, sans clicks, shows them"
  (should (string-match-p (regexp-quote "\\class{tooltip osbe-tip-hint osbe-hint osbe-folded}")
                          (org-block/math 'html "x ={ h } y" nil :hints '(between folded))))
  ;; Folding hides the whole hint, its format's tall bracket too; the fold is the format about a lone ⋯.
  (should (string-match-p (regexp-quote (concat "\\class{tooltip osbe-tip-hint osbe-hint osbe-folded}"
                                                "{\\class{osbe-hint-body}{{\\color{teal} \\langle \\text{h} \\rangle}}"
                                                "\\class{osbe-hint-fold}{{\\color{teal} \\langle \\cdots \\rangle}}}"))
                          (org-block/math 'html "x ={ h } y" nil :hints '(between "{\\color{teal} \\langle %s \\rangle}" folded))))
  (should (string-match-p (regexp-quote "\\text{h}") (org-block/math 'latex "x ={ h } y" nil :hints '(between folded)))))

(define-org-call-test "a math block's REL{GROUP}, ^{ 2 }, \\frac{ a }{ b }, and a REL{ … } within cases are no hints"
  :call (org-block/math 'latex "
x ={a} + \\frac{ a }{ b }^{ 2 } = \\begin{cases} y ={ no } z & p \\end{cases}" nil)
  :returns "#+begin_export latex
\\[
x ={a} + \\frac{ a }{ b }^{ 2 } = \\begin{cases} y ={ no } z & p \\end{cases}
\\]
#+end_export")

;;; Numbered once ---many lines, one number

(define-org-export-test "a math block :numbered once whose lines align is one equation, split; its #+name: labels it"
  :to latex
  :input "
          #+name: Square
          #+begin_math :numbered once
          (a + b)^2 &= (a + b)(a + b) \\\\
          &= a^2 + 2ab + b^2
          #+end_math"
  :equals "\\begin{equation}
\\begin{split}
(a + b)^2 &= (a + b)(a + b) \\\\
&= a^2 + 2ab + b^2
\\end{split} \\label{Square}
\\end{equation}
")

(define-org-call-test "a math block :numbered once whose lines are gathered is one equation, gathered"
  :call (org-block/math 'latex "
a = 1 \\\\
b = 2" nil :numbered 'once)
  :returns "#+begin_export latex
\\begin{equation}
\\begin{gathered}
a = 1 \\\\
b = 2
\\end{gathered}
\\end{equation}
#+end_export")

(define-org-call-test "a math block :numbered once takes its one tag out of the split, onto the whole equation"
  :call (org-block/math 'latex "
x &= y   :tag-blue: Sym
&= z" nil :numbered 'once)
  :returns "#+begin_export latex
\\begin{equation}
\\begin{split}
x &= y \\\\
&= z
\\end{split} \\tag{$\\color{blue}{\\text{Sym}}$} \\label{Sym}
\\end{equation}
#+end_export")

(deftest "a math block :numbered once refuses two tags, as one equation has one number"
  (should-error (org-block/math 'latex "x &= y   :tag-blue: A\n&= z   :tag-red: B" nil :numbered 'once) :type 'user-error))

;;; Inline maths ---$…$ reads our markup too

(define-org-export-test "inline maths, $…$ and \\(…\\), reads our links and Org's emphasis, as a math block does"
  :to latex
  :input "We have $red:x + *y*$ and \\(blue:z\\)."
  :equals "We have \\({\\color{red}x} + \\boldsymbol{y}\\) and \\({\\color{blue}z}\\).
")

(deftest "inline maths' tooltip is a margin note in LaTeX, after the maths, which holds no \\marginpar"
  (let ((latex (exporting "Let $x[fn::the current element] = 1$." :to latex)))
    (should (string-prefix-p "Let \\({x}^{\\textnormal{\\the\\numexpr\\value{footnote}+1\\relax}} = 1\\)\\refstepcounter" latex))
    (should (string-match-p "\\\\marginpar{.*}.*\\\\(\\\\)\\.\n*\\'" latex))))

(define-org-export-test "inline maths reads our links in HTML too, for MathJax"
  :to html
  :input "We have $red:x$."
  :equals "<p>
We have \\({\\color{red}x}\\).</p>
")

;;; In Emacs

(org-deftest "a math block shows each COLOUR:ATOM in its colour, as the maths reads it, and each tag in its colour" [fontify]
  :text "
#+begin_math
teal:\\bigoplus_{violet:x = red:a}^{cyan:b}   :tag-blue: Fold
#+end_math"
  (font-lock-ensure)
  (search-forward "red:")
  (should (member '(:foreground "red") (ensure-list (get-text-property (point) 'face))))
  ;; Org's own plain link would run on to “cyan:b”; ours stops at the atom.
  (search-forward "}^{")
  (should-not (member '(:foreground "red") (ensure-list (get-text-property (point) 'face))))
  (search-forward "cyan:")
  (should (member '(:foreground "cyan") (ensure-list (get-text-property (point) 'face))))
  (search-forward ":tag-blue")
  (should (member '(:inherit org-tag :foreground "blue") (ensure-list (get-text-property (point) 'face)))))

(org-deftest "a math block looks as a #+begin_src latex block does, with our links and Org's emphasis atop" [fontify]
  :text "
#+begin_math
\\def\\xstart{red:a}
\\sum_{i = red:a} *f*
#+end_math"
  (font-lock-ensure)
  (search-forward "\\def")
  (should (memq 'font-lock-keyword-face (ensure-list (get-text-property (1- (point)) 'face))))
  (should (memq 'org-block (ensure-list (get-text-property (1- (point)) 'face))))
  (search-forward "red:")
  (should (member '(:foreground "red") (ensure-list (get-text-property (point) 'face))))
  ;; Org's link face is gone from the brace after the atom, which keeps LaTeX's look.
  (forward-char 1)
  (should-not (member '(:foreground "red") (ensure-list (get-text-property (point) 'face))))
  (should (memq 'org-block (ensure-list (get-text-property (point) 'face))))
  (search-forward "*f")
  (should (memq 'bold (ensure-list (get-text-property (point) 'face)))))

(deftest "C-c ' edits a math block in latex-mode"
  (should (eq (alist-get "math" org--block-edit-special-handlers nil nil #'equal) 'org-math--do-edit-special)))

(org-deftest "C-c ' edits a math block in latex-mode, which shows its hints and tags as the Org buffer does"
  :text "
#+begin_math
x ={ by IH } y   :tag-blue: Step
={ by symmetry } z
#+end_math"
  :at "by IH"
  (org-edit-special)
  (unwind-protect
      (progn
        (should (derived-mode-p 'latex-mode))
        (font-lock-ensure)
        (dolist (hint '("by IH" "by symmetry"))
          (goto-char (point-min))
          (search-forward hint)
          (should (memq 'org-math-hint (ensure-list (get-text-property (1- (point)) 'face)))))
        (goto-char (point-min))
        (search-forward ":tag-blue")
        (should (member '(:inherit org-tag :foreground "blue") (ensure-list (get-text-property (1- (point)) 'face)))))
    (org-edit-src-abort)))

(org-deftest "C-c C-c within a math block shows its maths as an image atop it; C-c C-c again removes the image"
  :text "
#+begin_math
red:x^2 + y^2 = z^2
#+end_math"
  :at "y^2"
  (skip-unless (and (executable-find "latex") (executable-find "dvipng")))
  ;; Sans a display, as in batch, Org's colours are unspecified; we name them.
  (setq-local org-format-latex-options (org-combine-plists org-format-latex-options '(:foreground "Black" :background "White")))
  (org-ctrl-c-ctrl-c)
  (let ((image (--first (overlay-get it 'org-math-preview) (overlays-in (point-min) (point-max)))))
    (should image)
    (should (eq (overlay-get image 'org-overlay-type) 'org-latex-overlay)))
  (org-ctrl-c-ctrl-c)
  (should-not (--first (overlay-get it 'org-math-preview) (overlays-in (point-min) (point-max)))))

(org-deftest "C-c C-x C-l, which previews Org's own maths in a region, previews each math block therein too"
  :text "
#+begin_math
x^2 + y^2 = z^2
#+end_math
#+begin_math
a &= b
#+end_math"
  (skip-unless (and (executable-find "latex") (executable-find "dvipng")))
  (setq-local org-format-latex-options (org-combine-plists org-format-latex-options '(:foreground "Black" :background "White")))
  ;; Org's own previewing wants a window system, which batch lacks; so we call what it is advised with.
  (should (advice-member-p #'org--math-preview-region 'org--latex-preview-region))
  (org--math-preview-region (point-min) (point-max))
  (should (= 2 (length (--filter (overlay-get it 'org-math-preview) (overlays-in (point-min) (point-max))))))
  ;; Again, the blocks shown are left be, not toggled off.
  (org--math-preview-region (point-min) (point-max))
  (should (= 2 (length (--filter (overlay-get it 'org-math-preview) (overlays-in (point-min) (point-max))))))
  ;; And Org's own clearing clears ours, as they are LaTeX overlays to it.
  (org-clear-latex-preview (point-min) (point-max))
  (should-not (--first (overlay-get it 'org-math-preview) (overlays-in (point-min) (point-max)))))

(org-deftest "a math block's :tag-COLOUR: shows as a tag in that colour, and as a plain tag for a colour Emacs lacks" [fontify]
  :text "
#+begin_math
x &= y   :tag-blue: Symmetry
y &= z   :tag-notacolour: Mystery
#+end_math
:tag-blue: outside a math block is left be."
  (font-lock-ensure)
  (search-forward ":tag-blue")
  (should (member '(:inherit org-tag :foreground "blue") (ensure-list (get-text-property (point) 'face))))
  (search-forward ":tag-notacolour")
  (should (memq 'org-tag (ensure-list (get-text-property (point) 'face))))
  (search-forward ":tag-blue")
  (should-not (memq 'org-tag (flatten-tree (get-text-property (point) 'face)))))

(org-deftest "a math block's hint, “REL{ HINT }”, shows as a comment, as the export reads it; LaTeX's own groups do not" [fontify]
  :text "
#+begin_math
x ={ by {IH} } y ={ this too, the next step } z \\\\
\\frac{ a }{ b } ={a} \\begin{cases} p ={ nor this } q \\end{cases}
\\leq{ by monotonicity } w
#+end_math"
  (font-lock-ensure)
  (search-forward "by {IH")
  (should (memq 'org-math-hint (ensure-list (get-text-property (point) 'face))))
  (search-forward "this too")
  (should (memq 'org-math-hint (ensure-list (get-text-property (point) 'face))))
  (dolist (plain '("{ a }" "{a}" "nor this"))
    (search-forward plain)
    (should-not (memq 'org-math-hint (ensure-list (get-text-property (1- (point)) 'face)))))
  (search-forward "by monotonicity")
  (should (memq 'org-math-hint (ensure-list (get-text-property (1- (point)) 'face)))))

(org-deftest "a math block's hint may span lines, and shows as a comment throughout" [fontify]
  :text "
#+begin_math
\\zero + \\suc \\zero
  ={
Addition moves the suc-s
  right.
}
  \\suc \\zero
#+end_math"
  (font-lock-ensure)
  (dolist (hint '("Addition" "right."))
    (search-forward hint)
    (should (memq 'org-math-hint (ensure-list (get-text-property (1- (point)) 'face))))))

(org-deftest "M-q within a math block's hint fills the hint alone, as prose; the maths around it is left be"
  :text "
#+begin_math
\\zero + \\suc \\zero   :tag-blue: Start
  ={
Addition moves the suc-s from the left argument to the
  right, one at a time.
}
  \\suc \\zero
#+end_math"
  :at "suc-s"
  (let ((fill-column 30))
    (fill-paragraph))
  (should (equal (buffer-string) "
#+begin_math
\\zero + \\suc \\zero   :tag-blue: Start
  ={
Addition moves the suc-s from
the left argument to the
right, one at a time.
}
  \\suc \\zero
#+end_math")))

(org-deftest "M-q within a math block's hint fills the paragraph at point alone; a nested calculation is left be"
  :text "
#+begin_math
x ={ First paragraph, left as it
     is.

     Second paragraph, which is rather long indeed.
     \\[ x
        ={ a nested hint, left be } y \\]
   } y
#+end_math"
  :at "rather long"
  (let ((fill-column 30) (before (buffer-string)))
    (fill-paragraph)
    (should (equal (buffer-string)
                   (replace-regexp-in-string "Second paragraph, which is rather long indeed."
                                             "Second paragraph, which\n     is rather long indeed." before t t)))
    ;; Within the nested calculation, M-q leaves all be.
    (search-forward "nested hint")
    (setq before (buffer-string))
    (fill-paragraph)
    (should (equal (buffer-string) before))))

(org-deftest "TAB on a math block's hint folds it to ={…}, and TAB again unfolds it; the text itself is untouched"
  :text "
#+begin_math
x ={ a long reason,
     over lines } y
#+end_math"
  :at "long reason"
  (let ((before (buffer-string))
        (folds (lambda () (--filter (overlay-get it 'org-math-hint-fold) (overlays-in (point-min) (point-max))))))
    (org-cycle)
    (should (equal (mapcar (lambda (o) (list (buffer-substring (overlay-start o) (overlay-end o)) (overlay-get o 'display)))
                           (funcall folds))
                   '((" a long reason,\n     over lines " "…"))))
    (should (equal (buffer-string) before))
    ;; On its relation too.
    (goto-char (point-min))
    (search-forward "x =")
    (backward-char)
    (org-cycle)
    (should-not (funcall folds))))

(org-deftest "C-TAB in a math block folds all its hints, unless all are folded, whence it unfolds them; so too under C-c '"
  :text "
#+begin_math
x ={ one } y ={ two } z
#+end_math
Outside."
  :at "x ="
  (let ((folds (lambda () (length (--filter (overlay-get it 'org-math-hint-fold) (overlays-in (point-min) (point-max)))))))
    (should (eq (key-binding (kbd "C-<tab>")) 'org--math-cycle-hints))
    ;; One folded already: C-TAB folds the rest.
    (search-forward "one")
    (org--math-fold-hint)
    (should (= 1 (funcall folds)))
    (call-interactively #'org--math-cycle-hints)
    (should (= 2 (funcall folds)))
    (call-interactively #'org--math-cycle-hints)
    (should (= 0 (funcall folds)))
    ;; Outside a math block, C-TAB is as ever.
    (search-forward "Outside")
    (should-not (eq (key-binding (kbd "C-<tab>")) 'org--math-cycle-hints))
    (goto-char (point-min))
    (search-forward "one")
    (org-edit-special)
    (unwind-protect
        (progn (should (eq (key-binding (kbd "C-<tab>")) 'org--math-cycle-hints))
               (call-interactively #'org--math-cycle-hints)
               (should (= 2 (funcall folds))))
      (org-edit-src-abort))))

(org-deftest "TAB folds a hint in a math block's C-c ' buffer too; elsewhere there, TAB does as latex-mode's"
  :text "
#+begin_math
x ={ a reason } y
#+end_math"
  :at "a reason"
  (org-edit-special)
  (unwind-protect
      (let ((folds (lambda () (--filter (overlay-get it 'org-math-hint-fold) (overlays-in (point-min) (point-max))))))
        (should org--math-edit-mode)
        (goto-char (point-min))
        (search-forward "a reason")
        (call-interactively (key-binding (kbd "TAB")))
        (should (equal (mapcar (lambda (o) (overlay-get o 'display)) (funcall folds)) '("…")))
        (call-interactively (key-binding (kbd "TAB")))
        (should-not (funcall folds))
        ;; Off a hint, TAB is latex-mode's own.
        (goto-char (point-min))
        (should-not (org--math-fold-hint)))
    (org-edit-src-abort)))

(org-deftest "on a math block's hint, eldoc says what it is, how to write it, and that TAB folds it; off it, nothing"
  :text "
#+begin_math
x ={ a reason } y
#+end_math"
  :at "a reason"
  (let (said)
    (org--math-hint-eldoc (lambda (doc &rest plist) (setq said (cons doc plist))))
    (should (string-match-p "TAB" (car said)))
    (should (string-match-p (regexp-quote "REL{ REASON }") (car said)))
    (setq said nil)
    (goto-char (point-min))
    (search-forward "x =")
    (backward-char 3)
    (org--math-hint-eldoc (lambda (doc &rest _) (setq said doc)))
    (should-not said)
    ;; So too in its C-c ' buffer.
    (search-forward "a reason")
    (org-edit-special)
    (unwind-protect
        (progn (goto-char (point-min))
               (search-forward "a reason")
               (org--math-hint-eldoc (lambda (doc &rest _) (setq said doc)))
               (should (string-match-p "TAB" said)))
      (org-edit-src-abort))))

(org-deftest "M-q within a math block, but not a hint, leaves the maths be, as it is no prose"
  :text "
#+begin_math
a + b
  = c   :tag-blue: Start
#+end_math"
  :at "a + b"
  (let ((before (buffer-string)) (fill-column 10))
    (fill-paragraph)
    (should (equal (buffer-string) before))))

(org-deftest "a math block's & shows as a keyword, since lines align there; an escaped \\& is LaTeX's alone" [fontify]
  :text "
#+begin_math
\\text{Q\\&A} &= 42
#+end_math"
  (font-lock-ensure)
  (search-forward "\\&")
  ;; LaTeX shows \\& as one command; were & marked as where lines align, it would wear a face more than \\.
  (should (equal (get-text-property (- (point) 2) 'face) (get-text-property (1- (point)) 'face)))
  (search-forward "&")
  (should (memq 'font-lock-keyword-face (ensure-list (get-text-property (1- (point)) 'face)))))

(provide 'org-math-block-tests)
;;; org-math-block-tests.el ends here
