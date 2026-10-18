;;; org-math-block-tests.el --- Tests for the math block, and the maths that align shares with it  -*- lexical-binding: t; -*-

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

(deftest "in maths, a COLOUR: sans atom is no link, and a word merely ending in a colour's name is no link either"
  (should (equal (org--math-markup "f : A \\to B, Fred:x" 'latex) "f : A \\to B, Fred:x")))

(deftest "in maths, [[TYPE:PATH][DESC]] is TYPE's LaTeX; an unknown TYPE shows its DESC alone"
  (should (equal (org--math-markup "[[red:][a + b]] [[color:#ff0066][c]] [[color:navy][d]]" 'latex)
                 "{\\color{red}a + b} {\\color[HTML]{ff0066}c} {\\color{navy}d}"))
  (should (equal (org--math-markup "[[https://orgmode.org][\\mathrm{Org}]] [[eqref:Golden Rule]] [[file:x.png][x]]" 'latex)
                 "\\href{https://orgmode.org}{\\mathrm{Org}} \\eqref{Golden Rule} x")))

(deftest "in maths, a tooltip: hovers its tip in HTML, whereas LaTeX shows the maths alone"
  (should (equal (org--math-markup "[[tooltip:violet:x][the current element]]" 'html)
                 "\\texttip{{\\color{violet}x}}{the current element}"))
  (should (equal (org--math-markup "[[tooltip:violet:x][the current element]]" 'latex) "{\\color{violet}x}")))

;;; Emphasis

(deftest "in maths, Org's emphasis is LaTeX's, when its markers hug their text"
  (should (equal (org--math-markup "*f(a)* + /g/ + _h_ + (+i+) + =j= + ~k~" 'latex)
                 "\\boldsymbol{f(a)} + \\mathit{g} + \\underline{h} + (\\cancel{i}) + \\texttt{j} + \\mathtt{k}")))

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

(define-org-export-test "an align block reads our links and Org's emphasis, as a math block does"
  :to latex
  :input "
          #+begin_align
          *x* &= red:y   :tag-blue: Colour
          #+end_align"
  :equals "\\begin{align*}
\\boldsymbol{x} &= {\\color{red}y}   \\tag{$\\color{blue}{\\text{Colour}}$} \\label{Colour}
\\end{align*}
")

(provide 'org-math-block-tests)
;;; org-math-block-tests.el ends here
