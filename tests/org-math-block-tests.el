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

(defun org-math-tests--tips (math backend)
  "(MATH, BACKEND) ↦ (MATH in LaTeX . its tooltips' <template>s, oldest first), each gensym numbered from g1."
  (let ((seed 0) org--math-tips)
    (cl-letf (((symbol-function 'gensym) (lambda (&rest _) (format "g%s" (cl-incf seed)))))
      (cons (org--math-markup math backend) (reverse org--math-tips)))))

(deftest "in maths, a tooltip: is one of tooltips.js', as in prose, its tip a <template> beside the maths; in LaTeX, none"
  (should (equal (org-math-tests--tips "[[tooltip:violet:x][the /current/ element]]" 'html)
                 '("\\class{tooltip osbe-tip-g1}{{\\color{violet}x}}"
                   "<template id=\"osbe-tip-g1\"><p>\nthe <i>current</i> element</p></template>")))
  (should (equal (org-math-tests--tips "[[tooltip:violet:x][the current element]]" 'latex) '("{\\color{violet}x}"))))

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
  (should (equal (org--math-markup "x[fn::the current element] + \\frac{a}{b}[fn::a ratio]" 'latex)
                 "x + \\frac{a}{b}")))

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

;;; Structure

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

(org-deftest "following eqref:NAME visits a math block's line tagged NAME, as it does an align block's"
  :text "
By [[eqref:Euler]].
#+begin_math
e^{i\\pi} + 1 = 0   :tag-teal: Euler
#+end_math"
  (funcall (org-link-get-parameter "eqref" :follow) "Euler" nil)
  (should (looking-at-p "e\\^{i")))

;;; In Emacs

(org-deftest "a math block shows each COLOUR:ATOM in its colour, as the maths reads it, and its tags as align's" [fontify]
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

(provide 'org-math-block-tests)
;;; org-math-block-tests.el ends here
