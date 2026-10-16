;;; tree.el --- Snapshot of the `tree' showcase example  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

(define-org-example-test "tree"
  :input "#+begin_box Programming ≈ Proving
#+begin_tree
+ Function Application :: f(a) : B
  - a : A
  - f : A → B

+ Modus Ponens :: q
  - p
  - p ⇒ q
#+end_tree
#+end_box"
  :html "<div style=\"padding: 1em;background-color: #CCFFCC;border-radius: 15px;font-size: 0.9em;\">
<h3>Programming ≈ Proving</h3> 
\\[\\frac{\\displaystyle a : A \\qquad f : A → B
}{f(a) : B}[\\text{Function Application}]\\]\\[\\frac{\\displaystyle p \\qquad p ⇒ q}{q}[\\text{Modus
Ponens}]\\]
</div>"
  :latex "\\colorlet{osbe-box-back}{red!5!white}\\colorlet{osbe-box-frame}{red!75!black}
\\colorlet{osbe-box-title}{yellow!50!red}\\begin{tcolorbox}[title={Programming ≈ Proving},
colback=osbe-box-back,colframe=osbe-box-frame,colbacktitle=osbe-box-title,coltitle=red!25!black,
fonttitle=\\bfseries,subtitle style={boxrule=0.4pt, colback=yellow!50!red!25!white}]
\\[\\frac{\\displaystyle a : A \\qquad f : A → B
}{f(a) : B}[\\text{Function Application}]\\]\\[\\frac{\\displaystyle p \\qquad p ⇒ q}{q}[\\text{Modus
Ponens}]\\]
\\end{tcolorbox}")
