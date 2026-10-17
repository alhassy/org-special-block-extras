;;; org-align-block-tests.el --- Tests for the align block and its eqref links  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(define-org-call-test "an align block's :tag-COLOUR: NAME is a coloured \tag, labelled NAME; the last line's \\ is dropped"
  :call (org-block/align 'latex "x &= x + 0 \\\\
x + 0 &= 0 + x   :tag-blue: Symmetry" nil)
  :returns "#+begin_export latex
\\begin{align*}
x &= x + 0 \\\\
x + 0 &= 0 + x   \\tag{$\\color{blue}{\\text{Symmetry}}$} \\label{Symmetry}
\\end{align*}
#+end_export")

(define-org-call-test "an align block that is :numbered t is LaTeX's align, sans a trailing \\ that would number an empty line"
  :call (org-block/align 'latex "a &= b \\\\
c &= d \\\\
" nil :numbered t)
  :returns "#+begin_export latex
\\begin{align}
a &= b \\\\
c &= d
\\end{align}
#+end_export")

(define-org-call-test "an align block escapes & and < in HTML, which MathJax then reads as LaTeX"
  :call (org-block/align 'html "0 &< 1" nil)
  :returns "#+begin_export html
\\begin{align*}
0 &amp;&lt; 1
\\end{align*}
#+end_export")

(define-org-export-test "an align block takes shorthands from :let, and eqref:NAME refers to its line tagged NAME"
  :to latex
  :input "
          #+begin_align :let (-> \"\\\\Rightarrow\" == \"\\\\equiv\")
          p -> q &== \\neg p \\lor q   :tag-blue: Implication
          #+end_align
          By [[eqref:Implication]], p -> p."
  :equals "\\begin{align*}
p \\Rightarrow q &\\equiv \\neg p \\lor q   \\tag{$\\color{blue}{\\text{Implication}}$} \\label{Implication}
\\end{align*}
By \\eqref{Implication}, p -> p.
")

(org-deftest "following eqref:NAME visits the line tagged :tag-COLOUR: NAME, else a \\tag{…NAME…}"
  :text "
By [[eqref:Golden Rule]] and [[eqref:Leibniz]].
#+begin_align
p \\land q &\\equiv p \\equiv q \\equiv p \\lor q   :tag-orange: Golden Rule
#+end_align
\\begin{equation} x = y \\Rightarrow f(x) = f(y) \\tag{Leibniz} \\end{equation}"
  (funcall (org-link-get-parameter "eqref" :follow) "Golden Rule" nil)
  (should (looking-at-p "p \\\\land q"))
  (funcall (org-link-get-parameter "eqref" :follow) "Leibniz" nil)
  (should (looking-at-p "\\\\begin{equation}"))
  (should-error (funcall (org-link-get-parameter "eqref" :follow) "Nowhere" nil) :type 'user-error))

(provide 'org-align-block-tests)
;;; org-align-block-tests.el ends here
