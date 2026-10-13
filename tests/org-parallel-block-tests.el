;;; org-parallel-block-tests.el --- Tests for the parallel block  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(define-org-call-test "parallel block produces a multi-column CSS div for HTML."
  :call (org-block/parallel 'html "col1\n#+columnbreak:\ncol2" "2")
  :returns "#+begin_export html
<div style=\"column-rule-style: none nil;column-count: 2;\">
#+end_export
col1
#+columnbreak:
col2
#+begin_export html
</div>
#+end_export")

(define-org-call-test "parallel block includes a column rule when :bar is set."
  :call (org-block/parallel 'html "text" "2" :bar "solid")
  :returns "#+begin_export html
<div style=\"column-rule-style: solid solid;column-count: 2;\">
#+end_export
text
#+begin_export html
</div>
#+end_export")

(define-org-call-test "parallel block sets none for column rule when :bar is nil."
  :call (org-block/parallel 'html "text" "2" :bar nil)
  :returns "#+begin_export html
<div style=\"column-rule-style: none nil;column-count: 2;\">
#+end_export
text
#+begin_export html
</div>
#+end_export")

(define-org-call-test "parallel block uses the multicols environment for LaTeX."
  :call (org-block/parallel 'latex "text" "2")
  :returns "#+begin_export latex
\\par \\setlength{\\columnseprule}{0 pt}
          \\begin{minipage}[t]{\\linewidth}
          \\begin{multicols}{2}
          
#+end_export
text
#+begin_export latex

          \\end{multicols}\\end{minipage}
#+end_export")

(define-org-call-test "parallel block converts #+columnbreak: to \\columnbreak in LaTeX."
  :call (org-block/parallel 'latex "a\n#+columnbreak:\nb" "2")
  :returns "#+begin_export latex
\\par \\setlength{\\columnseprule}{0 pt}
          \\begin{minipage}[t]{\\linewidth}
          \\begin{multicols}{2}
          
#+end_export
a
\\columnbreak
b
#+begin_export latex

          \\end{multicols}\\end{minipage}
#+end_export")

(provide 'org-parallel-block-tests)
;;; org-parallel-block-tests.el ends here
