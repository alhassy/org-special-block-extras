;;; org-latex-definitions-block-tests.el --- Tests for the latex-definitions block  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(define-org-call-test "latex-definitions block hides content in HTML."
  :call (org-block/latex-definitions 'html "\\newcommand{\\foo}{bar}" nil)
  :returns "#+begin_export html
<p style=\"display:none\">\\[\\newcommand{\\foo}{bar}\\]</p>
#+end_export")

(define-org-call-test "latex-definitions block sends its content to the LaTeX preamble, as #+latex_header: lines."
  :call (org-block/latex-definitions 'latex "\\newcommand{\\foo}{bar}" nil)
  :returns "#+latex_header: \\newcommand{\\foo}{bar}
#+begin_export latex

#+end_export")

(provide 'org-latex-definitions-block-tests)
;;; org-latex-definitions-block-tests.el ends here
