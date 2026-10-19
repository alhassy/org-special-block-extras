;;; org-substitution-blocks-tests.el --- Tests for the stutter block  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(define-org-call-test "stutter block repeats the content the given number of times."
  :call (org-block/stutter 'html "ha " "3")
  :returns "ha
#+begin_export html

#+end_export
ha
#+begin_export html

#+end_export
ha
#+begin_export html

#+end_export")

(provide 'org-substitution-blocks-tests)
;;; org-substitution-blocks-tests.el ends here
