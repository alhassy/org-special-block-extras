;;; org-spoiler-block-tests.el --- Tests for the spoiler block  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(deftest "spoiler block wraps hidden text in a styled <span> for HTML."
  (let ((result (org-block/spoiler 'html "The answer is ((42))." nil)))
    (should (string-match-p "span" result))
    (should (string-match-p "42" result))))

(define-org-call-test "spoiler block produces \\fbox{\\phantom{…}} for LaTeX."
  :call (org-block/spoiler 'latex "Answer: ((x = 5))." nil)
  :returns "Answer: @@latex:\\fbox{\\phantom{x = 5}}\\footnote{x = 5}@@.
#+begin_export latex

#+end_export")

(provide 'org-spoiler-block-tests)
;;; org-spoiler-block-tests.el ends here
