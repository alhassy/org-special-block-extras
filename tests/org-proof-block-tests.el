;;; org-proof-block-tests.el --- Tests for calc and tree proof blocks  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(define-org-call-test "calc block produces an align* LaTeX environment."
  :call (org-block/calc 'latex "- x\n- = y\n" nil)
  :returns "#+begin_export latex
$$\\begin{align*} & \\begin{split}x\\end{split} 
\\\\\\begin{split}= y\\end{split} 
 
\\end{align*}$$
#+end_export")

(define-org-call-test "calc block produces MathJax-compatible output for HTML."
  :call (org-block/calc 'html "- x\n- = y\n" nil)
  :returns "#+begin_export html
$$\\begin{align*} & \\begin{split}x\\end{split} 
\\\\\\begin{split}= y\\end{split} 
 
\\end{align*}$$
#+end_export")

(deftest "calc block signals a user-error when the content is not a valid list."
  (should-error
   (org-block/calc 'latex "not a list at all" nil)
   :type 'user-error))

(define-org-call-test "tree block produces \\frac for a simple rule."
  :call (org-block/tree 'latex "+ Trans :: P = Q\n  - P = P\n    + ✓\n" nil)
  :returns "#+begin_export latex
\\[\\frac{\\displaystyle  \\qquad \\frac{\\displaystyle  \\qquad ✓}{P = P}}{P = Q}[\\text{Trans}]\\]
#+end_export")

(define-org-call-test "tree block wraps each tree in \\[…\\]."
  :call (org-block/tree 'latex "+ A\n  - B\n" nil)
  :returns "#+begin_export latex
\\[\\frac{\\displaystyle  \\qquad B}{A}\\]
#+end_export")

(deftest "tree block signals a user-error when the content is not a valid list."
  (should-error
   (org-block/tree 'latex "no bullets here" nil)
   :type 'user-error))

(deftest "org--list-to-math returns a leaf node as-is."
  (should (equal "A = B" (org--list-to-math '("A = B" nil)))))

(define-org-call-test "org--list-to-math wraps premiss/conclusion in \\frac."
  :call (org--list-to-math '("R :: C = D" (("P = Q" nil))))
  :returns "\\frac{\\displaystyle P = Q}{C = D}[\\text{R}]")

(provide 'org-proof-block-tests)
;;; org-proof-block-tests.el ends here
