;;; org-proof-block-tests.el --- Tests for calc and tree proof blocks  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'cl-lib)

(load-file (expand-file-name "~/org-special-block-extras/org-defblock.el"))
(load-file (expand-file-name "~/org-special-block-extras/org-proof-block.el"))

;; (ert-run-tests-interactively "^org-defblock-proof")

(ert-deftest org-defblock-proof-calc-align-env ()
  "calc block produces an align* LaTeX environment."
  (let ((result (org-block/calc 'latex "- x\n- = y\n" nil)))
    (should (string-match-p "align\\*" result))
    (should (string-match-p "\\$\\$" result))))

(ert-deftest org-defblock-proof-calc-html ()
  "calc block produces MathJax-compatible output for HTML."
  (let ((result (org-block/calc 'html "- x\n- = y\n" nil)))
    (should (string-match-p "\\$\\$" result))))

(ert-deftest org-defblock-proof-calc-invalid-list-error ()
  "calc block signals a user-error when the content is not a valid list."
  (should-error
   (org-block/calc 'latex "not a list at all" nil)
   :type 'user-error))

(ert-deftest org-defblock-proof-tree-frac ()
  "tree block produces \\frac for a simple rule."
  (let ((result (org-block/tree 'latex "+ Trans :: P = Q\n  - P = P\n    + ✓\n" nil)))
    (should (string-match-p "\\\\frac" result))
    (should (string-match-p "Trans" result))))

(ert-deftest org-defblock-proof-tree-wrapped-in-math ()
  "tree block wraps each tree in \\[…\\]."
  (let ((result (org-block/tree 'latex "+ A\n  - B\n" nil)))
    (should (string-match-p "\\\\\\[" result))
    (should (string-match-p "\\\\\\]" result))))

(ert-deftest org-defblock-proof-tree-invalid-list-error ()
  "tree block signals a user-error when the content is not a valid list."
  (should-error
   (org-block/tree 'latex "no bullets here" nil)
   :type 'user-error))

(ert-deftest org-defblock-proof-list-to-math-leaf ()
  "org--list-to-math returns a leaf node as-is."
  (should (equal "A = B" (org--list-to-math '("A = B" nil)))))

(ert-deftest org-defblock-proof-list-to-math-rule ()
  "org--list-to-math wraps premiss/conclusion in \\frac."
  (let ((result (org--list-to-math '("R :: C = D" (("P = Q" nil))))))
    (should (string-match-p "\\\\frac" result))
    (should (string-match-p "C = D" result))))

(provide 'org-proof-block-tests)
;;; org-proof-block-tests.el ends here
