;;; org-parallel-block-tests.el --- Tests for the parallel block  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'cl-lib)

(load-file (expand-file-name "~/org-special-block-extras/org-defblock.el"))
(load-file (expand-file-name "~/org-special-block-extras/org-parallel-block.el"))

;; (ert-run-tests-interactively "^org-defblock-layout")

(ert-deftest org-defblock-layout-parallel-html-two-col ()
  "parallel block produces a multi-column CSS div for HTML."
  (let ((result (org-block/parallel 'html "col1\n#+columnbreak:\ncol2" "2")))
    (should (string-match-p "column-count" result))
    (should (string-match-p "col1" result))
    (should (string-match-p "col2" result))))

(ert-deftest org-defblock-layout-parallel-html-with-bar ()
  "parallel block includes a column rule when :bar is set."
  (let ((result (org-block/parallel 'html "text" "2" :bar "solid")))
    (should (string-match-p "solid" result))))

(ert-deftest org-defblock-layout-parallel-html-no-bar ()
  "parallel block sets none for column rule when :bar is nil."
  (let ((result (org-block/parallel 'html "text" "2" :bar nil)))
    (should (string-match-p "none" result))))

(ert-deftest org-defblock-layout-parallel-latex-multicols ()
  "parallel block uses the multicols environment for LaTeX."
  (let ((result (org-block/parallel 'latex "text" "2")))
    (should (string-match-p "multicols" result))
    (should (string-match-p "text" result))))

(ert-deftest org-defblock-layout-parallel-latex-columnbreak ()
  "parallel block converts #+columnbreak: to \\columnbreak in LaTeX."
  (let ((result (org-block/parallel 'latex "a\n#+columnbreak:\nb" "2")))
    (should (string-match-p "\\\\columnbreak" result))))

(provide 'org-parallel-block-tests)
;;; org-parallel-block-tests.el ends here
