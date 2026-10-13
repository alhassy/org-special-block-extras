;;; org-defblock-tests.el --- Tests for org-defblock core infrastructure  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'cl-lib)

(load-file (expand-file-name "~/org-special-block-extras/org-defblock.el"))

;; (ert-run-tests-interactively "^org-defblock")

(ert-deftest org-defblock-basic-registration ()
  "A freshly defined block appears in org--supported-blocks."
  (let ((org--supported-blocks nil))
    (org-defblock test-basic-reg () "")
    (should (member 'test-basic-reg org--supported-blocks))))

(ert-deftest org-defblock-produces-handler-function ()
  "org-defblock creates an org-block/NAME function."
  (org-defblock test-handler-fn () "handler body"
    (upcase contents))
  (should (fboundp 'org-block/test-handler-fn)))

(ert-deftest org-defblock-export-html ()
  "Block handler output contains the HTML markup and the content."
  (org-defblock test-html-export () ""
    (format "<b>%s</b>" contents))
  (let ((result (org-block/test-html-export 'html "hello" nil)))
    (should (string-match-p "<b>" result))
    (should (string-match-p "hello" result))
    (should (string-match-p "</b>" result))))

(ert-deftest org-defblock-export-latex ()
  "Block handler output contains the LaTeX markup and the content."
  (org-defblock test-latex-export () ""
    (format "\\textbf{%s}" contents))
  (let ((result (org-block/test-latex-export 'latex "world" nil)))
    (should (string-match-p "\\\\textbf{" result))
    (should (string-match-p "world" result))))

(ert-deftest org-defblock-kwdargs ()
  "Keyword args bind correctly inside the block body."
  (org-defblock test-kwdargs (main-arg "" color "red") ""
    (format "%s:%s" color contents))
  (let ((result (org-block/test-kwdargs 'html "text" nil :color "blue")))
    (should (string-match-p "blue:" result))
    (should (string-match-p "text" result))))

(ert-deftest org-defblock-main-arg-default ()
  "Main arg uses its default when omitted."
  (org-defblock test-main-default (label "DEFAULT") ""
    label)
  (let ((result (org-block/test-main-default 'html "" nil)))
    (should (string-match-p "DEFAULT" result))))

(ert-deftest org-defblock-only-no-link ()
  "org-defblock-only does NOT create an org-link/NAME function."
  (org-defblock-only test-only-block () ""
    contents)
  (should (fboundp 'org-block/test-only-block))
  (should-not (fboundp 'org-link/test-only-block)))

(ert-deftest org-undefblock-removes-support ()
  "org-undefblock removes a block from org--supported-blocks."
  (let ((org--supported-blocks nil))
    (org-defblock test-undefblock-target () "" contents)
    (should (member 'test-undefblock-target org--supported-blocks))
    (org-undefblock test-undefblock-target)
    (should-not (member 'test-undefblock-target org--supported-blocks))))

(ert-deftest org-set-block-header-args-applies ()
  "org-set-block-header-args overrides default when no local value is passed."
  (org-defblock test-header-args-block (main-arg "" color "black") ""
    color)
  (let ((org--header-args nil))
    (org-set-block-header-args test-header-args-block :color "purple")
    ;; Pass color as "" so the header-args fallback fires.
    (let ((result (org-block/test-header-args-block 'html "" nil :color "")))
      (should (string-match-p "purple" result)))))

(ert-deftest org-special-block-after-point-parses ()
  "Parser extracts name, main-arg, and contents from a temp buffer."
  (with-temp-buffer
    (insert "#+begin_myblock hello\nsome content\n#+end_myblock\n")
    (goto-char (point-min))
    (let ((blk (org-special-block-after-point "myblock")))
      (should (equal (org-special-block-name blk) "myblock"))
      (should (equal (org-special-block-main-arg blk) "hello"))
      (should (string-match-p "some content" (org-special-block-contents blk))))))

(ert-deftest org-thread-blockcall-identity ()
  "org-thread-blockcall with no forms returns the body unchanged."
  (should (equal "hello" (org-thread-blockcall "hello"))))

(provide 'org-defblock-tests)
;;; org-defblock-tests.el ends here
