;;; org-defblock-tests.el --- Tests for org-defblock core infrastructure  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(deftest "A freshly defined block appears in org--supported-blocks."
  (let ((org--supported-blocks nil))
    (org-defblock test-basic-reg () "")
    (should (member 'test-basic-reg org--supported-blocks))))

(deftest "org-defblock creates an org-block/NAME function."
  (org-defblock test-handler-fn () "handler body"
    (upcase contents))
  (should (fboundp 'org-block/test-handler-fn)))

(define-org-call-test "Block handler output contains the HTML markup and the content."
  :call (using (org-defblock test-html-export () ""
                 (format "<b>%s</b>" contents))
          (org-block/test-html-export 'html "hello" nil))
  :returns "#+begin_export html
<b>
#+end_export
hello
#+begin_export html
</b>
#+end_export")

(define-org-call-test "Block handler output contains the LaTeX markup and the content."
  :call (using (org-defblock test-latex-export () ""
                 (format "\\textbf{%s}" contents))
          (org-block/test-latex-export 'latex "world" nil))
  :returns "#+begin_export latex
\\textbf{
#+end_export
world
#+begin_export latex
}
#+end_export")

(define-org-call-test "Keyword args bind correctly inside the block body."
  :call (using (org-defblock test-kwdargs (main-arg "" color "red") ""
                 (format "%s:%s" color contents))
          (org-block/test-kwdargs 'html "text" nil :color "blue"))
  :returns "#+begin_export html
blue:
#+end_export
text
#+begin_export html

#+end_export")

(define-org-call-test "Main arg uses its default when omitted."
  :call (using (org-defblock test-main-default (label "DEFAULT") ""
                 label)
          (org-block/test-main-default 'html "" nil))
  :returns "#+begin_export html
DEFAULT
#+end_export")

(deftest "org-defblock-only does NOT create an org-link/NAME function."
  (org-defblock-only test-only-block () ""
    contents)
  (should (fboundp 'org-block/test-only-block))
  (should-not (fboundp 'org-link/test-only-block)))

(deftest "org-undefblock removes a block from org--supported-blocks."
  (let ((org--supported-blocks nil))
    (org-defblock test-undefblock-target () "" contents)
    (should (member 'test-undefblock-target org--supported-blocks))
    (org-undefblock test-undefblock-target)
    (should-not (member 'test-undefblock-target org--supported-blocks))))

(deftest "org-set-block-header-args overrides default when no local value is passed."
  (org-defblock test-header-args-block (main-arg "" color "black") ""
    color)
  (let ((org--header-args nil))
    (org-set-block-header-args test-header-args-block :color "purple")
    (let ((result (org-block/test-header-args-block 'html "" nil)))
      (should (string-match-p "purple" result)))))

(deftest "Parser extracts name, main-arg, and contents from a temp buffer."
  (with-temp-buffer
    (insert "#+begin_myblock hello\nsome content\n#+end_myblock\n")
    (goto-char (point-min))
    (let ((blk (org-special-block-after-point "myblock")))
      (should (equal (org-special-block-name blk) "myblock"))
      (should (equal (org-special-block-main-arg blk) "hello"))
      (should (string-match-p "some content" (org-special-block-contents blk))))))

(deftest "org-thread-blockcall with no forms returns the body unchanged."
  (should (equal "hello" (org-thread-blockcall "hello"))))

(provide 'org-defblock-tests)
;;; org-defblock-tests.el ends here
