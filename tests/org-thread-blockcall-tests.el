;;; org-thread-blockcall-tests.el --- Tests for threading text through blocks  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(deftest "org-thread-blockcall with no forms returns the body unchanged."
  (should (equal "hello" (org-thread-blockcall "hello"))))

(deftest "org-thread-blockcall nests its blocks and leaks no export markers, as composing the handlers would."
  (using (org-defblock test-threaded (title "T") "A box within details." (org-thread-blockcall raw-contents (box "B") (details title)))
    (let ((html (org-export-string "
#+begin_test-threaded
*hello*
#+end_test-threaded" 'html)))
      (should (string-match-p (rx "<details" (* anything) "<h3>B</h3>" (* anything) "<b>hello</b>" (* anything) "</div>" (* anything) "</details>") html))
      (should-not (string-match-p "export" html)))))
