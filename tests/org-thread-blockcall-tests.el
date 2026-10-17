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

(deftest "org-thread-blockcall reuses rename within an align block, as raw LaTeX that Org leaves be; issue #23."
  (using ((org-defblock test-align () "Aligned equations." (format "\\begin{align*}\n%s\n\\end{align*}" raw-contents))
          (org-defblock test-goftex (shorthands "-> to \\Rightarrow") "Gofer as LaTeX equations."
            (org-thread-blockcall raw-contents (test-align) (rename shorthands))))
    (should (equal (s-trim (org-export-string "
#+begin_test-goftex \"== to \\\\equiv, -> to \\\\Rightarrow\"
p -> p &== TT
#+end_test-goftex" 'latex))
                   "\\begin{align*}
p \\Rightarrow p &\\equiv TT
\\end{align*}"))))
