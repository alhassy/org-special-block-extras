;;; org-color-links-tests.el --- Tests for color blocks  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(define-org-call-test "red block wraps text in a red <span> for HTML."
  :call (org-block/red 'html "hello" nil)
  :returns "#+begin_export html
<span style=\"color:red;\">
#+end_export
hello
#+begin_export html
</span>
#+end_export")

(define-org-call-test "blue block uses \\begingroup\\color{blue} for LaTeX."
  :call (org-block/blue 'latex "world" nil)
  :returns "#+begin_export latex
\\begingroup\\color{blue}
#+end_export
world
#+begin_export latex
\\endgroup\\,
#+end_export")

(define-org-call-test "green block wraps text in a green <span> for HTML."
  :call (org-block/green 'html "text" nil)
  :returns "#+begin_export html
<span style=\"color:green;\">
#+end_export
text
#+begin_export html
</span>
#+end_export")

(define-org-call-test "generic color block respects the color main-arg in HTML."
  :call (org-block/color 'html "text" "purple")
  :returns "#+begin_export html
<span style=\"color:purple;\">
#+end_export
text
#+begin_export html
</span>
#+end_export")

(define-org-call-test "generic color block uses \\color for LaTeX."
  :call (org-block/color 'latex "text" "orange")
  :returns "#+begin_export latex
\\begingroup\\color{orange}
#+end_export
text
#+begin_export latex
\\endgroup\\,
#+end_export")

(deftest "All 19 named colour blocks are registered as handler functions."
  (dolist (colour '(black blue brown cyan darkgray gray green lightgray lime
                          magenta olive orange pink purple red teal violet white yellow))
    (should (fboundp (intern (format "org-block/%s" colour))))))

(provide 'org-color-links-tests)
;;; org-color-links-tests.el ends here
