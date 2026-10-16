;;; org-special-block-edit-tests.el --- C-c ' edits any special block's contents in an Org buffer  -*- lexical-binding: t; -*-

(unless (fboundp 'org-deftest)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(org-deftest "C-c ' edits any special block's contents in an Org buffer, even one no org-defblock declares" [edit]
  :text "
#+begin_aside Mind
One *two*.
#+end_aside"
  :at "One"
  (org-edit-special)
  (should (eq major-mode 'org-mode))
  (should (equal (buffer-string) "One *two*.\n"))
  (goto-char (point-max))
  (insert "Three.\n")
  (org-edit-src-exit)
  (should (equal (buffer-string) "
#+begin_aside Mind
One *two*.
Three.
#+end_aside")))

(org-deftest "C-c ' edits an empty special block, giving it a line to write in" [edit]
  :text "
#+begin_aside
#+end_aside"
  :at "#+begin"
  (org-edit-special)
  (insert "Hi")
  (org-edit-src-exit)
  (should (equal (buffer-string) "
#+begin_aside
Hi
#+end_aside")))

(org-deftest "C-c ' in a src block within a special block edits the src block, as Org does" [edit]
  :text "
#+begin_box
#+begin_src emacs-lisp
(+ 1 2)
#+end_src
#+end_box"
  :at "(+ 1"
  (org-edit-special)
  (unwind-protect (should (eq major-mode 'emacs-lisp-mode))
    (org-edit-src-abort)))

(org-deftest "outside any special block, C-c ' is Org's as ever, erring where there is nothing to edit" [edit]
  :text "Plain prose."
  (should-error (org-edit-special) :type 'user-error))

(provide 'org-special-block-edit-tests)
;;; org-special-block-edit-tests.el ends here
