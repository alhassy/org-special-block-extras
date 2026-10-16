;;; org-special-block-split-tests.el --- C-c C-v C-d splits a special block, as it does a src block  -*- lexical-binding: t; -*-

(unless (fboundp 'org-deftest)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(org-deftest "C-c C-v C-d at a line's start splits the special block there, each piece keeping its #+begin_ line" [split]
  :text "
#+begin_tip Mind :folded t
One.
Two.
#+end_tip"
  :at "One.\n"
  (org-babel-demarcate-block)
  (should (equal (buffer-string) "
#+begin_tip Mind :folded t
One.
#+end_tip

#+begin_tip Mind :folded t
Two.
#+end_tip"))
  (should (looking-at "Two.")))

(org-deftest "at a line's end, the split falls after that line; mid-line, it falls there, sans the space around it" [split]
  :text "
#+begin_note
One. Two.
#+end_note"
  :at "One."
  (org-special-block-split)
  (should (equal (buffer-string) "
#+begin_note
One.
#+end_note

#+begin_note
Two.
#+end_note"))
  (goto-char (point-min))
  (search-forward "#+end_note")
  (forward-line -1)
  (end-of-line)
  (org-special-block-split)
  (should (equal (buffer-string) "
#+begin_note
One.
#+end_note

#+begin_note
#+end_note

#+begin_note
Two.
#+end_note")))

(org-deftest "an active region splits the block in three, the region the middle piece; with C-u, headings between" [split]
  :text "
* Notes
#+begin_box
One.
Two.
Three.
#+end_box"
  :at "One.\n"
  (let ((transient-mark-mode t))
    (set-mark (point))
    (forward-line 1)
    (activate-mark)
    (org-babel-demarcate-block '(4)))
  ;; Sans trailing whitespace, which editors strip from this file: each heading is a star and a space.
  (should (equal (replace-regexp-in-string " +$" "" (buffer-string)) "
* Notes
#+begin_box
One.
#+end_box
*
#+begin_box
Two.
#+end_box
*
#+begin_box
Three.
#+end_box"))
  (should (looking-at "Two.")))

(org-deftest "in a src block within a special block, C-c C-v C-d splits the src block, as Org does" [split]
  :text "
#+begin_box
#+begin_src emacs-lisp
(+ 1 2)
#+end_src
#+end_box"
  :at "(+ 1"
  (should-not (org--special-block-demarcate))
  (should-error (org-special-block-split) :type 'user-error))

(provide 'org-special-block-split-tests)
;;; org-special-block-split-tests.el ends here
