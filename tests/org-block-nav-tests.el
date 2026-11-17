;;; org-block-nav-tests.el --- Tests for C-h, C-n, and C-p on a block's delimiters -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(org-deftest "C-h, C-n, and C-p act on a block from its #+begin_ and #+end_ lines, as they do on a link"
  :text "
#+begin_details Why?
Since the measure shrinks.
#+end_details"
  (font-lock-ensure)
  (dolist (at '("#+begin_details" "#+end_details"))
    (goto-char (point-min))
    (search-forward at)
    (backward-char)
    (should (eq (key-binding (kbd "C-h")) #'org-this-block-show-docs))
    (should (eq (key-binding (kbd "C-n")) #'org-this-block-next))
    (should (eq (key-binding (kbd "C-p")) #'org-this-block-previous))))

(org-deftest "C-h, C-n, and C-p keep their usual meaning within a block's contents, and after its name"
  :text "
#+begin_details Why?
Since the measure shrinks.
#+end_details"
  (font-lock-ensure)
  (dolist (at '("measure" "Why"))
    (goto-char (point-min))
    (search-forward at)
    (should-not (eq (key-binding (kbd "C-h")) #'org-this-block-show-docs))
    (should-not (eq (key-binding (kbd "C-n")) #'org-this-block-next))))

(org-deftest "org-this-block-next and org-this-block-previous visit blocks of the same name, skipping all others"
  :text "
#+begin_details One
#+end_details

#+begin_box Not me
#+end_box

#+begin_details Two
#+end_details"
  :at "begin_"
  (org-this-block-next)
  (should (looking-at "#\\+begin_details Two"))
  (org-this-block-previous)
  (should (looking-at "#\\+begin_details One")))

(org-deftest "org-this-block-show-docs shows the block's documentation, as C-h f does for its Lisp function"
  :text "
#+begin_details Why?
#+end_details"
  :at "begin_"
  (org-this-block-show-docs)
  (with-current-buffer "*Help: Org Block “details”*"
    (should (s-contains-p (documentation 'org--details-block) (buffer-string)))))

(org-deftest "org-this-block-next off a block's delimiters says so"
  :text "Just prose."
  (should-error (org-this-block-next) :type 'user-error))

(provide 'org-block-nav-tests)
;;; org-block-nav-tests.el ends here
