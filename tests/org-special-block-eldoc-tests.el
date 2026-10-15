;;; org-special-block-eldoc-tests.el --- Hover documentation for blocks and links, via eldoc  -*- lexical-binding: t; -*-

(unless (fboundp 'using)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(defun org-test--eldoc-at (text needle)
  "(TEXT, NEEDLE) ↦ (DOC . PLIST) that `org--special-block-eldoc' reports with point on NEEDLE in Org TEXT; else nil."
  (with-org-test-buffer (:text text :at needle)
    (goto-char (match-beginning 0))
    (let (report)
      (org--special-block-eldoc (lambda (doc &rest plist) (setq report (cons doc plist))))
      report)))

(deftest "`org-block-signature' shows a block's main and keyword arguments with their defaults"
  (should (equal (org-block-signature "details")
                 "#+begin_details TITLE=\"Details\" :background-color \"#e5f5e5\" :title-color \"green\""))
  ;; Keywords without a default are shown bare; an anonymous main argument, not at all.
  (should (equal (org-block-signature "red") "#+begin_red"))
  (should (equal (org-block-signature "box")
                 "#+begin_box TITLE=\"\" :background-color :shadow :frame-color :title-background-color"))
  (should-not (org-block-signature "no-such-block")))

(deftest "eldoc on a #+begin_ or #+end_ line shows the block's signature and the first line of its docstring"
  (let ((text "#+begin_details Hi\nbody\n#+end_details\n"))
    (dolist (needle '("#+begin_details" "#+end_details"))
      (should (equal (org-test--eldoc-at text needle)
                     (list "Enclose contents in a folded up box, for HTML."
                           :thing (org-block-signature "details")
                           :face 'font-lock-function-name-face))))))

(deftest "eldoc stays quiet inside a block's body, so typing there is not interrupted"
  (should-not (org-test--eldoc-at "#+begin_details Hi\nbody\n#+end_details\n" "body")))

(deftest "eldoc on a link shows its block's signature, else its `org-deflink' docstring"
  (should (equal (car (org-test--eldoc-at "Some red:hello text" "red:")) "Show text in red color."))
  (should (equal (plist-get (cdr (org-test--eldoc-at "Some red:hello text" "red:")) :thing) "#+begin_red"))
  (should (equal (plist-get (cdr (org-test--eldoc-at "See kbd:C-x here" "kbd:")) :thing) "kbd:"))
  ;; Org's own links are not ours to document.
  (should-not (org-test--eldoc-at "See https://example.com here" "https:")))

(deftest "the mode installs the eldoc function, and disabling it uninstalls it"
  (unwind-protect
      (progn
        (org-special-block-extras-mode -1)
        (should-not (memq #'org--special-block-eldoc eldoc-documentation-functions))
        (org-special-block-extras-mode 1)
        (should (memq #'org--special-block-eldoc eldoc-documentation-functions)))
    (org-special-block-extras-mode 1)))
