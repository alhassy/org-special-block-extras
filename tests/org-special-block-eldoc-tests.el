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

(deftest "eldoc on a #+begin_ or #+end_ line shows the block's signature, its docstring's first line, and its keys"
  (let ((text "#+begin_details Hi\nbody\n#+end_details\n"))
    (dolist (needle '("#+begin_details" "#+end_details"))
      (should (equal (org-test--eldoc-at text needle)
                     (list "Enclose contents in a folded up box, for HTML.
C-c '    Edit the block in `org-mode'; C-c ' again writes it back.
Docs · See the manual"
                           :thing (org-block-signature "details")
                           :face 'font-lock-function-name-face))))))

(deftest "eldoc's keys are those the block declares: math's C-c C-c previews it, and its C-c ' edits in latex-mode"
  (should (equal (car (org-test--eldoc-at "#+begin_math\nx\n#+end_math\n" "#+begin_math"))
                 "Display maths, wherein our links ---e.g., red:x, [[doc:fold][f]]--- and Org's *markup* are LaTeX's.
C-c C-c  Show the maths as an image atop the block; again, to remove it.  See `org--math-preview'.
C-c '    Edit the block in `latex-mode'; C-c ' again writes it back.
Docs · See the manual")))

(deftest "eldoc shows a block's :hover prose beneath its gist, and closes with buttons to its Docs and :manual"
  (using (org-defblock shy (:hover "Some prose too long for one line." :manual "https://example.com/shy") "Be shy." contents)
    (let ((doc (car (org-test--eldoc-at "#+begin_shy\nhi\n#+end_shy\n" "#+begin_shy"))))
      (should (equal doc "Be shy.
Some prose too long for one line.
C-c '    Edit the block in `org-mode'; C-c ' again writes it back.
Docs · See the manual"))
      ;; “Docs” describes the block's function; “See the manual” browses its :manual.
      (let ((push (lambda (label) (button-activate (button-at (1+ (string-match label doc))))))
            described browsed)
        (cl-letf (((symbol-function 'describe-function) (lambda (f) (setq described f)))
                  ((symbol-function 'browse-url) (lambda (url &rest _) (setq browsed url))))
          (with-temp-buffer
            (insert doc)
            (funcall push "Docs")
            (funcall push "See the manual")))
        (should (eq described 'org-block/shy))
        (should (equal browsed "https://example.com/shy"))))))

(deftest "a :manual of #ANCHOR is that section of our manual; and every block and link we ship has a :manual"
  (should (string-suffix-p "org-special-block-extras-blocks-and-links#math"
                           (get-text-property 0 'help-echo (car (last (split-string (org--hover-footer 'org-block/math)
                                                                                    " · "))))))
  (let ((ours (expand-file-name "lisp/" "~/org-special-block-extras/")))
    (should-not
     (cl-loop for symbol in (append (--map (intern (format "org-block/%s" it)) org--supported-blocks)
                                    (--map (intern (format "org-link/%s" (car it))) org-link-parameters))
              when (and (fboundp symbol)
                        (string-prefix-p ours (or (symbol-file symbol 'defun) ""))
                        ;; A block's link shares its block's manual.
                        (not (get (intern (string-replace "org-link/" "org-block/" (symbol-name symbol))) 'org-manual))
                        (not (get symbol 'org-manual)))
              collect symbol))))

(deftest "eldoc stays quiet inside a block's body, so typing there is not interrupted"
  (should-not (org-test--eldoc-at "#+begin_details Hi\nbody\n#+end_details\n" "body")))

(deftest "eldoc on a link shows its block's signature, else its `org-deflink' docstring"
  (should (equal (car (org-test--eldoc-at "Some red:hello text" "red:")) "Show text in red color.\nDocs · See the manual"))
  (should (equal (plist-get (cdr (org-test--eldoc-at "Some red:hello text" "red:")) :thing) "#+begin_red"))
  (should (equal (plist-get (cdr (org-test--eldoc-at "See kbd:C-x here" "kbd:")) :thing) "kbd:"))
  ;; A link that does something when followed says so.
  (should (equal (car (org-test--eldoc-at "By [[eqref:Fold]]." "eqref:"))
                 "Refer to the equation tagged NAME; e.g., [[eqref:Golden Rule]] for an align block's “:tag-orange: Golden Rule”.
C-c C-o  Follow it.
Docs · See the manual"))
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

(org-deftest "on a #+begin_ line, completion offers our blocks, each with its gist, and Org's own beside them" [completion]
  :text "#+begin_ti"
  :at "ti"
  (-let (((beg end table . props) (org--special-block-capf)))
    (should (equal (buffer-substring beg end) "ti"))
    (should (member "tip" (all-completions "ti" table)))
    (should (member "src" (all-completions "s" table)))
    (should (equal (funcall (plist-get props :annotation-function) "tip")
                   "  Helpful advice for doing things better or more easily."))
    (should (equal (funcall (plist-get props :annotation-function) "src") "  Org's own"))))

(org-deftest "elsewhere, completion of block names stays out of the way" [completion]
  :text "Some prose."
  :at "prose"
  (should-not (org--special-block-capf)))
