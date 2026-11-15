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

(deftest "eldoc on a #+begin_ or #+end_ line shows the block's signature, gist, example, keys, and switches"
  (let ((text "
#+begin_details Hi
body
#+end_details"))
    (dolist (needle '("#+begin_details" "#+end_details"))
      (should (equal (org-test--eldoc-at text needle)
                     (list (string-join (append (list "Enclose contents in a folded up box, for HTML."
                                                      (concat "\n" (org--hover-example 'org--details-block) "\n")
                                                      "C-c '    Edit the block in `org-mode'; C-c ' again writes it back."
                                                      "C-c C-v C-d  Split the block in two at point; around the region, in three."
                                                      "C-c C-v C-j  Insert an argument, or a switch, completing its name, on the #+begin_ line.")
                                                (org--block-switch-lines "details")
                                                '("Docs · See the manual"))
                                        "\n")
                           :thing (org-block-signature "details")
                           :face 'font-lock-function-name-face))))))

(deftest "eldoc's keys are those the block declares: math's C-c C-c previews it, and its C-c ' edits in latex-mode"
  (let ((doc (car (org-test--eldoc-at "
#+begin_math
x
#+end_math" "#+begin_math"))))
    (should (s-contains-p "
C-c C-c  Show the maths as an image atop the block; again, to remove it.  See `org--math-preview'.
C-c '    Edit the block in `latex-mode'; C-c ' again writes it back.
C-c C-v C-d  Split the block in two at point; around the region, in three.
C-c C-v C-j  Insert an argument, or a switch, completing its name, on the #+begin_ line.
Switches, which every block understands:" doc))))

(deftest "eldoc lists the switches every block understands, save those a block shadows by a keyword of its own"
  (should (member "  :folded TITLE     The result within a `details' titled TITLE; for :folded t, the block's NAME, capitalised."
                  (org--block-switch-lines "details")))
  (should (member "  :C-c_C-c ACTION   What C-c C-c on the block runs, in Emacs: ACTION, a form or a command; export ignores it.  See `org--block-cc-switch'."
                  (org--block-switch-lines "details")))
  (using (org-defblock tucked (&key folded &context contents) "Fold, its own way." contents)
    (should-not (--first (string-prefix-p "  :folded " it) (org--block-switch-lines "tucked")))
    (should (--first (string-prefix-p "  :boxed " it) (org--block-switch-lines "tucked")))))

(deftest "eldoc shows a block's :hover prose beneath its gist, and closes with buttons to its Docs and :manual"
  (using (org-defblock shy (:hover "Some prose too long for one line." :manual "https://example.com/shy" &context contents) "Be shy." contents)
    (let ((doc (car (org-test--eldoc-at "
#+begin_shy
hi
#+end_shy" "#+begin_shy"))))
      (should (equal doc (string-join (append '("Be shy."
                                                "Some prose too long for one line."
                                                "C-c '    Edit the block in `org-mode'; C-c ' again writes it back."
                                                      "C-c C-v C-d  Split the block in two at point; around the region, in three."
                                                      "C-c C-v C-j  Insert an argument, or a switch, completing its name, on the #+begin_ line.")
                                              (org--block-switch-lines "shy")
                                              '("Docs · See the manual"))
                                      "\n")))
      ;; “Docs” describes the block's function; “See the manual” browses its :manual.
      (let ((push (lambda (label) (button-activate (button-at (1+ (string-match label doc))))))
            described browsed)
        (cl-letf (((symbol-function 'describe-function) (lambda (f) (setq described f)))
                  ((symbol-function 'browse-url) (lambda (url &rest _) (setq browsed url))))
          (with-temp-buffer
            (insert doc)
            (funcall push "Docs")
            (funcall push "See the manual")))
        (should (eq described 'org--shy-block))
        (should (equal browsed "https://example.com/shy"))))))

(deftest "a :manual of #ANCHOR is that section of our manual; and every block and link we ship has a :manual"
  (should (string-suffix-p "org-special-block-extras-blocks-and-links#math"
                           (get-text-property 0 'help-echo (car (last (split-string (org--hover-footer 'org--math-block)
                                                                                    " · "))))))
  (let ((ours (expand-file-name "lisp/" "~/org-special-block-extras/")))
    (should-not
     (cl-loop for symbol in (append (--map (intern (format "org--%s-block" it)) org--supported-blocks)
                                    (--map (intern (format "org--%s-link" (car it))) org-link-parameters))
              when (and (fboundp symbol)
                        (string-prefix-p ours (or (symbol-file symbol 'defun) ""))
                        ;; A block's link shares its block's manual.
                        (not (get (intern (concat (string-remove-suffix "-link" (symbol-name symbol)) "-block")) 'org-manual))
                        (not (get symbol 'org-manual)))
              collect symbol))))

(deftest "eldoc shows a block's :example as Org shows it, set apart by blank lines, its #-comments as comments"
  (using (org-defblock shy (&context contents) "Be shy." contents)
    (org-defblock-attribute shy :example "
# Bold words stay *bold*; links show their brackets.
#+begin_shy
Hush, *now*; see [[https://example.com][there]].
#+end_shy")
    (let* ((doc (car (org-test--eldoc-at "
#+begin_shy
hi
#+end_shy" "#+begin_shy")))
           (face-at (lambda (needle) (get-text-property (string-match (regexp-quote needle) doc) 'face doc))))
      (should (s-contains-p "Be shy.

# Bold words stay *bold*; links show their brackets.
#+begin_shy
Hush, *now*; see [[https://example.com][there]].
#+end_shy

C-c '" doc))
      (should (eq (funcall face-at "# Bold") 'font-lock-comment-face))
      (should (memq 'bold (ensure-list (funcall face-at "*now*"))))
      ;; Org hides a link's brackets; an example, which teaches the syntax, shows them.
      (should-not (text-property-not-all 0 (length doc) 'invisible nil doc)))))

(deftest "every block we ship has an :example, which exports to HTML sans error"
  (require 'org-graph-block)
  (should-not (--remove (get (intern (format "org--%s-block" it)) 'org-example) org--supported-blocks))
  (dolist (block org--supported-blocks)
    (should (stringp (org-export-string-as (get (intern (format "org--%s-block" block)) 'org-example) 'html t)))))

(deftest "eldoc stays quiet inside a block's body, so typing there is not interrupted"
  (should-not (org-test--eldoc-at "#+begin_details Hi\nbody\n#+end_details\n" "body")))

(deftest "eldoc on a link shows its block's signature, else its `org-deflink' docstring"
  ;; A colour's example shows its link too.
  (should (equal (car (org-test--eldoc-at "Some red:hello text" "red:"))
                 (format "Show text in red color.\n\n%s\n\nDocs · See the manual" (org--hover-example 'org--red-block))))
  (should (equal (plist-get (cdr (org-test--eldoc-at "Some red:hello text" "red:")) :thing) "#+begin_red"))
  (should (equal (plist-get (cdr (org-test--eldoc-at "See kbd:C-x here" "kbd:")) :thing) "kbd:"))
  ;; A link that does something when followed says so.
  (unwind-protect
      (progn (org-deflink test-follow (label) "Visit the page named LABEL." :follow (message "%s" label) :export label)
             (should (equal (car (org-test--eldoc-at "By [[test-follow:Fold]]." "test-follow:"))
                            "Visit the page named LABEL.\nC-c C-o  Follow it.\nDocs")))
    (setq org-link-parameters (assoc-delete-all "test-follow" org-link-parameters)))
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

(org-deftest "C-c C-v C-j within a special block appends an argument, and its value, to its #+begin_ line" [eldoc insert-arg]
  :text "
#+begin_details Hint
body
#+end_details"
  :at "body"
  (org-babel-insert-header-arg :title-color "red")
  (org-babel-insert-header-arg :folded "")
  (should (string-match-p "^#\\+begin_details Hint :title-color red :folded$" (buffer-string))))

(deftest "C-c C-v C-j offers a block's own keywords, with their defaults, then the switches it does not shadow" [eldoc insert-arg]
  (let ((candidates (org--block-arg-candidates "details")))
    (should (equal (assq :title-color candidates) '(:title-color . "green")))
    (should (assq :folded candidates))
    (should (assq :copy candidates))))
