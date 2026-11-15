;;; org-defblock-options-tests.el --- `org-defblock''s :block, :link, :tag options and `org-defblock-links-and-tags'  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(defun org-test--made-by (name)
  "NAME ↦ which of a block, link, and tag `org-defblock' made for NAME; e.g., (:block t :link t :tag nil)."
  (list :block (and (memq name org--supported-blocks) t)
        :link  (fboundp (intern (format "org-link/%s" name)))
        :tag   (fboundp (intern (format "org-deftag/%s" name)))))

;;; Which of block, link, and tag get made

(define-org-call-test "org-defblock makes a block, a link, and a tag by default"
  :call (using (org-defblock opt-all (&optional x) "Echo X." x)
          (org-test--made-by 'opt-all))
  :returns (:block t :link t :tag t))

(define-org-call-test "org-defblock declines just the tag with :tag nil"
  :call (using (org-defblock opt-notag (&optional x :tag nil) "Echo X." x)
          (org-test--made-by 'opt-notag))
  :returns (:block t :link t :tag nil))

(define-org-call-test "org-defblock declines just the link with :link nil"
  :call (using (org-defblock opt-nolink (&optional x :link nil) "Echo X." x)
          (org-test--made-by 'opt-nolink))
  :returns (:block t :link nil :tag t))

(define-org-call-test "org-defblock makes only a link and a tag with :block nil"
  :call (using (org-defblock opt-noblock (&optional x :block nil) "Echo X." x)
          (org-test--made-by 'opt-noblock))
  :returns (:block nil :link t :tag t))

;;; The global default

(deftest "with org-defblock-links-and-tags nil, org-defblock makes only a block"
  (let ((org-defblock-links-and-tags nil))
    (using (org-defblock opt-off (&optional x) "Echo X." x)
      (should (equal (org-test--made-by 'opt-off) '(:block t :link nil :tag nil))))))

(deftest "a block's own :link and :tag override org-defblock-links-and-tags"
  (let ((org-defblock-links-and-tags nil))
    (using (org-defblock opt-override (&optional x :tag t) "Echo X." x)
      (should (equal (org-test--made-by 'opt-override) '(:block t :link nil :tag t))))))

;;; With :block nil, the link and tag still work, through the block's handler

(define-org-export-test "with :block nil, #+begin_NAME is left to Org"
  :input "#+begin_opt-handler T
          body
          #+end_opt-handler"
  :to latex
  :using (org-defblock opt-handler (&optional x :block nil &context contents) "Show X and CONTENTS." (format "[%s|%s]" x contents))
  :equals "\\begin{opt-handler}
body
\\end{opt-handler}
")

(define-org-export-test "with :block nil, the link still works"
  :input "[[opt-handler:T][body]]"
  :to latex
  :using (org-defblock opt-handler (&optional x :block nil &context contents) "Show X and CONTENTS." (format "[%s|%s]" x contents))
  :equals "[T|body]
")

(define-org-export-test "with :block nil, the tag still works"
  :input "* Heading :opt-handler_T:
          body"
  :to latex
  :using (org-defblock opt-handler (&optional x :block nil &context contents) "Show X and CONTENTS." (format "[%s|%s]" x contents))
  :equals "\\section{Heading}
\\label{sec:org-id-1}
[T|
body
]
")

(deftest "org-defblock's :link [DISPLAY] gives the link that display, with or without arguments"
  (using ((org-defblock opt-face-cl (&optional x :link [:face '(:foreground "red")]) "Echo X." x)
          (org-defblock opt-face-none (:link [:face '(:foreground "red")] &context contents) "Echo." contents))
    (dolist (name '("opt-face-cl" "opt-face-none"))
      (should (equal (funcall (org-link-get-parameter name :face) "label") '(:foreground "red"))))))

(deftest "org-defblock rejects the retired display vector after KWDS, naming the :link option instead"
  (should (string-match-p ":link \\[:face"
                          (cadr (should-error (macroexpand '(org-defblock opt-old (&context contents) [:face bold] "Doc." contents)))))))

;;; org-defblock-attribute ---an option set after the fact

(deftest "`org--defblock-set-option' replaces an option's value, else adds it; argument names are left be"
  (should (equal (org--defblock-set-option '(&optional title :manual "#a" &context contents) :manual "#b")
                 '(:manual "#b" &optional title &context contents)))
  (should (equal (org--defblock-set-option '(&key color :tag nil) :hover "Hi.")
                 '(:hover "Hi." &key color :tag nil))))

(deftest "`org-defblock-attribute' sets any option of a block after its definition; a later call keeps the earlier"
  (using (org-defblock late (&optional (who "you") &context contents) "Greet WHO." (format "Hi %s: %s" who contents))
    (org-defblock-attribute late :hover "Say hi.")
    (org-defblock-attribute late :manual "https://example.com/late")
    (should (equal (get 'org-block/late 'org-hover) "Say hi."))
    (should (equal (get 'org-block/late 'org-manual) "https://example.com/late"))
    ;; The block itself, its main argument's default and all, is as it was.
    (should (s-contains-p "Hi you: " (org-block/late 'html "there"))))
  (should (string-match-p "no org-defblock"
                          (cadr (should-error (org-defblock-attribute no-such-block :hover "Hi."))))))

(provide 'org-defblock-options-tests)
;;; org-defblock-options-tests.el ends here
