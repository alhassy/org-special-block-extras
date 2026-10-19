;;; org-defblock-cl-defun-style-tests.el --- `cl-defun'-style arglists for `org-defblock' and `org-defblock-only'  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

;;; `org--defblock-parse-args': a cl-defun-style arglist ↦ its parts, by name

(deftest "org--defblock-parse-args rejects a flattened arglist"
  (should-error (org--defblock-parse-args '(title "Details" color "green"))))

(define-org-call-test "org--defblock-parse-args reads an empty arglist as no arguments, and no other keys"
  :call (org--defblock-parse-args '())
  :returns (:main nil :keys nil :other-keys nil :context nil :options nil))

(define-org-call-test "org--defblock-parse-args reads a bare &optional as the main argument without default"
  :call (org--defblock-parse-args '(&optional title))
  :returns (:main (title nil) :keys nil :other-keys nil :context nil :options nil))

(define-org-call-test "org--defblock-parse-args reads an &optional (name default) pair"
  :call (org--defblock-parse-args '(&optional (title "Details")))
  :returns (:main (title "Details") :keys nil :other-keys nil :context nil :options nil))

(define-org-call-test "org--defblock-parse-args has no main argument when only &key is given"
  :call (org--defblock-parse-args '(&key (style "color:red") class))
  :returns (:main nil :keys ((style "color:red") (class nil)) :other-keys nil :context nil :options nil))

(define-org-call-test "org--defblock-parse-args reads &optional and &key together"
  :call (org--defblock-parse-args '(&optional (title "Details") &key (color "green") style))
  :returns (:main (title "Details") :keys ((color "green") (style nil)) :other-keys nil :context nil :options nil))

(define-org-call-test "org--defblock-parse-args reads &allow-other-keys as :other-keys t"
  :call (org--defblock-parse-args '(&optional title &key color &allow-other-keys))
  :returns (:main (title nil) :keys ((color nil)) :other-keys t :context nil :options nil))

(define-org-call-test "org--defblock-parse-args gathers options such as :edit-indirect-mode, wherever they are"
  :call (org--defblock-parse-args '(&optional (kind "directed") :manual "#x" &key file :edit-indirect-mode org-mode))
  :returns (:main (kind "directed") :keys ((file nil)) :other-keys nil :context nil
            :options (:manual "#x" :edit-indirect-mode org-mode)))

(deftest "org--defblock-parse-args rejects arguments before &optional or &key"
  (should-error (org--defblock-parse-args '(backend &optional title))))

(deftest "org--defblock-parse-args rejects more than one &optional argument"
  (should-error (org--defblock-parse-args '(&optional title subtitle))))

;;; Blocks defined with cl-defun-style arglists

(define-org-export-test "cl-defun-style org-defblock uses its defaults when the block gives no arguments"
  :input "#+begin_folded
          Hello
          #+end_folded"
  :to latex
  :using (org-defblock folded (&optional (title "More") &key (color "green") &context contents)
           "Fold CONTENTS under TITLE."
           (format "%s/%s: %s" title color contents))
  :equals "More/green: 
Hello
")

(define-org-export-test "cl-defun-style org-defblock binds the main argument and keyword arguments"
  :input "#+begin_folded \"Click me\" :color red
          Hello
          #+end_folded"
  :to latex
  :using (org-defblock folded (&optional (title "More") &key (color "green") &context contents)
           "Fold CONTENTS under TITLE."
           (format "%s/%s: %s" title color contents))
  :equals "Click me/red: 
Hello
")

(define-org-export-test "cl-defun-style org-defblock with only &key arguments"
  :input "#+begin_tag :label NEW
          Hello
          #+end_tag"
  :to latex
  :using (org-defblock tag (&key (label "TODO") &context contents)
           "Prefix CONTENTS with LABEL."
           (format "[%s] %s" label contents))
  :equals "[NEW] 
Hello
")

(define-org-export-test "cl-defun-style org-defblock also defines a link"
  :input "folded:Hello"
  :to latex
  :using (org-defblock folded (&optional (title "More") &key (color "green") &context contents)
           "Fold CONTENTS under TITLE."
           (format "%s/%s: %s" title color contents))
  :equals "Hello/green: Hello
")

(define-org-call-test "cl-defun-style org-defblock-only handler binds main and keyword arguments"
  :call (using (org-defblock-only folded (&optional (title "More") &key style &context contents)
                 (format "%s|%s|%s" title style contents))
          (org-block/folded 'html "body" nil :style "bold"))
  :returns "#+begin_export html
More|bold|
#+end_export
body
#+begin_export html

#+end_export")

(deftest "cl-defun-style org-defblock refuses unknown header-line keywords, as cl-defun would"
  (using (org-defblock folded (&optional (title "More") &key (color "green") &context contents)
           (format "%s/%s: %s" title color contents))
    (should (equal (cadr (should-error (org-export-string "#+begin_folded :colour red\nHello\n#+end_folded" 'latex)))
                   "‘folded’ block: unknown header argument :colour; it takes :title :color"))))

(deftest "cl-defun-style org-defblock with &allow-other-keys ignores unknown header-line keywords"
  (using (org-defblock folded (&optional (title "More") &key (color "green") &allow-other-keys &context contents)
           (format "%s/%s: %s" title color contents))
    (should (s-contains-p "More/green" (org-export-string "#+begin_folded :colour red\nHello\n#+end_folded" 'latex)))))

(deftest "an org-defblock of no arguments refuses every header-line keyword, as cl-defun would"
  (using (org-defblock folded (&context contents) contents)
    (should-error (org-export-string "#+begin_folded :colour red\nHello\n#+end_folded" 'latex))))

;;; &context: what a body asks of the export, by name

(define-org-call-test "org--defblock-parse-args reads what follows &context as :context, an option after it too"
  :call (org--defblock-parse-args '(&optional title &key color &context backend ((:contents text)) :tag nil))
  :returns (:main (title nil) :keys ((color nil)) :other-keys nil :context (backend ((:contents text)))
            :options (:tag nil)))

(deftest "a body sees only what its &context asks for; a main argument named contents is just that"
  (using (org-defblock quip (&optional contents &context ((:raw-contents text)))
           (format "%s, said of %s" contents (s-trim text)))
    (should (s-contains-p "Ha!, said of Life" (org-export-string "#+begin_quip Ha!\nLife\n#+end_quip" 'latex)))))

(deftest "a body that asks for no backend sees none of ours: no anaphor captures its free variables"
  (using (org-defblock whose (&optional title) (format "%s/%s" title backend))
    (should (equal (should-error (org-export-string "#+begin_whose T\nx\n#+end_whose" 'latex))
                   '(void-variable backend)))))

(deftest "a &context item not on offer, or one named as an argument too, is refused as the block expands"
  (should (string-match-p "contnets.*not on offer"
                          (cadr (should-error (macroexpand-all '(org-defblock typo (&context contnets) contnets))))))
  (should (string-match-p "contents is both an argument and in &context"
                          (cadr (should-error (macroexpand-all '(org-defblock twice (&optional contents &context contents) contents)))))))

(provide 'org-defblock-cl-defun-style-tests)
;;; org-defblock-cl-defun-style-tests.el ends here
