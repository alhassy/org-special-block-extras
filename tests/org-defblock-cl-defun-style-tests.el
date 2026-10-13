;;; org-defblock-cl-defun-style-tests.el --- `cl-defun'-style arglists for `org-defblock' and `org-defblock-only'  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

;;; `org--defblock-desugar-args': cl-defun-style ↦ flattened

(define-org-call-test "org--defblock-desugar-args leaves flattened arglists untouched"
  :call (org--defblock-desugar-args '(title "Details" color "green"))
  :returns (title "Details" color "green"))

(define-org-call-test "org--defblock-desugar-args leaves an empty arglist untouched"
  :call (org--defblock-desugar-args '())
  :returns nil)

(define-org-call-test "org--defblock-desugar-args reads a bare &optional as the main argument without default"
  :call (org--defblock-desugar-args '(&optional title))
  :returns (title nil))

(define-org-call-test "org--defblock-desugar-args reads an &optional (name default) pair"
  :call (org--defblock-desugar-args '(&optional (title "Details")))
  :returns (title "Details"))

(define-org-call-test "org--defblock-desugar-args names the main argument main-arg when only &key is given"
  :call (org--defblock-desugar-args '(&key (style "color:red") class))
  :returns (main-arg nil style "color:red" class nil))

(define-org-call-test "org--defblock-desugar-args reads &optional and &key together"
  :call (org--defblock-desugar-args '(&optional (title "Details") &key (color "green") style))
  :returns (title "Details" color "green" style nil))

(define-org-call-test "org--defblock-desugar-args passes options such as :edit-indirect-mode through"
  :call (org--defblock-desugar-args '(&optional (kind "directed") &key file :edit-indirect-mode org-mode))
  :returns (kind "directed" file nil :edit-indirect-mode org-mode))

(deftest "org--defblock-desugar-args rejects arguments before &optional or &key"
  (should-error (org--defblock-desugar-args '(backend &optional title))))

(deftest "org--defblock-desugar-args rejects more than one &optional argument"
  (should-error (org--defblock-desugar-args '(&optional title subtitle))))

;;; Blocks defined with cl-defun-style arglists

(define-org-export-test "cl-defun-style org-defblock uses its defaults when the block gives no arguments"
  :input "#+begin_folded
          Hello
          #+end_folded"
  :to latex
  :using (org-defblock folded (&optional (title "More") &key (color "green"))
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
  :using (org-defblock folded (&optional (title "More") &key (color "green"))
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
  :using (org-defblock tag (&key (label "TODO"))
           "Prefix CONTENTS with LABEL."
           (format "[%s] %s" label contents))
  :equals "[NEW] 
Hello
")

(define-org-export-test "cl-defun-style org-defblock also defines a link"
  :input "folded:Hello"
  :to latex
  :using (org-defblock folded (&optional (title "More") &key (color "green"))
           "Fold CONTENTS under TITLE."
           (format "%s/%s: %s" title color contents))
  :equals "Hello/green: Hello
")

(deftest "cl-defun-style org-defblock honours org-set-block-header-args"
  (using (org-defblock folded (&optional (title "More") &key (color "green"))
           "Fold CONTENTS under TITLE."
           (format "%s/%s: %s" title color contents))
    (let ((org--header-args nil))
      (org-set-block-header-args folded :color "teal")
      (should (s-contains-p "More/teal" (org-export-string "#+begin_folded\nHello\n#+end_folded" 'latex))))))

(deftest "cl-defun-style and flattened arglists define the same block"
  (let ((input "#+begin_folded \"Click me\"\nHello\n#+end_folded"))
    (should (equal (using (org-defblock folded (&optional (title "More") &key (color "green"))
                            (format "%s/%s: %s" title color contents))
                     (org-export-string input 'latex))
                   (using (org-defblock folded (title "More" color "green")
                            (format "%s/%s: %s" title color contents))
                     (org-export-string input 'latex))))))

(define-org-call-test "cl-defun-style org-defblock-only handler binds main and keyword arguments"
  :call (using (org-defblock-only folded (&optional (title "More") &key style)
                 (format "%s|%s|%s" title style contents))
          (org-block/folded 'html "body" nil :style "bold"))
  :returns "#+begin_export html
More|bold|
#+end_export
body
#+begin_export html

#+end_export")

(provide 'org-defblock-cl-defun-style-tests)
;;; org-defblock-cl-defun-style-tests.el ends here
