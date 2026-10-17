;;; org-block-header-args-tests.el --- Tests for default block arguments, as src blocks' header-args  -*- lexical-binding: t; -*-

(unless (fboundp 'using)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(deftest "org-set-block-header-args overrides default when no local value is passed."
  (org-defblock test-header-args-block (main-arg "" color "black") ""
    color)
  (let ((org--header-args nil))
    (org-set-block-header-args test-header-args-block :color "purple")
    (let ((result (org-block/test-header-args-block 'html "" nil)))
      (should (string-match-p "purple" result)))))

(deftest "`org-defblock' uses default values set by `org-set-block-header-args'"
  (using (org-defblock greeting (name "user" punct "!") "Greeting block"
                (format "Hello, %s%s" name punct))
  (org-set-block-header-args greeting :main-arg "dev" :punct "!×4")
  (should (string= (org-block/greeting 'test-backend "some content" nil :punct nil)
                   (lf-string "#+begin_export test-backend
                               Hello, dev!×4
                               #+end_export")))))

(deftest "`org-defblock-only' uses default values set by `org-set-block-header-args'"
  (using (org-defblock-only greeting (name "user" punct "!") "Greeting block"
           (format "Hello, %s%s" name punct))
    (org-set-block-header-args greeting :main-arg "dev" :punct "!×4")
    (should (string= (org-block/greeting 'test-backend "some content" nil :punct nil)
                     (lf-string "#+begin_export test-backend
                               Hello, dev!×4
                               #+end_export")))))

(deftest "cl-defun-style org-defblock honours org-set-block-header-args"
  (using (org-defblock folded (&optional (title "More") &key (color "green"))
           "Fold CONTENTS under TITLE."
           (format "%s/%s: %s" title color contents))
    (let ((org--header-args nil))
      (org-set-block-header-args folded :color "teal")
      (should (s-contains-p "More/teal" (org-export-string "#+begin_folded\nHello\n#+end_folded" 'latex))))))

(provide 'org-block-header-args-tests)
;;; org-block-header-args-tests.el ends here
