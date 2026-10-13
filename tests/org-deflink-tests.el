;;; org-deflink-tests.el --- Tests for `org-deflink' -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(deftest "Test that a simple org-deflink creates an export function."
  (org-deflink shout
               "Capitalize text"
               [:help-echo "SHOUT!"]
               (upcase (or o-description o-label)))
  (should (fboundp 'org-link/shout))
  (should (equal (org-link/shout "hi" nil 'html) "HI"))
  (should (equal (org-link/shout "ho" "yo" 'html) "YO")))

(deftest "Ensure help-echo falls back to default when not explicitly provided."
  (org-deflink whisper "Lowercase fallback")
  (let ((res (org-element-interpret-data
              '(link (:type "whisper" :path "HELLO")))))
    (should (string-match "HELLO" res))))

(deftest "Ensure local bindings work in both display and body."
  (org-deflink boldwrap
               "Wrap with <b> tag"
               [:let (wraptag "<b>")
                     :help-echo "Boldify!"]
               (format "%s%s%s" wraptag (or o-description o-label) "</b>"))
  (should (equal (org-link/boldwrap "yes" nil 'html) "<b>yes</b>")))

(deftest "Test that a custom face is evaluated and returns a plist."
  (org-deflink greenlink
               "Green text"
               [:face '(:foreground "green")]
               (format "<span style=\"color:green\">%s</span>" (or o-description o-label)))
  (let* ((face-fn (plist-get (cdr (assoc "greenlink" org-link-parameters)) :face)))
    (should (equal (funcall face-fn "green-text") '(:foreground "green")))))

(deftest "Test that :follow executes correctly and can use prefix arg."
  (let ((followed nil))
    (org-deflink test-follow
                 "Follow test"
                 [:follow (setq followed o-label)]
                 (format "Ignored"))
    (let* ((follow-fn (plist-get (cdr (assoc "test-follow" org-link-parameters)) :follow)))
      (funcall follow-fn "payload" nil)
      (should (equal followed "payload")))))

(defvar org-deflink-test--keymap-actions nil
  "Scratch list populated by the keymaplink :keymap action in tests.")

(deftest "Test that keys are bound to the keymap properly."
  (setq org-deflink-test--keymap-actions nil)
  (org-deflink keymaplink
               "Test keys"
               [:keymap ("x" (push 'x org-deflink-test--keymap-actions))]
               (format "no-op"))
  (let* ((map (plist-get (cdr (assoc "keymaplink" org-link-parameters)) :keymap)))
    (should (keymapp map))
    (funcall (lookup-key map (kbd "x")))
    (should (equal org-deflink-test--keymap-actions '(x)))))

;;; Arglist style: naming the label, description, and backend

(org-deflink test-arg-shout (label &optional description)
  "Shout the description, else the label."
  :export (upcase (or description label)))

(org-deflink test-arg-whisper (label &optional (description "psst") backend)
  "Whisper the label, with a default description, naming the backend."
  :export (format "%s/%s/%s" (downcase label) description backend))

(org-deflink test-arg-face (label)
  "The arglist's names also reach the display forms, such as :face."
  :face (list :foreground label)
  :export label)

(define-org-call-test "org-deflink binds the arglist's first name to the label"
  :call (org-link/test-arg-shout "hi" nil 'html)
  :returns "HI")

(define-org-call-test "org-deflink binds the arglist's second name to the description"
  :call (org-link/test-arg-shout "hi" "yo" 'html)
  :returns "YO")

(define-org-call-test "org-deflink falls back to an arglist default, and binds the third name to the backend"
  :call (org-link/test-arg-whisper "HI" nil 'latex)
  :returns "hi/psst/latex")

(deftest "org-deflink arglist names reach the :face display form"
  (should (equal '(:foreground "green")
                 (funcall (plist-get (cdr (assoc "test-arg-face" org-link-parameters)) :face) "green"))))

(define-org-call-test "org--deflink-arglist-p recognises an arglist with &optional"
  :call (and (org--deflink-arglist-p '(label &optional description) '(:export label)) t)
  :returns t)

(define-org-call-test "org--deflink-arglist-p reads a one-form body as a body, not an arglist"
  :call (org--deflink-arglist-p '(upcase o-label) nil)
  :returns nil)

(define-org-call-test "org--deflink-arglist-p reads a function call as a body, even when more forms follow"
  :call (org--deflink-arglist-p '(identity o-label) '((upcase o-label)))
  :returns nil)

(provide 'org-deflink-tests)
;;; org-deflink-tests.el ends here
