;;; org-deflink-tests.el --- Tests for `org-deflink' -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'cl-lib)

(load-file (expand-file-name "~/org-special-block-extras/org-defblock.el"))
(load-file (expand-file-name "~/org-special-block-extras/org-deflink.el"))

;; (ert-run-tests-interactively "^org-deflink")

(ert-deftest org-deflink-basic-functionality ()
  "Test that a simple org-deflink creates an export function."
  (org-deflink shout
    "Capitalize text"
    [:help-echo "SHOUT!"]
    (upcase (or o-description o-label)))
  (should (fboundp 'org-link/shout))
  (should (equal (org-link/shout "hi" nil 'html) "HI"))
  (should (equal (org-link/shout "ho" "yo" 'html) "YO")))

(ert-deftest org-deflink-help-echo-default ()
  "Ensure help-echo falls back to default when not explicitly provided."
  (org-deflink whisper "Lowercase fallback")
  (let ((res (org-element-interpret-data
              '(link (:type "whisper" :path "HELLO")))))
    (should (string-match "HELLO" res))))

(ert-deftest org-deflink-with-let-binding ()
  "Ensure local bindings work in both display and body."
  (org-deflink boldwrap
    "Wrap with <b> tag"
    [:let (wraptag "<b>")
     :help-echo "Boldify!"]
    (format "%s%s%s" wraptag (or o-description o-label) "</b>"))
  (should (equal (org-link/boldwrap "yes" nil 'html) "<b>yes</b>")))

(ert-deftest org-deflink-custom-face ()
  "Test that a custom face is evaluated and returns a plist."
  (org-deflink greenlink
    "Green text"
    [:face '(:foreground "green")]
    (format "<span style=\"color:green\">%s</span>" (or o-description o-label)))
  (let* ((face-fn (plist-get (cdr (assoc "greenlink" org-link-parameters)) :face)))
    (should (equal (funcall face-fn "green-text") '(:foreground "green")))))

(ert-deftest org-deflink-follow-action ()
  "Test that :follow executes correctly and can use prefix arg."
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

(ert-deftest org-deflink-keymap-handling ()
  "Test that keys are bound to the keymap properly."
  (setq org-deflink-test--keymap-actions nil)
  (org-deflink keymaplink
    "Test keys"
    [:keymap ("x" (push 'x org-deflink-test--keymap-actions))]
    (format "no-op"))
  (let* ((map (plist-get (cdr (assoc "keymaplink" org-link-parameters)) :keymap)))
    (should (keymapp map))
    (funcall (lookup-key map (kbd "x")))
    (should (equal org-deflink-test--keymap-actions '(x)))))

(provide 'org-deflink-tests)
;;; org-deflink-tests.el ends here
