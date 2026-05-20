;;; org-color-links-tests.el --- Tests for color blocks  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'cl-lib)

(load-file (expand-file-name "~/org-special-block-extras/org-defblock.el"))
(load-file (expand-file-name "~/org-special-block-extras/org-color-links.el"))

;; (ert-run-tests-interactively "^org-defblock-colors")

(ert-deftest org-defblock-colors-red-html ()
  "red block wraps text in a red <span> for HTML."
  (let ((result (org-block/red 'html "hello" nil)))
    (should (string-match-p "color:red" result))
    (should (string-match-p "<span" result))
    (should (string-match-p "hello" result))))

(ert-deftest org-defblock-colors-blue-latex ()
  "blue block uses \\begingroup\\color{blue} for LaTeX."
  (let ((result (org-block/blue 'latex "world" nil)))
    (should (string-match-p "\\\\color{blue}" result))
    (should (string-match-p "world" result))))

(ert-deftest org-defblock-colors-green-html ()
  "green block wraps text in a green <span> for HTML."
  (let ((result (org-block/green 'html "text" nil)))
    (should (string-match-p "color:green" result))))

(ert-deftest org-defblock-colors-generic-color-html ()
  "generic color block respects the color main-arg in HTML."
  (let ((result (org-block/color 'html "text" "purple")))
    (should (string-match-p "color:purple" result))
    (should (string-match-p "text" result))))

(ert-deftest org-defblock-colors-generic-color-latex ()
  "generic color block uses \\color for LaTeX."
  (let ((result (org-block/color 'latex "text" "orange")))
    (should (string-match-p "\\\\color{orange}" result))))

(ert-deftest org-defblock-colors-all-20-defined ()
  "All 20 named colour blocks are registered as handler functions."
  (dolist (colour '(black blue brown cyan darkgray gray green lightgray lime
                    magenta olive orange pink purple red teal violet white yellow))
    (should (fboundp (intern (format "org-block/%s" colour))))))

(provide 'org-color-links-tests)
;;; org-color-links-tests.el ends here
