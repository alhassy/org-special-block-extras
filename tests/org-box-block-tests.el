;;; org-box-block-tests.el --- Tests for the box block and org-subtle-colors  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'cl-lib)

(load-file (expand-file-name "~/org-special-block-extras/org-defblock.el"))
(load-file (expand-file-name "~/org-special-block-extras/org-box-block.el"))

;; (ert-run-tests-interactively "^org-defblock-box")

(ert-deftest org-defblock-box-html-basic ()
  "box block wraps content in a styled <div> for HTML."
  (let ((result (org-block/box 'html "inner text" nil)))
    (should (string-match-p "<div" result))
    (should (string-match-p "inner text" result))))

(ert-deftest org-defblock-box-html-with-title ()
  "box block renders the title in an <h3> for HTML."
  (let ((result (org-block/box 'html "content" "My Box")))
    (should (string-match-p "My Box" result))
    (should (string-match-p "<h3>" result))))

(ert-deftest org-defblock-box-html-background-color ()
  "box block passes background-color to the HTML div style."
  (let ((result (org-block/box 'html "text" nil :background-color "blue")))
    (should (string-match-p "background-color" result))))

(ert-deftest org-defblock-box-latex-tcolorbox ()
  "box block uses tcolorbox for LaTeX."
  (let ((result (org-block/box 'latex "content" nil)))
    (should (string-match-p "tcolorbox" result))
    (should (string-match-p "content" result))))

(ert-deftest org-defblock-box-latex-with-title ()
  "box block passes the title to tcolorbox in LaTeX."
  (let ((result (org-block/box 'latex "content" "Title")))
    (should (string-match-p "Title" result))))

(ert-deftest org-subtle-colors-known-name ()
  "org-subtle-colors maps known names to hex codes."
  (should (equal "#CCFFCC" (org-subtle-colors "green")))
  (should (equal "#CCFFFF" (org-subtle-colors "blue")))
  (should (equal "#FFFF99" (org-subtle-colors "yellow"))))

(ert-deftest org-subtle-colors-passthrough ()
  "org-subtle-colors passes unknown values through unchanged."
  (should (equal "#1a2b3c" (org-subtle-colors "#1a2b3c"))))

(provide 'org-box-block-tests)
;;; org-box-block-tests.el ends here
