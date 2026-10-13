;;; org-details-block-tests.el --- Tests for the details block  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'cl-lib)

(load-file (expand-file-name "~/org-special-block-extras/org-defblock.el"))
(load-file (expand-file-name "~/org-special-block-extras/org-details-block.el"))

;; (ert-run-tests-interactively "^org-defblock-details")

(ert-deftest org-defblock-details-html-default-title ()
  "details block uses \"Details\" as the default title in HTML."
  (let ((result (org-block/details 'html "body text" nil)))
    (should (string-match-p "<details" result))
    (should (string-match-p "Details" result))
    (should (string-match-p "body text" result))))

(ert-deftest org-defblock-details-html-custom-title ()
  "details block renders a custom title in HTML."
  (let ((result (org-block/details 'html "content" "Spoiler")))
    (should (string-match-p "Spoiler" result))
    (should (string-match-p "<details" result))))

(ert-deftest org-defblock-details-html-background-color ()
  "details block passes background-color through to HTML."
  (let ((result (org-block/details 'html "content" nil :background-color "#ff0000")))
    (should (string-match-p "#ff0000" result))))

(ert-deftest org-defblock-details-latex-tcolorbox ()
  "details block produces a tcolorbox for LaTeX."
  (let ((result (org-block/details 'latex "content" nil)))
    (should (string-match-p "tcolorbox" result))
    (should (string-match-p "content" result))))

(ert-deftest org-defblock-details-latex-custom-title ()
  "details block renders a custom title in LaTeX."
  (let ((result (org-block/details 'latex "content" "My Title")))
    (should (string-match-p "My Title" result))))

(provide 'org-details-block-tests)
;;; org-details-block-tests.el ends here
