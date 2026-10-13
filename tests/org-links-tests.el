;;; org-links-tests.el --- Tests for built-in link types  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'cl-lib)

(load-file (expand-file-name "~/org-special-block-extras/org-defblock.el"))
(load-file (expand-file-name "~/org-special-block-extras/org-links.el"))

;; (ert-run-tests-interactively "^org-defblock-links")

(ert-deftest org-defblock-links-melpa-html ()
  "melpa link produces an <a> pointing to melpa.org."
  (let ((result (org-link/melpa "some-pkg" nil 'html)))
    (should (string-match-p "melpa.org" result))
    (should (string-match-p "<a href" result))
    (should (string-match-p "some-pkg" result))))

(ert-deftest org-defblock-links-kbd-html ()
  "kbd link wraps the label in <kbd> for HTML."
  (let ((result (org-link/kbd "C-x C-s" nil 'html)))
    (should (string-match-p "<kbd" result))
    (should (string-match-p "C-x C-s" result))))

(ert-deftest org-defblock-links-kbd-latex ()
  "kbd link uses \\texttt for LaTeX."
  (let ((result (org-link/kbd "C-x_C-s" nil 'latex)))
    (should (string-match-p "\\\\texttt" result))))

(ert-deftest org-defblock-links-badge-html ()
  "badge link produces a shields.io img tag for HTML."
  (let ((result (org-link/badge "key|value|blue" nil 'html)))
    (should (string-match-p "shields.io" result))
    (should (string-match-p "<img" result))))

(ert-deftest org-defblock-links-badge-latex-empty ()
  "badge link returns empty string for LaTeX."
  (let ((result (org-link/badge "key|value|blue" nil 'latex)))
    (should (equal result ""))))

(ert-deftest org-defblock-links-doc-html-tooltip ()
  "doc link wraps the label in an <abbr> tooltip for HTML."
  (let ((org--docs nil)
        (org--docs-from-libraries nil)
        (org--docs-actually-used nil))
    (org-docs-set "testword" "Test Word" "A made-up glossary entry.")
    (let ((result (org-link/doc "testword" nil 'html)))
      (should (string-match-p "<abbr" result))
      (should (string-match-p "Test Word" result)))))

(ert-deftest org-defblock-links-ospe-whitespace ()
  "org-ospe-html-export-preserving-whitespace replaces newlines with <br>."
  (let ((result (org-ospe-html-export-preserving-whitespace "line1\nline2")))
    (should (string-match-p "<br>" result))))

(ert-deftest org-defblock-links-show-expr ()
  "show link evaluates and returns the expression's value."
  (let ((result (org-link/show "(+ 1 2)" nil 'html)))
    (should (string-match-p "3" result))))

(provide 'org-links-tests)
;;; org-links-tests.el ends here
