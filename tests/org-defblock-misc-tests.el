;;; org-defblock-misc-tests.el --- Tests for miscellaneous special blocks  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'cl-lib)

(load-file (expand-file-name "~/org-special-block-extras/org-defblock.el"))
(load-file (expand-file-name "~/org-special-block-extras/org-links.el"))
(load-file (expand-file-name "~/org-special-block-extras/org-box-block.el"))
(load-file (expand-file-name "~/org-special-block-extras/org-details-block.el"))
(load-file (expand-file-name "~/org-special-block-extras/org-defblock-misc.el"))

;; (ert-run-tests-interactively "^org-defblock-misc")

(ert-deftest org-defblock-misc-tooltip-html ()
  "tooltip block produces an <abbr> tooltip in HTML."
  (let ((result (org-block/tooltip 'html "tip text" "°")))
    (should (string-match-p "<abbr" result))
    (should (string-match-p "tooltip" result))
    (should (string-match-p "tip text" result))))

(ert-deftest org-defblock-misc-latex-definitions-html-hidden ()
  "latex-definitions block hides content in HTML."
  (let ((result (org-block/latex-definitions 'html "\\newcommand{\\foo}{bar}" nil)))
    (should (string-match-p "display:none" result))))

(ert-deftest org-defblock-misc-latex-definitions-latex-passthrough ()
  "latex-definitions block passes content through in LaTeX."
  (let ((result (org-block/latex-definitions 'latex "\\newcommand{\\foo}{bar}" nil)))
    (should (string-match-p "\\\\newcommand" result))))

(ert-deftest org-defblock-misc-stutter-reps ()
  "stutter block repeats the content the given number of times."
  (let ((result (org-block/stutter 'html "ha " "3")))
    (should (string-match-p "ha " result))
    (should (= 3 (length (s-match-strings-all "ha " result))))))

(ert-deftest org-defblock-misc-rename-substitution ()
  "rename block performs comma-separated 'to' substitutions."
  (let ((result (org-block/rename 'html "Hello World" "Hello to Hi, World to Earth")))
    (should (string-match-p "Hi" result))
    (should (string-match-p "Earth" result))))

(ert-deftest org-defblock-misc-spoiler-html-hides ()
  "spoiler block wraps hidden text in a styled <span> for HTML."
  (let ((result (org-block/spoiler 'html "The answer is ((42))." nil)))
    (should (string-match-p "span" result))
    (should (string-match-p "42" result))))

(ert-deftest org-defblock-misc-spoiler-latex-fbox ()
  "spoiler block produces \\fbox{\\phantom{…}} for LaTeX."
  (let ((result (org-block/spoiler 'latex "Answer: ((x = 5))." nil)))
    (should (string-match-p "fbox" result))
    (should (string-match-p "footnote" result))))

(ert-deftest org-defblock-misc-remark-visible ()
  "remark block renders content when org-hide-editor-comments is nil."
  (let ((org-hide-editor-comments nil)
        (result (org-block/remark 'html "This is a remark." nil)))
    (should (string-match-p "remark" (downcase result)))))

(ert-deftest org-defblock-misc-remark-hidden ()
  "remark block produces no visible content when org-hide-editor-comments is t."
  (let ((org-hide-editor-comments nil))
    (setq org-hide-editor-comments t)
    (let ((result (org-block/remark 'html "This is a remark." nil)))
      (setq org-hide-editor-comments nil)
      (should-not (string-match-p "This is a remark" result)))))

(ert-deftest org-defblock-misc-margin-html-abbr ()
  "margin block produces an <abbr> tooltip for HTML."
  (let ((result (org-block/margin 'html "note text" nil :marker "†" :counter "footnote")))
    (should (string-match-p "abbr" result))
    (should (string-match-p "note text" result))))

(ert-deftest org-defblock-misc-margin-latex-marginpar ()
  "margin block produces a \\marginpar for LaTeX."
  (let ((result (org-block/margin 'latex "note" nil :marker "†" :counter "footnote")))
    (should (string-match-p "marginpar" result))))

(provide 'org-defblock-misc-tests)
;;; org-defblock-misc-tests.el ends here
