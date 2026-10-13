;;; org-examples.el --- Org source ⟶ HTML & LaTeX snapshots, the relation shared by tests/examples/*.el  -*- lexical-binding: t; -*-

;; `C-u C-x C-e' on any `define-org-example-test' refreshes its `:html' and `:latex' snapshots,
;; and `osbe-example:NAME' in `org-special-block-extras.org' renders NAME's `:input' beside its `:html'.

(require 'ert)
(require 'org)
(require 'cl-lib)

(load-file (expand-file-name "~/snap/snap.el"))
(load-file (expand-file-name "~/org-special-block-extras/org-special-block-extras.el"))
(org-special-block-extras-mode 1)

(defun org-example--export (input backend)
  "(INPUT, BACKEND) ↦ body-only BACKEND export of Org INPUT, with `gensym' and machine-specific values pinned."
  (let ((seed 0)
        (user-full-name "Musa Al-hassy")
        (emacs-version "29.4"))
    (cl-letf (((symbol-function 'gensym) (lambda (&rest _) (format "g%s" (cl-incf seed)))))
      (s-trim (org-export-string-as (format "\n%s\n" input) backend t)))))

(defun org-example--normalise (s)
  "S ↦ S without whitespace, downcased."
  ;; ponytail: lets the old prettier/tidy-formatted goldens compare to raw exports; blind to whitespace-only regressions.
  (downcase (replace-regexp-in-string "[ \t\n\r]+" "" s)))

(snap-define-relation org-example (input html latex)
  "Org INPUT exports to HTML and LATEX, modulo whitespace and case; a nil expectation goes unchecked."
  (let ((html* (org-example--export input 'html))
        (latex* (org-example--export input 'latex)))
    (when html (should (equal (org-example--normalise html) (org-example--normalise html*))))
    (when latex (should (equal (org-example--normalise latex) (org-example--normalise latex*))))
    (list :html html* :latex latex*)))

(provide 'org-examples)
