;;; org-examples.el --- Org source ⟶ HTML & LaTeX snapshots, the relation shared by tests/examples/*.el  -*- lexical-binding: t; -*-

;; `C-u C-x C-e' on any `define-org-example-test' refreshes its `:html' and `:latex' snapshots,
;; and `osbe-example:NAME' in `org-special-block-extras.org' renders NAME's `:input' beside its `:html'.
;; An `:html' of `:live' is never snapshotted, only exported when the docs render; e.g., the `graph'
;; block's HTML embeds a Graphviz-rendered image, which varies by machine and needs Graphviz installed.

(require 'ert)
(require 'org)
(require 'cl-lib)

(load-file (expand-file-name "~/snap/snap.el"))
(unless (fboundp 'org-test--fill) (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))
(load-file (expand-file-name "~/org-special-block-extras/org-special-block-extras.el"))
(add-to-list 'load-path (expand-file-name "~/org-special-block-extras/org-graph"))
(load-file (expand-file-name "~/org-special-block-extras/org-graph/org-graph-block.el"))
(org-special-block-extras-mode 1)
;; Examples run the Lisp of their own blocks' :when and :let switches without asking.
(setq org-defblock-evaluate-switches t)

(defun org-example--export (input backend)
  "(INPUT, BACKEND) ↦ body-only BACKEND export of Org INPUT, with `gensym', Org's ids, and machine-specific values pinned.
Headings export sans table of contents and section numbers, which would only distract from the example."
  (let ((seed 0)
        (org-html-htmlize-output-type nil)   ; htmlize's colours vary by machine, and CI has no htmlize.
        (user-full-name "Musa Al-hassy")
        (emacs-version "29.4")
        (org-export-with-toc nil)
        (org-export-with-section-numbers nil))
    (cl-letf (((symbol-function 'gensym) (lambda (&rest _) (format "g%s" (cl-incf seed))))
              ;; Org draws each heading's id at random; we number them instead, so snapshots are stable.
              ((symbol-function 'org-export-new-reference) (lambda (references) (1+ (length references)))))
      (s-trim (org-export-string-as (format "\n%s\n" input) backend t)))))

(defun org-example--normalise (s)
  "S ↦ S without whitespace, downcased."
  (downcase (replace-regexp-in-string "[ \t\n\r]+" "" s)))

(snap-define-relation org-example (input html latex)
  "Org INPUT exports to HTML and LATEX, modulo whitespace and case; a nil or `:live' expectation goes unchecked.
Snapshots are filled by `org-test--fill', so they are read without scrolling sideways."
  (let ((html* (if (eq html :live) :live (org-example--export input 'html)))
        (latex* (org-example--export input 'latex)))
    (when (stringp html) (should (equal (org-example--normalise html) (org-example--normalise html*))))
    (when latex (should (equal (org-example--normalise latex) (org-example--normalise latex*))))
    (list :html (if (stringp html*) (org-test--fill html*) html*) :latex (org-test--fill latex* t))))

(provide 'org-examples)
