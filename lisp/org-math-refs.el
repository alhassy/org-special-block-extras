;;; org-math-refs.el --- Citing a math block's tagged line  -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.18
;; Package-Requires: ((dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A math block's line tagged “:tag-COLOUR: NAME”, or a block “#+name: NAME”, is cited by NAME; following the
;; citation in Emacs visits the line.  See `org--eqref-visit'.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'org-defblock)
(require 'org-math-structure)

;;;; Citations

(defun org--eqref-visit (name)
  "NAME ↦ point at the equation tagged NAME: a math block's “:tag-COLOUR: NAME” line, else the block “#+name: NAME”,
else a \\tag{…NAME…}."
  (let ((at (save-excursion
              (goto-char (point-min))
              (or (re-search-forward (format ":tag-[^: \t\n]+:[ \t]*%s[ \t]*$" (regexp-quote name)) nil t)
                  (re-search-forward (format "^[ \t]*#\\+[nN][aA][mM][eE]:[ \t]*%s[ \t]*$" (regexp-quote name)) nil t)
                  (re-search-forward (format "\\\\tag{[^}\n]*%s" (regexp-quote name)) nil t)))))
    (unless at (user-error "No equation is tagged “%s”" name))
    (push-mark)
    (goto-char at)
    (beginning-of-line)))

(org-deflink eqref
  "Refer to the equation tagged NAME; e.g., [[eqref:Golden Rule]] for a math block's “:tag-orange: Golden Rule”.
Following it visits that line, else any \\tag{…NAME…}; it exports as \\eqref{NAME}, which LaTeX and MathJax resolve."
  :follow (org--eqref-visit o-label)
  :manual "#math"
  :export (cond ((org-export-markdown-p) (format "(%s)" o-label))
                (t (when (equal o-backend 'html) (org-html-head-require 'mathjax))
                   (format "\\eqref{%s}" (org--math-label o-label)))))

(provide 'org-math-refs)
;;; org-math-refs.el ends here
