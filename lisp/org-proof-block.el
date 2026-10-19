;;; org-proof-block.el --- Proof trees, as Org lists  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.18
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, math, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; The `tree' block draws a natural-deduction inference tree from an Org list.  Calculational proofs
;; are a math block's hints, “X ={ HINT } Y”; see `org-block/math'.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'org-defblock)

(defun org--parse-plain-list (string block)
  "(STRING, BLOCK) ↦ `org-list-to-lisp' of the Org plain list STRING; else a `user-error' naming the BLOCK."
  (condition-case err
      (with-temp-buffer
        (delay-mode-hooks (org-mode))
        (insert string)
        (goto-char (point-min))
        (org-list-to-lisp))
    (error (user-error "#+begin_%s block does not contain a valid Org plain list; each item must be a `-' or `+' bullet.\nParsing error: %s"
                       block (error-message-string err)))))

;;;;;; Mathematical Proofs
;;;;;;; inference proof tree
(defun org--list-to-math (item)
  "Render ITEM, from `org-list-to-lisp', as a proof tree: its line is “REASON :: CONCLUSION”, its nested items the premises."
  (-let* (((line children) item)               ;; CHILDREN is (unordered ITEM…), or nil.
          (parts (s-split-up-to " :: " line 1))
          ((name conclusion) (if (cdr parts) parts (list nil line)))
          (premises (-map #'org--list-to-math (cdr children))))
    (cond (premises (format "\\frac{\\displaystyle %s}{%s}%s"
                            (s-join " \\qquad " premises)
                            conclusion
                            (if name (format "[\\text{%s}]" name) "")))
          (name (format "\\frac{}{%s}[%s]" conclusion name))
          (t conclusion))))

(org-defblock tree (main-arg nil :html-head (mathjax) :manual "#Aligned-equations")
              "Write a proof tree using Org-lists.

To get

         premises₀  …   premisesₙ
       ────────────────────────────[ reason ]
               conclusion

You type

       #+begin_tree
       + reason :: conclusion
         - premises₀
         - premises₁
         ⋮
         - premisesₙ
       #+end_tree

Where each premisesᵢ may, recursively, also have named reasons
and (indented) child premises of its own.

If there are multiple trees, they are shown one after the other.

The text in this block should be considered LaTeX;
as such, Org markup is not recognised.

A proof tree, derivation, is then just a deeply nested
itemisation.  For instance, assuming P = Q(X), X = Y, Q(Y) = R,
the following proves P = R.

  #+begin_tree
  + Trans :: P = R
    - P = Q(X)
      + ✓
    - Trans :: Q(X) = R
      + Trans :: Q(X) = Q(Y)
        - Refl :: Q(X) = Q(X)
          + ✓
        - Leibniz :: Q(X) = Q(Y)
          + X = Y
            - ✓
      + Sym :: Q(Y) = R
        - R = Q(Y)
          - ✓
  #+end_tree

In Markdown, each tree is a $$…$$ display, since GitHub's maths knows no \\[…\\]."
              (-let [md? (org-export-markdown-p)]
                (mapconcat (lambda (it) (format (if md? "$$%s$$" "\\[%s\\]") (org--list-to-math it)))
                           (cdr (org--parse-plain-list raw-contents "tree"))
                           (if md? "\n\n" ""))))

(provide 'org-proof-block)
;;; org-proof-block.el ends here
