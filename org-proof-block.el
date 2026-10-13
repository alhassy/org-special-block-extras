;;; org-proof-block.el --- Calculational and tree proof blocks  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 4.1.1
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "27.1") (org "9.1"))
;; Keywords: org, blocks, math, convenience
;; URL: https://alhassy.github.io/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Mathematical proof blocks: `calc' for calculational/equational proofs
;; and `tree' for natural-deduction inference trees.
;;
;; Full documentation at https://alhassy.github.io/org-special-block-extras

;;; Code:

(require 'org-defblock)

;;;;;; Mathematical Proofs
;;;;;;; Calculational style proofs

(defun org--list-to-calc (lst rel hint-format NL-length color)
  "Get a result from org-list-to-lisp and render it as a calculational proof.

LST is an expression, possibly with a hint and dedicated relation.
The expression may contain multiple lines, as may the hints.

REL is the default relation in the left-most column.

HINT_FORMAT is the formatting string for hints; e.g.,
\"\\color{maroon}{\\langle\\large\\substack{\\text{ %s }}⟩}\"

NL-length is how long the explicit vertical space, \\, should be.
The number refers to the vspace occupied nearly by the height of
a single normal sized letter.

COLOR is the colour of the hints."
  (cond
   ((symbolp lst) "")
   ((symbolp (car lst)) (org--list-to-calc (cadr lst)))
   (:otherwise (-let* (((conclusion₀ children) lst)
                 ((expr₀ hint) (s-split "--" conclusion₀))
                       ((op₀ expr₁) (cdr (s-match "^\\[\\(.*\\)\\]\\(.*\\)" expr₀)))
                       (op (or op₀ rel))
                       (expr (or expr₁ expr₀)))
                 (if (not children)
                     (if hint
                         (format
                          "\n %s \\;\\; & \\qquad \\color{%s}{%s} \n \\\\ & \\begin{split}%s\\end{split}"
                          op
                          color
                          ;; the hfill is so that we do not use substack's default
                          ;; centering, but instead left-align justificatiion
                          ;; hints.
                          (format (s-replace "%s" "{\\large\\substack{\\text{ %s } \\hfill\\\\\n}}" hint-format)
                                  (s-replace "\n" " } \\hfill\\\\\n\\text{ "
                                             (s-replace "\n\n" (s-repeat (* 6 NL-length) "\n $\\,$") (s-trim hint))))
                          expr)
                       (format "\\begin{split}%s\\end{split} \n" expr))
                   ;; MA: The following could be improved.
                   (format "\n %s \\;\\; & \\qquad \\color{%s}{%s} \n \\\\ & \\begin{split}%s\\end{split}"
                           op color
                           ;; BEGIN similar as above
                           (format (s-replace
                                    "%s"
                                    "{\\large\\substack{\\text{ %s } \\hfill\\\\ \\begin{split} & %s \n\\end{split}\\hfill\\\\\n}}"
                                    hint-format)
                                   (s-replace "\n" " } \\hfill\\\\\n\\text{ "
                                              (s-replace "\n\n" (s-repeat (* 6 NL-length) "\n $\\,$") (s-trim hint)))
                                   ;; END similar
                                   (s-chop-prefix
                                    "\\\\" (s-join
                                            "\\\\"
                                            (mapcar (lambda (it) (format "%s" (org--list-to-calc it rel hint-format NL-length color)))
                                                   children))))
                           expr))))))

(org-defblock calc
              (main-arg nil
                        rel "=" hint-format "\\left[ %s \\right." explicit-vspace 2 color "maroon")
              "Render an Org-list as an equational proof.

Sometimes the notation delimiting justification hints may clash
with the domain topic, so we can change the hint format, e.g., to
\"\\left\\langle %s \\right\\rangle\".  Set HINT-FORMAT to the
empty string, \"\", to comment-out all hints in the exported
version.

The hint is the text immediately after a “--”, if there are
multiple such delimiters only the first is shown; this can be
useful if we want to have multiple alternatives, say for extra
details in the source but not so much in the export.

Line breaks are taken literally in the hints, but not in the
math; where one uses \\\\.  For math with multiple lines, use ‘&’
as an alignment marker; otherwise math is right-justified.

For HTML, to use an TeX it must be enclosed in $, since that is
what is required by MathJaX."
              (let ((parsed (condition-case err
                                (with-temp-buffer
                                  (insert raw-contents)
                                  (goto-char (point-min))
                                  (org-list-to-lisp))
                              (error (user-error "#+begin_calc block does not contain a valid Org plain list.\n\
Each step must be a `- item` bullet.\nParsing error: %s" (error-message-string err))))))
                (thread-last (cdr parsed)
                             (mapcar (lambda (it) (format "%s" (org--list-to-calc it rel hint-format explicit-vspace color))))
                             (s-join "\\\\")
                             (format "$$\\begin{align*} & %s \n\\end{align*}$$"))))

;;;;;;; inference proof tree
(defun org--list-to-math (lst)
  "Get a result LST from ORG-LIST-TO-LISP and render it as a proof tree."
  (cond
   ((symbolp lst) "")
   ((symbolp (car lst)) (org--list-to-math (cadr lst)))
   (t
    (-let* (((conclusion₀ children) lst)
      ((name named?) (s-split " :: " conclusion₀))
            (conclusion (or named? conclusion₀)))
      (if (not children)
          (if named? (format "\\frac{}{%s}[%s]" conclusion name) conclusion)
        (format "\\frac{\\displaystyle %s}{%s}%s"
                (s-join " \\qquad "
                        (mapcar #'org--list-to-math children))
                conclusion
                (if named? (format "[\\text{%s}]" name) "")))))))

(org-defblock tree (main-arg)
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
  #+end_tree"
              (let ((parsed (condition-case err
                                (with-temp-buffer
                                  (insert raw-contents)
                                  (goto-char (point-min))
                                  (org-list-to-lisp))
                              (error (user-error "#+begin_tree block does not contain a valid Org plain list.\n\
Each step must be a `- item` bullet.\nParsing error: %s" (error-message-string err))))))
                (s-join "" (mapcar (lambda (it) (format "\\[%s\\]" (org--list-to-math it)))
                                   (cdr parsed)))))

(provide 'org-proof-block)
;;; org-defblock-proof.el ends here
