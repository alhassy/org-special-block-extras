;;; org-proof-block.el --- Calculational and tree proof blocks  -*- lexical-binding: t; -*-

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

;; Mathematical proof blocks: `calc' for calculational/equational proofs
;; and `tree' for natural-deduction inference trees.
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
;;;;;;; Calculational style proofs

(defun org--list-to-calc (item rel hint-format NL-length color)
  "Render ITEM, from `org-list-to-lisp', and its nested items as one step of a calculational proof.

ITEM's line is “[OP] EXPRESSION -- HINT”, each part optional; both the expression
and the hint may span multiple lines.

REL is the default relation in the left-most column.

HINT-FORMAT is the formatting string for hints; e.g.,
\"\\color{maroon}{\\langle\\large\\substack{\\text{ %s }}⟩}\"

NL-length is how long the explicit vertical space, \\, should be.
The number refers to the vspace occupied nearly by the height of
a single normal sized letter.

COLOR is the colour of the hints."
  (-let* (((line children) item)               ;; CHILDREN is (unordered ITEM…), or nil.
          ((expr₀ hint) (s-split "--" line))
          ((op₀ expr₁) (cdr (s-match "^\\[\\(.*\\)\\]\\(.*\\)" expr₀)))
          (op (or op₀ rel))
          (expr (or expr₁ expr₀))
          (steps (--map (org--list-to-calc it rel hint-format NL-length color) (cdr children)))
          ;; Hints keep their line breaks; a blank line becomes NL-length vertical space.
          (hint (s-replace "\n" " } \\hfill\\\\\n\\text{ "
                           (s-replace "\n\n" (s-repeat (* 6 NL-length) "\n $\\,$") (s-trim (or hint ""))))))
    (if (and (equal hint "") (null steps))
        (format "\\begin{aligned}%s\\end{aligned} \n" expr)
      (format "\n %s \\;\\; & \\qquad \\color{%s}{%s} \n \\\\ & \\begin{aligned}%s\\end{aligned}"
              op
              color
              ;; The hfill is so that we do not use substack's default centering, but instead left-align
              ;; justification hints; nested steps appear beneath their hint.
              ;; HINT-FORMAT's %s is replaced verbatim, never via `format', so its other characters ---say, %--- stay literal.
              (s-replace "%s"
                         (if steps
                             (format "{\\large\\substack{\\text{ %s } \\hfill\\\\ \\begin{aligned} & %s \n\\end{aligned}\\hfill\\\\\n}}"
                                     hint (s-join "\\\\" steps))
                           (format "{\\large\\substack{\\text{ %s } \\hfill\\\\\n}}" hint))
                         hint-format)
              expr))))

(org-defblock calc
              (main-arg nil
                        rel "=" hint-format "\\left[ %s \\right." explicit-vspace 2 color "#800000"
                        :latex-head ("xcolor") :html-head (mathjax) :manual "#Aligned-equations")
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
              ;; LaTeX's \\color takes a name, so a hex COLOR, such as the default maroon, is named first.
              (concat
               (if (equal backend 'latex) (org--latex-define-color "osbe-calc" color) "")
               (thread-last (cdr (org--parse-plain-list raw-contents "calc"))
                           (--map (org--list-to-calc it rel hint-format explicit-vspace
                                                     (if (equal backend 'latex) "osbe-calc" color)))
                           (s-join "\\\\")
                           (format "\\begin{align*} & %s \n\\end{align*}")
                           ;; A blank line, say between a hint's paragraphs, would end the paragraph mid-align*.
                           (replace-regexp-in-string "\n[ \t]*\n" "\n")
                           ;; align* is a display already; MathJax, in HTML, needs it within $$ delimiters.
                           (format (if (equal backend 'latex) "%s" "$$%s$$")))))

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
