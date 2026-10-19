;;; org-math-structure.el --- A math block's comments, matrices, cases, tags, and definitions  -*- lexical-binding: t; -*-

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

;; The lines of a math block that are Org's, not LaTeX's, and become LaTeX; by way of example,
;;
;;     # a comment, absent from the export
;;     \def\N{\mathbb{N}}               ↦  declared, in the preamble, but not shown
;;     | 1 | 0 |                         ↦  \begin{pmatrix} 1 & 0 \\ 0 & 1 \end{pmatrix}
;;     | 0 | 1 |
;;     - 1 :: n = 0                      ↦  \begin{cases} 1 & n = 0 \\ 0 & \text{otherwise} \end{cases}
;;     - 0 :: otherwise
;;     e^{iπ} = -1   :tag-teal: Euler    ↦  … \tag{$\color{teal}{\text{Euler}}$} \label{Euler}
;;
;; A tag's name may hold maths, as \meet or ⊑ do; its label is then a key that links take; see `org--math-label'.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'dash)
(require 'org-math-markup)

;;;; Comments, matrices, and cases

(defun org--math-uncomment (math)
  "MATH ↦ MATH sans its Org comment lines, “# …”, which TeX would read as a macro's parameter; and sans its tables'
formula lines, “#+TBLFM: …”, which are Org's, whereby a matrix may compute its entries."
  (let ((case-fold-search t))
    (replace-regexp-in-string "^[ \t]*#\\(?:[ \t].*\\|\\+tblfm:.*\\)?\\(?:\n\\|\\'\\)" "" math t t)))

(defun org--math-tables (math environment)
  "(MATH, ENVIRONMENT) ↦ MATH whose each Org table is an ENVIRONMENT, on one line, sans its rules;
e.g., “| a | b |⏎| c | d |” ↦ “\\begin{pmatrix} a & b \\\\ c & d \\end{pmatrix}”."
  (with-temp-buffer
    (insert math)
    (goto-char (point-min))
    (while (re-search-forward "^\\(?:[ \t]*|.*\\(?:\n\\|\\'\\)\\)+" nil t)
      (let ((start (match-beginning 0))
            (rows (--remove (string-match-p "^[ \t]*|-" it) (split-string (match-string 0) "\n" t))))
        (delete-region start (point))
        (insert (format "\\begin{%s} %s \\end{%1$s}\n" environment
                        (mapconcat (lambda (row) (mapconcat #'string-trim (split-string (string-trim row "[ \t]*|" "|?[ \t]*") "|")
                                                            " & "))
                                   rows " \\\\ ")))))
    (buffer-string)))

(defconst org--math-case-re "[ \t]*[-+][ \t]+\\(.*?\\)[ \t]+::\\(?:[ \t]+\\(.*?\\)\\)?[ \t]*"
  "A description item, “- VALUE :: CONDITION”, a case of `org--math-cases'; sans anchors, so lines of them may repeat.")

(defun org--math-cases (math)
  "MATH ↦ MATH whose each run of description items is a cases environment, on one line: each item's term a value, its
description that value's condition, as prose when it is only words, such as “otherwise”; e.g.,
“- 1 :: n = 0⏎- n · (n-1)! :: otherwise” ↦ “\\begin{cases} 1 & n = 0 \\\\ n · (n-1)! & \\text{otherwise} \\end{cases}”.
As an Org table is a matrix; a line that begins “- x” sans “ :: ” stays maths."
  (with-temp-buffer
    (insert math)
    (goto-char (point-min))
    (while (re-search-forward (concat "^\\(?:" org--math-case-re "\\(?:\n\\|\\'\\)\\)+") nil t)
      (let* ((start (match-beginning 0))
             (cases (--map (progn (string-match (concat "\\`" org--math-case-re "\\'") it)
                                  (let ((condition (or (match-string 2 it) "")))
                                    (format "%s & %s" (match-string 1 it)
                                            (if (string-match-p "\\`[a-zA-Z ,]*[a-zA-Z]\\{2,\\}[a-zA-Z ,]*\\'" condition)
                                                (format "\\text{%s}" condition)
                                              condition))))
                           (split-string (match-string 0) "\n" t))))
        (delete-region start (point))
        (insert (format "\\begin{cases} %s \\end{cases}\n" (string-join cases " \\\\ ")))))
    (buffer-string)))

;;;; Tags ---a line's coloured name, and the label citations reach

(defconst org--math-tag-re ":tag-\\([^: \t\n]+\\):[ \t]*\\(.*?\\)[ \t]*$"
  "A line's ending “:tag-COLOUR: NAME”.")

(defconst org--math-symbol-re "[Ͱ-Ͽ℀-⅏←-⇿∀-⋿⌀-⏿⟀-⟿⤀-⫿𝐀-𝟿]"
  "A character that is maths, not text: Greek, letterlike, an arrow, an operator, a technical or maths symbol, or a
maths alphanumeric, such as ⊑ or 𝒩; a LaTeX text font has none of these.")

(defun org--math-tag-text (name)
  "NAME ↦ NAME as text, for a tag, its maths within $…$, which text lacks: a \\command, a sub- or superscript, or a
symbol such as ⊑; e.g., “Distributivity of \\meet” ↦ “Distributivity of $\\meet$”.  MathJax too reads $…$ in text."
  (replace-regexp-in-string
   (concat "\\(?:\\\\[[:alpha:]]+\\(?:{[^{}]*}\\)*\\|[_^]\\(?:{[^{}]*}\\|[^{}]\\)\\|" org--math-symbol-re "\\)+")
   (lambda (maths) (format "$%s$" maths)) name t t))

(defun org--math-label (name)
  "NAME ↦ a label for it, which LaTeX's and MathJax's links alike take: each \\command as its name, and each character
other than an ASCII letter, digit, space, or -:.,()' as uXXXX; e.g., “⊑ of \\meet” ↦ “u2291 of meet”.  So a plain
name is its own label; an eqref: of NAME cites the same."
  (mapconcat (lambda (c) (if (string-match-p "[A-Za-z0-9 :.,()'-]" (string c)) (string c) (format "u%04x" c)))
             (replace-regexp-in-string "\\\\\\([[:alpha:]]+\\)" "\\1" name) ""))

(defun org--math-tags (math)
  "MATH ↦ MATH whose each line ending in “:tag-COLOUR: NAME” ends instead in NAME as a COLOUR \\tag, labelled NAME,
and \\\; e.g., “x :tag-blue: Id” ↦ “x \\tag{$\\color{blue}{\\text{Id}}$} \\label{Id} \\\\”."
  (replace-regexp-in-string org--math-tag-re
                            (lambda (line)
                              (string-match org--math-tag-re line)
                              (format "\\tag{$\\color{%s}{\\text{%s}}$} \\label{%s} \\\\" (match-string 1 line)
                                      (org--math-tag-text (match-string 2 line)) (org--math-label (match-string 2 line))))
                            math t t))

(defun org--math-tag-face ()
  "↦ The face of the “:tag-COLOUR:” just matched: a tag, in COLOUR when Emacs knows that colour."
  (if (color-defined-p (match-string 1)) (list :inherit 'org-tag :foreground (match-string 1)) 'org-tag))

;;;; Definitions, and the marks that set a display's environment

(defconst org--math-definition-re "^[ \t]*\\\\\\(def\\|let\\|newcommand\\|renewcommand\\|DeclareMathOperator\\)\\b"
  "A line that declares a macro, rather than showing maths.")

(defun org--math-split-definitions (math)
  "MATH ↦ (DEFINITIONS . DISPLAY): each definition, from its \\def, \\newcommand, …, line through the line that
closes its braces; and the remaining lines, trimmed, sans blank ones."
  (with-temp-buffer
    (insert math)
    (goto-char (point-min))
    (let (definitions)
      (while (re-search-forward org--math-definition-re nil t)
        (let ((start (match-beginning 0)))
          (with-syntax-table org--math-syntax
            (end-of-line)
            (while (and (< 0 (car (parse-partial-sexp start (point)))) (zerop (forward-line 1)))
              (end-of-line)))
          (push (string-trim (buffer-substring start (point))) definitions)
          (delete-region start (min (1+ (point)) (point-max)))))
      (cons (nreverse definitions) (string-join (split-string (buffer-string) "\n" t "[ \t]+") "\n")))))

(defun org--math-top-level-marks (math)
  "MATH ↦ its &s, \\\\s, and {s, each (MARK . POSITION), outside every {group} and \\begin…\\end; e.g., “a &= \\cases{1 & 0}”
↦ ((\"&\" . 2) (\"{\" . 11)), as the cases' & is its own.  An escaped \\& is none."
  (let ((depth 0) (start 0) marks)
    (while (string-match "\\\\[[:alpha:]]+\\|\\\\.\\|[{}&]" math start)
      (setq start (match-end 0))
      (let ((mark (match-string 0 math)))
        (when (and (zerop depth) (member mark '("&" "\\\\" "{"))) (push (cons mark (match-beginning 0)) marks))
        (pcase mark
          ((or "{" "\\begin") (cl-incf depth))
          ((or "}" "\\end") (cl-decf depth)))))
    (nreverse marks)))

(defconst org--math-tagged-re "\\(\\\\tag{\\$\\\\color.*?\\$} \\\\label{[^}]*}\\)"
  "A tag, as `org--math-tags' writes it; its label, from `org--math-label', has no braces.")

(provide 'org-math-structure)
;;; org-math-structure.el ends here
