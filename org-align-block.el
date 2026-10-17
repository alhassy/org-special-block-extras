;;; org-align-block.el --- Aligned equations with coloured tags, and eqref links to them  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.16
;; Package-Requires: ((emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; An align block lines up many equations at once: each line is an equation, and the & on each
;; line marks the column at which they all align.  By way of example,
;;
;;     #+begin_align :let (-> "\\Rightarrow" == "\\equiv")
;;     p -> q    &== \neg p \lor q             :tag-blue: Implication
;;     p \land q &== p == q == p \lor q        :tag-orange: Golden Rule
;;     #+end_align
;;     By [[eqref:Golden Rule]], …
;;
;; exports to LaTeX's align*: both equations hinge at their first ≡, one atop the other, as in a
;; textbook derivation; each is tagged ---“(Implication)” in blue, “(Golden Rule)” in orange---
;; and an eqref: link cites a tag by name.  In HTML, MathJax typesets it alike.  See
;; `org-block/align' and the `eqref' link.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'org-defblock)

(defun org--align-tag-face ()
  "↦ The face of the “:tag-COLOUR:” just matched: a tag, in COLOUR when Emacs knows that colour."
  (if (color-defined-p (match-string 1)) (list :inherit 'org-tag :foreground (match-string 1)) 'org-tag))

(org-defblock align (&key numbered :link nil :latex-head ("amsmath" "xcolor") :html-head (mathjax)
                          :fontify [:keywords '((":tag-\\([^: \t\n]+\\):" 0 org--align-tag-face)
                                                ;; Each & is where the equations align; an escaped \& is a mere ampersand.
                                                ("\\(?:^\\|[^\\]\\)\\(&\\)" 1 font-lock-keyword-face))])
              "Equations aligned at each &, numbered when NUMBERED; a line may end in a coloured tag, “:tag-blue: Name”.

  #+begin_align :let (-> \"\\\\Rightarrow\" == \"\\\\equiv\")
  p -> q &== \\neg p \\lor q          :tag-blue: Implication
  p \\land q &== p == q == p \\lor q  :tag-orange: Golden Rule
  #+end_align

Then [[eqref:Golden Rule]] refers to the second line, as (Golden Rule) in orange; following that
link in Emacs visits the line.

+ The contents are LaTeX, not Org markup; as in LaTeX, a line sans a tag ends in \\\\.
+ “:tag-COLOUR: NAME” ≈ “\\tag{$\\color{COLOUR}{\\text{NAME}}$} \\label{NAME} \\\\”, for any xcolor COLOUR.
+ Shorthands, such as -> above, come from the :let switch, which every block has; :let NAME shares them, from
  the table #+name: NAME or the variable NAME.
+ In Markdown, the equations are a fenced math block."
              (let* ((tag ":tag-\\([^: \t\n]+\\):[ \t]*\\(.*?\\)[ \t]*$")
                     (body (replace-regexp-in-string
                            tag (lambda (line)
                                  (string-match tag line)
                                  (format "\\tag{$\\color{%s}{\\text{%s}}$} \\label{%2$s} \\\\"
                                          (match-string 1 line) (match-string 2 line)))
                            raw-contents t t))
                     ;; A \\ after the last line would begin an empty, and so numbered, line of its own.
                     (body (replace-regexp-in-string "[ \t]*\\\\\\\\[ \t\n]*\\'" "" body t t))
                     (star (if numbered "" "*"))
                     (equations (format "\\begin{align%s}\n%s\n\\end{align%s}" star body star)))
                (cond ((org-export-markdown-p) (format "```math\n%s\n```" equations))
                      ;; MathJax reads the page's text, wherein a < would begin an HTML tag.
                      ((equal backend 'html) (org-html-encode-plain-text equations))
                      (t equations))))

(defun org--eqref-visit (name)
  "NAME ↦ point at the equation tagged NAME: an align block's “:tag-COLOUR: NAME” line, else a \\tag{…NAME…}."
  (let ((at (save-excursion
              (goto-char (point-min))
              (or (re-search-forward (format ":tag-[^: \t\n]+:[ \t]*%s[ \t]*$" (regexp-quote name)) nil t)
                  (re-search-forward (format "\\\\tag{[^}\n]*%s" (regexp-quote name)) nil t)))))
    (unless at (user-error "No equation is tagged “%s”" name))
    (push-mark)
    (goto-char at)
    (beginning-of-line)))

(org-deflink eqref
  "Refer to the equation tagged NAME; e.g., [[eqref:Golden Rule]] for an align block's “:tag-orange: Golden Rule”.
Following it visits that line, else any \\tag{…NAME…}; it exports as \\eqref{NAME}, which LaTeX and MathJax resolve."
  :follow (org--eqref-visit o-label)
  :export (cond ((org-export-markdown-p) (format "(%s)" o-label))
                (t (when (equal o-backend 'html) (org-html-head-require 'mathjax))
                   (format "\\eqref{%s}" o-label))))

(provide 'org-align-block)
;;; org-align-block.el ends here
