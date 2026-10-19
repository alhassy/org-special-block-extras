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

;; A math block's line tagged “COLOUR:<<NAME>>”, or a block “#+name: NAME”, is cited by NAME, as Org cites a named
;; element; by way of example,
;;
;;     #+begin_math
;;     p ⊑ q ≡ p ⊓ q = p   blue:<<⊑-Definition>>
;;     #+end_math
;;     By [[⊑-Definition]], or by [[⊑-Definition][its definition]], …
;;
;; exports as \eqref, which LaTeX and MathJax resolve; a description links its words to the equation.  A tag within a
;; hint's \[ … \] has no \eqref, as an aligned has no \tag, so its citation links to its anchor instead.  In Emacs,
;; C-c C-o on the link visits the line.  A NAME may hold maths, as ⊑ or \meet; see `org--math-label'.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'org)
(require 'org-special-block-at-point)
(require 'org-math-structure)

(declare-function org-html-head-require "org-block-export")

;;;; Following a citation ---C-c C-o visits the tagged line

(defun org--math-ref-follow (name)
  "When NAME is a \\tag{…NAME…}, visit it and ↦ t; else ↦ nil, so Org searches as ever, and finds a tag, <<NAME>>, as
its own target, and a block's “#+name: NAME”.  A function of `org-execute-file-search-functions', so C-c C-o on
[[NAME]] visits it."
  (when (derived-mode-p 'org-mode)
    (let ((at (save-excursion
                (goto-char (point-min))
                (re-search-forward (format "\\\\tag{[^}\n]*%s" (regexp-quote name)) nil t))))
      (when at
        (push-mark)
        (goto-char at)
        (beginning-of-line)
        t))))

;;;; Exporting citations ---[[NAME]] as \eqref, before Org parses

(defun org--math-ref-names ()
  "↦ each name a math block of the buffer gives its maths to cite, as (NAME . NESTED): its tags, “COLOUR:<<NAME>>”, and its
#+name:; a NESTED one is within a hint's \\[ … \\], whose maths is an aligned, sans \\tag nor \\eqref."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t) names)
      (while (re-search-forward "^[ \t]*#\\+begin_math\\b" nil t)
        (save-excursion
          (beginning-of-line)
          ;; Its affiliated keywords, #+name: among them, are the lines just above it.
          (while (and (zerop (forward-line -1)) (looking-at "[ \t]*#\\+\\([[:alnum:]_]+\\):[ \t]*\\(.*?\\)[ \t]*$"))
            (when (equal (downcase (match-string 1)) "name") (push (list (match-string 2)) names))))
        (let ((start (point)) (end (save-excursion (and (re-search-forward "^[ \t]*#\\+end_math\\b" nil t) (point)))))
          (while (and end (re-search-forward org--math-tag-re end t))
            ;; Nested: within a \[ that no \] has closed yet; a \\[ is a line break, not one.
            (let ((name (match-string 1)) (at (match-beginning 0)))
              (push (cons name (< (save-match-data (how-many "\\(?:^\\|[^\\]\\)\\\\\\]" start at))
                                  (save-match-data (how-many "\\(?:^\\|[^\\]\\)\\\\\\[" start at))))
                    names)))))
      names)))

(defun org--math-ref (name desc backend in-maths &optional nested)
  "(NAME, DESC, BACKEND, IN-MATHS, NESTED) ↦ a citation of the equation NAME tags, for BACKEND: \\eqref{LABEL}; or, with
DESC, its words linked to the equation.  Within maths, LaTeX; in prose, export snippets about DESC, which stays Org.
In Markdown, (NAME), as GitHub resolves no \\eqref in prose.  A NESTED tag has no \\eqref, so its DESC defaults to
“(NAME)”, which links to the tag's anchor; see `org--math-tags'."
  (let* ((label (org--math-label name))
         (desc (or desc (and nested (if in-maths (format "\\text{(%s)}" (org--math-tag-text name)) (format "(%s)" name)))))
         ;; MathJax's anchor for a label, as a link writes it, percent-encoded.
         (anchor (url-hexify-string (org--math-tag-anchor name)))
         (latex? (org-export-derived-backend-p backend 'latex))
         (md? (org-export-derived-backend-p backend 'md)))
    (cond (in-maths (cond ((not desc) (format "\\eqref{%s}" label))
                          (latex? (format (if nested "\\hyperlink{%s}{%s}" "\\hyperref[%s]{%s}") label desc))
                          (t (format "\\href{#%s}{%s}" anchor desc))))
          (md? (or desc (format "(%s)" name)))
          (latex? (cond (nested (format "@@latex:\\hyperlink{%s}{@@%s@@latex:}@@" label desc))
                        (desc (format "@@latex:\\hyperref[%s]{@@%s@@latex:}@@" label desc))
                        (t (format "@@latex:\\eqref{%s}@@" label))))
          ((org-export-derived-backend-p backend 'html)
           (org-html-head-require 'mathjax)
           (if desc (format "@@html:<a href=\"#%s\">@@%s@@html:</a>@@" anchor desc) (format "@@html:\\eqref{%s}@@" label)))
          (t (or desc (format "(%s)" name))))))

(defun org--math-cite (backend)
  "Before BACKEND's export parses the buffer, make each [[NAME]] or [[NAME][DESC]] whose NAME a math block gives a
citation of it, `org--math-ref'; while the blocks' tags are still to be read, ere they are rewritten.  A link in
verbatim text, such as a src block's or =this=, is left be; so is one to anything else, for Org to resolve, or report."
  (-when-let (names (org--math-ref-names))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-link-bracket-re nil t)
        (let ((name (match-string-no-properties 1)) (desc (match-string-no-properties 2))
              (start (match-beginning 0)) (end (match-end 0)))
          (when (and (assoc name names)
                     (save-excursion
                       (goto-char start)
                       (not (or (memq (org-element-type (org-element-at-point))
                                      '(src-block example-block export-block fixed-width comment-block))
                                (memq (org-element-type (org-element-context)) '(verbatim code inline-src-block))))))
            (let ((in-maths (save-excursion
                              (goto-char start)
                              (let ((block (org-special-block-at-point)))
                                (and block (equal (downcase (org-element-property :type block)) "math"))))))
              (delete-region start end)
              (goto-char start)
              (insert (org--math-ref name desc backend in-maths (cdr (assoc name names)))))))))))

(provide 'org-math-refs)
;;; org-math-refs.el ends here
