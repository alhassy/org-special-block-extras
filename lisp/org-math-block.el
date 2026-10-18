;;; org-math-block.el --- Display maths that knows our links and Org's emphasis  -*- lexical-binding: t; -*-

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

;; A math block is display maths, wherein our links and Org's emphasis are LaTeX's; by way of example,
;;
;;     #+begin_math
;;     \def\xstart{red:a}
;;     teal:\bigoplus_{violet:x = \xstart}^{cyan:b} brown:f(violet:x) = *f(a)* teal:\oplus \cdots
;;     #+end_math
;;
;; colours ⊕ teal, x violet, a red, and b cyan, as red:𝒳 colours prose; f(a) is bold.  The \def line is a
;; definition, declared but not shown: in LaTeX, in the preamble; in HTML, for MathJax, hidden.  A block of only
;; definitions shows nothing.  The align block's equations are read alike.  See `org-block/math'.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'org-defblock)
(require 'org-color-links)

(declare-function org-docs-get "org-links")

(defvar org--math-backend nil "The backend a math block is exporting to, for the links that differ by it.")

(defun org--math-tip (math tip)
  "(MATH, TIP) ↦ MATH, with TIP hovering over it in HTML; LaTeX and GitHub's maths have no hovers, so there it is MATH."
  (if (and (eq org--math-backend 'html) (not (org-export-markdown-p))) (format "\\texttip{%s}{%s}" math tip) math))

(defvar org-math-links
  (append
   (--map (cons (symbol-name it) (lambda (path desc) (format "{\\color{%s}%s}" it (or desc path))))
          org--named-colors)
   `(("color" . ,(lambda (path desc)
                   (format "{\\color%s%s}"
                           (if (string-prefix-p "#" path) (format "[HTML]{%s}" (substring path 1)) (format "{%s}" path))
                           desc)))
     ("highlight" . ,(lambda (path desc) (format "\\colorbox{%s}{$%s$}" (or (org-string-nw-p path) "yellow") desc)))
     ("tooltip" . ,(lambda (path desc) (org--math-tip path desc)))
     ("doc" . ,(lambda (path desc) (org--math-tip (or desc (car (org-docs-get path))) (cadr (org-docs-get path)))))
     ("eqref" . ,(lambda (path _) (format "\\eqref{%s}" path)))
     ("https" . ,(lambda (path desc) (format "\\href{https:%s}{%s}" path (or desc path))))
     ("http" . ,(lambda (path desc) (format "\\href{http:%s}{%s}" path (or desc path))))))
  "Link types that maths knows, each TYPE ↦ (λ PATH DESC ↦ LaTeX); DESC is nil for [[TYPE:PATH]] and TYPE:PATH.
An unknown TYPE shows its DESC, else its PATH.")

(defconst org--math-emphasis
  '(("*" . "\\boldsymbol{%s}") ("/" . "\\mathit{%s}") ("_" . "\\underline{%s}")
    ("+" . "\\cancel{%s}") ("=" . "\\texttt{%s}") ("~" . "\\mathtt{%s}"))
  "Org's emphasis markers, each ↦ its LaTeX in maths.")

(defun org--math-plain-links (&optional end)
  "Render each TYPE:ATOM from point to END as LaTeX, wherein ATOM is a {group}, a \\command{with}{arguments}, or a word;
e.g., red:{a + blue:b}, red:\\frac{a}{b}."
  (let ((end (copy-marker (or end (point-max)) t)))
    (while (re-search-forward (format "\\_<\\(%s\\):" (regexp-opt (mapcar #'car org-math-links))) end t)
      (let ((type (match-string 1)) (start (match-beginning 0)) (atom-start (point)))
        (with-syntax-table (let ((s (make-syntax-table)))
                             (modify-syntax-entry ?{ "(}" s)
                             (modify-syntax-entry ?} "){" s)
                             s)
          (cond ((eq (char-after) ?{) (forward-sexp))
                ((eq (char-after) ?\\) (forward-char) (skip-chars-forward "[:alpha:]")
                 (while (eq (char-after) ?{) (forward-sexp)))
                (t (skip-chars-forward "[:alnum:]"))))
        (when (< atom-start (point))
          ;; The ATOM's own links first; then we resume past the rendering, which may mention TYPE: itself.
          (save-excursion (let ((atom-end (point))) (goto-char atom-start) (org--math-plain-links atom-end)))
          (let ((atom (buffer-substring atom-start (point))))
            (delete-region start (point))
            (goto-char start)
            (insert (funcall (cdr (assoc type org-math-links)) atom nil))))))))

(defun org--math-markup (math backend)
  "(MATH, BACKEND) ↦ MATH in LaTeX, sans Org's links and emphasis; e.g., “red:x + *y*” ↦ “{\\color{red}x} + \\boldsymbol{y}”."
  (let ((org--math-backend backend))
    (with-temp-buffer
      (insert math)
      ;; [[TYPE:PATH][DESC]] and [[TYPE:PATH]]; a DESC may itself hold links, so we go innermost first.
      (goto-char (point-max))
      (while (re-search-backward org-link-bracket-re nil t)
	(let* ((start (match-beginning 0)) (end (match-end 0))
               (link (match-string 1)) (desc (match-string 2))
               (type (car (split-string link ":"))) (path (string-remove-prefix (concat type ":") link))
               (render (cdr (assoc type org-math-links))))
          (delete-region start end)
          (goto-char start)
          (save-excursion (insert (if render (funcall render path desc) (or desc link))))))
      (goto-char (point-min))
      (org--math-plain-links)
      ;; Org's own emphasis: a marker hugs its text, and follows a space, (, {, or ', as in Org's prose; so a = b and
      ;; a*b*c are left be.
      (goto-char (point-min))
      (while (re-search-forward (concat "\\(?:^\\|[-[:space:]('\"{]\\)"
					"\\(\\([*/_+=~]\\)\\([^[:space:]]\\|[^[:space:]].*?[^[:space:]]\\)\\2\\)"
					"\\(?:[-[:space:].,:!?;'\")}^_\\[]\\|$\\)")
				nil t)
	(replace-match (format (cdr (assoc (match-string 2) org--math-emphasis)) (match-string 3)) t t nil 1)
	(goto-char (match-beginning 1)))
      (buffer-string))))

(defconst org--math-definition-re "^\\\\\\(def\\|let\\|newcommand\\|renewcommand\\|DeclareMathOperator\\)\\b"
  "A line that declares a macro, rather than showing maths.")

(org-defblock math (:link nil :latex-head ("amsmath" "xcolor" "cancel") :html-head (mathjax))
	      "Display maths, wherein our links ---e.g., red:x, [[doc:fold][f]]--- and Org's *emphasis* are LaTeX's.

  #+begin_math
  \\def\\xstart{red:a}
  teal:\\bigoplus_{violet:x = \\xstart}^{cyan:b} brown:f(violet:x)
  #+end_math

+ red:ATOM colours a {group}, a \\command{with}{arguments}, or a word; [[red:][a + b]] colours a + b.  So too for any colour,
  [[color:#ff0066][…]], highlight:, eqref:, web links, and tooltip: and doc:, which hover in HTML; see `org-math-links'.
+ *bold* /italic/ _underline_ +cancel+ =texttt= ~mathtt~, when the markers hug their text, as in Org's prose.
+ A line that begins \\def, \\let, \\newcommand, \\renewcommand, or \\DeclareMathOperator is a definition: declared,
  but not shown.  In LaTeX, it goes to the preamble; in HTML, MathJax reads it hidden, and then knows it page-wide.
+ In Markdown, the block is a fenced math block, its definitions within, since GitHub forgets them between blocks."
	      (let* ((lines (split-string (org--math-markup raw-contents backend) "\n" t "[ \t]+"))
		     (defs (--filter (string-match-p org--math-definition-re it) lines))
		     (math (string-join (--remove (string-match-p org--math-definition-re it) lines) "\n"))
		     (shown (unless (equal math "") (format "\\[\n%s\n\\]" math))))
		(cond ((org-export-markdown-p) (format "```math\n%s\n```" (string-join (append defs (and shown (list math))) "\n")))
		      ((equal backend 'latex) (concat (org-parse (mapconcat (lambda (d) (concat "#+latex_header: " d)) defs "\n"))
						      shown))
		      ;; MathJax reads the page's text, wherein a < would begin an HTML tag.
		      (t (concat (when defs (format "<p style=\"display:none\">\\[%s\\]</p>\n" (string-join defs "\n")))
				 (and shown (org-html-encode-plain-text shown)))))))

(provide 'org-math-block)
;;; org-math-block.el ends here
