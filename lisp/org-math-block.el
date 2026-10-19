;;; org-math-block.el --- Display maths that knows our links and Org's markup  -*- lexical-binding: t; -*-

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

;; A math block is display maths, wherein our links and Org's markup are LaTeX's; by way of example,
;;
;;     #+begin_math
;;     # The bounds, declared once; this line is a comment.
;;     \def\xstart{red:a}
;;     teal:\bigoplus_{violet:x[fn::the current element] = \xstart}^{cyan:b} brown:f(violet:x)   :tag-teal: Fold
;;     #+end_math
;;
;; colours ⊕ teal, x violet, a red, and b cyan, as red:𝒳 colours prose; hovering over x shows its footnote; the
;; equation is tagged “(Fold)”, in teal, which [[eqref:Fold]] cites.  The \def line is a definition, declared but not
;; shown: in LaTeX, in the preamble; in HTML, for MathJax, hidden.  An Org table is a matrix; lines that end in \\ are
;; gathered; lines with an & are aligned at it, as in a textbook derivation.  In Emacs, C-c C-c shows the maths as an
;; image atop the block, and C-c ' edits it in `latex-mode'.  See `org-block/math'.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'cl-lib)
(require 'org-defblock)
(require 'org-color-links)

(declare-function org-docs-get "org-links")
(declare-function org--docs-plain-p "org-links")
(declare-function org--tooltip-template "org-links")
(declare-function org--latex-margin-note "org-margin-blocks")
(defvar org--docs-actually-used)
(defvar org--handler-anchor)

(defvar org--math-backend nil "The backend a math block is exporting to, for the links that differ by it.")

(defvar org--math-hint-folded nil
  "Whether a hint starts folded in HTML, so a reader clicks to see it.  A math block binds it by its :hints.")

(defvar org--math-hint-format nil
  "How a hint shows, as LaTeX, its %s the hint, replaced verbatim; e.g., “{\\color{teal} \\langle %s \\rangle}”; or none,
to show no hints.  A math block binds it by its :hints; nil is as the layout has it: see `org--math-hint-styled'.")

(defun org--math-color (colour)
  "COLOUR ↦ the argument of \\color for it, in maths: “#ff0066” ↦ “[RGB]{255,0,102}”, “teal” ↦ “{teal}”.
RGB, as LaTeX's xcolor and MathJax both know it; MathJax knows no HTML model."
  (if (string-prefix-p "#" colour)
      (apply #'format "[RGB]{%d,%d,%d}" (--map (string-to-number (substring colour it (+ it 2)) 16) '(1 3 5)))
    (format "{%s}" colour)))

(defvar org--math-tips nil
  "What follows the maths being exported, for its tooltips, newest first: in HTML, their <template>s; in LaTeX, their
margin notes; see `org--math-tip'.  A block binds it, and emits it after its maths.")

(defun org--math-tip (math tip &optional plain?)
  "(MATH, TIP, PLAIN?) ↦ MATH with TIP as a tooltip, as a prose tooltip: link is, noting in `org--math-tips' what must
follow the maths.  TIP is Org, or PLAIN? text, as for `org--html-tooltip'.
+ HTML: “x”, “the /current/ element”  ↦  \\class{tooltip osbe-tip-g1}{x}, for tooltips.js, beside
  <template id=\"osbe-tip-g1\"><p>the <i>current</i> element</p></template>, as MathJax keeps no <template>.
+ LaTeX: MATH wears the number of TIP's margin note, which follows the maths since maths holds no \\marginpar;
  in beamer, which has no margins, a footnote.
+ Markdown: MATH alone, as GitHub's maths has no hovers."
  (pcase (if (org-export-markdown-p) 'md org--math-backend)
    ('html (let ((id (format "osbe-tip-%s" (gensym))))
             (push (org--tooltip-template id tip plain?) org--math-tips)
             (format "\\class{tooltip %s}{%s}" id math)))
    ('latex (let ((note (string-trim (org-export-string-as tip 'latex t))))
              (push (if (org-export-beamer-p) (format "\\stepcounter{footnote}\\footnotetext{%s}" note)
                      (org--latex-margin-note "{\\thefootnote}" "\\marginparwidth" "gray!80" note
                                              "\\refstepcounter{footnote}" t))
                    org--math-tips)
              ;; The number its note steps to, read sans stepping, as amsmath may typeset the maths twice.
              (format "{%s}^{\\textnormal{\\the\\numexpr\\value{footnote}+%d\\relax}}" math (length org--math-tips))))
    (_ math)))

(defvar org-math-links
  (append
   (--map (cons (symbol-name it) (lambda (path desc) (format "{\\color{%s}%s}" it (or desc path))))
          org--named-colors)
   `(("color" . ,(lambda (path desc) (format "{\\color%s%s}" (org--math-color path) desc)))
     ("highlight" . ,(lambda (path desc) (format "\\colorbox{%s}{$%s$}" (or (org-string-nw-p path) "yellow") desc)))
     ;; Sans description, it has no tip; unless it follows an atom, which `org--math-footnotes' sees to.
     ("tooltip" . ,(lambda (path desc) (if desc (org--math-tip path desc) path)))
     ("doc" . ,(lambda (path desc)
                 (-let [(name docs) (org-docs-get path)]
                   ;; As a doc: link in prose, for the glossary that show:GLOSSARY lists; in LaTeX, that is all.
                   (add-to-list 'org--docs-actually-used (list path name docs))
                   (if (eq org--math-backend 'latex) (or desc name)
                     (org--math-tip (or desc name) docs (org--docs-plain-p path))))))
     ("eqref" . ,(lambda (path _) (format "\\eqref{%s}" (org--math-label path))))
     ("https" . ,(lambda (path desc) (format "\\href{https:%s}{%s}" path (or desc path))))
     ("http" . ,(lambda (path desc) (format "\\href{http:%s}{%s}" path (or desc path))))))
  "Link types that maths knows, each TYPE ↦ (λ PATH DESC ↦ LaTeX); DESC is nil for [[TYPE:PATH]] and TYPE:PATH.
An unknown TYPE shows its DESC, else its PATH.")

(defconst org--math-emphasis
  '(("*" . "\\boldsymbol{%s}") ("/" . "\\mathit{%s}") ("_" . "\\underline{%s}")
    ("+" . "\\cancel{%s}") ("=" . "\\texttt{%s}") ("~" . "\\mathtt{%s}"))
  "Org's emphasis markers, each ↦ its LaTeX in maths.")

(defconst org--math-text-emphasis
  '(("*" . "$\\textbf{%s}$") ("/" . "$\\textit{%s}$") ("_" . "$\\underline{\\text{%s}}$")
    ("+" . "$\\cancel{\\text{%s}}$") ("=" . "$\\texttt{%s}$") ("~" . "$\\texttt{%s}$"))
  "Org's emphasis markers, each ↦ its LaTeX in a \\text{…}'s prose, such as a hint's, where maths' own is an error.
Each is maths about text, $\\textbf{…}$, as MathJax reads no macro in \\text{…} sans its textmacros, yet reads $…$.")

(defun org--math-text-p (pos)
  "Whether POS, in the buffer's maths, is within a \\text{…}'s prose, rather than in maths, or in $…$ within the prose.
E.g., in “\\text{by *x* and $*y*$}”, at *x* yes, and at *y* no."
  (save-match-data
    (with-syntax-table org--math-syntax
      (-when-let (brace (--last (save-excursion (goto-char it) (looking-back "\\\\text" (line-beginning-position)))
                                (nth 9 (parse-partial-sexp (point-min) pos))))
        (cl-evenp (cl-count ?$ (buffer-substring (1+ brace) pos)))))))

;;;; Atoms ---what red:ATOM colours, and what an ATOM[fn::…] tip hovers over

(defconst org--math-syntax
  (let ((s (make-syntax-table)))
    (modify-syntax-entry ?{ "(}" s)
    (modify-syntax-entry ?} "){" s)
    ;; A $ delimits maths, as in a hint's $red:x$; in the standard table it is a symbol's, whence “$red” would be one.
    (modify-syntax-entry ?$ "." s)
    s)
  "Braces are parentheses, so a {group} is a sexp; as in the standard table, \\ escapes, so \\{ is no brace; a $ delimits.")

(defconst org--math-sizers
  '("big" "Big" "bigg" "Bigg" "bigl" "bigr" "Bigl" "Bigr" "biggl" "biggr" "Biggl" "Biggr" "middle")
  "The commands that size the delimiter after them, which is so part of their atom; e.g., \\big(.")

(defun org--math-skip-delimiter ()
  "Move past the delimiter at point: a \\command such as \\langle, an escaped \\{, or a character such as (."
  (when (looking-at "\\\\[[:alpha:]]+\\|\\\\.\\|[^[:space:]]") (goto-char (match-end 0))))

(defun org--math-atom-end ()
  "↦ the end of the TeX atom at point, else nil: a {group}; \\left(…\\right); a \\command{with}{arguments}, or a
sizer with its delimiter; a word; else one character.  E.g., at “\\frac{a}{b} + c”, the end of \\frac{a}{b}."
  (save-excursion
    (with-syntax-table org--math-syntax
      (let ((start (point)))
        (cond ((eq (char-after) ?{) (forward-sexp))
              ;; \left( … \right) is one atom: an atom of \left( alone is an error.
              ((looking-at "\\\\left\\b")
               (let ((depth 0))
                 (while (and (re-search-forward "\\\\\\(left\\|right\\)\\b" nil t)
                             (< 0 (cl-incf depth (if (equal (match-string 1) "left") 1 -1)))))
                 (org--math-skip-delimiter)))
              ((looking-at "\\\\\\([[:alpha:]]+\\)")
               (goto-char (match-end 0))
               (if (member (match-string 1) org--math-sizers)
                   (org--math-skip-delimiter)
                 (while (eq (char-after) ?{) (forward-sexp))))
              ((looking-at "[[:alnum:]]+") (goto-char (match-end 0)))
              ((looking-at "[^][:space:]{}\\\\]") (forward-char)))
        (and (< start (point)) (point))))))

(defun org--math-atom-start ()
  "↦ the start of the TeX atom that ends at point, else nil: a \\command{with}{arguments}, a {group}, a \\command,
a word, else one character.  E.g., before the [fn::…] of “a + \\frac{x}{y}[fn::…]”, the start of \\frac{x}{y}."
  (save-excursion
    (with-syntax-table org--math-syntax
      (let ((end (point)))
        (while (eq (char-before) ?}) (backward-sexp))
        (cond ((< (point) end)
               (when (looking-back "\\\\[[:alpha:]]+" (line-beginning-position)) (goto-char (match-beginning 0))))
              ((< (skip-chars-backward "[:alnum:]") 0)
               (when (looking-back "\\\\[[:alpha:]]*" (line-beginning-position)) (goto-char (match-beginning 0))))
              ((not (memq (char-before) '(nil ?\s ?\t ?\n))) (backward-char)))
        (and (< (point) end) (point))))))

;;;; Markup ---Org's footnotes, links, and emphasis, as LaTeX

(defun org--math-footnotes (origin)
  "Make each ATOM[fn::TIP], ATOM[fn:NAME], or ATOM[[tooltip:TIP]], in the buffer a hover of TIP over ATOM, as a graph's
node footnote is its tooltip; e.g., x[fn::the current element].  A [fn:NAME]'s TIP is its definition, “[fn:NAME] TIP”,
in the ORIGIN buffer, as in prose; so one long tip may serve many atoms.  A [[tooltip:TIP]] sans description has no
marker of its own, so it marks the ATOM before it."
  (goto-char (point-min))
  (while (re-search-forward "\\[fn:[^]:[:space:]]*:\\|\\[fn:\\([^]:[:space:]]+\\)\\]\\|\\[\\[tooltip:\\([^]]*\\)\\]\\]" nil t)
    (let* ((fn-start (match-beginning 0))
           (name (match-string 1))
           (link-tip (match-string 2))
           (tip-end (if (or name link-tip) (match-end 0)
                      (save-excursion (goto-char fn-start) (with-syntax-table org--math-syntax (forward-sexp)) (point))))
           (tip (string-trim (cond (link-tip)
                                   (name (or (with-current-buffer origin (nth 3 (org-footnote-get-definition name)))
                                             (user-error "math: [fn:%s] has no definition" name)))
                                   (t (buffer-substring (point) (1- tip-end))))))
           (atom-start (save-excursion (goto-char fn-start) (org--math-atom-start))))
      (when atom-start
        (let ((atom (buffer-substring atom-start fn-start)))
          (delete-region atom-start tip-end)
          (goto-char atom-start)
          (insert (org--math-tip atom tip)))))))

(defun org--math-match-plain-link (limit)
  "Font-lock matcher: up to LIMIT, the next TYPE:ATOM of `org-math-links', group 1 its TYPE; else nil.
A web link's path is no maths, so it is only ever bracketed, [[https://…][…]]; whence https:/ is no TYPE:ATOM."
  (let ((types (regexp-opt (--remove (member it '("http" "https")) (mapcar #'car org-math-links))))
        found)
    (while (and (not found) (with-syntax-table org--math-syntax (re-search-forward (format "\\_<\\(%s\\):" types) limit t)))
      (-when-let* ((link (list (match-beginning 0) (match-beginning 1) (match-end 1)))
                   (end (org--math-atom-end)))
        (set-match-data (list (nth 0 link) end (nth 1 link) (nth 2 link)))
        (goto-char end)
        (setq found t)))
    found))

(defun org--math-plain-links (&optional end)
  "Render each TYPE:ATOM from point to END as LaTeX; e.g., red:{a + blue:b}, red:\\frac{a}{b}, red:\\left(x\\right)."
  (let ((end (copy-marker (or end (point-max)) t)))
    (while (org--math-match-plain-link end)
      (let ((type (match-string 1)) (start (match-beginning 0)) (atom-start (match-end 1)) (atom-end (point)))
        ;; The ATOM's own links first; then we resume past the rendering, which may mention TYPE: itself.
        (save-excursion (goto-char (1+ atom-start)) (org--math-plain-links atom-end))
        (let ((atom (buffer-substring (1+ atom-start) (point))))
          (delete-region start (point))
          (goto-char start)
          (insert (funcall (cdr (assoc type org-math-links)) atom nil)))))))

(defconst org--math-section-link-re "\\`\\(?:[*#]\\|id:\\)"
  "A link to a section: [[*Heading]], [[#custom-id]], or [[id:ID]].")

(defun org--math-section-link (link desc)
  "(LINK, DESC) ↦ DESC, linking to the section LINK names, as a graph's node does; e.g., “*Folds”, “f” ↦ \\href{*Folds}{f}
in HTML, \\hyperref[*Folds]{f} in LaTeX, which `org--resolve-section-links' points at the section once it lands.  Sans
DESC, the heading shows, as text; Markdown's maths has no links, so there DESC alone."
  (let ((desc (or desc (format "\\text{%s}" (replace-regexp-in-string org--math-section-link-re "" link)))))
    (cond ((org-export-markdown-p) desc)
          ((eq org--math-backend 'latex) (format "\\hyperref[%s]{%s}" link desc))
          (t (format "\\href{%s}{%s}" link desc)))))

(defun org--math-markup (math backend)
  "(MATH, BACKEND) ↦ MATH in LaTeX, sans Org's footnotes, links, and emphasis; e.g., “red:x + *y*” ↦ “{\\color{red}x}
+ \\boldsymbol{y}”."
  (let ((org--math-backend backend) (origin (current-buffer)))
    (with-temp-buffer
      (insert math)
      (org--math-footnotes origin)
      ;; [[TYPE:PATH][DESC]] and [[TYPE:PATH]]; a DESC may itself hold links, so we go innermost first.
      (goto-char (point-max))
      (while (re-search-backward org-link-bracket-re nil t)
        (let* ((start (match-beginning 0)) (end (match-end 0))
               (link (match-string 1)) (desc (match-string 2))
               (type (car (split-string link ":"))) (path (string-remove-prefix (concat type ":") link))
               (render (cdr (assoc type org-math-links))))
          (delete-region start end)
          (goto-char start)
          (save-excursion (insert (cond ((string-match-p org--math-section-link-re link) (org--math-section-link link desc))
                                        (render (funcall render path desc))
                                        (t (or desc link)))))))
      (goto-char (point-min))
      (org--math-plain-links)
      ;; Org's own emphasis: a marker hugs its text, and follows a space, (, {, ', or a hint's $, as in Org's prose; so
      ;; a = b and a*b*c are left be.  In a hint's prose, it is text's emphasis; see `org--math-text-p'.
      (goto-char (point-min))
      (while (re-search-forward (concat "\\(?:^\\|[-[:space:]('\"{$]\\)"
                                        "\\(\\([*/_+=~]\\)\\([^[:space:]]\\|[^[:space:]].*?[^[:space:]]\\)\\2\\)"
                                        "\\(?:[-[:space:].,:!?;'\")}^_\\[$]\\|$\\)")
                                nil t)
        (replace-match (format (cdr (assoc (match-string 2) (if (org--math-text-p (match-beginning 1)) org--math-text-emphasis
                                                             org--math-emphasis)))
                               (match-string 3))
                       t t nil 1)
        (goto-char (match-beginning 1)))
      (buffer-string))))

;;;; Structure ---comments, tables, tags, and definitions

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

(defun org--math-link-type-face (type path)
  "(TYPE, PATH) ↦ the face Org shows a TYPE:PATH link in, by TYPE's :face; e.g., red ↦ (:foreground \"red\")."
  (let ((face (org-link-get-parameter type :face)))
    (cond ((functionp face) (funcall face path)) (face) (t 'org-link))))

(defun org--math-link-face ()
  "↦ The face of the TYPE:ATOM just matched: TYPE's own, as [[red:…]] shows red in prose."
  (org--math-link-type-face (match-string 1) (buffer-substring (1+ (match-end 1)) (match-end 0))))

(defun org--math-unlink (limit)
  "Font-lock matcher that matches nothing: up to LIMIT, it clears Org's look of each plain TYPE: link of
`org-math-links', since Org reads red:a}^{cyan:b as one link; `org--math-match-plain-link' then shows the maths' own.
Org prepended the link's face to those beneath, LaTeX's, which we keep."
  (while (re-search-forward org-link-plain-re limit t)
    (-when-let* ((type (car (assoc (match-string 1) org-math-links)))
                 (beg (match-beginning 0)) (end (match-end 0))
                 (link-face (org--math-link-type-face type (match-string 2))))
      (remove-text-properties beg end '(mouse-face nil keymap nil help-echo nil htmlize-link nil))
      (while (< beg end)
        (let ((faces (get-text-property beg 'face)) (next (next-single-property-change beg 'face nil end)))
          (put-text-property beg next 'face (cond ((equal faces link-face) nil)
                                                  ((consp faces) (remove link-face faces))
                                                  (t faces)))
          (setq beg next)))))
  nil)

(defface org-math-hint '((t :inherit font-lock-comment-face))
  "How a math block's hint, “REL{ HINT }”, looks: muted, as a comment, since it says why a step holds, not what it is.
Not `shadow', which some themes make the colour of all text."
  :group 'org-special-block-extras)

(defconst org--math-fontify-keywords
  '((org--math-unlink 0 nil)
    (":tag-\\([^: \t\n]+\\):" 0 org--math-tag-face)
    (org--math-match-plain-link 0 org--math-link-face)
    ;; Each & is where lines may align; an escaped \& is a mere ampersand.
    ("\\(?:^\\|[^\\]\\)\\(&\\)" 1 font-lock-keyword-face)
    ;; A hint shows as a comment, so we see at a glance which {…} the export reads as one.
    (org--math-match-hint 0 org-math-hint))
  "How a math block's tags, TYPE:ATOM links, &s, and hints look in Org buffers; see `org--fontify-blocks'.")

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

(defconst org--math-relation-re
  (concat ":?[=<>≤≥≡≠≈≃≅∼⇒⇐⇔→←↔⟶⟹⟸⟺⊆⊇⊂⊃⊑⊒∈]+\\|\\\\"
          (regexp-opt '("equiv" "le" "leq" "ge" "geq" "lt" "gt" "ne" "neq" "approx" "sim" "simeq" "cong" "in" "mid"
                        "Rightarrow" "Leftarrow" "Leftrightarrow" "rightarrow" "leftarrow" "longrightarrow" "to"
                        "implies" "impliedby" "iff" "subseteq" "supseteq" "subset" "supset" "sqsubseteq" "sqsupseteq")
                      t))
  "A relation, which a hint may follow; e.g., =, ≤, \\equiv.")

(defun org--math-hint-spans (math)
  "MATH ↦ each top-level hint, “REL{ HINT }”, as (REL BRACE END): where REL starts, where its { starts, and where its }
ends; e.g., “x ={ by IH } y” ↦ ((2 3 12)).  The { must hug REL, and a space or newline must follow it; so “x ={a}”, “^{ 2 }”, and
“\\frac{ a }” are no hints, nor is one with an unclosed {, as while we type it."
  (with-temp-buffer
    (insert math)
    (let (spans)
      (pcase-dolist (`(,mark . ,at) (org--math-top-level-marks math))
        (goto-char (1+ at))
        (when (and (equal mark "{") (looking-at "{[ \t\n]") (looking-back org--math-relation-re (line-beginning-position) t))
          (let ((rel (1- (match-beginning 0)))
                (end (ignore-errors (with-syntax-table org--math-syntax (forward-sexp) (1- (point))))))
            (when end (push (list rel at end) spans)))))
      (nreverse spans))))

(defun org--math-hint (text &optional between)
  "(TEXT, BETWEEN) ↦ the LaTeX of a hint: its lines, top-aligned with its step, a blank one a gap; and each
\\[ CALCULATION \\] within, a nested calculation, whose hints are read alike, so they may nest too, set BETWEEN its
expressions as the block's are; see `org--math-hints'.  A hint of one line is a \\text{…}.
E.g., “Why:⏎\\[ x ={ h } y \\]” ↦ a column of \\text{Why:} atop the aligned x &\\quad = \\quad y && \\text{h}."
  (let ((pos 0) rows)
    ;; A line that is an environment already, as an Org table's matrix is, stays maths; the rest is prose.
    (cl-flet ((lines (string) (--map (cond ((equal it "") "") ((string-prefix-p "\\begin{" it) it) (t (format "\\text{%s}" it)))
                                     (split-string (string-trim string) "[ \t]*\n[ \t]*"))))
      ;; Each \[ with the \] that closes it, as nested calculations may nest; a \\[ is a line break, not one.
      (while (string-match "\\(?:\\`\\|[^\\]\\)\\(\\\\\\[\\)" text pos)
        (let ((open (match-beginning 1)) (close (match-end 1)) (depth 1))
          (while (and (< 0 depth) (string-match "\\\\[][]" text close))
            (setq close (match-end 0))
            (cl-incf depth (if (eq (aref text (1- close)) ?\[) 1 -1)))
          (unless (string-blank-p (substring text pos open)) (setq rows (append rows (lines (substring text pos open)))))
          (setq rows (append rows (list (format "\\begin{aligned}[t] %s \\end{aligned}"
                                                (string-trim (replace-regexp-in-string
                                                              "[ \t]*\n[ \t]*" " "
                                                              (org--math-hints (substring text (+ open 2) (- close 2)) between t)))))))
          (setq pos close)))
      (unless (string-blank-p (substring text pos)) (setq rows (append rows (lines (substring text pos))))))
    ;; Beside its step, a hint's first line is level with the step's; between steps, its row is its own, so it is centred,
    ;; whence a \\left[ about it, which grows alike above and below, fits it.
    (if (cdr rows) (format "\\begin{array}%s{@{}l@{}} %s \\end{array}" (if between "" "[t]") (string-join rows " \\\\ "))
      (car rows))))

(defconst org--math-hint-html-head
  "<template id=\"osbe-tip-hint\"><p>Click to hide this hint, or to show it again; shift-click, every hint of this proof.</p></template>
<style>
  .osbe-hint { cursor: pointer; }
  /* A hint is plainly a hint ---prose, maybe coloured--- so it wears no tooltip's dotted underline. */
  .tooltip.osbe-hint { border-bottom: none; }
  .osbe-hint-fold, .osbe-hint.osbe-folded .osbe-hint-body { display: none; }
  .osbe-hint.osbe-folded .osbe-hint-fold { display: inline-block; }
</style>
<script>
  // A click on a math block's hint folds it to [⋯], or unfolds it, as TAB does in Emacs; a shift-click folds every
  // hint of its proof, unless all are folded, whence it unfolds them, as C-TAB does.
  document.addEventListener('click', (event) => {
    const hint = event.target.closest?.('.osbe-hint');
    if (!hint) return;
    if (!event.shiftKey) return hint.classList.toggle('osbe-folded');
    const hints = [...(hint.closest('mjx-container') ?? document).querySelectorAll('.osbe-hint')];
    const fold = hints.some((h) => !h.classList.contains('osbe-folded'));
    hints.forEach((h) => h.classList.toggle('osbe-folded', fold));
  });
</script>"
  "The tip, style, and script by which a click folds a math block's hint; added to a page whose maths has hints.")

(defun org--math-hint-styled (hint between)
  "(HINT, BETWEEN) ↦ HINT as `org--math-hint-format' says; else, as the layout has it: a hint BETWEEN expressions is
maroon, after a bracket that opens it, lest it be mistaken for maths; a hint beside its step is plain, as its column
sets it apart already.  E.g., between, “\\text{h}” ↦ “{\\color[RGB]{128,0,0} \\left[ \\text{h}
\\right.}”.  A format of none shows no hint.  In HTML, a click on the hint folds it; see `org--math-hint-html-head'."
  (let ((format (format "%s" (or org--math-hint-format
                                 (if between (format "{\\color%s \\left[ %%s \\right.}" (org--math-color "#800000")) "%s")))))
    (cond ((equal format "none") "")
          ;; In HTML, a click folds the hint to [⋯], as TAB folds it to {…} in Emacs; its tooltip says so.  The whole
          ;; hint folds, its format too, whose tall bracket would else keep its room.  The fold is closed, as folded code
          ;; is shown, its ⋯ level with the relation; in the hint's colour, or a format of our own about a lone ⋯.  The
          ;; page's head, which serves these, the block asks for: we are within a temporary buffer, whose buffer-local
          ;; requests are lost.
          ((and (eq org--math-backend 'html) (not (org-export-markdown-p)))
           (format "\\class{tooltip osbe-tip-hint osbe-hint%s}{\\class{osbe-hint-body}{%s}\\class{osbe-hint-fold}{%s}}"
                   (if org--math-hint-folded " osbe-folded" "")
                   (string-replace "%s" hint format)
                   (cond (org--math-hint-format (string-replace "%s" "\\cdots" format))
                         (between (format "{\\color%s [\\,\\cdots\\,]}" (org--math-color "#800000")))
                         (t "[\\,\\cdots\\,]"))))
          (t (string-replace "%s" hint format)))))

(defun org--math-hints (math &optional between nested)
  "MATH ↦ MATH whose each hint, “REL{ HINT }” of `org--math-hint-spans', is “&\\quad REL \\quad”, its HINT at its step's
end ---before the next hint, else the maths' end--- yet before a tag or \\\\ there; e.g., “x ={ by IH } y ={ by IH } z”
↦ “x &\\quad = \\quad y && \\text{by IH} \\\\ &\\quad = \\quad z && \\text{by IH}”.  So each hint but the first begins a
row, wherever the newlines fall; LaTeX reads them as spaces.  When BETWEEN, each hint is a row of its own between the
expressions it relates, as Feijen and Dijkstra set a calculation: “x ={ by IH } y” ↦ “& x \\\\ = & \\quad \\text{by IH}
\\notag \\\\ & y”; so a wide expression leaves its hint room.  A NESTED calculation's hint rows are no \\notag, as
an aligned within maths has no numbers to forgo."
  (with-temp-buffer
    (insert math)
    (let ((spans (org--math-hint-spans math)) (next (point-max)))
      ;; The last first, lest rewriting a step move those before it; a string's index i is the buffer's position i + 1.
      (pcase-dolist (`(,rel ,brace ,end) (reverse spans))
        (let ((hint (org--math-hint-styled (org--math-hint (buffer-substring (+ brace 2) end) between) between)))
          (if between
              (let* ((relation (buffer-substring (1+ rel) (1+ brace)))
                     ;; The expression before ends its row, unless a tag or \\ ends it already.
                     (line (save-excursion (goto-char (1+ rel)) (skip-chars-backward " \t\n")
                                           (buffer-substring (line-beginning-position) (point))))
                     (ended (or (string-match-p org--math-tag-re line) (string-match-p "\\\\\\\\\\'" line))))
                (delete-region (1+ rel) (progn (goto-char (1+ end)) (skip-chars-forward " \t\n") (point)))
                (goto-char (1+ rel))
                (insert (if ended "" "\\\\ ") relation " & \\quad " hint (if nested "" " \\notag") " \\\\ & "))
            ;; The step's end, where its hint shows; one before another step ends its row, unless a tag or \\ ends it.
            (goto-char next)
            (skip-chars-backward " \t\n")
            (let* ((line (buffer-substring (line-beginning-position) (point)))
                   (ended (or (string-match org--math-tag-re line) (string-match "\\\\\\\\\\'" line)))
                   (last? (= next (point-max))))
              (when ended (goto-char (+ (line-beginning-position) ended)) (skip-chars-backward " \t"))
              (unless (equal hint "") (insert " && " hint))
              (unless (or ended last?) (insert " \\\\")))
            ;; A step's relation stands apart, by a quad either side, as in a calculational proof.
            (delete-region (1+ brace) (progn (goto-char (1+ end)) (skip-chars-forward " \t") (point)))
            (insert " \\quad ")
            (goto-char (1+ rel))
            (insert "&\\quad ")
            (setq next (1+ rel)))))
      ;; The first expression, after the definitions, is a row of the expressions' column too.
      (when (and between spans)
        (goto-char (point-min))
        (while (and (not (eobp)) (looking-at (concat "[ \t]*$\\|" org--math-definition-re))) (forward-line 1))
        (skip-chars-forward " \t")
        (insert "& ")))
    (buffer-string)))

(defun org--math-match-hint (limit)
  "Font-lock matcher: up to LIMIT, the next hint's “{ HINT }”, as `org--math-hint-spans' reads the block; else nil.
The block begins after its #+begin_math line; or, in its C-c ' buffer, which has none, at the buffer's start."
  (let* ((start (save-excursion (if (re-search-backward "^[ \t]*#\\+begin_math\\b" nil t) (line-beginning-position 2)
                                  (point-min))))
         (span (--first (<= (point) (+ start (nth 1 it)))
                        (org--math-hint-spans (buffer-substring-no-properties start limit)))))
    (when span
      (set-match-data (list (+ start (nth 1 span)) (+ start (nth 2 span))))
      (goto-char (+ start (nth 2 span))))))

(defvar-local org--math-edit-buffer nil
  "Whether this buffer is a math block's C-c ' buffer, all of whose text is the block's contents.")

(defun org--math-block-contents-start ()
  "↦ where the contents of the math block at point start, else nil; in its C-c ' buffer, at the buffer's start."
  (if org--math-edit-buffer (point-min)
    (let ((block (org-special-block-at-point)))
      (and block (equal (downcase (org-element-property :type block)) "math")
           (org-element-property :contents-begin block)))))

(defun org--math-hint-spans-here ()
  "↦ each hint, (REL BRACE END) of `org--math-hint-spans', of the math block at point, as buffer positions; else nil."
  (-when-let (start (org--math-block-contents-start))
    (let ((end (if org--math-edit-buffer (point-max) (org-element-property :contents-end (org-special-block-at-point)))))
      (--map (--map (+ start it) it) (org--math-hint-spans (buffer-substring-no-properties start end))))))

(defun org--math-hint-at-point ()
  "↦ (REL BRACE END) of the hint at point, “REL{ HINT }” of `org--math-hint-spans', as buffer positions; else nil.
Point is at the hint when it is on its relation, or within its braces."
  (--first (and (<= (car it) (point)) (< (point) (nth 2 it))) (org--math-hint-spans-here)))

(defun org--math-hint-fold (brace end)
  "↦ the overlay folding the hint whose { is at BRACE, its } before END, to “{…}”, if it is folded; else nil."
  (--first (overlay-get it 'org-math-hint-fold) (overlays-in (1+ brace) (1- end))))

(defun org--math-fold (brace end)
  "Fold the hint whose { is at BRACE, its } before END, to “{…}”; the text itself is untouched."
  (let ((fold (make-overlay (1+ brace) (1- end))))
    (overlay-put fold 'org-math-hint-fold t)
    (overlay-put fold 'display "…")
    (overlay-put fold 'evaporate t)))

(defun org--math-fold-hint ()
  "When point is at a math block's hint, fold it to “REL{…}”, or unfold it if folded, and ↦ t; else ↦ nil.
A function of `org-tab-first-hook', so TAB folds a hint as it folds a heading; the text itself is untouched."
  (-when-let ((_ brace end) (org--math-hint-at-point))
    (-if-let (fold (org--math-hint-fold brace end)) (delete-overlay fold) (org--math-fold brace end))
    t))

(defun org--math-cycle-hints ()
  "Fold every hint of the math block at point; or, when all are folded already, unfold them all.  C-TAB, there."
  (interactive)
  (let* ((spans (org--math-hint-spans-here))
         (folds (--map (org--math-hint-fold (nth 1 it) (nth 2 it)) spans)))
    (if (and spans (-all-p #'identity folds))
        (progn (mapc #'delete-overlay folds) (message "Every hint shown"))
      (--each spans (unless (org--math-hint-fold (nth 1 it) (nth 2 it)) (org--math-fold (nth 1 it) (nth 2 it))))
      (message "Every hint folded"))))

(defun org--math-fill-hint (&rest _)
  "Within a math block, fill the paragraph at point of the hint at point alone, as prose, and ↦ t; elsewhere in the
block, leave its maths be, and ↦ t, as maths is no prose ---so too a nested calculation, \\[ … \\], within a hint.
Outside a math block, ↦ nil, so Org fills as ever.  A hint's paragraph ends at a blank line, or a nested calculation;
its lines align at its first word."
  (when (org--math-block-contents-start)
    (-when-let ((_ brace end) (org--math-hint-at-point))
      (let* ((hint-start (1+ brace))
             (hint-end (1- end))
             (boundary "^[ \t]*$\\|\\\\\\[\\|\\\\\\]")
             (before (save-excursion (and (re-search-backward boundary hint-start t) (match-string 0))))
             (beg (save-excursion (if (re-search-backward boundary hint-start t) (match-end 0) hint-start)))
             ;; The paragraph's last text; not the newline before a boundary, lest filling join the two.
             (end (save-excursion (goto-char (if (re-search-forward boundary hint-end t) (match-beginning 0) hint-end))
                                  (skip-chars-backward " \t\n")
                                  (point))))
        ;; Within a nested calculation, the nearest boundary before point opens it.
        (unless (equal before "\\[")
          (save-excursion
            (goto-char beg)
            (skip-chars-forward " \t\n" end)
            ;; Its lines align at its first word, not as adaptive filling would guess from them.
            (let ((fill-prefix (make-string (current-column) ?\s)) (adaptive-fill-mode nil))
              (fill-region-as-paragraph (point) end))))))
    t))

(defconst org--math-tagged-re "\\(\\\\tag{\\$\\\\color.*?\\$} \\\\label{[^}]*}\\)"
  "A tag, as `org--math-tags' writes it; its label, from `org--math-label', has no braces.")

(defun org--math-render (raw backend numbered matrix &optional name hints)
  "(RAW, BACKEND, NUMBERED, MATRIX, NAME, HINTS) ↦ (DEFINITIONS . DISPLAY) of a math block's RAW contents; DISPLAY is nil
sans maths.  DISPLAY is aligned at & when its lines have one, else gathered when they end in \\\\, as
`org--math-top-level-marks' sees them; numbered when NUMBERED, and as one equation when it is `once'; and labelled
NAME, tagged so when unnumbered.  Its hints are set `between' its expressions when HINTS is so, else beside; see
`org--math-hints'.  Each table is a MATRIX environment."
  ;; Bound throughout, so hints know the backend as links do.
  (let ((org--math-backend backend))
    (pcase-let* ((`(,definitions . ,math)
                  (org--math-split-definitions
                   (org--math-markup
                    (org--math-tags (org--math-hints (org--math-cases (org--math-tables (org--math-uncomment raw) matrix))
                                                     (eq hints 'between)))
                    backend)))
                 ;; A \\ after the last line would begin an empty, and so numbered, line of its own.
                 (math (replace-regexp-in-string "[ \t]*\\\\\\\\[ \t\n]*\\'" "" math t t))
                 (marks (org--math-top-level-marks math))
                 (lines (cond ((assoc "&" marks) "align") ((assoc "\\\\" marks) "gather"))))
      (cons definitions
            (cond ((equal math "") nil)
                  ;; One equation, one number: the lines split, or gathered, within it; its one tag, else NAME, labels it.
                  ((eq numbered 'once)
                   (let ((tags (mapcar #'cadr (s-match-strings-all org--math-tagged-re math))))
                     (when (cdr tags) (user-error "A math block :numbered once has one number, so one tag; not %d" (length tags)))
                     (format "\\begin{equation}\n%s%s\n\\end{equation}"
                             (if lines (format "\\begin{%s}\n%s\n\\end{%1$s}" (if (equal lines "align") "split" "gathered")
                                               (replace-regexp-in-string (concat "[ \t]*" org--math-tagged-re) "" math t t))
                               (replace-regexp-in-string (concat "[ \t]*" org--math-tagged-re) "" math t t))
                             (cond (tags (concat " " (car tags))) (name (format " \\label{%s}" (org--math-label name)))
                                   (t "")))))
                  (t (let ((math (cond ((not name) math)
                                       (numbered (format "%s \\label{%s}" math (org--math-label name)))
                                       (t (format "%s \\tag{%s} \\label{%s}" math (org--math-tag-text name)
                                                  (org--math-label name)))))
                           (environment (concat (or lines "equation") (unless numbered "*"))))
                       (if (equal environment "equation*") (format "\\[\n%s\n\\]" math)
                         (format "\\begin{%s}\n%s\n\\end{%1$s}" environment math)))))))))

;;;; In Emacs ---a hint's TAB, M-q, and eldoc, in Org and C-c ' buffers alike

(defun org--math-hint-eldoc (callback &rest _)
  "Hand eldoc's CALLBACK what a hint is, how it is written, and how it folds, when point is at one; else nil."
  (when (and (or (derived-mode-p 'org-mode) org--math-edit-buffer) (org--math-hint-at-point))
    (funcall callback "A proof hint, REL{ REASON }, the { hugging REL, a space after it; TAB folds it, C-TAB all hints."
             :thing "Proof hint" :face 'org-math-hint)))

(defun org--math-tab ()
  "Fold, or unfold, the hint at point; elsewhere, do as TAB does sans `org--math-edit-mode'."
  (interactive)
  (unless (org--math-fold-hint)
    (let ((org--math-edit-mode nil)) (call-interactively (key-binding (kbd "TAB"))))))

(defun org--math-fill ()
  "Fill the hint at point, as `org--math-fill-hint' does in Org buffers."
  (interactive)
  (org--math-fill-hint))

(define-minor-mode org--math-edit-mode
  "In a math block's C-c ' buffer: TAB folds a hint, C-TAB all, M-q fills one alone, and eldoc tells of it, as in Org."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "TAB") #'org--math-tab)
            (define-key map (kbd "M-q") #'org--math-fill)
            (define-key map (kbd "C-<tab>") #'org--math-cycle-hints)
            map)
  (setq org--math-edit-buffer org--math-edit-mode)
  (if org--math-edit-mode
      (progn (add-hook 'eldoc-documentation-functions #'org--math-hint-eldoc nil t) (eldoc-mode 1))
    (remove-hook 'eldoc-documentation-functions #'org--math-hint-eldoc t)))

(defun org--math-edit-setup (name)
  "In the C-c ' buffer of a NAME block, a math block's, turn on `org--math-edit-mode'."
  (when (equal (downcase name) "math") (org--math-edit-mode 1)))

;;;; The block

(defun org--math-hints-switch (hints)
  "HINTS, a math block's :hints, ↦ (LAYOUT FORMAT FOLDED): between, beside, or none, as LAYOUT, its FORMAT as the layout
has it; or a list of LAYOUT, then a FORMAT string, or folded, or both.  E.g., none ↦ (beside \"none\" nil), as hidden
hints leave a compact chain of steps; (between folded) ↦ (between nil t)."
  (let ((layout (if (consp hints) (car hints) hints))
        (rest (and (consp hints) (cdr hints))))
    (if (eq layout 'none) (list 'beside "none" nil)
      (list layout (-first #'stringp rest) (and (memq 'folded rest) t)))))

(org-defblock math (&key numbered (matrix "pmatrix") (hints 'between)
                         :link nil :latex-head ("amsmath" "xcolor" "cancel") :html-head (mathjax)
                         :edit-indirect-mode latex-mode :manual "#math"
                         :fontify [:language "latex" :keywords org--math-fontify-keywords])
  "Display maths, wherein our links ---e.g., red:x, [[doc:fold][f]]--- and Org's *markup* are LaTeX's.

  #+begin_math
  # The bounds, declared once; this line is a comment.
  \\def\\xstart{red:a}
  teal:\\bigoplus_{violet:x[fn::the current element] = \\xstart}^{cyan:b} brown:f(violet:x)   :tag-teal: Fold
  #+end_math

Then [[eqref:Fold]] cites the equation, as (Fold) in teal.

+ red:ATOM colours an ATOM: a {group}; \\left(…\\right); a \\command{with}{arguments}, or a sizer with its
  delimiter, \\big(; a word; else one character.  [[red:][a + b]] colours a + b.  So too for any colour,
  [[color:#ff0066][…]], highlight:, eqref:, web links, and tooltip: and doc:, which hover in HTML; see `org-math-links'.
+ ATOM[fn::TIP], or ATOM[[tooltip:TIP]], is a tooltip, as [[tooltip:ATOM][TIP]] is, and as in prose: in HTML, TIP hovers over ATOM, by
  tooltips.js; in LaTeX, TIP is a margin note, its number on ATOM.  ATOM[fn:NAME]'s TIP is its “[fn:NAME] TIP”
  definition, elsewhere in the buffer; so one long tip may serve many atoms.
+ A section link, [[*Heading][f]], [[#custom-id][f]], or [[id:ID][f]], links f to that section, as a graph's node does.
+ *bold* /italic/ _underline_ +cancel+ =texttt= ~mathtt~, when the markers hug their text, as in Org's prose.
+ An Org table is a MATRIX, pmatrix by default; as in Org, a line that begins | is a table, so write \\lvert x \\rvert.
+ A description list, “- VALUE :: CONDITION” lines, is a cases environment; a CONDITION of mere words, such as
  otherwise, is prose.
+ A line ending “:tag-COLOUR: NAME” is tagged NAME, in COLOUR, which [[eqref:NAME]] cites; NUMBERED numbers the rest.
  A block's “#+name: NAME” labels its last line, so [[eqref:NAME]] cites it too; it is tagged NAME when unnumbered.
+ Lines with an & are aligned at it, as in LaTeX's align; else, lines that end in \\\\ are gathered, each centred on
  a line of its own.  Only the block's own & and \\\\ count, not those in a {group} or a \\begin{cases}…\\end{cases}.
+ “X ={ HINT } Y” states why the step holds, as in a calculational proof; so too for any relation, ≤{ … }, \\equiv{ … },
  … The { hugs the relation, and a space or newline follows it, so x ={a} is no hint; in Emacs, a hint shows as a
  comment does, so we see which {…} is one.  A step runs from its hint to the next, wherever the newlines fall, sans
  our writing \\\\.  A hint may span lines, and shows on those lines, a blank one a gap; within it, \\[ … \\] is a nested
  calculation.  TAB on a hint folds it, as a click does in HTML; M-q within it fills it alone, as prose, whereas M-q
  elsewhere in the block leaves the maths be.
+ HINTS sets each hint between the expressions it relates, maroon, after a bracket that opens it, lest it be mistaken
  for maths; beside, at its step's end, plain, as its column sets it apart; none, hidden from the export; or as a list,
  LAYOUT then FORMAT, LaTeX whose %s is the hint, or folded, or both ---e.g., (beside \"{\\color{teal} %s}\" folded).
  Folded hints start as [⋯] in HTML, so a reader clicks to see each, as a puzzle; LaTeX, sans clicks, shows them.
  To set hints beside throughout a file: #+property: header-args:math :hints beside.
+ NUMBERED is t, to number each line; or once, to number the lines as one equation, split at their &s; its one tag,
  else its #+name:, labels it.
+ Inline maths, $red:x$ or \\(red:x\\), reads our links and Org's markup too.
+ Unicode, such as ∀ x ∈ ℕ, typesets in HTML, but in LaTeX only with #+latex_compiler: lualatex and
  #+latex_header: \\usepackage{unicode-math}; else, its symbols are dropped, silently.
+ A line “# …” is a comment.  A line that begins \\def, \\let, \\newcommand, \\renewcommand, or \\DeclareMathOperator
  begins a definition, through the line that closes its braces: declared, but not shown.  In LaTeX, it goes to the
  preamble; in HTML, MathJax reads it hidden, and then knows it page-wide.
+ In Markdown, the block is a fenced math block, its definitions within, since GitHub forgets them between blocks.
+ In Emacs, C-c C-c shows the maths as an image atop the block, until C-c C-c again, or an edit; C-c ' edits the
  block in `latex-mode', its hints, tags, and colours shown as in the Org buffer.  C-c C-x C-l, which shows Org's own maths as images, shows math blocks too."
  (pcase-let ((`(,layout ,format ,folded) (org--math-hints-switch hints)))
    (let ((org--math-tips nil) (org--math-hint-format format) (org--math-hint-folded folded))
      (pcase-let ((`(,definitions . ,shown) (org--math-render raw-contents backend numbered matrix org--handler-anchor layout)))
        (cond ((org-export-markdown-p)
               ;; A fenced math block is displayed already, so \[…\] would be one display too many.
               (let ((shown (and shown (string-remove-suffix "\n\\]" (string-remove-prefix "\\[\n" shown)))))
                 (format "```math\n%s\n```" (string-join (append definitions (and shown (list shown))) "\n"))))
              ((equal backend 'latex)
               (concat (when definitions
                         (org-parse (mapconcat (lambda (d) (replace-regexp-in-string "^" "#+latex_header: " d))
                                               definitions "\n")))
                       shown
                       ;; The margin notes of the maths' tooltips, after it, as maths holds no \marginpar.
                       (string-join (reverse org--math-tips))))
              ;; MathJax reads the page's text, wherein a < would begin an HTML tag.
              (t (when org--math-tips (org-html-head-require 'org--tooltip-html-head))
                 ;; Hints fold on a click, by their tip and script; see `org--math-hint-styled'.
                 (when (and shown (string-match-p "osbe-hint" shown))
                   (org-html-head-require 'org--tooltip-html-head)
                   (org-html-head-require 'org--math-hint-html-head))
                 (concat (when definitions
                           (format "<p style=\"display:none\">\\[%s\\]</p>\n"
                                   (org-html-encode-plain-text (string-join definitions "\n"))))
                         (and shown (org-html-encode-plain-text shown))
                         ;; The tips of the maths' tooltips, beside it, since MathJax keeps no <template>.
                         (mapconcat (lambda (tip) (concat "\n" tip)) (reverse org--math-tips) ""))))))))

;;;; eqref ---citing a tagged line

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

;;;; C-c C-c ---the maths, as an image atop its block

(defun org--math-preview (raw numbered matrix &optional hints)
  "Show the RAW maths of the block at point, NUMBERED and with MATRIX tables, as an image atop the block; if one is
already shown, remove it instead.  An edit to the block, or C-c C-x C-l, removes it too."
  (let* ((block (org-special-block-at-point))
         (beg (org-element-property :post-affiliated block))
         (end (save-excursion (goto-char (org-element-property :end block)) (skip-chars-backward " \t\n") (point)))
         (shown (--first (overlay-get it 'org-math-preview) (overlays-in beg end))))
    (if shown (delete-overlay shown)
      (pcase-let* ((`(,definitions . ,math) (let (org--math-tips) (org--math-render raw 'latex numbered matrix nil hints)))
                   (png (make-temp-file "org-math-" nil ".png"))
                   (org-latex-packages-alist (append '(("" "xcolor") ("" "cancel")) org-latex-packages-alist))
                   (overlay (make-overlay beg end)))
        (unless math (user-error "This math block shows no maths, only definitions"))
        (org-create-formula-image (string-join (append definitions (list math)) "\n") png org-format-latex-options
                                  (current-buffer))
        (overlay-put overlay 'org-math-preview t)
        (overlay-put overlay 'org-overlay-type 'org-latex-overlay)
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'modification-hooks (list (lambda (o after &rest _) (when after (delete-overlay o)))))
        (overlay-put overlay 'display (create-image png 'png nil :ascent 'center))))
    t))

(org-def-action math (&key numbered (matrix "pmatrix") (hints 'between))
  "Show the maths as an image atop the block; again, to remove it.  See `org--math-preview'."
  (pcase-let ((`(,layout ,format) (org--math-hints-switch hints)))
    (let ((org--math-hint-format format))
      (org--math-preview raw-contents numbered matrix layout))))

(defun org--math-preview-region (beg end)
  "Show each math block from BEG to END as an image atop it, as Org's C-c C-x C-l shows its own maths there; a block
shown already is left be.  Org's own images of the maths within a block, such as its \\begin{cases}, are cleared."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^[ \t]*#\\+begin_math\\b" end t)
      (unless (--first (overlay-get it 'org-math-preview) (overlays-at (point)))
        (let ((block-end (org-element-property :end (org-special-block-at-point))))
          (org-clear-latex-preview (line-beginning-position) block-end)
          ;; A block of only definitions shows nothing, which is no reason to stop showing the rest.
          (ignore-error user-error (org--block-action-call #'org-action/math 'main-arg))
          (goto-char block-end))))))

;;;; Inline maths ---$…$ reads our markup too

(defun org--math-inline (transcode fragment contents info)
  "Advice around a latex-fragment's TRANSCODE, of FRAGMENT, CONTENTS, and INFO: inline maths, such as $red:x$ or
\\(red:x\\), reads our links and Org's markup, as a math block does, its tooltips' tips following it.  Only maths:
a bare LaTeX command in prose, such as \\ref{sec:red:x}, Org also calls a fragment, yet is no maths, so is left be.
Org's LaTeX transcoder strips the delimiters, then adds its own about the result; HTML's keeps them."
  (let ((value (org-element-property :value fragment)))
    (if (not (string-match "\\`\\(\\$\\$?\\|\\\\[[(]\\)\\(\\(?:.\\|\n\\)*\\)\\(\\$\\$?\\|\\\\[])]\\)\\'" value))
        (funcall transcode fragment contents info)
      (let* ((open (match-string 1 value)) (math (match-string 2 value)) (close (match-string 3 value))
             (backend (if (org-export-derived-backend-p (plist-get info :back-end) 'latex) 'latex 'html))
             (org--math-tips nil)
             (marked (concat open (org--math-markup math backend) close))
             (out (funcall transcode (org-element-put-property (org-element-copy fragment) :value marked) contents info)))
        (cond ((null org--math-tips) out)
              ((eq backend 'html)
               (org-html-head-require 'org--tooltip-html-head)
               (concat out (string-join (reverse org--math-tips))))
              ;; Maths holds no \\marginpar, and Org closes these maths itself; so we close them for the notes, then reopen.
              (t (concat out "\\)" (string-join (reverse org--math-tips)) "\\(")))))))

(provide 'org-math-block)
;;; org-math-block.el ends here
