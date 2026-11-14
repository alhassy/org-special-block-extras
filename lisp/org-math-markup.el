;;; org-math-markup.el --- Maths that reads our links, tooltips, and Org's markup  -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.11.14
;; Package-Requires: ((dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A math block's maths, and inline maths, read our links and Org's markup as LaTeX; by way of example,
;;
;;     red:x + [[tooltip:y][the unknown]] + *z* + w[fn::a tip]   ↦   {\color{red}x} + \class{tooltip …}{y} + \boldsymbol{z} + …
;;
;; An ATOM ---a {group}, a \command{with}{arguments}, a word, else one character--- is what red:ATOM colours and what
;; ATOM[fn::TIP] hovers TIP over.  See `org--math-markup', and `org--math-inline' for $…$ in prose.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'org-defblock)
(require 'org-markup-links)

(declare-function org--tooltip-template "org-links")
(declare-function org--latex-margin-note "org-margin-blocks")

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

;;;; Links and tooltips ---colours, doc:, tooltip:, and the like, as LaTeX

(defvar org--math-backend nil "The backend a math block is exporting to, for the links that differ by it.")

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
    ('latex (let ((note (string-trim (org--export-nested tip 'latex))))
              (push (if (org-export-beamer-p) (format "\\stepcounter{footnote}\\footnotetext{%s}" note)
                      (org--latex-margin-note "{\\thefootnote}" "\\marginparwidth" "gray!80" note
                                              "\\refstepcounter{footnote}" t))
                    org--math-tips)
              ;; The number its note steps to, read sans stepping, as amsmath may typeset the maths twice.
              (format "{%s}^{\\textnormal{\\the\\numexpr\\value{footnote}+%d\\relax}}" math (length org--math-tips))))
    (_ math)))

(defun org--math-meaning (meaning shown)
  "Returns SHOWN, LaTeX maths, rendered as MEANING says, a plist of `org-markup-links'; e.g., (:color \"red\") and
“x” give {\\color{red}x}.  A glossary's tip shows only in HTML, as LaTeX lists the glossary at the end."
  (-let [(&plist :color :background :url :tip :plain :glossary) meaning]
    (--> shown
         (if color (format "{\\color%s%s}" (org--math-color color) it) it)
         (if background (format "\\colorbox{%s}{$%s$}" background it) it)
         (if url (format "\\href{%s}{%s}" url it) it)
         (if (and tip (not (and glossary (eq org--math-backend 'latex)))) (org--math-tip it tip plain) it))))

(defun org--math-link (type path desc)
  "Returns the LaTeX of a TYPE link of `org-markup-links' to PATH, described by DESC, in maths, as that list says."
  (let ((meaning (org-markup-link-meaning type path desc)))
    (org--math-meaning meaning (or (plist-get meaning :text) desc path))))

;;;; Emphasis ---Org's markers, as maths' commands, or text's within a \text{…}

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

(defvar org--math-plain-link-cache nil
  "(LINKS . REGEXP): the regexp `org--math-plain-link-re' last made, and the `org-markup-links' it was made of.")

(defun org--math-plain-link-re ()
  "Returns the regexp of a TYPE: of `org-markup-links', group 1 its TYPE, made anew only when that list changes.
A web link's path is no maths, so it is only ever bracketed, [[https://…][…]]; whence https:/ is no TYPE:ATOM."
  (unless (eq (car org--math-plain-link-cache) org-markup-links)
    (setq org--math-plain-link-cache
          (cons org-markup-links
                (format "\\_<\\(%s\\):"
                        (regexp-opt (--remove (member it '("http" "https")) (mapcar #'car org-markup-links)))))))
  (cdr org--math-plain-link-cache))

(defun org--math-match-plain-link (limit)
  "Font-lock matcher: up to LIMIT, the next TYPE:ATOM of `org-markup-links', group 1 its TYPE; else nil."
  (let ((re (org--math-plain-link-re)) found)
    (while (and (not found) (with-syntax-table org--math-syntax (re-search-forward re limit t)))
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
          (insert (org--math-link type atom nil)))))))

(defun org--math-section-link (link desc)
  "(LINK, DESC) ↦ DESC, linking to the section LINK names, as a graph's node does; e.g., “*Folds”, “f” ↦ \\href{*Folds}{f}
in HTML, \\hyperref[*Folds]{f} in LaTeX, which `org--resolve-section-links' points at the section once it lands.  Sans
DESC, the heading shows, as text; Markdown's maths has no links, so there DESC alone."
  (let ((desc (or desc (format "\\text{%s}" (replace-regexp-in-string org--section-link-re "" link)))))
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
               (type (car (split-string link ":"))) (path (string-remove-prefix (concat type ":") link)))
          (delete-region start end)
          (goto-char start)
          (save-excursion (insert (cond ((string-match-p org--section-link-re link) (org--math-section-link link desc))
                                        ((assoc type org-markup-links) (org--math-link type path desc))
                                        (t (or desc link)))))))
      (goto-char (point-min))
      (org--math-plain-links)
      ;; A link in a hint's prose is maths within it, $…$, as MathJax reads no macro in \text{…} sans its textmacros; its
      ;; words stay text.  E.g., “\text{by \href{#a}{its law}}” ↦ “\text{by $\href{#a}{\text{its law}}$}”.
      (goto-char (point-min))
      (while (re-search-forward "\\\\\\(eqref\\|href\\|hyperref\\|hyperlink\\)\\([{[]\\)" nil t)
        (let ((start (match-beginning 0)) (eqref? (equal (match-string 1) "eqref")))
          ;; A save-excursion, as parsing up to START moves point there.
          (when (save-excursion (org--math-text-p start))
            (with-syntax-table org--math-syntax
              (let* ((target (progn (goto-char (match-beginning 2)) (forward-sexp) (buffer-substring start (point))))
                     (desc (unless eqref? (buffer-substring (1+ (point)) (progn (forward-sexp) (1- (point)))))))
                (delete-region start (point))
                (insert "$" target (cond ((not desc) "") ((string-prefix-p "\\text{" desc) (format "{%s}" desc))
                                                (t (format "{\\text{%s}}" desc)))
                        "$"))))))
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

;;;; In Emacs ---a TYPE:ATOM link shows in its colour, as the maths reads it

(defun org--math-link-type-face (type path)
  "(TYPE, PATH) ↦ the face Org shows a TYPE:PATH link in, by TYPE's :face; e.g., red ↦ (:foreground \"red\")."
  (let ((face (org-link-get-parameter type :face)))
    (cond ((functionp face) (funcall face path)) (face) (t 'org-link))))

(defun org--math-link-face ()
  "↦ The face of the TYPE:ATOM just matched: TYPE's own, as [[red:…]] shows red in prose."
  (org--math-link-type-face (match-string 1) (buffer-substring (1+ (match-end 1)) (match-end 0))))

(defun org--math-unlink (limit)
  "Font-lock matcher that matches nothing: up to LIMIT, it clears Org's look of each plain TYPE: link of
`org-markup-links', since Org reads red:a}^{cyan:b as one link; `org--math-match-plain-link' then shows the maths' own.
Org prepended the link's face to those beneath, LaTeX's, which we keep."
  (while (re-search-forward org-link-plain-re limit t)
    (-when-let* ((type (car (assoc (match-string 1) org-markup-links)))
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

;;;; The tips that follow maths

(defun org--math-tips-after (backend)
  "Returns what follows maths for its tooltips, `org--math-tips', along BACKEND: in HTML, their <template>s, since
MathJax keeps none within; in LaTeX, their margin notes, since maths holds no \\marginpar."
  (cond ((null org--math-tips) "")
        ((eq backend 'latex) (string-join (reverse org--math-tips)))
        (t (org-html-head-require 'org--tooltip-html-head)
           (mapconcat (lambda (tip) (concat "\n" tip)) (reverse org--math-tips) ""))))

;;;; Inline maths ---$…$ reads our markup too

(defconst org--math-delimited-re "\\`\\(\\$\\$?\\|\\\\[[(]\\)\\(\\(?:.\\|\n\\)*\\)\\(\\$\\$?\\|\\\\[])]\\)\\'"
  "Maths with its delimiters: $…$, $$…$$, \\(…\\), or \\[…\\]; group 1 opens it, group 2 is its inside, and group 3
closes it.  A bare LaTeX command, such as \\ref{x}, Org also calls a fragment, yet matches not.")

(defun org--math-inline (transcode fragment contents info)
  "Advice around a latex-fragment's TRANSCODE, of FRAGMENT, CONTENTS, and INFO: inline maths, such as $red:x$ or
\\(red:x\\), reads our links and Org's markup, as a math block does, its tooltips' tips following it.  Only maths:
a bare LaTeX command in prose, such as \\ref{sec:red:x}, Org also calls a fragment, yet is no maths, so is left be.
Org's LaTeX transcoder strips the delimiters, then adds its own about the result; HTML's keeps them."
  (let ((value (org-element-property :value fragment)))
    (if (not (string-match org--math-delimited-re value))
        (funcall transcode fragment contents info)
      (let* ((open (match-string 1 value)) (math (match-string 2 value)) (close (match-string 3 value))
             (backend (if (org-export-derived-backend-p (plist-get info :back-end) 'latex) 'latex 'html))
             (org--math-tips nil)
             (marked (concat open (org--math-markup math backend) close))
             (out (funcall transcode (org-element-put-property (org-element-copy fragment) :value marked) contents info)))
        ;; Maths holds no \\marginpar, and Org closes these maths itself; so we close them for LaTeX's notes, then
        ;; reopen.
        (if (and org--math-tips (eq backend 'latex)) (concat out "\\)" (org--math-tips-after backend) "\\(")
          (concat out (org--math-tips-after backend)))))))

(provide 'org-math-markup)
;;; org-math-markup.el ends here
