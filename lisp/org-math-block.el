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
   `(("color" . ,(lambda (path desc)
                   (format "{\\color%s%s}"
                           (if (string-prefix-p "#" path) (format "[HTML]{%s}" (substring path 1)) (format "{%s}" path))
                           desc)))
     ("highlight" . ,(lambda (path desc) (format "\\colorbox{%s}{$%s$}" (or (org-string-nw-p path) "yellow") desc)))
     ;; Sans description, it has no tip; unless it follows an atom, which `org--math-footnotes' sees to.
     ("tooltip" . ,(lambda (path desc) (if desc (org--math-tip path desc) path)))
     ("doc" . ,(lambda (path desc)
                 (-let [(name docs) (org-docs-get path)]
                   ;; As a doc: link in prose, for the glossary that show:GLOSSARY lists; in LaTeX, that is all.
                   (add-to-list 'org--docs-actually-used (list path name docs))
                   (if (eq org--math-backend 'latex) (or desc name)
                     (org--math-tip (or desc name) docs (org--docs-plain-p path))))))
     ("eqref" . ,(lambda (path _) (format "\\eqref{%s}" path)))
     ("https" . ,(lambda (path desc) (format "\\href{https:%s}{%s}" path (or desc path))))
     ("http" . ,(lambda (path desc) (format "\\href{http:%s}{%s}" path (or desc path))))))
  "Link types that maths knows, each TYPE ↦ (λ PATH DESC ↦ LaTeX); DESC is nil for [[TYPE:PATH]] and TYPE:PATH.
An unknown TYPE shows its DESC, else its PATH.")

(defconst org--math-emphasis
  '(("*" . "\\boldsymbol{%s}") ("/" . "\\mathit{%s}") ("_" . "\\underline{%s}")
    ("+" . "\\cancel{%s}") ("=" . "\\texttt{%s}") ("~" . "\\mathtt{%s}"))
  "Org's emphasis markers, each ↦ its LaTeX in maths.")

;;;; Atoms ---what red:ATOM colours, and what an ATOM[fn::…] tip hovers over

(defconst org--math-syntax
  (let ((s (make-syntax-table)))
    (modify-syntax-entry ?{ "(}" s)
    (modify-syntax-entry ?} "){" s)
    s)
  "Braces are parentheses, so a {group} is a sexp; as in the standard table, \\ escapes, so \\{ is no brace.")

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

(defun org--math-footnotes ()
  "Make each ATOM[fn::TIP], or ATOM[[tooltip:TIP]], in the buffer a hover of TIP over ATOM, as a graph's node footnote
is its tooltip; e.g., x[fn::the current element].  A [[tooltip:TIP]] sans description has no marker of its own, so
it marks the ATOM before it."
  (goto-char (point-min))
  (while (re-search-forward "\\[fn::\\|\\[\\[tooltip:\\([^]]*\\)\\]\\]" nil t)
    (let* ((fn-start (match-beginning 0))
           (link-tip (match-string 1))
           (tip-end (if link-tip (match-end 0)
                      (save-excursion (goto-char fn-start) (with-syntax-table org--math-syntax (forward-sexp)) (point))))
           (tip (string-trim (or link-tip (buffer-substring (point) (1- tip-end)))))
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
    (while (and (not found) (re-search-forward (format "\\_<\\(%s\\):" types) limit t))
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

(defun org--math-markup (math backend)
  "(MATH, BACKEND) ↦ MATH in LaTeX, sans Org's footnotes, links, and emphasis; e.g., “red:x + *y*” ↦ “{\\color{red}x}
+ \\boldsymbol{y}”."
  (let ((org--math-backend backend))
    (with-temp-buffer
      (insert math)
      (org--math-footnotes)
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

;;;; Structure ---comments, tables, tags, and definitions

(defun org--math-uncomment (math)
  "MATH ↦ MATH sans its Org comment lines, “# …”, which TeX would read as a macro's parameter."
  (replace-regexp-in-string "^[ \t]*#\\(?:[ \t].*\\)?\\(?:\n\\|\\'\\)" "" math t t))

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

(defconst org--math-tag-re ":tag-\\([^: \t\n]+\\):[ \t]*\\(.*?\\)[ \t]*$"
  "A line's ending “:tag-COLOUR: NAME”.")

(defun org--math-tags (math)
  "MATH ↦ MATH whose each line ending in “:tag-COLOUR: NAME” ends instead in NAME as a COLOUR \\tag, labelled NAME,
and \\\; e.g., “x :tag-blue: Id” ↦ “x \\tag{$\\color{blue}{\\text{Id}}$} \\label{Id} \\\\”."
  (replace-regexp-in-string org--math-tag-re
                            (lambda (line)
                              (string-match org--math-tag-re line)
                              (format "\\tag{$\\color{%s}{\\text{%s}}$} \\label{%2$s} \\\\"
                                      (match-string 1 line) (match-string 2 line)))
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

(defconst org--math-fontify-keywords
  '((org--math-unlink 0 nil)
    (":tag-\\([^: \t\n]+\\):" 0 org--math-tag-face)
    (org--math-match-plain-link 0 org--math-link-face)
    ;; Each & is where lines may align; an escaped \& is a mere ampersand.
    ("\\(?:^\\|[^\\]\\)\\(&\\)" 1 font-lock-keyword-face)
    ;; A hint shows grey, so we see at a glance which {…} the export reads as one.
    (org--math-match-hint 0 shadow))
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
  "MATH ↦ each line's first top-level hint, “REL{ HINT }”, as (REL BRACE END): where REL starts, where its { starts, and
where its } ends; e.g., “x ={ by IH } y” ↦ ((2 3 12)).  The { must hug REL, and a space must follow it; so “x ={a}”,
“^{ 2 }”, and “\\frac{ a }” are no hints, nor is one with an unclosed {, as while we type it."
  (with-temp-buffer
    (insert math)
    (let (spans lines)
      (pcase-dolist (`(,mark . ,at) (org--math-top-level-marks math))
        (goto-char (1+ at))
        (when (and (equal mark "{") (looking-at "{[ \t]") (not (memq (line-number-at-pos) lines))
                   (looking-back org--math-relation-re (line-beginning-position) t))
          (let ((rel (1- (match-beginning 0)))
                (end (ignore-errors (with-syntax-table org--math-syntax (forward-sexp) (1- (point))))))
            (when end
              (push (line-number-at-pos) lines)
              (push (list rel at end) spans)))))
      (nreverse spans))))

(defun org--math-hints (math)
  "MATH ↦ MATH whose each hint, “REL{ HINT }” of `org--math-hint-spans', is “&REL”, its HINT at the line's end, before
its tag or \\\\; e.g., “x ={ by IH } y” ↦ “x &= y && \\text{by IH}”."
  (with-temp-buffer
    (insert math)
    ;; The last first, lest rewriting a hint move those after it; a string's index i is the buffer's position i + 1.
    (pcase-dolist (`(,rel ,brace ,end) (reverse (org--math-hint-spans math)))
      (let ((hint (string-trim (buffer-substring (+ brace 2) end))))
        (delete-region (1+ brace) (progn (goto-char (1+ end)) (skip-chars-forward " \t") (point)))
        (insert " ")
        (goto-char (1+ rel))
        (insert "&")
        (beginning-of-line)
        (re-search-forward "[ \t]*\\(?::tag-[^: \t\n]+:.*\\|\\\\\\\\[ \t]*\\)?$" (line-end-position))
        (goto-char (match-beginning 0))
        (insert (format " && \\text{%s}" hint))))
    (buffer-string)))

(defun org--math-match-hint (limit)
  "Font-lock matcher: up to LIMIT, the next hint's “{ HINT }”, as `org--math-hint-spans' reads the block; else nil."
  (let* ((start (save-excursion (re-search-backward "^[ \t]*#\\+begin_math\\b") (forward-line 1) (point)))
         (span (--first (<= (point) (+ start (nth 1 it)))
                        (org--math-hint-spans (buffer-substring-no-properties start limit)))))
    (when span
      (set-match-data (list (+ start (nth 1 span)) (+ start (nth 2 span))))
      (goto-char (+ start (nth 2 span))))))

(defconst org--math-tagged-re "\\(\\\\tag{\\$\\\\color{[^}]*}{\\\\text{[^}]*}}\\$} \\\\label{[^}]*}\\)"
  "A tag, as `org--math-tags' writes it.")

(defun org--math-render (raw backend numbered matrix &optional name)
  "(RAW, BACKEND, NUMBERED, MATRIX, NAME) ↦ (DEFINITIONS . DISPLAY) of a math block's RAW contents; DISPLAY is nil sans
maths.  DISPLAY is aligned at & when its lines have one, else gathered when they end in \\\\, as
`org--math-top-level-marks' sees them; numbered when NUMBERED, and as one equation when it is `once'; and labelled
NAME, tagged so when unnumbered.  Each table is a MATRIX environment."
  (pcase-let* ((`(,definitions . ,math)
                (org--math-split-definitions
                 (org--math-markup (org--math-tags (org--math-hints (org--math-tables (org--math-uncomment raw) matrix)))
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
                           (cond (tags (concat " " (car tags))) (name (format " \\label{%s}" name)) (t "")))))
                (t (let ((math (cond ((not name) math)
                                     (numbered (format "%s \\label{%s}" math name))
                                     (t (format "%s \\tag{%s} \\label{%2$s}" math name))))
                         (environment (concat (or lines "equation") (unless numbered "*"))))
                     (if (equal environment "equation*") (format "\\[\n%s\n\\]" math)
                       (format "\\begin{%s}\n%s\n\\end{%1$s}" environment math))))))))

;;;; The block

(org-defblock math (&key numbered (matrix "pmatrix")
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
  tooltips.js; in LaTeX, TIP is a margin note, its number on ATOM.
+ *bold* /italic/ _underline_ +cancel+ =texttt= ~mathtt~, when the markers hug their text, as in Org's prose.
+ An Org table is a MATRIX, pmatrix by default; as in Org, a line that begins | is a table, so write \\lvert x \\rvert.
+ A line ending “:tag-COLOUR: NAME” is tagged NAME, in COLOUR, which [[eqref:NAME]] cites; NUMBERED numbers the rest.
  A block's “#+name: NAME” labels its last line, so [[eqref:NAME]] cites it too; it is tagged NAME when unnumbered.
+ Lines with an & are aligned at it, as in LaTeX's align; else, lines that end in \\\\ are gathered, each centred on
  a line of its own.  Only the block's own & and \\\\ count, not those in a {group} or a \\begin{cases}…\\end{cases}.
+ “X ={ HINT } Y” aligns its line at =, and shows HINT at the line's end, as in a calculational proof; so too for any
  relation, ≤{ … }, \\equiv{ … }, … The { hugs the relation, and a space follows it, so x ={a} is no hint; in
  Emacs, a hint shows grey, so we see which {…} is one.
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
  block in `latex-mode'.  C-c C-x C-l, which shows Org's own maths as images, shows math blocks too."
  (let ((org--math-tips nil))
    (pcase-let ((`(,definitions . ,shown) (org--math-render raw-contents backend numbered matrix org--handler-anchor)))
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
               (concat (when definitions
                         (format "<p style=\"display:none\">\\[%s\\]</p>\n"
                                 (org-html-encode-plain-text (string-join definitions "\n"))))
                       (and shown (org-html-encode-plain-text shown))
                       ;; The tips of the maths' tooltips, beside it, since MathJax keeps no <template>.
                       (mapconcat (lambda (tip) (concat "\n" tip)) (reverse org--math-tips) "")))))))

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
                   (format "\\eqref{%s}" o-label))))

;;;; C-c C-c ---the maths, as an image atop its block

(defun org--math-preview (raw numbered matrix)
  "Show the RAW maths of the block at point, NUMBERED and with MATRIX tables, as an image atop the block; if one is
already shown, remove it instead.  An edit to the block, or C-c C-x C-l, removes it too."
  (let* ((block (org-special-block-at-point))
         (beg (org-element-property :post-affiliated block))
         (end (save-excursion (goto-char (org-element-property :end block)) (skip-chars-backward " \t\n") (point)))
         (shown (--first (overlay-get it 'org-math-preview) (overlays-in beg end))))
    (if shown (delete-overlay shown)
      (pcase-let* ((`(,definitions . ,math) (let (org--math-tips) (org--math-render raw 'latex numbered matrix)))
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

(org-def-action math (&key numbered (matrix "pmatrix"))
  "Show the maths as an image atop the block; again, to remove it.  See `org--math-preview'."
  (org--math-preview raw-contents numbered matrix))

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

(defun org--math-inline (fragment backend _info)
  "(FRAGMENT, BACKEND) ↦ the inline maths FRAGMENT, such as $red:x$ or \\(red:x\\), wherein our links and Org's markup
are LaTeX's, as in a math block; followed by its tooltips' tips, as a math block's are.  Org hands LaTeX's maths over
sans its delimiters, which it adds afterwards; HTML's, with them."
  (let* ((delimited? (string-match "\\`\\(\\$\\$?\\|\\\\[[(]\\)\\(\\(?:.\\|\n\\)*\\)\\(\\$\\$?\\|\\\\[])]\\)\\'" fragment))
         (open (if delimited? (match-string 1 fragment) ""))
         (close (if delimited? (match-string 3 fragment) ""))
         (math (if delimited? (match-string 2 fragment) fragment))
         (backend (if (org-export-derived-backend-p backend 'latex) 'latex 'html))
         (org--math-tips nil)
         (math (org--math-markup math backend)))
    (cond ((null org--math-tips) (concat open math close))
          ((eq backend 'html) (org-html-head-require 'org--tooltip-html-head)
           (concat open math close (string-join (reverse org--math-tips))))
          ;; Maths holds no \\marginpar, and Org closes these maths itself; so we close them for the notes, then reopen.
          (t (concat math "\\)" (string-join (reverse org--math-tips)) "\\(")))))

(provide 'org-math-block)
;;; org-math-block.el ends here
