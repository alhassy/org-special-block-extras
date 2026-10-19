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
;;     teal:\bigoplus_{violet:x[fn::the current element] = \xstart}^{cyan:b} brown:f(violet:x)   teal:<<Fold>>
;;     #+end_math
;;
;; colours ⊕ teal, x violet, a red, and b cyan, as red:𝒳 colours prose; hovering over x shows its footnote; the
;; equation is tagged “(Fold)”, in teal, which [[Fold]] cites.  The \def line is a definition, declared but not
;; shown: in LaTeX, in the preamble; in HTML, for MathJax, hidden.  An Org table is a matrix; lines that end in \\ are
;; gathered; lines with an & are aligned at it, as in a textbook derivation.  In Emacs, C-c C-c shows the maths as an
;; image atop the block, and C-c ' edits it in `latex-mode'.  See `org-block/math'.
;;
;; The block is assembled here from its parts: org-math-markup.el reads our links and Org's markup; org-math-structure.el
;; its comments, matrices, cases, tags, and definitions; org-math-hints.el a calculation's hints; and org-math-refs.el
;; citations of a tagged line.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'org-defblock)
(require 'org-math-markup)
(require 'org-math-structure)
(require 'org-math-hints)
(require 'org-math-refs)

(declare-function org--block-action-call "org-ctrl-c-ctrl-c-for-special-block")
(defvar org--handler-anchor)

;;;; The block

(defconst org--math-fontify-keywords
  '((org--math-unlink 0 nil)
    ;; A tag, “COLOUR:<<NAME>>”, shows as Org's target, in COLOUR.
    (org--math-match-tag 0 org--math-tag-face)
    (org--math-match-plain-link 0 org--math-link-face)
    ;; Each & is where lines may align; an escaped \& is a mere ampersand.
    ("\\(?:^\\|[^\\]\\)\\(&\\)" 1 font-lock-keyword-face)
    ;; A hint shows as a comment, so we see at a glance which {…} the export reads as one.
    (org--math-match-hint 0 org-math-hint))
  "How a math block's tags, TYPE:ATOM links, &s, and hints look in Org buffers; see `org--fontify-blocks'.")

(defun org--math-render (raw backend numbered matrix &optional name hints environment)
  "(RAW, BACKEND, NUMBERED, MATRIX, NAME, HINTS) ↦ (DEFINITIONS . DISPLAY) of a math block's RAW contents; DISPLAY is nil
sans maths.  DISPLAY is aligned at & when its lines have one, else gathered when they end in \\\\, as
`org--math-top-level-marks' sees them; numbered when NUMBERED, and as one equation when it is `once'; and labelled
NAME, tagged so when unnumbered.  Its hints are set `between' its expressions when HINTS is so, else beside; see
`org--math-hints'.  Each table is a MATRIX environment.  An ENVIRONMENT, such as multline* or alignat*{2}, overrules
all that its lines imply; sans star, it is numbered."
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
                  ;; An environment of our own: its name, before any {arguments}, closes it; sans star, it is numbered.
                  (environment
                   (let* ((environment (format "%s" environment))
                          (env-name (car (split-string environment "{")))
                          (numbered (not (string-suffix-p "*" env-name)))
                          (math (cond ((not name) math)
                                      (numbered (format "%s \\label{%s}" math (org--math-label name)))
                                      (t (format "%s \\tag{%s} \\label{%s}" math (org--math-tag-text name)
                                                 (org--math-label name))))))
                     (format "\\begin{%s}%s\n%s\n\\end{%1$s}" env-name (substring environment (length env-name)) math)))
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

(org-defblock math (&key numbered (matrix "pmatrix") (hints 'between) environment
                         :link nil :latex-head ("amsmath" "xcolor" "cancel") :html-head (mathjax)
                         :edit-indirect-mode latex-mode :manual "#math"
                         :fontify [:language "latex" :keywords org--math-fontify-keywords])
  "Display maths, wherein our links ---e.g., red:x, [[doc:fold][f]]--- and Org's *markup* are LaTeX's.

  #+begin_math
  # The bounds, declared once; this line is a comment.
  \\def\\xstart{red:a}
  teal:\\bigoplus_{violet:x[fn::the current element] = \\xstart}^{cyan:b} brown:f(violet:x)   teal:<<Fold>>
  #+end_math

Then [[Fold]] cites the equation, as (Fold) in teal; [[Fold][these words]] link to it.

+ red:ATOM colours an ATOM: a {group}; \\left(…\\right); a \\command{with}{arguments}, or a sizer with its
  delimiter, \\big(; a word; else one character.  [[red:][a + b]] colours a + b.  So too for any colour,
  [[color:#ff0066][…]], highlight:, web links, and tooltip: and doc:, which hover in HTML; see `org-math-links'.
+ ATOM[fn::TIP], or ATOM[[tooltip:TIP]], is a tooltip, as [[tooltip:ATOM][TIP]] is, and as in prose: in HTML, TIP hovers over ATOM, by
  tooltips.js; in LaTeX, TIP is a margin note, its number on ATOM.  ATOM[fn:NAME]'s TIP is its “[fn:NAME] TIP”
  definition, elsewhere in the buffer; so one long tip may serve many atoms.
+ A section link, [[*Heading][f]], [[#custom-id][f]], or [[id:ID][f]], links f to that section, as a graph's node does.
+ *bold* /italic/ _underline_ +cancel+ =texttt= ~mathtt~, when the markers hug their text, as in Org's prose.
+ An Org table is a MATRIX, pmatrix by default; as in Org, a line that begins | is a table, so write \\lvert x \\rvert.
+ A description list, “- VALUE :: CONDITION” lines, is a cases environment; a CONDITION of mere words, such as
  otherwise, is prose.
+ A line ending “COLOUR:<<NAME>>”, Org's own target, is tagged NAME, in COLOUR, which [[NAME]] cites, as Org cites
  a target, and C-c C-o on it visits; sans COLOUR, “<<NAME>>” is plain.  NAME may hold maths, as \\meet, ⊑, or a
  subscript, as <<Base Case_1>>.  A hint's \\[ … \\] may be tagged too: “\\[ 0 + n = n teal:<<Base Case>> \\]”.
  NUMBERED numbers the rest.  A block's “#+name: NAME” labels its last line, so [[NAME]] cites it too; it is
  tagged NAME when unnumbered.
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
+ ENVIRONMENT overrules the environment the lines imply, for a layout we infer not: e.g., multline* for one long
  equation, or alignat*{2} for columns of a set width.  Sans star, it is numbered; its #+name: labels it.
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
      (pcase-let ((`(,definitions . ,shown) (org--math-render raw-contents backend numbered matrix org--handler-anchor layout environment)))
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

;;;; In Emacs ---cdlatex reads a math block as maths

(defvar texmathp-why)

(defun org--math-maths-at-point-p ()
  "Whether point is in a math block's maths: its contents, save a hint's prose, yet within that prose's $…$; e.g., in
“x ={ by $y$ } z”, at x, y, and z, yes, and at “by” no.  In an Org buffer, or the block's C-c ' buffer."
  (-when-let (start (and (or org--math-edit-buffer (derived-mode-p 'org-mode)) (org--math-block-contents-start)))
    (let ((end (if org--math-edit-buffer (point-max) (org-element-property :contents-end (org-special-block-at-point)))))
      (and (<= start (point)) (< (point) end)
           (pcase (org--math-hint-at-point)
             (`(,_ ,brace ,_) (or (<= (point) brace) (cl-oddp (cl-count ?$ (buffer-substring (1+ brace) (point))))))
             (_ t))))))

(defun org--math-inside-fragment (inside &optional element)
  "Advice around `org-inside-LaTeX-fragment-p', INSIDE, of ELEMENT: sans ELEMENT, a math block's maths is maths too,
`org--math-maths-at-point-p'; so org-cdlatex-mode's _, ^, ', and TAB act there, as within $…$."
  (or (and (not element) (org--math-maths-at-point-p)) (funcall inside element)))

(defun org--math-texmathp (texmathp &rest args)
  "Advice around AUCTeX's `texmathp', TEXMATHP, of ARGS: a math block's maths is maths, `org--math-maths-at-point-p';
so cdlatex wraps no $…$ about what it inserts there."
  (if (org--math-maths-at-point-p) (progn (setq texmathp-why '("a math block" . 0)) t) (apply texmathp args)))

;;;; C-c C-c ---the maths, as an image atop its block

(defun org--math-preview (raw numbered matrix &optional hints environment)
  "Show the RAW maths of the block at point, NUMBERED and with MATRIX tables, as an image atop the block; if one is
already shown, remove it instead.  An edit to the block, or C-c C-x C-l, removes it too."
  (let* ((block (org-special-block-at-point))
         (beg (org-element-property :post-affiliated block))
         (end (save-excursion (goto-char (org-element-property :end block)) (skip-chars-backward " \t\n") (point)))
         (shown (--first (overlay-get it 'org-math-preview) (overlays-in beg end))))
    (if shown (delete-overlay shown)
      (pcase-let* ((`(,definitions . ,math) (let (org--math-tips) (org--math-render raw 'latex numbered matrix nil hints environment)))
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

(org-def-action math (&key numbered (matrix "pmatrix") (hints 'between) environment)
  "Show the maths as an image atop the block; again, to remove it.  See `org--math-preview'."
  (pcase-let ((`(,layout ,format) (org--math-hints-switch hints)))
    (let ((org--math-hint-format format))
      (org--math-preview raw-contents numbered matrix layout environment))))

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

(provide 'org-math-block)
;;; org-math-block.el ends here
