;;; org-math-hints.el --- Calculational proofs: a math block's hints, X ={ HINT } Y  -*- lexical-binding: t; -*-

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

;; A hint states why a step of a calculation holds, right after its relation; by way of example,
;;
;;       p ∧ q
;;     ={ Golden Rule }
;;       p ≡ q ≡ p ∨ q
;;
;; sets “Golden Rule” between the two expressions, maroon and bracketed; :hints beside sets it at its step's end.  A
;; hint may span lines, and nest a calculation, \[ … \].  In Emacs, a hint shows as a comment, TAB folds it, C-TAB
;; folds them all, and M-q fills it alone; in HTML, a click folds it.  See `org--math-hints'.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'org-special-block-at-point)
(require 'org-math-markup)
(require 'org-math-structure)

;;;; Reading hints ---where each REL{ HINT } lies

(defvar org--math-hint-folded nil
  "Whether a hint starts folded in HTML, so a reader clicks to see it.  A math block binds it by its :hints.")

(defvar org--math-hint-format nil
  "How a hint shows, as LaTeX, its %s the hint, replaced verbatim; e.g., “{\\color{teal} \\langle %s \\rangle}”; or none,
to show no hints.  A math block binds it by its :hints; nil is as the layout has it: see `org--math-hint-styled'.")

(defface org-math-hint '((t :inherit font-lock-comment-face))
  "How a math block's hint, “REL{ HINT }”, looks: muted, as a comment, since it says why a step holds, not what it is.
Not `shadow', which some themes make the colour of all text."
  :group 'org-special-block-extras)

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

;;;; Showing hints ---as text, styled, nested, and folding in HTML

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

;;;; In Emacs ---a hint's face, TAB, C-TAB, M-q, and eldoc, in Org and C-c ' buffers alike

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

;;;; The :hints switch

(defun org--math-hints-switch (hints)
  "HINTS, a math block's :hints, ↦ (LAYOUT FORMAT FOLDED): between, beside, or none, as LAYOUT, its FORMAT as the layout
has it; or a list of LAYOUT, then a FORMAT string, or folded, or both.  E.g., none ↦ (beside \"none\" nil), as hidden
hints leave a compact chain of steps; (between folded) ↦ (between nil t)."
  (let ((layout (if (consp hints) (car hints) hints))
        (rest (and (consp hints) (cdr hints))))
    (if (eq layout 'none) (list 'beside "none" nil)
      (list layout (-first #'stringp rest) (and (memq 'folded rest) t)))))

(provide 'org-math-hints)
;;; org-math-hints.el ends here
