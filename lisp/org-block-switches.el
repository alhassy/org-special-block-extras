;;; org-block-switches.el --- Header arguments every block understands, such as :folded and :when  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.17
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Switches are header arguments that every block understands, whoever defined it:
;;
;;     #+begin_src emacs-lisp :folded t        #+begin_details "emacs-lisp"
;;     (+ 1 2)                          ↦      #+begin_src emacs-lisp
;;     #+end_src                               (+ 1 2)
;;                                             #+end_src
;;                                             #+end_details
;;
;; See `org-block-switches' for the lot, and `org--block-with-switches' for each one's meaning.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'org-deflink)
(require 'org-block-header-args)
(require 'org-special-block-parse)

;; Blocks of our own, which require us in turn; switches call them only upon export.
(declare-function org-block/box "org-box-block")
(declare-function org-block/details "org-details-block")
(declare-function org-block/org-demo "org-demo-block")

(defconst org-block-switches '(:ignore :noexport :when :let :boxed :folded :verbatim :C-c_C-c)
  "Header arguments that every block understands ---an `org-defblock' one, unless it declares a keyword of the same
name; a src, example, quote, or any other #+begin_𝒳 block, too; see `org--rewrite-any-block-switches'.

  #+begin_solution :when (eq backend \\='html) :folded \"Peek?\"   ;; Only in HTML, and folded away
  #+begin_remark :noexport t                                  ;; Never exported
  #+begin_box :ignore t                                       ;; Just the contents, as plain Org
  #+begin_note :let (N 3) :boxed \"Exercise\"                    ;; Each whole-word N becomes 3; boxed
  #+begin_src emacs-lisp :folded t                            ;; Folded away, titled by its language
  #+begin_tip :verbatim demo                                  ;; Its source, beside what it exports to

See `org--block-with-switches' for each one's meaning, and `org--block-cc-switch' for :C-c_C-c.")

(defun org--block-switch-p (key)
  "KEY ↦ whether it is one of `org-block-switches'; e.g., :folded is, :color is not."
  (memq key org-block-switches))

(defun org--header-sans-switches (header switch?)
  "(HEADER, SWITCH?) ↦ (SWITCHES . REST): the plist of HEADER's :KEY VALUE pairs whose KEY satisfies SWITCH?, and HEADER sans them.
  \"emacs-lisp :folded \\\"See it\\\" :exports code\"  ↦  ((:folded \"See it\") . \"emacs-lisp :exports code\")
A VALUE reads as Lisp, as `org--read-header' reads it; a KEY followed by another :KEY, or by nothing, is t."
  (let ((pos 0) switches spans)
    (while (string-match "[^ \t\n]+" header pos)
      (let* ((beg (match-beginning 0)) (end (match-end 0))
             (key (intern (match-string 0 header))))
        (if (not (and (keywordp key) (funcall switch? key)))
            (setq pos end)
          (let* ((vbeg (and (string-match "[^ \t\n]+" header end) (not (eq ?: (aref header (match-beginning 0))))
                            (match-beginning 0)))
                 (vend (and vbeg (match-end 0)))
                 (read (and vbeg (condition-case nil (read-from-string header vbeg) (error nil))))
                 (whole? (and read (>= (cdr read) vend)))
                 (stop (cond ((null vbeg) end) (whole? (cdr read)) (t vend))))
            (setq switches (append switches (list key (cond ((null vbeg) t) (whole? (car read)) (t (substring header vbeg vend)))))
                  spans (cons (cons beg stop) spans)
                  pos stop)))))
    ;; Last span first, so the earlier ones' positions stay put.
    (dolist (span spans)
      (setq header (concat (substring header 0 (car span)) (substring header (cdr span)))))
    (cons switches (string-trim (replace-regexp-in-string "[ \t]+" " " header)))))

;;;; Evaluating a switch's Lisp: :when, :let, :C-c_C-c

(defcustom org-defblock-evaluate-switches 'query
  "Whether the Lisp in a block's :when, :let, and :C-c_C-c switches may be evaluated.

+ t: evaluate freely.
+ `query': ask once for each distinct form, per Emacs session.
+ nil: never; a block using one of these switches is then an error.

Lisp written in a document runs with your privileges; as with Babel's
`org-confirm-babel-evaluate', we ask before running a stranger's Lisp."
  :type '(choice (const :tag "Evaluate" t) (const :tag "Ask" query) (const :tag "Never" nil))
  :group 'org-special-block-extras)

(defvar org--block-approved-forms nil
  "The switch forms approved this session, when `org-defblock-evaluate-switches' is `query'.")

(defun org--block-eval (form backend)
  "(FORM, BACKEND) ↦ FORM's value, with `backend' bound to BACKEND; as `org-defblock-evaluate-switches' permits."
  (pcase org-defblock-evaluate-switches
    ('nil (user-error "Block switches may not evaluate Lisp, such as %S; see `org-defblock-evaluate-switches'" form))
    ('query (unless (member form org--block-approved-forms)
              (if (yes-or-no-p (format "Evaluate this block switch's Lisp? %S " form))
                  (push form org--block-approved-forms)
                (user-error "Declined to evaluate %S" form)))))
  (eval form `((backend . ,backend))))

(defun org--block-let-named (name)
  "NAME ↦ the (VAR VALUE …) of the table #+name: NAME, row by row, sans any header above its rule; else NAME's value.
  | -> | \\Rightarrow |  ↦  (-> \"\\\\Rightarrow\")
Cells stay strings: Babel's reading of a table, `org-babel-ref-resolve', would evaluate a cell such as (f x)."
  (let ((rows (save-excursion
                (goto-char (point-min))
                (when (re-search-forward (format "^[ \t]*#\\+name:[ \t]*%s[ \t]*$" (regexp-quote (symbol-name name))) nil t)
                  (forward-line 1)
                  (when (org-at-table-p) (org-table-to-lisp))))))
    (cond (rows (--mapcat (list (intern (car it)) (cadr it))
                          (-filter #'consp (or (cdr (memq 'hline rows)) rows))))
          ((boundp name) (symbol-value name))
          (t (user-error "‘:let %s’ names neither a table, by #+name:, nor a Lisp variable" name)))))

(defun org--block-let (bindings contents backend)
  "(BINDINGS, CONTENTS, BACKEND) ↦ CONTENTS with each whole-word VAR replaced by its value, for BINDINGS (VAR VALUE …).
  (N (+ 1 2) WHO \"Jasim\"), \"WHO scored N\"  ↦  \"Jasim scored 3\"
  (-> \"⇒\"), \"p->q\"                    ↦  \"p⇒q\"
Each VALUE may refer to the VARs before it, as with `let*'.  BINDINGS may instead be a symbol, naming a table
of them, or a variable holding them; see `org--block-let-named'."
  (let* ((pairs (-partition 2 (if (symbolp bindings) (org--block-let-named bindings) bindings)))
         (values (org--block-eval `(let* ,pairs (list ,@(mapcar #'car pairs))) backend))
         ;; Case matters: a variable SUM leaves the word “sum” be.
         (case-fold-search nil))
    (cl-loop for (var) in pairs
             for value in values
             ;; A word boundary only at VAR's word ends: an operator such as -> has none, else it would never match.
             for name = (symbol-name var)
             for wordy? = (lambda (char) (string-match-p "[[:alnum:]_]" (string char)))
             do (setq contents (replace-regexp-in-string (concat (if (funcall wordy? (aref name 0)) "\\_<" "")
                                                                 (regexp-quote name)
                                                                 (if (funcall wordy? (aref name (1- (length name)))) "\\_>" ""))
                                                         (format "%s" value) contents t t)))
    contents))

;;;; What each switch does

(defun org--block-with-switches (name switches backend contents render &optional header)
  "(NAME, SWITCHES, BACKEND, CONTENTS, RENDER, HEADER) ↦ RENDER's result on CONTENTS, as the plist SWITCHES of block NAME say:

+ :noexport t ↦ nothing at all; likewise :when FORM, when FORM is nil.  FORM may mention `backend'.
+ :let (VAR VALUE …) ↦ each whole-word VAR in CONTENTS, or operator such as ->, becomes its VALUE, beforehand;
  :let NAME takes them from the table #+name: NAME, else the variable NAME.  See `org--block-let'.
+ :ignore t ↦ CONTENTS as plain Org, bypassing RENDER; i.e., the block's own behaviour.
+ :boxed TITLE ↦ the result within a `box' titled TITLE; for :boxed t, untitled.
+ :folded TITLE ↦ the result within a `details' titled TITLE; for :folded t, the block's NAME, capitalised.
  Hence :boxed and :folded together give a folded box.
+ :verbatim t ↦ the block as written, its #+begin_ line's HEADER sans switches, shown as Org source, not rendered;
  :verbatim demo ↦ that source beside what it renders to, via `org-block/org-demo'.  Other switches then apply to
  this showing; e.g., :verbatim t :folded t folds away the block's source.

:C-c_C-c is for the block in Emacs, not upon export; see `org--block-cc-switch'."
  (if (or (plist-get switches :noexport)
          (and (plist-member switches :when) (not (org--block-eval (plist-get switches :when) backend))))
      ""
    (let* ((contents (-if-let (bindings (plist-get switches :let)) (org--block-let bindings contents backend) contents))
           (source (lambda () (org--block-source name (string-trim-right contents) header)))
           (result (pcase (plist-get switches :verbatim)
                     ('nil (if (plist-get switches :ignore) contents (funcall render contents)))
                     ((or 'demo "demo") (org-block/org-demo backend (funcall source)))
                     (_ (format "#+begin_src org\n%s\n#+end_src" (org-escape-code-in-string (funcall source))))))
           (title (lambda (switch default) (if (eq (plist-get switches switch) t) default (format "%s" (plist-get switches switch))))))
      ;; The result is Org text, export blocks and all; so it serves as the raw contents of an enclosing block.
      (when (plist-get switches :boxed)
        (setq result (org-block/box backend result (funcall title :boxed ""))))
      (when (plist-get switches :folded)
        (setq result (org-block/details backend result (funcall title :folded (capitalize (format "%s" name))))))
      result)))

;;;; org--rewrite-any-block-switches

(defun org--block-switch-site (block)
  "Org's own BLOCK, or a special one, ↦ (BEGIN END INDENT NAME HEADER SWITCHES) when it has switches; else nil.
BEGIN and END are markers at the “#” of its #+begin_ line and the end of its #+end_ line; HEADER is that
line's text sans SWITCHES, the plist of its own switches atop those it inherits; see `org--block-header-args'."
  (save-excursion
    (goto-char (org-element-property :post-affiliated block))
    (when (looking-at "^\\([ \t]*\\)\\(#\\+begin_\\(\\S-+\\)\\)[ \t]*\\(.*\\)$")
      (-let* ((indent (length (match-string 1)))
              (hash (match-beginning 2))
              (name (match-string 3))
              ((switches . header) (org--header-sans-switches (match-string 4) #'org--block-switch-p))
              (inherited (org--block-header-args block (downcase (or (org-element-property :language block) name))))
              (switches (org--plist-merge (car (org--plist-partition inherited #'org--block-switch-p)) switches)))
        (when switches
          (when (and (eq (org-element-type block) 'src-block) (eq (plist-get switches :folded) t))
            (setq switches (plist-put switches :folded (or (org-element-property :language block) "Code"))))
          (list (copy-marker hash) (copy-marker (org--block-end-point block) t) indent name header switches))))))

(defun org--rewrite-block-switch-site (site backend)
  "Replace the block at SITE, from `org--block-switch-site', by what its switches make of it for BACKEND."
  (pcase-let* ((`(,begin ,end ,indent ,name ,header ,switches) site)
               (start (marker-position begin))
               (lines (split-string (buffer-substring start end) "\n"))
               ;; The contents sans the block's own indentation, which the rewrite puts back.
               (contents (mapconcat (lambda (line) (if (string-prefix-p (make-string indent ?\s) line) (substring line indent) line))
                                    (butlast (cdr lines)) "\n")))
    (org-replace-text-while-preserving-indentation
     start end
     (org--block-with-switches name switches backend contents
                               (lambda (contents) (org--block-source name contents header))
                               header))))

(defun org--rewrite-any-block-switches (backend)
  "Apply `org-block-switches' to each block that no `org-defblock' declares, as BACKEND exports it; e.g.,

  #+begin_src emacs-lisp :folded t        #+begin_details \"emacs-lisp\"
  (+ 1 2)                          ↦      #+begin_src emacs-lisp
  #+end_src                               (+ 1 2)
                                          #+end_src
                                          #+end_details

Src, example, quote, verse, center, comment, and export blocks, and special blocks of any name, are left to Org,
sans their switches; a src block's :folded t is titled by its language.  We run before Babel, so a src block's
:let changes the code it runs, :noexport keeps it from running, and :folded folds its results too; and again
after, for the blocks of #+INCLUDE: files."
  (setq backend (org--base-backend backend))
  ;; Parsing the whole buffer is dear, so we only do it when some block may have a switch.
  (when (save-excursion (goto-char (point-min))
                        (re-search-forward (regexp-opt (mapcar #'symbol-name org-block-switches)) nil t))
    (let ((case-fold-search t)
          (ours (mapcar (lambda (b) (downcase (format "%s" b))) org--supported-blocks))
          sites)
      (org-element-map (org-element-parse-buffer 'element)
          '(src-block example-block export-block comment-block quote-block center-block verse-block special-block)
        (lambda (block)
          (unless (and (eq (org-element-type block) 'special-block)
                       (member (downcase (org-element-property :type block)) ours))
            (-when-let (site (org--block-switch-site block))
              (push site sites)))))
      ;; `sites' lists the last block first; inner blocks, beginning later, are so rewritten before outer ones.
      (dolist (site sites)
        (org--rewrite-block-switch-site site backend)))))

(provide 'org-block-switches)
;;; org-block-switches.el ends here
