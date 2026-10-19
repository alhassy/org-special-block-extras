;;; org-block-switches.el --- Header arguments every block understands, such as :folded and :when  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.18
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

(defvar org-block-switches nil
  "The (KEY DEPTH . FUNCTION) of each header argument that every block understands, outermost first; see `org-defswitch'.
They apply to an `org-defblock' block, unless it declares a keyword of the same name; to a src, example, quote, or
any other #+begin_𝒳 block, too; see `org--rewrite-any-block-switches'.

  #+begin_solution :when (eq backend \\='html) :folded \"Peek?\"   ;; Only in HTML, and folded away
  #+begin_remark :noexport t                                  ;; Never exported
  #+begin_box :ignore t                                       ;; Just the contents, as plain Org
  #+begin_note :let (N 3) :boxed \"Exercise\"                    ;; Each whole-word N becomes 3; boxed
  #+begin_src emacs-lisp :folded t                            ;; Folded away, titled by its language
  #+begin_tip :demo t                                         ;; Its source, then what it exports to")

(defmacro org-defswitch (key arglist docstring &rest body)
  "Make KEY a switch that every block understands: a block with KEY VALUE renders as BODY, a string, says.
  (org-defswitch :shout (loud? &key render contents)
    \"Upper-case the block's rendering, when LOUD?.\"
    (upcase (funcall render contents)))
ARGLIST names VALUE, then, as `cl-defun' keys, whichever of these BODY needs:
+ :render, the rest of the switches and the block itself, as a function of its contents;
+ :contents; :backend; the block's :name; and its #+begin_ line's :header, sans switches.
Not calling `render' replaces the block; transforming `contents' first, or the result after, wraps it.

Every switch is offered the whole record, and takes only what it names ---dependency injection, by keyword:
+ Hygiene: BODY sees only the names it lists, so a `let' around the definition keeps its own `name';
  a switch wanting both renames the key, as in (_ &key ((:name block)) render contents).
+ Growth: a key we offer in due time, such as :anchor, is invisible to the switches that do not ask for it;
  whereas a new anaphor would shadow each existing switch's variable of that name.
+ Clarity: what BODY depends on is read off its head, not from this docstring.

BODY may open with :depth N, an integer as `add-hook' takes, to place KEY among the other switches: a smaller N
wraps a larger one, so that :noexport (-90) hides even a :folded (-60) box (-50).  N is 0 by default; i.e.,
within :let (-20), so that BODY sees its substitutions, yet without :verbatim (50) and :ignore (60).
The switch fires only when VALUE is non-nil; e.g., :folded nil leaves a block be."
  (declare (indent defun) (doc-string 3))
  (let ((depth (if (eq (car body) :depth) (prog1 (cadr body) (setq body (cddr body))) 0)))
    `(org--add-switch ,key ,depth
                      (cl-function (lambda (,@arglist ,@(unless (memq '&key arglist) '(&key))
                                                      ,@(unless (memq '&allow-other-keys arglist) '(&allow-other-keys)))
                                     ,docstring
                                     ,@body)))))

(defun org--add-switch (key depth function)
  "(KEY, DEPTH, FUNCTION) ↦ `org-block-switches' with KEY now meaning FUNCTION, at DEPTH; see `org-defswitch'."
  (setf (alist-get key org-block-switches) (cons depth function))
  ;; A stable sort, sans mutation: switches of equal depth stay in the order they were defined.
  (setq org-block-switches (seq-sort-by #'cadr #'< org-block-switches)))

(defun org--block-switch-p (key)
  "KEY ↦ whether it is one of `org-block-switches'; e.g., :folded is, :color is not."
  (assq key org-block-switches))

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
  "(NAME, SWITCHES, BACKEND, CONTENTS, RENDER, HEADER) ↦ RENDER's result on CONTENTS, as the plist SWITCHES of block NAME say.
Each switch of `org-block-switches' that SWITCHES gives a non-nil value wraps RENDER, the outermost first:
  (:boxed \"B\" :folded \"F\")  ↦  details(F, box(B, RENDER(CONTENTS)))
:demo is rendered before any other switch, by `org--rewrite-demos', which hands its showing to us as RENDER."
  (funcall (-reduce-r-from (-lambda ((key _ . switch) render)
                             (-if-let (value (plist-get switches key))
                                 (lambda (contents)
                                   (funcall switch value :render render :contents contents
                                            :backend backend :name name :header header))
                               render))
                           render org-block-switches)
           contents))

(org-defswitch :noexport (_)
  "Nothing at all."
  :depth -90
  "")

(org-defswitch :when (form &key render contents backend)
  "The block when FORM, which may mention `backend', is non-nil; else nothing."
  :depth -80
  (if (org--block-eval form backend) (funcall render contents) ""))

(org-defswitch :folded (title &key render contents backend name)
  "The result within a `details' titled TITLE; for :folded t, the block's NAME, capitalised."
  :depth -60
  (org-block/details backend (funcall render contents)
                     (if (eq title t) (capitalize (format "%s" name)) (format "%s" title))))

(org-defswitch :boxed (title &key render contents backend)
  "The result within a `box' titled TITLE; for :boxed t, untitled.  Within :folded, a folded box."
  :depth -50
  (org-block/box backend (funcall render contents) (if (eq title t) "" (format "%s" title))))

(org-defswitch :let (bindings &key render contents backend)
  "Each whole-word VAR in the contents, or operator such as ->, becomes its VALUE, for BINDINGS (VAR VALUE …);
or for those of the table #+name: BINDINGS, else the variable BINDINGS.  See `org--block-let'."
  :depth -20
  (funcall render (org--block-let bindings contents backend)))

(org-defswitch :verbatim (_ &key contents name header)
  "The block as written, its #+begin_ line's header sans switches, shown as Org source, not rendered.
Outer switches apply to this showing; e.g., :verbatim t :folded t folds away the block's source."
  :depth 50
  (format "#+begin_src org\n%s\n#+end_src"
          (org-escape-code-in-string (org--block-source name (string-trim-right contents) header))))

(org-defswitch :ignore (_ &key contents)
  "The contents as plain Org, bypassing the block's own behaviour."
  :depth 60
  contents)

;; Both act elsewhere, so upon export they leave the block be.
(org-defswitch :demo (_ &key render contents) "See `org-demo'." :depth 90 (funcall render contents))
(org-defswitch :C-c_C-c (_ &key render contents) "For the block in Emacs, not upon export; see `org--block-cc-switch'." :depth 90
  (funcall render contents))

;;;; org--rewrite-any-block-switches

(defconst org--switch-block-types
  '(src-block example-block export-block comment-block quote-block center-block verse-block special-block)
  "The `org-element' types of the blocks that switches apply to: Org's own #+begin_𝒳 blocks, and special ones.")

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

(defun org--block-switch-site-contents (site)
  "SITE, from `org--block-switch-site', ↦ its block's contents, sans the block's own indentation."
  (pcase-let ((`(,begin ,end ,indent) site))
    (mapconcat (lambda (line) (if (string-prefix-p (make-string indent ?\s) line) (substring line indent) line))
               (butlast (cdr (split-string (buffer-substring begin end) "\n"))) "\n")))

(defun org--rewrite-block-switch-site (site backend)
  "Replace the block at SITE, from `org--block-switch-site', by what its switches make of it for BACKEND."
  (pcase-let ((`(,begin ,end ,_ ,name ,header ,switches) site))
    ;; The rewrite puts the block's indentation back.
    (org-replace-text-while-preserving-indentation
     (marker-position begin) end
     (org--block-with-switches name switches backend (org--block-switch-site-contents site)
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
                        (re-search-forward (regexp-opt (--map (symbol-name (car it)) org-block-switches)) nil t))
    (let ((case-fold-search t)
          (ours (mapcar (lambda (b) (downcase (format "%s" b))) org--supported-blocks))
          sites)
      (org-element-map (org-element-parse-buffer 'element) org--switch-block-types
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
