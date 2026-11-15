;;; org-block-switches.el --- Header arguments every block understands, such as :folded and :when  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.11.14
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
(require 'ob-core)
(require 'ob-tangle)
(require 'org-deflink)
(require 'org-block-header-args)
(require 'org-special-block-parse)
(require 'org-deftag)

;; Blocks of our own, which require us in turn; switches call them only upon export.
(declare-function org--box-block "org-box-block")
(declare-function org--details-block "org-details-block")
(defvar org--permalink-svg) ;; Of org-links, which requires us in turn.

(defvar org-block-switches nil
  "The (KEY DEPTH . FUNCTION) of each header argument that every block understands, outermost first; see `org-defswitch'.
They apply to an `org-defblock' block, unless it declares a keyword of the same name; to a src, example, quote, or
any other #+begin_𝒳 block, too; see `org--rewrite-any-block-switches'.

  #+begin_solution :when (eq backend \\='html) :folded \"Peek?\"   ;; Only in HTML, and folded away
  #+begin_remark :noexport t                                  ;; Never exported
  #+begin_box :ignore t                                       ;; Just the contents, as plain Org
  #+begin_note :let (N 3) :boxed \"Exercise\"                    ;; Each whole-word N becomes 3; boxed
  #+begin_src emacs-lisp :folded t                            ;; Folded away, titled by its language
  #+begin_tip :demo t                                         ;; Its source, then what it exports to
  #+begin_box Answer :tags solution                           ;; Not when #+exclude_tags: solution
  #+begin_quote :within ((details \"Hint\") center)             ;; Within any blocks, outermost first
  #+begin_box Lemma :permalink t :backend html                ;; A 🔗 to #Lemma; only in HTML
  #+begin_src C :filename main.c :highlight \"2-3\" :tab t       ;; See `org-display-switches'")

(eval-and-compile ;; `org-defswitch' and `org-defblock' call this as they expand.
(defun org--injected-vars (who spec offered)
  "(WHO, SPEC, OFFERED) ↦ the variables SPEC, a `cl-defun' &key list, binds; an error, naming WHO, for a key not
OFFERED.
  (backend ((:contents text))), (:backend :contents)  ↦  (backend text)
Hence a misspelt key, such as contnets, is refused as the definition expands, rather than silently bound to nil."
  (--map (pcase-let ((`(,key ,var) (pcase it
                                     ((pred symbolp) (list (intern (format ":%s" it)) it))
                                     (`((,key ,var) . ,_) (list key var))
                                     (`(,var . ,_) (list (intern (format ":%s" var)) var)))))
           (unless (memq key offered)
             (error "‘%s’ asks for %s, which is not on offer; only %s are" who key offered))
           var)
         spec)))

(defmacro org-defswitch (key arglist docstring &rest body)
  "Make KEY a switch that every block understands: a block with KEY VALUE renders as BODY, a string, says.
  (org-defswitch :shout (loud? &key render contents)
    \"Upper-case the block's rendering, when LOUD?.\"
    (upcase (funcall render contents)))
ARGLIST names VALUE, then, as `cl-defun' keys, whichever of these BODY needs:
+ :render, the rest of the switches and the block itself, as a function of its contents;
+ :contents; :backend; the block's :name; its #+begin_ line's :header, sans switches; and its :anchor, its #+name:;
+ :source, the block ---or section--- as written sans switches, as a function of its contents.
Not calling `render' replaces the block; transforming `contents' first, or the result after, wraps it.

Every switch is offered the whole record, and takes only what it names ---dependency injection, by keyword:
+ Hygiene: BODY sees only the names it lists, so a `let' around the definition keeps its own `name'.
+ Growth: a key we offer in due time is invisible to the switches that do not ask for it;
  whereas a new anaphor would shadow each existing switch's variable of that name.
+ Clarity: what BODY depends on is read off its head, not from this docstring; a key not on offer is an error.
A key may be bound under another name, ((:KEY VAR)), as when BODY uses KEY's name for something else already:
  (org-defswitch :greet (_ &key ((:name block)) render contents) …)  ;; `block' is the block's name
See “Keyword Parameters” in Practical Common Lisp, https://gigamonkeys.com/book/functions.html, for a gentle tour.

BODY may open with :depth N, an integer as `add-hook' takes, to place KEY among the other switches: a smaller N
wraps a larger one, so that :noexport (-90) hides even a :folded (-60) box (-50).  N is 0 by default; i.e.,
within :let (-20), so that BODY sees its substitutions, yet without :verbatim (50) and :ignore (60).
The switch fires only when VALUE is non-nil; e.g., :folded nil leaves a block be."
  (declare (indent defun) (doc-string 3))
  (org--injected-vars key
                      (-take-while (lambda (it) (not (memq it '(&allow-other-keys &aux)))) (cdr (memq '&key arglist)))
                      '(:render :contents :backend :name :header :source :anchor))
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

(defun org--block-with-switches (name switches backend contents render &optional header source anchor)
  "(NAME, SWITCHES, BACKEND, CONTENTS, RENDER, HEADER, SOURCE, ANCHOR) ↦ RENDER's result on CONTENTS, as the plist SWITCHES of block NAME say.
SOURCE, a function of the contents, gives the block as written sans switches; by default, #+begin_NAME HEADER … #+end_NAME.
ANCHOR is the block's #+name:, if any.
Each switch of `org-block-switches' that SWITCHES gives a non-nil value wraps RENDER, the outermost first:
  (:boxed \"B\" :folded \"F\")  ↦  details(F, box(B, RENDER(CONTENTS)))
:demo is rendered before any other switch, by `org--rewrite-demos', which hands its showing to us as RENDER."
  (funcall (-reduce-r-from (-lambda ((key _ . switch) render)
                             (-if-let (value (plist-get switches key))
                                 (lambda (contents)
                                   (funcall switch value :render render :contents contents
                                            :backend backend :name name :header header :anchor anchor
                                            :source (or source (lambda (contents) (org--block-source name contents header)))))
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

(org-defswitch :backend (backends &key render contents backend)
  "The block when we export along one of BACKENDS, a symbol or list of them, or a backend deriving from one; else nothing.
  :backend html, :backend (latex beamer), :backend md  ;; md: Markdown, which derives from html, is html too
Sugar for the commonest :when, which needs no Lisp, so no question before it runs; see `org-defblock-evaluate-switches'."
  :depth -80
  (if (--some (if org-export-current-backend (org-export-derived-backend-p org-export-current-backend it) (eq backend it))
              (ensure-list backends))
      (funcall render contents)
    ""))

(defun org--tags-of (tags)
  "Returns the tag names TAGS denotes, a symbol, a string of words, or a list of either; e.g., (solution \"draft\")."
  (--mapcat (split-string (format "%s" it) "[ \t:]+" t) (ensure-list tags)))

(defun org--export-exclude-tags ()
  "Returns the tags this export excludes: those #+exclude_tags: lines name, else `org-export-exclude-tags'.
As with Org, a section tagged with one of them is not exported; nor, by :tags, is a block."
  (let ((lines (cdar (org-collect-keywords '("EXCLUDE_TAGS")))))
    (if lines (--mapcat (split-string it nil t) lines) org-export-exclude-tags)))

(org-defswitch :tags (tags &key render contents)
  "The block, unless one of its TAGS is excluded from this export, as #+exclude_tags: excludes a section so tagged.
  #+exclude_tags: solution  …  #+begin_src haskell :tags solution  ⇒  the solution is not exported
TAGS is a symbol, a string of words, or a list of them; see `org--tags-of' and `org--export-exclude-tags'."
  :depth -85
  (if (-intersection (org--tags-of tags) (org--export-exclude-tags)) "" (funcall render contents)))

(defun org--wrap-specs (value)
  "Returns the (BLOCK ARG … :KEY VALUE …) list a :within VALUE denotes, outermost first; a bare BLOCK is (BLOCK).
  box  ⇒  ((box));  (box \"T\" :shadow t)  ⇒  ((box \"T\" :shadow t));  ((details \"Hint\") box)  ⇒  ((details \"Hint\") (box))"
  (--map (if (consp it) it (list it))
         (if (and (consp value) (consp (car value))) value (list value))))

(defun org--wrap-in (spec backend contents)
  "Returns CONTENTS within the block SPEC, (BLOCK ARG … :KEY VALUE …), along BACKEND: by BLOCK's handler, when
`org-defblock' made it; else as #+begin_BLOCK ARG … #+end_BLOCK, for Org to export, as for quote or center."
  (-let* (((name . args) spec)
          (handler (intern (format "org--%s-block" name)))
          ((main . keys) (if (keywordp (car args)) (cons nil args) args)))
    (if (fboundp handler)
        (apply handler backend contents (and main (format "%s" main)) keys)
      (org--block-source name contents
                         (mapconcat (lambda (it) (if (and (stringp it) (string-match-p "[ \t]" it)) (prin1-to-string it)
                                                   (format "%s" it)))
                                    args " ")))))

(org-defswitch :within (specs &key render contents backend)
  "The result within each block SPECS names, the first outermost; a SPEC is BLOCK, or (BLOCK ARG … :KEY VALUE …).
  :within (details \"Hint\")                  ≡  :folded \"Hint\"
  :within ((details \"Hint\") (box \"Lemma\"))  ≡  :folded \"Hint\" :boxed \"Lemma\"
  :within (box \"Aside\" :shadow t), :within center, :within (margin \"see\")
Not :wrap, which is Babel's, wrapping a src block's results rather than the block.
Any block will do: one of ours, or of `org-defblock', by its handler; else Org's own, such as quote; see `org--wrap-in'."
  :depth -70
  (-reduce-r-from (lambda (spec result) (org--wrap-in spec backend result))
                  (funcall render contents)
                  (org--wrap-specs specs)))

(defun org--permalink-id (value anchor header)
  "Returns the id a :permalink VALUE gives a block: VALUE itself, when a string or symbol; for t, its ANCHOR, the
#+name:, else its HEADER's leading words, its title, as for #+begin_box Exercise; else an error."
  (let ((title (car (split-string (or header "") "[ \t]+:" t "[ \t\"]+"))))
    (cond ((not (eq value t)) (format "%s" value))
          (anchor anchor)
          ((org-string-nw-p title) title)
          (t (user-error ":permalink t needs a #+name:, or a title, to name the link; else write :permalink ID")))))

(org-defswitch :permalink (id &key render contents header anchor)
  "The result, after a permalink, <<ID>>: in HTML, a 🔗 whose URL ends in #ID; [[ID]] links to it, as ever.
For :permalink t, ID is the block's #+name:, else its title; see `org--permalink-id' and Org's dedicated targets."
  :depth -75
  (format "<<%s>>\n%s" (org--permalink-id id anchor header) (funcall render contents)))

(org-defswitch :folded (title &key render contents backend name)
  "The result within a `details' titled TITLE; for :folded t, the block's NAME, capitalised."
  :depth -60
  (org--details-block backend (funcall render contents)
                     (if (eq title t) (capitalize (format "%s" name)) (format "%s" title))))

(org-defswitch :boxed (title &key render contents backend)
  "The result within a `box' titled TITLE; for :boxed t, untitled.  Within :folded, a folded box."
  :depth -50
  (org--box-block backend (funcall render contents) (if (eq title t) "" (format "%s" title))))

(org-defswitch :let (bindings &key render contents backend)
  "Each whole-word VAR in the contents, or operator such as ->, becomes its VALUE, for BINDINGS (VAR VALUE …).
BINDINGS may instead name a #+name: table of them, else a variable holding them.  See `org--block-let'."
  :depth -20
  (funcall render (org--block-let bindings contents backend)))

(org-defswitch :verbatim (_ &key contents source)
  "The block as written, its #+begin_ line's header sans switches, shown as Org source, not rendered.
Outer switches apply to this showing; e.g., :verbatim t :folded t folds away the block's source."
  :depth 50
  (format "#+begin_src org\n%s\n#+end_src" (org-escape-code-in-string (funcall source (string-trim-right contents)))))

(org-defswitch :ignore (_ &key contents)
  "The contents as plain Org, bypassing the block's own behaviour."
  :depth 60
  contents)

;; Both act elsewhere, so upon export they leave the block be.
(org-defswitch :demo (_ &key render contents) "The block's source, then what it exports to; see `org-demo'." :depth 90 (funcall render contents))
(org-defswitch :C-c_C-c (_action &key render contents)
  "What C-c C-c on the block runs, in Emacs: ACTION, a form or a command; export ignores it.  See `org--block-cc-switch'."
  :depth 90
  (funcall render contents))

;;;; org--rewrite-any-block-switches

(defconst org--switch-block-types
  '(src-block example-block export-block comment-block quote-block center-block verse-block special-block)
  "The `org-element' types of the blocks that switches apply to: Org's own #+begin_𝒳 blocks, and special ones.")

(defun org--block-switch-site (block)
  "Org's own BLOCK, or a special one, ↦ (BEGIN END INDENT NAME HEADER SWITCHES) when it has switches; else nil.
BEGIN and END are markers at the “#” of its #+begin_ line and the end of its #+end_ line; HEADER is that
line's text sans SWITCHES, the plist of its own switches atop those it inherits; see `org--block-header-args'.
A block a rewrite already made has its switches all nil, so none left; see `org--switched-source'."
  (save-excursion
    (goto-char (org-element-property :post-affiliated block))
    (when (looking-at "^\\([ \t]*\\)\\(#\\+begin_\\(\\S-+\\)\\)[ \t]*\\(.*\\)$")
      (-let* ((indent (length (match-string 1)))
              (hash (match-beginning 2))
              (name (match-string 3))
              (language (org-element-property :language block))
              ;; A src block's language keeps its own header arguments, as a block of ours keeps its keywords.
              (switch? (if (eq (org-element-type block) 'src-block)
                           (lambda (key) (and (org--block-switch-p key) (not (org--babel-declares-p language key))))
                         #'org--block-switch-p))
              ((switches . header) (org--header-sans-switches (match-string 4) switch?))
              ;; A src block inherits, weakest first, Babel's language-agnostic header-args, then those of every
              ;; src block, header-args:src, then its language's; e.g., #+PROPERTY: header-args:src :copy nil.
              (inherited (apply #'org--plist-merge
                                (mapcar (lambda (it) (org--block-header-args block it))
                                        (if (eq (org-element-type block) 'src-block) (list nil "src" (downcase (or language "src")))
                                          (list (downcase name))))))
              (switches (org--plist-merge (car (org--plist-partition inherited switch?)) switches)))
        ;; A block whose switches are all off, as a rewrite leaves it, has none to apply.
        (when (cl-loop for (_ value) on switches by #'cddr thereis value)
          (when (eq (org-element-type block) 'src-block)
            (setq switches (org--src-block-switches block switches)))
          (list (copy-marker hash) (copy-marker (org--block-end-point block) t) indent name header switches))))))

(defun org--babel-declares-p (language key)
  "Returns whether Babel declares KEY a header argument of LANGUAGE's src blocks, so no switch of that name is theirs:
one of `org-babel-common-header-args-w-values', such as :wrap, or of `org-babel-header-args:LANGUAGE'."
  (let ((name (intern (substring (symbol-name key) 1)))
        (own (intern-soft (format "org-babel-header-args:%s" language))))
    (or (assq name org-babel-common-header-args-w-values)
        (and own (boundp own) (assq name (symbol-value own))))))

(defun org--src-block-switches (block switches)
  "Returns the SWITCHES of src BLOCK, each t that stands for something of the block's own made so:
:folded t and :tab t are titled by its language; :filename t names the file it tangles to.
A block whose code is not exported, by :exports none or results, has no :filename, :copy, or :highlight; and
:permalink t needs a #+name:, since a src block has no title."
  (let ((language (or (org-element-property :language block) "Code")))
    (dolist (key '(:folded :tab))
      (when (eq (plist-get switches key) t) (setq switches (plist-put switches key language))))
    (when (and (eq (plist-get switches :permalink) t) (not (org-element-property :name block)))
      (user-error ":permalink t names the link by the src block's #+name:, yet this %s block has none" language))
    (when (--some (plist-get switches it) '(:filename :copy :highlight))
      (let* ((params (nth 2 (save-excursion (goto-char (org-element-property :post-affiliated block))
                                            (org-babel-get-src-block-info 'no-eval block))))
             (exports (format "%s" (alist-get :exports params))))
        (when (member exports '("none" "results"))
          (setq switches (org-plist-delete (org-plist-delete (org-plist-delete switches :filename) :copy) :highlight)))
        (when (eq (plist-get switches :filename) t)
          (setq switches (plist-put switches :filename (org--tangle-file-name language (alist-get :tangle params)))))))
    switches))

(defun org--tangle-file-name (language tangle)
  "Returns the file a src block of LANGUAGE tangles to, by its :tangle TANGLE; e.g., \"init.el\", or for yes, the
document's own name with LANGUAGE's extension, as `org-babel-tangle' names it; else an error, as for no."
  (pcase tangle
    ((or 'nil "no") (user-error ":filename t names the file a src block tangles to, yet this %s block tangles to none"
                                language))
    ("yes" (concat (file-name-base (or (buffer-file-name (buffer-base-buffer))
                                       (user-error ":filename t, with :tangle yes, needs the document to be a file")))
                   "." (or (cdr (assoc language org-babel-tangle-lang-exts)) language)))
    (_ (file-name-nondirectory tangle))))

(defun org--switched-source (name contents header switches)
  "Returns the block #+begin_NAME HEADER … #+end_NAME of CONTENTS, as `org--block-source' does, with each of the
plist SWITCHES, now applied, turned off; e.g., \"emacs-lisp :boxed nil\".  So a later pass of
`org--rewrite-any-block-switches' leaves it be, rather than applying what it inherits twice; a text property would
not survive a switch that rewrites the text, such as one upcasing it."
  (org--block-source name contents
                     (string-join (cons header (cl-loop for (key) on switches by #'cddr collect (format "%s nil" key)))
                                  " ")))

(defun org--block-switch-site-contents (site)
  "SITE, from `org--block-switch-site', ↦ its block's contents, sans the block's own indentation.
A section's site, from `org-demo--section-site', has as contents its text and subsections, below its drawer."
  (pcase-let ((`(,begin ,end ,indent ,_ ,_ ,_ ,section? ,body) site))
    (if (eq section? 'section) (buffer-substring body end)
     (mapconcat (lambda (line) (if (string-prefix-p (make-string indent ?\s) line) (substring line indent) line))
               (butlast (cdr (split-string (buffer-substring begin end) "\n"))) "\n"))))

(defun org--rewrite-block-switch-site (site backend)
  "Replace the block at SITE, from `org--block-switch-site', by what its switches make of it for BACKEND.
Its affiliated keywords, such as #+name:, stay with the block itself, within whatever the switches wrap it in;
else they would name the first thing a switch writes, such as :permalink's target, and Babel would lose the block."
  (pcase-let* ((`(,begin ,end ,indent ,name ,header ,switches) site)
               (block (save-excursion (goto-char begin) (org-element-at-point)))
               (from (save-excursion (goto-char (org-element-property :begin block)) (skip-chars-forward " \t") (point)))
               (affiliated (if (>= from (- begin indent)) ""
                             (mapconcat (lambda (line) (concat (string-trim-left line) "\n"))
                                        (split-string (buffer-substring from (- begin indent)) "\n" t) ""))))
    ;; The rewrite puts the block's indentation back.
    (org-replace-text-while-preserving-indentation
     from end
     (org--block-with-switches name switches backend (org--block-switch-site-contents site)
                               (lambda (contents) (concat affiliated (org--switched-source name contents header switches)))
                               header nil (org-element-property :name block)))))

(defun org--default-switches-p ()
  "Returns whether some `org-babel-default-header-args:NAME', or Babel's own, gives a switch; e.g., :copy, by default.
Then a block may have a switch that its document never mentions."
  (catch 'found
    (mapatoms (lambda (symbol)
                (when (and (boundp symbol) (consp (symbol-value symbol))
                           (string-prefix-p "org-babel-default-header-args" (symbol-name symbol))
                           (--some (and (consp it) (org--block-switch-p (car it))) (symbol-value symbol)))
                  (throw 'found t))))
    nil))

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
  (when (or (save-excursion (goto-char (point-min))
                            (re-search-forward (regexp-opt (--map (symbol-name (car it)) org-block-switches)) nil t))
            (org--default-switches-p))
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

;;;; Switches on sections: org--rewrite-heading-switches

(defconst org--wrapping-switches '(:folded :boxed :within :tab :copy)
  "The switches that wrap a section's contents in a block; so the section may not have subsections.")

;; A section and a block are two ways to say the same thing; so a heading's property drawer takes every switch.

(defun org--heading-switch-value (string)
  "Returns the switch value a property's STRING denotes: the Lisp it reads as, when wholly; else STRING itself.
By way of example, \"(eq backend \='html)\" reads as a form, \"t\" and \"\" as t, yet \"Peek at it\" stays a string."
  (let ((string (string-trim string)))
    (if (string-empty-p string) t
      (pcase (ignore-errors (read-from-string string))
        (`(,value . ,end) (if (= end (length string)) value string))
        (_ string)))))

(defun org--heading-switches-at-point ()
  "Returns the plist of `org-block-switches' that the drawer of the heading at point gives, sans inheritance.
A :folded t is titled by the heading, as a src block's is by its language."
  (let ((switches (cl-loop for (key) in org-block-switches
                           for value = (org-entry-get nil (substring (symbol-name key) 1))
                           when value append (list key (org--heading-switch-value value)))))
    (if (eq (plist-get switches :folded) t) (plist-put switches :folded (org-get-heading t t t t)) switches)))

(defun org--heading-source (heading contents)
  "Returns the section of HEADING, its line as written, and CONTENTS, as it reads sans its drawer of switches."
  (concat heading "\n" contents))

(defun org--heading-sans-deftags ()
  "Axes, from the heading line at point, each tag that an `org-deftag' gives behaviour; e.g., :details_Hint:."
  (save-excursion
    (let ((case-fold-search t))
      (cl-loop for tag in (org-get-tags nil t)
               for name = (car (split-string tag "_"))
               when (fboundp (intern (format "org--%s-tag" name)))
               do (beginning-of-line)
                  (when (re-search-forward (org--deftag-regexp name) (line-end-position) t)
                    (replace-match "" t t nil 1)))
      ;; A lone colon left behind means no tags remain.
      (beginning-of-line)
      (when (re-search-forward "[ \t]+:[ \t]*$" (line-end-position) t) (replace-match "")))))

(defun org--heading-permalink (id title)
  "Gives the heading at point, TITLE, a 🔗 beside it, in HTML, a permalink to its CUSTOM_ID, ID or else its own.
  * Lemmas, :permalink: t  ⇒  <h2 id=\"Lemmas\">Lemmas 🔗</h2>, the 🔗 linking to #Lemmas
The CUSTOM_ID is, for ID t, the heading's own, else TITLE with dashes for spaces; the table of contents, by
ALT_TITLE, shows TITLE sans 🔗.  Other backends get the CUSTOM_ID alone; LaTeX, as its \\label."
  (let ((id (if (eq id t) (or (org-entry-get nil "CUSTOM_ID") (replace-regexp-in-string "[ \t]+" "-" title))
              (format "%s" id))))
    (org-entry-put nil "CUSTOM_ID" id)
    (unless (org-entry-get nil "ALT_TITLE") (org-entry-put nil "ALT_TITLE" title))
    (org-edit-headline
     (format "%s @@html:<a class=\"anchor\" href=\"#%s\" aria-label=\"Permalink to %s\">%s</a>@@"
             title id id org--permalink-svg))))

(defun org--rewrite-heading-switches (backend)
  "Applies the switches each heading's drawer gives to its section, as BACKEND exports it, as they would to a block:

  * Answers                             * Answers
  :PROPERTIES:                   ↦      #+begin_details \"Peek?\"
  :folded: Peek?                        The answer is 6.
  :let: (N 6)                           #+end_details
  :END:
  The answer is N.

The section's contents are its text and subsections, below its drawer; its heading stays put, being its title.
+ :noexport, or a :when whose form is nil, axes the whole subtree, heading included.
+ :ignore bypasses the section's own behaviour: its tags that an `org-deftag' defines, such as :details_Hint:.
+ :verbatim shows the section as written, heading included, as Org source; :demo too, beside its rendering,
  by `org--rewrite-demos', which runs first.
+ :folded, :boxed, and the rest of `org--wrapping-switches' wrap the contents, which then may not have
  subsections, as a block cannot hold a heading.
+ :C-c_C-c is for C-c C-c within the section, in Emacs; see `org--section-cc-switch'.
+ :permalink puts its 🔗 beside the heading, rather than atop the contents; see `org--heading-permalink'.
Each switch is axed from the drawer once applied, lest the pass after #+INCLUDE: apply it again."
  (setq backend (org--base-backend backend))
  (when (save-excursion (goto-char (point-min))
                        (let ((case-fold-search t))
                          (re-search-forward (format "^[ \t]*:%s:" (regexp-opt (--map (substring (symbol-name (car it)) 1)
                                                                                       org-block-switches)))
                                             nil t)))
    (let (headings)
      (org-map-entries (lambda () (when (org--heading-switches-at-point) (push (point-marker) headings))))
      ;; `headings' lists the last heading first; subsections, beginning later, are so rewritten before their parents.
      (dolist (heading headings)
        (goto-char heading)
        (let ((switches (org--heading-switches-at-point))
              (title (org-get-heading t t t t)))
          (dolist (it org-block-switches) (org-entry-delete nil (substring (symbol-name (car it)) 1)))
          (when (plist-get switches :ignore) (org--heading-sans-deftags))
          (-when-let (id (plist-get switches :permalink))
            (org--heading-permalink id title)
            (setq switches (org-plist-delete switches :permalink)))
          (if (or (plist-get switches :noexport)
                  (and (plist-member switches :when) (not (org--block-eval (plist-get switches :when) backend))))
              (delete-region (point) (save-excursion (org-end-of-subtree t t) (point)))
            (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
                   ;; Shown as source, the heading is part of what is shown; else it stays put, atop its contents.
                   (shown? (plist-get switches :verbatim))
                   (begin (if shown? (point) (save-excursion (org-end-of-meta-data) (point))))
                   (end (save-excursion (org-end-of-subtree t t) (point)))
                   (contents (buffer-substring (save-excursion (org-end-of-meta-data) (point)) end)))
              (-when-let (wrapper (and (not shown?) (string-match (concat "^" org-outline-regexp) contents)
                                       (--first (plist-get switches it) org--wrapping-switches)))
                (user-error "Section “%s” is %s, as a block, which cannot hold its subsection “%s”"
                            title wrapper (string-trim (substring contents (match-beginning 0)
                                                                  (string-search "\n" contents (match-beginning 0))))))
              (delete-region begin end)
              (goto-char begin)
              (insert (string-trim-right (org--block-with-switches title (org-plist-delete switches :when) backend
                                                                   contents #'identity nil
                                                                   (lambda (contents) (org--heading-source line contents))
                                                                   (org-entry-get nil "CUSTOM_ID")))
                      "\n")))
          (set-marker heading nil))))))

(provide 'org-block-switches)
;;; org-block-switches.el ends here
