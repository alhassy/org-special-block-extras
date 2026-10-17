;;; org-defblock.el --- Core org-defblock / org-deflink infrastructure  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.16
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8") (lf "26.10.1") (seq "2.0"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.github.io/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Core infrastructure for `org-defblock': the struct, parser, export pipeline,
;; and supporting macros.  The `org-deflink' macro, keymap, and nav helpers
;; live in `org-deflink' (required at the end of this file).
;;
;; Full documentation at https://alhassy.github.io/org-special-block-extras

;;; Code:

;;;; Imports

(require 's)
(require 'dash)
(require 'subr-x)
(require 'cl-lib)
(require 'cus-edit)
(require 'org)
(require 'ox-latex)
(require 'ox-html)
(require 'seq)
(require 'lf)
(require 'org-special-block-indirect-edit)
(require 'org-special-block-split)
(require 'org-deftag)

;;;; Core
;;;;; defstruct org-special-block

(cl-defstruct org-special-block
  "A representation of an Org special block."
  ;; Specification of slots:
  ;;
  ;;    ⟨start-point⟩#+begin_blk main-arg :key₀ val₀ … :keyₙ valₙ  ;; ⟵ ⟨kwdargs⟩
  ;;    contents
  ;;    #+end_blk⟨end-point⟩
  (name             nil :type string  :documentation "The name of the special block.")
  (start-point      nil :type integer :documentation "Point at start of “#+begin_” line.")
  (end-point        nil :type integer :documentation "Point at end of clause “#+end_⟨name⟩”.")
  (main-arg         nil :type string  :documentation "First non-keyword argument")
  (kwdargs          nil :type plist   :documentation "The key-value arguments for the header.")
  (contents         nil :type string  :documentation "Body text contents")
  (anchor           nil :type string  :documentation "The block's #+name:, which [[links]] reach; else nil."))

(defvar org--handler-anchor nil
  "The #+name: of the block whose handler is running; else nil.
E.g., a graph named `lineage' is a figure that [[lineage]] reaches.")


;; At compile time too, since `-let' patterns are expanded then.
(eval-and-compile
(defun dash-expand:&org-special-block (key source)
  "Destructuring which works with `org-special-block', for use with `-let'.

For example,

       (let ((blk (make-org-special-block :name \"foo\" :main-arg \"hola\")))
         (-let (((&org-special-block 'name 'main-arg) blk))
           (message \"Block %s with arg %s\" name main-arg)))

"
  `(progn (lf-assert ,source (org-special-block-p it))
          (,(intern (s-replace "'" "" (format "org-special-block-%s" key))) ,source))))


(defconst org--builtin-block-types '(quote-block verse-block center-block example-block comment-block export-block src-block)
  "Org's own block types, which an `org-defblock' of their name ---say, quote--- overrides; see `org-builtin-block'.")

(defun org--block-name (elem)
  "A special block, or one of Org's own, ELEM ↦ its name; e.g., \"tip\" for #+begin_tip, \"quote\" for #+begin_quote."
  (if (eq (org-element-type elem) 'special-block)
      (org-element-property :type elem)
    (save-excursion
      (goto-char (org-element-property :post-affiliated elem))
      (and (looking-at "[ \t]*#\\+begin_\\([^ \t\n]+\\)") (match-string-no-properties 1)))))

(defun org--block-parameters (elem)
  "A special block, or one of Org's own, ELEM ↦ its #+begin_ line's text after the name; e.g., \"Mind :color red\".
Org records it for a special block, yet not as such for its own, whose arguments mean things of their own."
  (if (eq (org-element-type elem) 'special-block)
      (org-element-property :parameters elem)
    (save-excursion
      (goto-char (org-element-property :post-affiliated elem))
      (and (looking-at "[ \t]*#\\+begin_[^ \t\n]+[ \t]+\\(.*[^ \t\n]\\)") (match-string-no-properties 1)))))

(defun org-builtin-block (name contents &optional header)
  "(NAME, CONTENTS, HEADER) ↦ Org's own #+begin_NAME HEADER block of CONTENTS, which `org-defblock' leaves to Org.
So a block overriding one of Org's own may hand some of them back; e.g., our quote, sans a citation:
  (org-parse (org-builtin-block \"quote\" raw-contents))
Its #+begin_ line carries a text property that `org--rewrite-special-blocks-by-handlers' steps past."
  (concat (propertize (format "#+begin_%s%s" name (if (org-string-nw-p header) (concat " " header) ""))
                      'org-builtin-block t)
          "\n" contents "\n#+end_" name))

(defun org--special-block-element-at-line (&optional name)
  "The `org-element' special block whose #+begin_ line point is on, named NAME if given; else nil.
Hence nil for a “#+begin_NAME” that Org reads as mere text; e.g., inside a src or example block.
One of Org's own blocks counts too, as `org-defblock' may override it; see `org--builtin-block-types'."
  (let ((elem (org-element-at-point)))
    (and (memq (org-element-type elem) (cons 'special-block org--builtin-block-types))
         (= (org-element-property :post-affiliated elem) (line-beginning-position))
         (or (null name) (equal (downcase (format "%s" name)) (downcase (org--block-name elem))))
         elem)))

(cl-defun org-special-block-after-point (&optional name)
  "The first `org-special-block' at or after point, named NAME if given; read via `org-element'.

Point should be at, or before, the start of the “#+begin_” clause.
A “#+begin_NAME” that Org reads as literal text ---say, inside a src block--- is skipped;
one it reads as a paragraph, since its “#+end_NAME” is missing, is an error."
  (save-excursion
    (let ((begin-rx (format "^[ \t]*#\\+begin_%s\\(?:[ \t]\\|$\\)"
                            (if name (regexp-quote (format "%s" name)) "[A-Za-z0-9_-]+")))
          elem)
      (beginning-of-line)
      (while (and (not elem) (re-search-forward begin-rx nil t))
        (beginning-of-line)
        (setq elem (if (eq (org-element-type (org-element-at-point)) 'paragraph)
                       'malformed
                     (org--special-block-element-at-line name)))
        (forward-line 1))
      (when (memq elem '(nil malformed))
        (error "‘org-special-block-after-point’: I had trouble parsing “#+begin_%s ⟨args⟩?\n⟨content⟩?\n#+end_%s”. Are the required pieces there? 🤔" name name))
      (-let* ((post-affiliated (org-element-property :post-affiliated elem))
              ;; Org's own blocks such as src record no contents region; theirs are the lines between the delimiters.
              (contents-begin (or (org-element-property :contents-begin elem)
                                  (and (not (eq (org-element-type elem) 'special-block))
                                       (save-excursion (goto-char (org-element-property :post-affiliated elem))
                                                       (forward-line 1) (point)))))
              (contents-end (or (org-element-property :contents-end elem)
                                (save-excursion (goto-char (org-element-property :end elem))
                                                (skip-chars-backward " \t\n\r") (line-beginning-position))))
              ;; The header line, after the name, as a Lisp list: positional arguments, then keyword ones.
              ((positional kwdargs) (--split-with (not (keywordp it))
                                                  (org--read-header (org--block-parameters elem)))))
        (make-org-special-block
         ;; Block keywords ignore case, as Org's do; e.g., #+BEGIN_SHOUT is a shout block.
         :name (downcase (org--block-name elem))
         :main-arg (mapconcat (lambda (it) (format "%s" it)) positional " ")
         :kwdargs kwdargs
         :anchor (org-element-property :name elem)
         :contents (if (and contents-begin (< contents-begin contents-end))
                       ;; Sans the least indentation of its lines, and the blank lines around them; so lines keep
                       ;; their indentation relative to one another, as an item nested in a list must (issue #8).
                       (string-trim-right
                        (replace-regexp-in-string
                         "\\`\\(?:[ \t]*\n\\)+" ""
                         (org-remove-indentation
                          (buffer-substring-no-properties contents-begin contents-end))))
                     "")
         ;; From the “#” of “#+begin_”, so the block's indentation stays put when it is replaced …
         :start-point (progn (goto-char post-affiliated) (skip-chars-forward " \t") (point))
         ;; … to the end of the “#+end_” line, sans the blank lines `org-element' counts as part of the block.
         :end-point (progn (goto-char (org-element-property :end elem)) (skip-chars-backward " \t\n\r") (line-end-position)))))))


(cl-defmethod org-eval-replace-block ((block org-special-block) backend)
  "Replace a special Org block with the result of evaluating its handler.

This function replaces the region from START-POINT to END-POINT of BLOCK
with the result of evaluating the corresponding handler function:
  `org-block/NAME'

Here, NAME is the `name' slot of the BLOCK (i.e., the name used in the
#+begin_NAME / #+end_NAME delimiters).

Each special block can include:
- A *main argument*: The first (optional) positional argument after the block name.
- *Keyword arguments*: Zero or more “:key value” pairs.
- A *body*: The content between the begin and end block markers.

The corresponding handler function must be named `org-block/NAME' with signature:

  (org-block/NAME BACKEND CONTENTS MAIN-ARG &rest KWDARGS)

Example:

  #+begin_foo mainarg :x 1 :y 2
  block content
  #+end_foo

Will be replaced with the result of evaluating:

  (org-block/foo BACKEND \"block content\" \"mainarg\" '(:x . 1) '(:y . 2))

The handler’s return value is inserted in place of the original block.
Indentation is preserved via `org-replace-text-while-preserving-indentation'.

This method is part of the Org export pipeline that processes supported
blocks listed in `org--supported-blocks', typically triggered during export
pre-processing steps."
  (-let* (((&org-special-block 'name 'main-arg 'kwdargs 'contents 'start-point 'end-point 'anchor) block)
          (handler (intern (format "org-block/%s" name)))
          ;; A block's own keyword of a switch's name wins; e.g., a block declaring `folded' gets :folded itself.
          (declared (--map (intern (format ":%s" it)) (-map #'car (-partition 2 (cddr (get handler 'org-defblock-args))))))
          (switch? (lambda (key) (and (memq key org-block-switches) (not (memq key declared)))))
          (switches (cl-loop for (key value) on kwdargs by #'cddr when (funcall switch? key) append (list key value)))
          (kwdargs (cl-loop for (key value) on kwdargs by #'cddr unless (funcall switch? key) append (list key value)))
          ;; The #+begin_ line's header, sans switches, for :verbatim to show the block as it is otherwise written.
          (header (save-excursion
                    (goto-char start-point)
                    (when (looking-at "#\\+begin_\\S-+[ \t]*\\(.*\\)$")
                      (cdr (org--header-sans-switches (match-string 1) switch?))))))
    (org-replace-text-while-preserving-indentation
     start-point
     end-point
     (org--block-with-switches
      name switches backend contents
      (lambda (contents)
        (let ((org--handler-anchor anchor))
          (apply handler backend contents main-arg kwdargs)))
      header))))

;;;;; Switches every block understands

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
           (source (lambda () (format "#+begin_%s%s\n%s\n#+end_%s" name (if (org-string-nw-p header) (concat " " header) "")
                                      (string-trim-right contents) name)))
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


(defun org-replace-text-while-preserving-indentation (start-point end-point multi-line-text)
  "Replace the region delimited by the given points with the given text, while preserving indentation."
  (save-excursion
    (goto-char start-point)
    ;; NOTE Related methods: current-column, indent-region, indent-line-to.
    (-let ((indent (current-indentation))
           ((head . tail) (split-string multi-line-text "\n")))
      (delete-region start-point end-point)
      (insert head)
      (when tail (insert "\n"))
      (insert
       (mapconcat
        (lambda (line) (concat (make-string indent ?\s) line))
        tail
        "\n")))))

;;;;; org-defblock-links-and-tags

(defgroup org-special-block-extras nil
  "Special blocks, links, and tags for Org, defined as one defines functions."
  :group 'org
  :prefix "org-")

(eval-and-compile ;; `org-defblock' reads this as it expands.
(defcustom org-defblock-links-and-tags t
  "Whether each `org-defblock' also makes a link and a tag of the same name.

With the default t, (org-defblock note …) gives three ways to use it:

  #+begin_note \"Heads up\"           ;; the block
  Mind the gap.
  #+end_note

  [[note:Heads up][Mind the gap.]]   ;; the link

  * Mind the gap.  :note_Heads_up:   ;; the tag, on a section

A block's own `:link' and `:tag' options override this; e.g., (org-defblock note
(&optional title :tag nil) …).  Since blocks read this when they are defined, set
it before loading `org-special-block-extras', whose own blocks are defined then.

The costs are slight: per block, one function and one link type or export hook;
and each export scans once for each tag, walking the headings only when the tag
appears.  The real concern is namespacing: every block claims the link type
NAME: and the tag :NAME:.  By way of example, the built-in colour blocks claim
:red:, :blue:, … ; so, with this on, a section you have tagged :red: for your
own reasons exports in red.  Set this to nil to make blocks only, then make
links with `org-deflink' and tags with `org-deftag' just where wanted."
  :type 'boolean
  :group 'org-special-block-extras))

;;;;; org--rewrite-special-blocks-by-handlers

(defvar org--supported-blocks nil
  "Which special blocks, defined with `org-defblock', are supported.

Such blocks are read using `org-special-block-after-point'.

This is a list of strings.")


(defvar org--block-fontifications nil
  "Alist of (NAME . PLIST): how each block NAME, declared with `:fontify', looks in Org buffers; see `org--fontify-blocks'.")

(defvar org--current-backend nil
  "A message-passing channel updated by `org--rewrite-special-blocks-by-handlers'
and used by `org-defblock'.

This is a symbol.")


(defun org--rewrite-special-blocks-by-handlers (backend)
  "Replace supported Org special blocks with the result of their handlers.

BACKEND is a symbol representing the current export backend (e.g. 'html, 'latex),
and is bound globally to `org--current-backend' for use by block handlers.

Note: This function mutates the current buffer."
  (setq backend (org--base-backend backend)
        org--current-backend backend)
  (cl-loop for blk in org--supported-blocks
           do (goto-char (point-min))
           (while (re-search-forward (format "^\\s-*\\#\\+begin_%s\\b" blk) nil t)
             (beginning-of-line)
             ;; A replaced block's output is rescanned, as it may hold blocks too; literal text, such as
             ;; in a src block, is stepped past; a malformed block reaches the parser, to report it.  One of
             ;; Org's own blocks, once overridden, is ours too ---save one a handler hands back by `org-builtin-block'.
             (if (let ((elem (org-element-at-point)))
                   (or (memq (org-element-type elem) '(special-block paragraph))
                       ;; Its own #+begin_ line, that is: a #+begin_note written within a src block is its text.
                       (and (memq (org-element-type elem) org--builtin-block-types)
                            (= (org-element-property :post-affiliated elem) (line-beginning-position))
                            (not (get-text-property (save-excursion (skip-chars-forward " \t") (point)) 'org-builtin-block)))))
                 (org-eval-replace-block (org-special-block-after-point blk) backend)
               (forward-line 1)))))

;;;;; LaTeX packages, loaded as needed

(defvar-local org--latex-head-used nil
  "What the blocks this export has rendered need in the LaTeX preamble, by their `:latex-head'; see `org--load-latex-head'.
Local to the buffer an export works in: each export, nested ones included, works in a copy of its own.")

(defun org--load-latex-head (output backend _info)
  "(OUTPUT, BACKEND) ↦ LaTeX OUTPUT whose preamble holds, once, each item its blocks need, `org--latex-head-used'.
A package name, \"xcolor\", is \\usepackage{xcolor} unless the preamble loads it already, options and all; any
other line, \"\\\\newcommand{\\\\QED}{∎}\", is added unless the preamble has it.  They go just before \\begin{document};
so after the document's own lines.  A body-only export, sans preamble, is left be."
  (if (not (and (org-export-derived-backend-p backend 'latex) org--latex-head-used
                (string-match "^\\\\begin{document}" output)))
      output
    (let* ((preamble (substring output 0 (match-beginning 0)))
           (package? (lambda (item) (string-match-p "\\`[[:alnum:]-]+\\'" item)))
           (present? (lambda (item)
                       (if (not (funcall package? item)) (s-contains? item preamble)
                         (string-match-p (format "\\\\\\(?:usepackage\\|RequirePackage\\)\\(?:\\[[^]]*\\]\\)?{\\(?:[^}]*,\\)?[ \t]*%s[ \t]*[,}]"
                                                 (regexp-quote item))
                                         preamble))))
           (missing (-remove present? (reverse org--latex-head-used))))
      (if (not missing) output
        (concat preamble
                (mapconcat (lambda (item) (format (if (funcall package? item) "\\usepackage{%s}\n" "%s\n") item)) missing "")
                (substring output (length preamble)))))))

(defvar-local org--html-head-used nil
  "What the blocks, and links, this export has rendered need in an HTML page's <head>; see `org--load-html-head'.
Local to the buffer an export works in, as `org--latex-head-used' is.")

(defun org-html-head-require (item)
  "Note that this HTML export's page needs ITEM in its <head>: HTML, a symbol whose value is HTML, or `mathjax'.
E.g., our tooltips ask for (org-html-head-require \='org--tooltip-html-head); a block asks by its `:html-head'."
  (cl-pushnew item org--html-head-used :test #'equal))

(defun org--html-mathjax (info)
  "INFO ↦ Org's own MathJax setup, by the user's MathJax options, as for a page whose maths is Org's own.
Org sets MathJax up only for maths it parsed itself; ours, such as a calc block's, is raw HTML to it."
  (org-html--build-mathjax-config
   (plist-put (copy-sequence info) :parse-tree '(org-data nil (latex-fragment (:value "\\(x\\)"))))))

(defun org--load-html-head (output backend info)
  "(OUTPUT, BACKEND, INFO) ↦ HTML OUTPUT whose <head> holds each item its blocks need, `org--html-head-used', once.
Our own CSS and JS only if `org-special-block-add-html-extra'; MathJax always, else our maths would not render."
  (if (not (and (org-export-derived-backend-p backend 'html) org--html-head-used
                (string-match "</head>" output)))
      output
    (let* ((at (match-beginning 0))
           (heads (-non-nil
                   (--map (cond ((eq it 'mathjax) (unless (string-match-p "MathJax" output) (org--html-mathjax info)))
                                ((not org-special-block-add-html-extra) nil)
                                ((symbolp it) (symbol-value it))
                                (t it))
                          (reverse org--html-head-used))))
           (missing (--remove (s-contains? it output) (-uniq heads))))
      (if (not missing) output
        (concat (substring output 0 at) (apply #'concat missing) (substring output at))))))

;;;;; org--rewrite-any-block-switches

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
    (let ((switch? (lambda (key) (memq key org-block-switches)))
          (case-fold-search t)
          blocks)
      (org-element-map (org-element-parse-buffer 'element)
          '(src-block example-block export-block comment-block quote-block center-block verse-block special-block)
        (lambda (block)
          (unless (and (eq (org-element-type block) 'special-block)
                       (member (downcase (org-element-property :type block)) (mapcar (lambda (b) (downcase (format "%s" b))) org--supported-blocks)))
            (save-excursion
              (goto-char (org-element-property :post-affiliated block))
              (when (looking-at "^\\([ \t]*\\)\\(#\\+begin_\\(\\S-+\\)\\)[ \t]*\\(.*\\)$")
                (-let* ((indent (length (match-string 1)))
                        (hash (match-beginning 2))
                        (name (match-string 3))
                        ((switches . header) (org--header-sans-switches (match-string 4) switch?)))
                  (when switches
                    (when (and (eq (org-element-type block) 'src-block) (eq (plist-get switches :folded) t))
                      (setq switches (plist-put switches :folded (or (org-element-property :language block) "Code"))))
                    (push (list (copy-marker hash)
                                (copy-marker (save-excursion (goto-char (org-element-property :end block))
                                                             (skip-chars-backward " \t\n\r")
                                                             (line-end-position))
                                             t)
                                indent name header switches)
                          blocks))))))))
      ;; `blocks' lists the last block first; inner blocks, beginning later, are so rewritten before outer ones.
      (pcase-dolist (`(,begin ,end ,indent ,name ,header ,switches) blocks)
        (let* ((start (marker-position begin))
               (lines (split-string (buffer-substring start end) "\n"))
               ;; The contents sans the block's own indentation, which the rewrite puts back.
               (contents (mapconcat (lambda (line) (if (string-prefix-p (make-string indent ?\s) line) (substring line indent) line))
                                    (butlast (cdr lines)) "\n")))
          (org-replace-text-while-preserving-indentation
           start end
           (org--block-with-switches
            name switches backend contents
            (lambda (contents) (format "#+begin_%s%s\n%s\n#+end_%s" name (if (org-string-nw-p header) (concat " " header) "")
                                       contents name))
            header)))))))

;;;;; org--read-header

(defun org--read-header (header)
  "HEADER ↦ its words as Lisp values, a word Lisp cannot read whole staying a string.
  \"Title :n 2 :color #6c7086 :font Helvetica,Arial\"  ↦  (Title :n 2 :color \"#6c7086\" :font \"Helvetica,Arial\")
So hex colours need no quotes, as with Org's own #+attr_html: lines; \"quoted strings\" and (lists) read as Lisp."
  (let ((header (or header "")) (pos 0) values)
    (while (string-match "[^ \t\n]+" header pos)
      (let* ((beg (match-beginning 0)) (word-end (match-end 0))
             (read (condition-case nil (read-from-string header beg) (error nil))))
        (if (and read (>= (cdr read) word-end))
            (setq values (cons (car read) values) pos (cdr read))
          (setq values (cons (substring header beg word-end) values) pos word-end))))
    (nreverse values)))

;;;;; org--parse-plain-list

(defun org--parse-plain-list (string block)
  "(STRING, BLOCK) ↦ `org-list-to-lisp' of the Org plain list STRING; else a `user-error' naming the BLOCK."
  (condition-case err
      (with-temp-buffer
        (delay-mode-hooks (org-mode))
        (insert string)
        (goto-char (point-min))
        (org-list-to-lisp))
    (error (user-error "#+begin_%s block does not contain a valid Org plain list; each item must be a `-' or `+' bullet.\nParsing error: %s"
                       block (error-message-string err)))))

;;;;; org--latex-define-color

(defun org--latex-define-color (name colour)
  "(NAME, COLOUR) ↦ LaTeX defining NAME as COLOUR: via \\definecolor when COLOUR is a hex code such as #e5f5e5, or a
name xcolor lacks but Emacs knows, such as navy; else \\colorlet, as for red or red!5!white.
COLOUR may be a symbol, as block arguments such as “:color red!5!white” are read as Lisp."
  (setq colour (format "%s" colour))
  (-if-let (rgb (and (not (string-prefix-p "#" colour))
                     (not (string-match-p "!" colour))       ; A mix, such as red!5!white, is xcolor's own.
                     (not (and (boundp 'org--named-colors) (memq (intern colour) org--named-colors)))
                     ;; Emacs's X11 colour table, which a display-less batch Emacs reads too; e.g., navy, gold.
                     (tty-color-standard-values colour)))
      (apply #'format "\\definecolor{%s}{rgb}{%.3f,%.3f,%.3f}" name (--map (/ it 65535.0) rgb))
    (if (string-prefix-p "#" colour)
        (format "\\definecolor{%s}{HTML}{%s}" name (substring colour 1))
      (format "\\colorlet{%s}{%s}" name colour))))

;;;;; org-defblock

(eval-and-compile ;; `org-defblock' calls this as it expands.
(defun org--defblock-desugar-args (args)
  "ARGS ↦ the flat (MAIN DEFAULT KEY₁ DEFAULT₁ …) arglist, when ARGS is `cl-defun'-style; otherwise ARGS.

  (&optional (title \"Details\") &key (color \"green\") style)  ↦  (title \"Details\" color \"green\" style nil :allow-other-keys nil)
  (&key style &allow-other-keys)                               ↦  (main-arg nil style nil :allow-other-keys t)

A block takes at most one positional argument ---its main argument--- so `&optional' binds at most one
variable, and nothing may precede `&optional' or `&key'.  Options such as `:tag t' may appear anywhere
---argument names are never keywords--- and pass through untouched, at the end.  As with `cl-defun',
unknown keywords are refused unless `&allow-other-keys' is written; the flattened form always allows them."
  (if (not (or (memq '&optional args) (memq '&key args)))
      args
    (let* ((options  (append (cl-loop for (option value) on args
                                      when (keywordp option) append (list option value))
                             (list :allow-other-keys (and (memq '&allow-other-keys args) t))))
           (args     (let ((rest (remq '&allow-other-keys args)) kept)
                       (while rest
                         (if (keywordp (car rest)) (setq rest (cddr rest)) (push (pop rest) kept)))
                       (nreverse kept)))
           ;; ((NAME DEFAULT POSITIONAL?) …), as for tags; a block's one positional argument is its main argument.
           (parsed   (org--deftag-parse-args args))
           (main     (--filter (nth 2 it) parsed)))
      (when (cdr main)
        (error "‘org-defblock’: a block takes at most one &optional (main) argument, but saw %S" (mapcar #'car main)))
      (append (if main (-take 2 (car main)) (list 'main-arg nil))
              (--mapcat (-take 2 it) (--remove (nth 2 it) parsed))
              options)))))

(eval-and-compile ;; `org-defblock' calls this as it expands.
(defun org--defblock-tag-subsections (name tag)
  "(NAME, TAG option) ↦ what NAME's tag does with a section's subsections: `ignore', `error', or `recurse'.

  t                        ↦  ignore
  [:subsections recurse]   ↦  recurse
  [:subsections sideways]  ↦  an error, at once, naming NAME

See `org--defblock-section-to-block' for what each mode does."
  (let* ((spec (if (vectorp tag) (append tag nil) nil))
         (mode (or (plist-get spec :subsections) 'ignore)))
    (unless (and (cl-evenp (length spec)) (cl-loop for key in spec by #'cddr always (eq key :subsections)))
      (error "‘org-defblock %s’: :tag takes t, nil, or [:subsections MODE], but saw %S" name tag))
    (unless (memq mode '(ignore error recurse))
      (error "‘org-defblock %s’: :subsections takes ignore, error, or recurse, but saw %S" name mode))
    mode)))

(defun org--defblock-section-to-block (name heading drawer tag-args properties main keywords backend
                                            &optional subsections other-keys?)
  "Rewrite the section at point ---its heading line and DRAWER erased by `org-deftag'--- as HEADING atop a NAME block.

  * Why bother? :details_Spoiler_alert:        * Why bother?
  :PROPERTIES:                                 :PROPERTIES:
  :COLOR: pink                          ↦      :COLOR: pink
  :END:                                        :END:
  Body text…                                   #+begin_details \"Spoiler alert\" :color \"pink\"
                                               Body text…
                                               #+end_details

TAG-ARGS, joined by spaces, form the main argument, else the section's MAIN property; each of the
KEYWORDS found among the section's PROPERTIES becomes a keyword argument ---and, when OTHER-KEYS? since
the block allows other keys, so does every other property bar Org's own, such as CATEGORY and CUSTOM_ID.
The section's own text is
wrapped, up to the next heading of any depth; a block cannot hold a heading, so SUBSECTIONS says what
becomes of the section's subsections:
+ `ignore' (the default): they stay outside the block, exported as usual.
+ `error': a section with a subsection is an error, naming both; e.g., for a block whose
  content would be silently cut short.
+ `recurse': each subsection's own text, at any depth, is wrapped likewise ---same arguments---
  in a NAME block of its own; a subsection tagged :NAME: itself is left to its own tag.

The block is then at once replaced by the result of `org-block/NAME' for BACKEND; so a tag works even
when NAME is not registered as a block, via `:block nil'.  Point ends after the last result."
  (let* ((property (lambda (key) (plist-get properties (intern (format ":%s" (downcase (symbol-name key)))))))
         (main-arg (if tag-args (string-join tag-args " ") (funcall property main)))
         (keywords (if (not other-keys?) keywords
                     (let ((orgs (--map (intern (downcase it)) (append org-special-properties org-default-properties))))
                       (-union keywords (cl-loop for key in properties by #'cddr
                                                 for name = (intern (substring (symbol-name key) 1))
                                                 unless (or (memq name orgs) (eq name main)) collect name)))))
         (header (concat (if main-arg (format " %S" main-arg) "")
                         (mapconcat (lambda (key) (-if-let (value (funcall property key)) (format " :%s %S" key value) ""))
                                    keywords "")))
         (level (progn (string-match "\\`\\*+" heading) (match-end 0)))
         ;; Point at a heading line ↦ its subheading's level, or nil when it's not deeper than LEVEL.
         (subheading-level (lambda () (let ((depth (- (save-excursion (skip-chars-forward "*") (point)) (point))))
                                        (and (> depth level) depth))))
         (wrap-own-text
          (lambda ()
            "From the start of a section's text, wrap it up to the next heading, and render that block."
            (let ((start (point)) (end (make-marker)))
              (insert (format "#+begin_%s%s\n" name header))
              (goto-char (if (re-search-forward org-outline-regexp-bol nil t) (match-beginning 0) (point-max)))
              (unless (bolp) (insert "\n"))
              (insert (format "#+end_%s" name))
              (set-marker end (point))
              (set-marker-insertion-type end t)
              ;; Keep #+end_NAME on a line of its own, apart from any following heading.
              (save-excursion (insert "\n"))
              (goto-char start)
              ;; As in `org--rewrite-special-blocks-by-handlers': blocks render for backends derived from html or latex.
              (org-eval-replace-block (org-special-block-after-point name) (org--base-backend backend))
              (goto-char end)
              (set-marker end nil)))))
    ;; Restore the heading, and its property drawer, so the section keeps its properties; e.g., CUSTOM_ID.
    (insert heading "\n" (or drawer ""))
    (unless (bolp) (insert "\n"))
    (when (eq subsections 'error)
      (save-excursion
        (when (and (re-search-forward org-outline-regexp-bol nil t)
                   (progn (beginning-of-line) (funcall subheading-level)))
          (user-error "Section “%s” is tagged :%s:, whose block cannot hold its subsection “%s”; move the subsection out, or declare `:tag [:subsections ignore]' or `[:subsections recurse]' in ‘org-defblock %s’"
                      (string-trim (substring heading level)) name
                      (string-trim (buffer-substring (+ (point) (funcall subheading-level)) (line-end-position)))
                      name))))
    (funcall wrap-own-text)
    (when (eq subsections 'recurse)
      (let ((own-tag (org--deftag-regexp name)))
        (while (and (re-search-forward org-outline-regexp-bol nil t)
                    (progn (beginning-of-line) (funcall subheading-level)))
          (let ((case-fold-search t))
            (if (looking-at (concat ".*" own-tag))
                (forward-line 1)
              (forward-line 1)
              (when (looking-at org-property-drawer-re)
                (goto-char (match-end 0))
                (forward-line 1))
              (funcall wrap-own-text))))))))

(cl-defmacro org-defblock
    (name kwds &optional docstring &rest body)
  "Declare a new special block, and link, in the style of `defun'.

To remove support for a block NAME, simply use `org-undefblock'.

A full featured example is at the end of this documentation string.

This is an anaphoric macro that provides export support for
special blocks *and* links named NAME. Just as an Org-mode
src-block consumes as main argument the language for the src
block, our special blocks too consume a MAIN-ARG; it may be a
symbol or a cons-list consisting of a symbolic name (with which
to refer to the main argument in the definition of the block)
followed by a default value, then, optionally, any information
for a one-time setup of the associated link type.

The main arg may be a sequence of symbols separated by spaces,
and a few punctuation with the exception of comma ‘,’ since it is
a special Lisp operator. In doubt, enclose the main arg in
quotes.

Then, just as Org-mode src blocks consume key-value pairs, our
special blocks consume a number of keyword arguments.

Together, the main and keyword arguments form KWDS, written as a
`cl-defun' arglist; by way of example,

   (&optional (title \"Details\") &key (color \"green\") style)

declares main argument TITLE, defaulting to \"Details\", and
keyword arguments COLOR and STYLE.  As a convenience, KWDS may
instead be flattened into (main default key₀ value₀ … keyₙ valueₙ);
the above is then (title \"Details\" color \"green\" style nil).
As with `cl-defun', a header line's unknown keywords are an export-time
error unless KWDS writes `&allow-other-keys'; the flattened form always
allows them.

KWDS may also carry options, which are not arguments:
+ `:block nil' declines block support: #+begin_NAME is left to Org, yet the
  link and tag still work, through `org-block/NAME'.
+ `:link nil' declines the link type NAME:; `:link t' makes it; and `:link
  [:face … :display …]' makes it with that `org-deflink' display; e.g.,
  [:face \='(:foreground \"red\")] shows such links in red.
+ `:tag nil' declines the tag: otherwise a section tagged :NAME: exports as a
  NAME block around its text; the tag's underscore-separated items form the
  main argument, and the section's properties supply keyword arguments by
  name ---every property bar Org's own, when the block allows other keys.
  E.g., \"* Stuck? :details_Hint:\" folds its text away under the
  title Hint.  See `org--defblock-section-to-block'.
  A block cannot hold a heading, so `:tag [:subsections MODE]' says what becomes
  of a tagged section's subsections: `ignore' leaves them outside the block, as
  `:tag t' does; `error' refuses a section that has any; and `recurse' wraps
  each subsection's text in a block of its own.
  The link and tag default to `org-defblock-links-and-tags'; for just one of
  them, see `org-deflink' and `org-deftag'.
+ `:edit-indirect-mode MODE' makes C-c \=' edit the block in MODE.
+ `:latex-head (ITEM …)' is what a LaTeX document using the block, or its link, needs in its preamble, added once:
  a package name, or any preamble line; e.g., unquoted,
      :latex-head (\"tcolorbox\" \"\\\\newtcolorbox{aside}{colback=gray!5}\")
  Name a package, \"tcolorbox\", rather than write \"\\\\usepackage{tcolorbox}\": a name is left be when the document
  loads it already, options and all; a line, only when the document has that very line.  See `org--load-latex-head'.
+ `:html-head (ITEM …)' is what an HTML page using the block needs in its <head>, added once: a string of HTML, a
  symbol whose value is one, or `mathjax' for Org's own MathJax setup; e.g., unquoted,
      :html-head (mathjax my-html-head \"<style>.aside {color: gray}</style>\")
  A symbol shares one head among many blocks, as `org--tooltip-html-head' serves every tooltip.  See
  `org--load-html-head'.
+ `:fontify [:begin STRING :end STRING :face FACE]' says how the block looks in Org buffers;
  e.g., [:begin \"Proof.\" :end \"⏹\" :face \='italic] shows “#+begin_proof” as “Proof.”,
  “#+end_proof” as “⏹”, and the contents in italics too.  Moreover, :keywords \='((REGEXP GROUP FACE) …) shows
  each REGEXP match in the contents in FACE; e.g., the `align' block's “:tag-blue:” shows as a tag, in blue.
  Each part is optional; see `org--fontify-blocks'.

After that is an optional DOCSTRING, a familar feature of DEFUN.
The docstring is displayed as part of the tooltip for the
produced link type.

Finally, the BODY is a (sequence of) Lisp forms ---no progn
needed--- that may refer to the names BACKEND and CONTENTS which
refer to the current export backend and the contents of the
special block ---or the description clause of a link.

CONTENTS refers to an Org-mode parsed string; i.e., Org-markup is
acknowledged.

In, hopefully, rare circumstances, one may refer to RAW-CONTENTS
to look at the fully unparsed contents.

Finally, this macro exposes two functions:
+ ORG-EXPORT: Wrap the argument in an export block for the current backend.
+ ORG-PARSE: This should ONLY be called within an ORG-EXPORT call,
             to escape text to Org, and out of the export block.

⇄ We use “@@html:⋯:@@” when altering CONTENTS, but otherwise use raw HTML *around* CONTENTS.
⇄ For example: (format \"<div>%s</div>\" (s-replace \"#+columnbreak:\" \"@@html:<hr>@@\" contents))

----------------------------------------------------------------------

The relationship between links and special blocks:

  [ [type:label][description]]
≈
   #+begin_type label
    description
   #+end_type

----------------------------------------------------------------------

Example declaration, with all possible features shown:

   ;; We can use variable values when defining new blocks
   (setq angry-red '(:foreground \"red\" :weight bold))

   (org-defblock remark
     (&optional (editor \"Editor Remark\") &key (color \"red\") (signoff \"\")
      :link [:face angry-red])
     \"Top level (HTML & LaTeX) editorial remarks; in Emacs they're angry red.\"
     (format (if (equal backend 'html)
               \"<strong style=\\\"color: %s;\\\">⟦%s:  %s%s⟧</strong>\"
               \"{\\color{%s}\\bfseries %s:  %s%s}\")
             color editor contents signoff))

   ;; I don't want to change the definition, but I'd like to have
   ;; the following as personalised defaults for the “remark” block.
   ;; OR, I'd like to set this for links, which do not have argument options.
   (org-set-block-header-args remark :main-arg \"Jasim Jameson\" :signoff \"( Aim for success! )\")

Three example uses:

    ;; ⟨0⟩ As a special blocks with arguments given.
    #+begin_remark Bobbert Barakallah :signoff \"Thank-you for pointing this out!\" :color green
    I was trying to explain that ${\large (n × (n + 1) \over 2}$ is always an integer.
    #+end_remark

    ;; ⟨1⟩ As a terse link, using default values for the args.
    ;;     Notice that Org-mode formatting is recoqgnised even in links.
    [ [remark:Jasim Jameson][Why are you taking about \"$\\mathsf{even}$\" here?]]

    ;; ⟨2⟩ So terse that no editor name is provided.
    [ [remark:][Please improve your transition sentences.]]

    ;; ⟨★⟩ Unlike 0, examples 1 and 2 will have the default SIGNOFF
    ;; catenated as well as the default red color."
  (declare (indent defun))

  (when (vectorp docstring)
    (error "‘org-defblock %s’: the link display vector now goes among the options, as `:link %S'" name docstring))
  (unless (stringp docstring) (setq body (cons docstring body) docstring nil))
  ;; The options :block, :link, :tag, :edit-indirect-mode, :fontify, :latex-head, and :html-head configure the macro;
  ;; they are not block arguments.
  (let* ((kwds      (org--defblock-desugar-args kwds))
         (edit-mode (plist-get kwds :edit-indirect-mode))
         (latex-head (plist-get kwds :latex-head))
         (html-head (plist-get kwds :html-head))
         (fontify   (plist-get kwds :fontify))
         ;; The block is on unless declined via `:block nil'; the link and tag follow
         ;; `org-defblock-links-and-tags' unless the block says otherwise; e.g., `:tag nil'.
         (enabled?  (lambda (option) (cond ((plist-member kwds option) (plist-get kwds option))
                                           ((eq option :block) t)
                                           (t org-defblock-links-and-tags))))
         (block?    (funcall enabled? :block))
         (link      (funcall enabled? :link)) ;; nil, t, or a display vector
         (tag?      (funcall enabled? :tag)) ;; nil, t, or [:subsections MODE]
         (subsections (when tag? (org--defblock-tag-subsections name tag?)))
         ;; As in `org-defblock-only': other keywords are welcome unless `cl-defun'-style KWDS omit `&allow-other-keys'.
         (other-keys? (or (not (plist-member kwds :allow-other-keys)) (plist-get kwds :allow-other-keys)))
         (kwds      (cl-reduce #'org-plist-delete '(:block :link :tag :edit-indirect-mode :fontify :latex-head :html-head)
                               :initial-value kwds)))
    `(progn
       ;; NOTE: We register the support at run-time and not earlier at macro-expansion time,
       ;; so that the global variable `org--supported-blocks' can be captured by any `let' clauses
       ;; that are around this `org-defblock' call.
       ,@(when block? `((add-to-list 'org--supported-blocks ',name)))
       ;; Overriding one of Org's own blocks is fair game, yet worth a word.
       ,@(when (and block? (memq (intern (format "%s-block" name)) org--builtin-block-types))
           `((message "org-defblock: %s overrides Org's own #+begin_%s; a handler may hand some back by `org-builtin-block'"
                      ',name ',name)))
       (org-defblock-only ,name ,kwds ,docstring ,@body)
       ;; ⇨ The LaTeX packages it needs, loaded for it when unloaded; a redefinition sans :latex-head needs none.
       (put ',(intern (format "org-block/%s" name)) 'org-latex-head ',latex-head)
       ;; ⇨ Likewise, the HTML its pages need in their <head>.
       (put ',(intern (format "org-block/%s" name)) 'org-html-head ',html-head)
       ;; ⇨ The link type support, unless declined via `:link nil'
       ,@(when link
           `((org-deflink ,name
                          ,(format "%s\n\nAs a link, [[%s:LABEL][DESCRIPTION]] ≈ #+begin_%s LABEL ⋯ DESCRIPTION ⋯ #+end_%s."
                                   (or docstring "") name name name)
                          ,(vconcat `[:help-echo (format "%s:%s\n\n%s" ',name o-label ,(or docstring ""))]
                                    (and (vectorp link) link))
                          (s-replace-regexp "@@" ""
                                            (,(intern (format "org-block/%s" name)) o-backend (or o-description o-label) o-label
                                             :contents-occur-as-link-description t)))))
       ;; ⇨ Auto-generate C-c ' handler when :edit-indirect-mode is specified.
       (org-defblock-setup-indirect-edit ,name ,edit-mode)
       ;; ⇨ How the block looks in Org buffers; a redefinition sans :fontify forgets any earlier look.
       (setf (alist-get ,(downcase (format "%s" name)) org--block-fontifications nil 'remove #'equal)
             ,(when fontify `(list ,@(append fontify nil))))
       ;; ⇨ The tag, unless declined via `:tag nil': a section tagged :NAME: exports as a NAME block around its text.
       ,@(when tag?
           `((org-deftag ,name ()
                         ,(format "Export each section tagged :%s: as a %s block around its text; see `org-defblock'." name name)
                         (org--defblock-section-to-block ',name o-heading o-drawer o-tag-args o-properties
                                                         ',(or (car kwds) 'main-arg)
                                                         ',(mapcar #'car (-partition 2 (cddr (org-plist-delete kwds :allow-other-keys))))
                                                         o-backend ',subsections ,other-keys?)))))))


;; WHERE ...


(cl-defmacro org-defblock-only (name args &optional docstring &rest body)
  "Generate a Lisp `org-block/NAME' export function from a `org-defblock' definition.


★ Like `org-defblock' but only supports blocks, not links. ★


- NAME         [Symbol]: The name of the block type.
- DOCSTRING    [Nullable String]: Documentation of the block.
- ARGS: A `cl-defun'-style arglist, such as (&optional (title \"Details\") &key (color \"green\") style),
        or its flattened convenience form (title \"Details\" color \"green\" style nil): the main
        argument and its default, followed by “keyword default-value” pairs.
        See `org--defblock-desugar-args'.
- BODY: Code to be executed just before export.
  ⇒ May mention symbol “backend” and string “raw-contents”.

Features:
+ Auto-defaults for main and keyword args via `org--header-args'.
  Default values can be set long after the associated handler is created.
+ Optional `contents-occur-as-link-description' flag for minimal output (used in links).
+ Automatic wrapping in export blocks (`org-export', `org-parse')."
  (declare (indent defun))

  (setq args (org--defblock-desugar-args args))
  ;; Unknown header-line keywords are welcome unless a `cl-defun'-style ARGS omitted `&allow-other-keys'.
  (let ((allow-other-keys? (or (not (plist-member args :allow-other-keys)) (plist-get args :allow-other-keys))))
    (setq args (org-plist-delete args :allow-other-keys))

  ;; Account for possibly omitted docstring
  (cond
   ;; Mimicking (cl-defun f () "hi") then (f) ⇒ "hi"
   ((null body) (setq body (list docstring)) (setq docstring ""))
   ;; Mimicking (cl-defun f () 'x 'y) then (f) ⇒ 'y
   ((not (stringp docstring)) (setq body (cons docstring body)) (setq docstring "")))

  (let ((defun-name (intern (format "org-block/%s" name)))
        (main-arg-name (or (cl-first args) 'main-arg))
        (main-arg-default-value (cl-second args))
        (keywords (cddr args)))
    (progn
      `(prog1
           (cl-defun ,defun-name
               (backend      ;; Symbol
                raw-contents ;; String
                &optional ,main-arg-name
                &rest  org--handler-args
                ;; No defaults here: an omitted keyword must arrive as nil, so the body below can
                ;; prefer any `org-set-block-header-args' value over the declared default.
                &key ,@(mapcar #'car (-partition 2 keywords)) (contents-occur-as-link-description nil)
                ;; Always allowed here: we refuse unknown keywords ourselves, below, naming only the user's own.
                &allow-other-keys)
             ;; Assertions are expanded here, once: the handler runs interpreted, which would re-expand them on every call.
             ,(macroexpand-all `(lf-assert ',name        t
                                           backend      (and it (symbolp it))
                                           raw-contents (stringp it)))

             ,@(unless allow-other-keys?
                 (let ((known (--map (intern (format ":%s" it)) (cons main-arg-name (mapcar #'car (-partition 2 keywords))))))
                   `((-when-let (unknown (cl-loop for key in org--handler-args by #'cddr
                                                  unless (memq key '(:contents-occur-as-link-description ,@known))
                                                  collect key))
                       (error "‘%s’ block: unknown header argument%s %s; it takes %s" ',name
                              (if (cdr unknown) "s" "") (mapconcat #'symbol-name unknown " ")
                              ,(if known (mapconcat #'symbol-name known " ") "no header arguments"))))))

             ;; A blank main argument may be given by its own name, as #+begin_satz :name N is #+begin_satz N
             ;; (issue #47); else it is the block's header-args default, else its declared default.
             (when (or (null ,main-arg-name) (equal ,main-arg-name ""))
               (setq ,main-arg-name
                     (or
                      (plist-get org--handler-args ,(intern (format ":%s" main-arg-name)))
                      (org--header-arg-of ',name :main-arg)
                      ,main-arg-default-value)))

             ;; Use any headers for this block type, if no local value is passed
             ,@(cl-loop for (key default-value) in (-partition 2 keywords)
                        collect `(when (or (null ,key) (equal ,key ""))
                                   (setq ,key (or
                                               (org--header-arg-of
                                                ',name
                                                ,(intern (format ":%s" key)))
                                               ,default-value))))

             ;; Lexical, since BODY is spliced right here; `cl-letf' would clobber any global `org-export'.
             (cl-flet ((org-export (x) "Wrap the given X in an export block for the current backend.
A result opening in Org opens sans an empty export block; so, say, a named block's #+name: and #+caption: label what
it begins with."
                         (if contents-occur-as-link-description x
                           ;; A result that opens in Org, as by `org-parse', does so sans an empty export block, which
                           ;; would be a stray blank line; likewise one closing in Org with a block's #+end_ line; e.g.,
                           ;; Org's own quote, handed back by `org-builtin-block', is just as Org would have it.
                           (let* ((wrapped (format "#+begin_export %s\n%s\n#+end_export" backend x))
                                  (empty-open (format "#+begin_export %s\n\n#+end_export\n" backend))
                                  (empty-close (format "\n#+begin_export %s\n\n#+end_export" backend)))
                             (when (string-prefix-p empty-open wrapped) (setq wrapped (substring wrapped (length empty-open))))
                             ;; Closing, only after a block's #+end_ line: after a paragraph, the empty export block
                             ;; is what keeps the text following our block from joining that paragraph.
                             (when (and (string-suffix-p empty-close wrapped)
                                        (string-match-p "^[ \t]*#\\+end_[^ \t\n]+[ \t]*\\'"
                                                        (substring wrapped 0 (- (length wrapped) (length empty-close)))))
                               (setq wrapped (substring wrapped 0 (- (length wrapped) (length empty-close)))))
                             ;; Org text within braces hugs them, as LaTeX reads each newline as a space; e.g.,
                             ;; \blockcquote{key}{%⏎ This…⏎\ifhmode\unskip\fi} is \blockcquote{key}{This…}; issue #27.
                             (if (not (eq backend 'latex)) wrapped
                               (--> wrapped
                                    ;; We add % after { to suppress the newline that opens the contents …
                                    (replace-regexp-in-string "\\(^\\|[^\\]\\){\n#\\+end_export\n"
                                                              "\\1{%\n#+end_export\n" it t)
                                    ;; … and \unskip before } to drop the space of the newline that closes them;
                                    ;; only amid text, as in vertical mode \unskip would eat the space after a list.
                                    (replace-regexp-in-string "\n#\\+begin_export latex\n}"
                                                              "\n#+begin_export latex\n\\ifhmode\\unskip\\fi}"
                                                              it t t))))))
                       (org-parse (x) "This should ONLY be called within an ORG-EXPORT call."
                         (if contents-occur-as-link-description x (format "\n#+end_export\n%s\n#+begin_export %s\n" x backend))))
               ;; The body must yield a string: otherwise, e.g. the nil of a stray `when',
               ;; would silently export as the text “nil”.
               (let ((result (let ((contents (org-parse raw-contents))) ,@body)))
                 ,(macroexpand-all `(lf-assert ',name  t
                                               result (stringp it)))
                 ;; The LaTeX packages it needs, for `org--load-latex-head'; sans any, when handed back to Org.
                 (unless (text-property-any 0 (length result) 'org-builtin-block t result)
                   (pcase backend
                     ('latex (dolist (package (get ',defun-name 'org-latex-head))
                               (cl-pushnew package org--latex-head-used :test #'equal)))
                     ('html (mapc #'org-html-head-require (get ',defun-name 'org-html-head)))))
                 (org-export result))))
         ;; The user's docstring, verbatim, for anything that wants it; e.g., custom special blocks
         ;; such as the one used to make the docs for this package.
         (put ',defun-name 'variable-documentation ,docstring)
         ;; The flat (MAIN DEFAULT KEY₁ DEFAULT₁ …) arglist, since the handler's own arglist hides it; see `org-block-signature'.
         (put ',defun-name 'org-defblock-args '(,main-arg-name ,main-arg-default-value ,@keywords))
         ;; Now that the defun is created, let's modify its docstring to make it richer.
         (let ((user-docs
                (org--replace-examples-in-string
                 (or (documentation-property ',defun-name 'variable-documentation) "")
                 (lambda (example backend)
                   (cl-flet ((indent (text) (s-join "\n" (--map (if (s-blank-p it) it (concat "    " it)) (s-lines text)))))
                     (format "For example,\n\n%s\n\n%s-exports to\n\n%s"
                             (indent example)
                             (pcase (or backend 'html) ('html "HTML") ('latex "LaTeX") (other other))
                             (indent (org-export-string example (or backend 'html))))))))
               (lisp-docs
                ,(concat
                  "\nRegarding the Lisp function:"
                  "\n+ BACKEND refers to the current export backend."
                  "\n+ RAW-CONTENTS refers to the text as the user wrote it, sans its least indentation."
                  "\n  ⇒ You may mention CONTENTS to refer to the ‘org parsed’ version of user text."
                  "\n  ⇒ CONTENTS and RAW-CONTENTS are identical whenever key CONTENTS-OCCUR-AS-LINK-DESCRIPTION is non-nil."
                  "\n\nFor example, upon LaTeX export, the Org special block"
                  "\n"
                  (format "\n     #+begin_%s" name)
                  "\n     Hello, world"
                  (format "\n     #+end_%s" name)
                  "\n"
                  "\n is rewritten to the result of the call"
                  "\n"
                  (format "\n     (org-block/%s `latex \"Hello, world\")" name))))
           (put ',defun-name 'function-documentation
                (format "%s%s%s" user-docs (if (s-ends-with? "\n" user-docs) "" "\n") lisp-docs)))
         )))))


(cl-defun org-export-string (string &optional (backend 'html))
  "Export Org STRING along BACKEND, with `org-special-block-extras' enabled."
  (with-temp-buffer
    (insert "\n") ;; Without the newline, we lose any initial string.
    (insert string)
    (let ((org-inhibit-startup t))
      (org-mode)
      (org-special-block-extras-mode)
      (org-export-as backend nil nil :body-only nil))))


(cl-defun org--header-arg-of (block-name arg-name)
  "Gets the header value for parameter ARG-NAME used with BLOCK-NAME blocks.

ARG-NAME is a keyword, whereas BLOCK-NAME is a symbol."
  (plist-get (cdr (assoc block-name org--header-args)) arg-name))


(defun org--replace-examples-in-string (string fun)
  "Replace each #+begin_example…#+end_example block in STRING
with a call to (FUN ⟨matched text⟩)."
  (let ((case-fold-search t)) ; org keywords are case-insensitive
    (replace-regexp-in-string
     "^#\\+begin_example\\([^\n]*\\)\n\\(\\(?:.\\|\n\\)*?\\)\n#\\+end_example\\b"
     (lambda (example)
       (let* ((hdr        (match-string 1 example))
              (body       (match-string 2 example))
              (backend         (save-match-data
                                 (and (string-match "\\(?:^\\|[ \t]\\):exporting-to\\s-+\\(\\S-+\\)" hdr)
                                      (intern (downcase (match-string 1 hdr)))))))
         (save-match-data (s-replace "\\" "\\\\" (funcall fun body backend)))))
     string)))

;;;;; org-undefblock

(cl-defmacro org-undefblock (name &rest _)
  "Remove `org-special-block-extras' support for NAME special blocks.

For example, if you have

    (org-defblock shout () (upcase contents))

Then, LaTeX export of

    #+begin_shout
    hello, world
    #+end_shout

results in

    HELLO, WORLD

To remove this support, simply add an “un” after the “-”
in the “org-defblock” declaration:

    (org-undefblock shout () (upcase contents))

Now, the original Org source exports to LaTeX as

    \\begin{shout}
    hello, world
    \\end{shout}

As such, “shout” now exports with the default Org behaviour
for special blocks.

Note: Link support is also removed, so that \"[[shout: hello, world]]\"
no longer LaTeX-exports to “HELLO, WORLD” but instead exports to
\"\\\\url{shout: hello, world}\".
"
  `(progn (fmakunbound ',(intern (format "org-block/%s" name)))
          (setq org--supported-blocks (cl-remove ',name org--supported-blocks))
          (setf (alist-get ,(downcase (format "%s" name)) org--block-fontifications nil 'remove #'equal) nil)
          (fmakunbound  ',(intern (format "org-link/%s" name)))
          (remove-hook 'org-export-before-parsing-functions #',(intern (format "org-deftag/%s" name)))
          (fmakunbound ',(intern (format "org-deftag/%s" name)))
          (-when-let (link-params (org-link-set-parameters ,(format "%s" name)))
            (--each '(:export :face :follow :display :keymap :help-echo)
              (setf (plist-get link-params it) nil)))
          (message ,(format "“%s” is no longer recognised by ‘org-special-block-extras’." name))))

(require 'org-deflink)

;;;;; header args support
(defvar org--header-args nil
  "Alist (name plist) where `:main-arg' is a special plist key.

  It serves a similar role to that of Org's src ‘header-args’.

  See doc of `org-set-block-header-args' for more information.")

(defmacro org-set-block-header-args (blk &rest kvs)
  "Set default values for special block arguments.

This is similar to, and inspired by, Org-src block header-args.

Example src use:
    #+PROPERTY: header-args:Language :key value

Example block use:
    (org-set-block-header-args Block :main-arg mainvalue :key value)

A full, working, example can be seen by “C-h o org-defblock”.
"
  `(add-to-list 'org--header-args (list (quote ,blk) ,@kvs)))

;; This is our 𝒳, “remark”.
;; As a link, it should be shown angry-red;
;; it takes two arguments: “color” and “signoff”
;; with default values being "red" and "".
(org-defblock rremark
              (editor "Editor Remark" color "red" signoff "" :latex-head ("xcolor")
                      :link [:face '(:foreground "red" :weight bold)])
                                        ; :please-preserve-new-lines
              "Top level (HTML & LaTeX) editorial remarks; in Emacs they're angry red."
              (format (if (equal backend 'html)
                          "<strong style=\"color: %s;\">⟦%s: %s%s⟧</strong>"
                        "{\\color{%s}\\bfseries %s:  %s%s}")
                      color editor contents signoff))

;; I don't want to change the definition, but I'd like to have
;; the following as personalised defaults for the “remark” block.
;; OR, I'd like to set this for links, which do not have argument options.
(org-set-block-header-args rremark :main-arg "Jasim Jameson" :signoff "( Aim for success! )")

;;;;; blockcall

(cl-defmacro org--blockcall (blk &optional main-arg &rest keyword-args-then-contents)
  "An anaologue to `funcall` but for blocks.

Usage: (org--blockcall blk-name main-arg even-many:key-values raw-contents)

One should rarely use this directly; instead use
`org-thread-blockcall'.
"
  `(concat "#+end_export\n" (,(intern (format "org-block/%s" blk))
                             backend ;; defblock internal
                             ,@(last keyword-args-then-contents) ;; contents
                             ,main-arg
                             ,@(-drop-last 1 keyword-args-then-contents)) "\n#+begin_export"))

;;;;; fontification

(defun org--display-as (from to string)
  "Display the text from FROM to TO as STRING; marked as ours, so `org--undisplay' clears it as Org refontifies."
  (when (< from to)
    (put-text-property from to 'display (propertize string 'org-special-block-extras-display t))))

(defun org--undisplay (beg end)
  "Remove, between BEG and END, the display strings of `org--display-as'; Org's unfontifying leaves foreign ones be."
  (let ((pos beg))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'display nil end)))
        (when (-some--> (get-text-property pos 'display) (and (stringp it) (get-text-property 0 'org-special-block-extras-display it)))
          (put-text-property pos next 'display nil))
        (setq pos next)))))

(defun org--fontify-blocks (limit)
  "Font-lock matcher: up to LIMIT, show each block in `org--block-fontifications' as its PLIST says; then nil.
  :begin STRING  ↦  “#+begin_NAME” shows as STRING; the header's arguments stay as they are
  :end STRING    ↦  “#+end_NAME” shows as STRING
  :face FACE     ↦  the contents show in FACE too, atop their Org fontification
  :keywords ((REGEXP GROUP FACE) …)  ↦  each REGEXP match in the contents shows its GROUP in FACE, atop all else;
                                       a function FACE is called, with the match data at hand, for the face"
  (when org--block-fontifications
    (let ((case-fold-search t)
          (delimiter (format "^[ \t]*\\(#\\+\\(begin\\|end\\)_\\(%s\\)\\)\\(?:[ \t]\\|$\\)"
                             (regexp-opt (mapcar #'car org--block-fontifications)))))
      (while (re-search-forward delimiter limit t)
        (let* ((name (downcase (match-string 3)))
               (spec (cdr (assoc name org--block-fontifications)))
               (begin? (equal (downcase (match-string 2)) "begin")))
          (-when-let (shown (plist-get spec (if begin? :begin :end)))
            (org--display-as (match-beginning 1) (match-end 1) shown))
          (when (and begin? (or (plist-get spec :face) (plist-get spec :keywords)))
            (let ((contents-start (min (1+ (line-end-position)) (point-max))))
              (save-excursion
                (when (re-search-forward (format "^[ \t]*#\\+end_%s[ \t]*$" (regexp-quote name)) nil t)
                  (let ((contents-end (line-beginning-position)))
                    (-when-let (face (plist-get spec :face))
                      (font-lock-append-text-property contents-start contents-end 'face face))
                    (pcase-dolist (`(,regexp ,group ,face) (plist-get spec :keywords))
                      (goto-char contents-start)
                      (while (re-search-forward regexp contents-end t)
                        (font-lock-prepend-text-property (match-beginning group) (match-end group) 'face
                                                         (if (functionp face) (funcall face) face)))))))))))))
  nil)

(defun osbe--block-fontifications ()
  "The block names whose contents are fontified as Org: tiny, center, quote, and every supported block."
  (append '("tiny" "center" "quote") (mapcar #'symbol-name org--supported-blocks)))

(defun osbe--fontify-blocks-as-org (fontify &rest args)
  "Call FONTIFY on ARGS, reading the language of each block in `osbe--block-fontifications' as \"org\".
`org-fontify-meta-lines-and-blocks' reads a block's language as group 7, and its name as group 4."
  (let ((match-string (symbol-function 'match-string))
        (org-blocks (osbe--block-fontifications)))
    (cl-letf (((symbol-function 'match-string)
               (lambda (n &optional string)
                 (or (and (eq n 7)
                          (member (-some->> (funcall match-string 4 string) (string-remove-prefix "_")) org-blocks)
                          "org")
                     (funcall match-string n string)))))
      (apply fontify args))))


(require 'org-ctrl-c-ctrl-c-for-special-block)

(provide 'org-defblock)
;;; org-defblock.el ends here
