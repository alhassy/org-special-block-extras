;;; org-defblock.el --- Core org-defblock / org-deflink infrastructure  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 4.1.1
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "27.1") (org "9.6") (lf "26.10.1") (seq "2.0"))
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
  (contents         nil :type string  :documentation "Body text contents"))


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


(defun org--special-block-element-at-line (&optional name)
  "The `org-element' special block whose #+begin_ line point is on, named NAME if given; else nil.
Hence nil for a “#+begin_NAME” that Org reads as mere text; e.g., inside a src or example block."
  (let ((elem (org-element-at-point)))
    (and (eq (org-element-type elem) 'special-block)
         (= (org-element-property :post-affiliated elem) (line-beginning-position))
         (or (null name) (equal (downcase (format "%s" name)) (downcase (org-element-property :type elem))))
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
              (contents-begin (org-element-property :contents-begin elem))
              ;; The header line, after the name, as a Lisp list: positional arguments, then keyword ones.
              ((positional kwdargs) (--split-with (not (keywordp it))
                                                  (read (format "(%s)" (or (org-element-property :parameters elem) ""))))))
        (make-org-special-block
         ;; Block keywords ignore case, as Org's do; e.g., #+BEGIN_SHOUT is a shout block.
         :name (downcase (org-element-property :type elem))
         :main-arg (mapconcat (lambda (it) (format "%s" it)) positional " ")
         :kwdargs kwdargs
         :contents (if contents-begin
                       (s-trim (buffer-substring-no-properties contents-begin (org-element-property :contents-end elem)))
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
  (-let [(&org-special-block 'name 'main-arg 'kwdargs 'contents 'start-point 'end-point) block]
    (org-replace-text-while-preserving-indentation
     start-point
     end-point
     (apply (intern (format "org-block/%s" name)) backend contents main-arg kwdargs))))


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
             ;; in a src block, is stepped past; a malformed block reaches the parser, to report it.
             (if (memq (org-element-type (org-element-at-point)) '(special-block paragraph))
                 (org-eval-replace-block (org-special-block-after-point blk) backend)
               (forward-line 1)))))

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
  "(NAME, COLOUR) ↦ LaTeX defining NAME as COLOUR: via \\definecolor when COLOUR is a hex code such as #e5f5e5, else \\colorlet.
COLOUR may be a symbol, as block arguments such as “:color red!5!white” are read as Lisp."
  (setq colour (format "%s" colour))
  (if (string-prefix-p "#" colour)
      (format "\\definecolor{%s}{HTML}{%s}" name (substring colour 1))
    (format "\\colorlet{%s}{%s}" name colour)))

;;;;; org-defblock

(eval-and-compile ;; `org-defblock' calls this as it expands.
(defun org--defblock-desugar-args (args)
  "ARGS ↦ the flat (MAIN DEFAULT KEY₁ DEFAULT₁ …) arglist, when ARGS is `cl-defun'-style; otherwise ARGS.

  (&optional (title \"Details\") &key (color \"green\") style)  ↦  (title \"Details\" color \"green\" style nil)
  (&key style)                                                 ↦  (main-arg nil style nil)

A block takes at most one positional argument ---its main argument--- so `&optional' binds at most one
variable, and nothing may precede `&optional' or `&key'.  Options such as `:tag t' may appear anywhere
---argument names are never keywords--- and pass through untouched, at the end."
  (if (not (or (memq '&optional args) (memq '&key args)))
      args
    (let* ((options  (cl-loop for (option value) on args
                              when (keywordp option) append (list option value)))
           (args     (let ((rest args) kept)
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
                                            &optional subsections)
  "Rewrite the section at point ---its heading line and DRAWER erased by `org-deftag'--- as HEADING atop a NAME block.

  * Why bother? :details_Spoiler_alert:        * Why bother?
  :PROPERTIES:                                 :PROPERTIES:
  :COLOR: pink                          ↦      :COLOR: pink
  :END:                                        :END:
  Body text…                                   #+begin_details \"Spoiler alert\" :color \"pink\"
                                               Body text…
                                               #+end_details

TAG-ARGS, joined by spaces, form the main argument, else the section's MAIN property; each of the
KEYWORDS found among the section's PROPERTIES becomes a keyword argument.  The section's own text is
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

KWDS may also carry options, which are not arguments:
+ `:block nil' declines block support: #+begin_NAME is left to Org, yet the
  link and tag still work, through `org-block/NAME'.
+ `:link nil' declines the link type NAME:; `:link t' makes it; and `:link
  [:face … :display …]' makes it with that `org-deflink' display; e.g.,
  [:face \='(:foreground \"red\")] shows such links in red.
+ `:tag nil' declines the tag: otherwise a section tagged :NAME: exports as a
  NAME block around its text; the tag's underscore-separated items form the
  main argument, and the section's properties supply keyword arguments by
  name.  E.g., \"* Stuck? :details_Hint:\" folds its text away under the
  title Hint.  See `org--defblock-section-to-block'.
  A block cannot hold a heading, so `:tag [:subsections MODE]' says what becomes
  of a tagged section's subsections: `ignore' leaves them outside the block, as
  `:tag t' does; `error' refuses a section that has any; and `recurse' wraps
  each subsection's text in a block of its own.
  The link and tag default to `org-defblock-links-and-tags'; for just one of
  them, see `org-deflink' and `org-deftag'.
+ `:edit-indirect-mode MODE' makes C-c \=' edit the block in MODE.

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
  ;; The options :block, :link, :tag and :edit-indirect-mode configure the macro; they are not block arguments.
  (let* ((kwds      (org--defblock-desugar-args kwds))
         (edit-mode (plist-get kwds :edit-indirect-mode))
         ;; The block is on unless declined via `:block nil'; the link and tag follow
         ;; `org-defblock-links-and-tags' unless the block says otherwise; e.g., `:tag nil'.
         (enabled?  (lambda (option) (cond ((plist-member kwds option) (plist-get kwds option))
                                           ((eq option :block) t)
                                           (t org-defblock-links-and-tags))))
         (block?    (funcall enabled? :block))
         (link      (funcall enabled? :link)) ;; nil, t, or a display vector
         (tag?      (funcall enabled? :tag)) ;; nil, t, or [:subsections MODE]
         (subsections (when tag? (org--defblock-tag-subsections name tag?)))
         (kwds      (cl-reduce #'org-plist-delete '(:block :link :tag :edit-indirect-mode) :initial-value kwds)))
    `(progn
       ;; NOTE: We register the support at run-time and not earlier at macro-expansion time,
       ;; so that the global variable `org--supported-blocks' can be captured by any `let' clauses
       ;; that are around this `org-defblock' call.
       ,@(when block? `((add-to-list 'org--supported-blocks ',name)))
       (org-defblock-only ,name ,kwds ,docstring ,@body)
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
       ;; ⇨ The tag, unless declined via `:tag nil': a section tagged :NAME: exports as a NAME block around its text.
       ,@(when tag?
           `((org-deftag ,name ()
                         ,(format "Export each section tagged :%s: as a %s block around its text; see `org-defblock'." name name)
                         (org--defblock-section-to-block ',name o-heading o-drawer o-tag-args o-properties
                                                         ',(or (car kwds) 'main-arg)
                                                         ',(mapcar #'car (-partition 2 (cddr kwds)))
                                                         o-backend ',subsections)))))))


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
                ;; Named, not `_': `cl-defun' reads the keywords from it, so it is never unused.
                &rest  org--handler-args
                ;; No defaults here: an omitted keyword must arrive as nil, so the body below can
                ;; prefer any `org-set-block-header-args' value over the declared default.
                &key ,@(mapcar #'car (-partition 2 keywords)) (contents-occur-as-link-description nil)
                &allow-other-keys)
             ;; Assertions are expanded here, once: the handler runs interpreted, which would re-expand them on every call.
             ,(macroexpand-all `(lf-assert ',name        t
                                           backend      (and it (symbolp it))
                                           raw-contents (stringp it)))

             ;; Use default value for blank main argument
             (when (or (null ,main-arg-name) (equal ,main-arg-name ""))
               (setq ,main-arg-name
                     (or
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
             (cl-flet ((org-export (x) "Wrap the given X in an export block for the current backend."
                         (if contents-occur-as-link-description x (format "#+begin_export %s\n%s\n#+end_export" backend x)))
                       (org-parse (x) "This should ONLY be called within an ORG-EXPORT call."
                         (if contents-occur-as-link-description x (format "\n#+end_export\n%s\n#+begin_export %s\n" x backend))))
               ;; The body must yield a string: otherwise, e.g. the nil of a stray `when',
               ;; would silently export as the text “nil”.
               (let ((result (let ((contents (org-parse raw-contents))) ,@body)))
                 ,(macroexpand-all `(lf-assert ',name  t
                                               result (stringp it)))
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
                  "\n+ RAW-CONTENTS refers to the text as the user wrote it verbatim."
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
         ))))


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
              (editor "Editor Remark" color "red" signoff "" :link [:face '(:foreground "red" :weight bold)])
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

;;;;; thread-blockcall

(defmacro org-thread-blockcall (body &rest forms)
  "Thread text through a number of blocks.

BODY is likely to be ‘raw-contents’, possibly with user manipulations.

Each FORMS is of the shape '(block-name main-argument :key-value-pairs)'

(thread-blockcall x)       = x
(thread-blockcall x (f a)) = (blockcall f a x)
(thread-blockcall x f₁ f₂) ≈ (f₂ (f₁ x))

The third is a ‘≈’, and not ‘=’, because the RHS contains
‘blockcall’s as well as massages the export matter
between conseqeuctive blockcalls.

A full example:

    (org-defblock nesting (name nil)
      \"Show text in a box, within details, which contains a box.\"

      (org-thread-blockcall raw-contents
                        (box name)
                        (details (upcase name) :title-color \"green\")
                        (box (format \"⇨ %s ⇦\" name) :background-color \"blue\")
                        ))
"
  (if (not forms) body
    `(-let [result (org--blockcall ,@(car forms) ,body)]
       ,@(cl-loop for b in (cdr forms)
            collect `(setq result (org--blockcall ,@b
                                                        (concat
                                                         "#+begin_export\n"
                                                         result
                                                         "\n#+end_export"
                                                         )))) result)))

;;;;; fontification

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
