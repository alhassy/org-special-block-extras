;;; org-defblock.el --- Core org-defblock / org-deflink infrastructure  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 4.1.1
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "27.1") (org "9.1") (lf "1.0") (seq "2.0"))
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

;;;; Core
;;;;; defstruct org-special-block

(cl-defstruct org-special-block
  "A representation of an Org special block."
  ;; Specification of slots:
  ;;
  ;;    ⟨blk-start/column⟩#+begin_⟨header-start⟩blk main-arg :key₀ val ₀ … :keyₙ valₙ  ;; ⟵ ⟨kwdargs⟩
  ;;    ⟨body-start⟩ body
  ;;    #+end_blk
  (name             nil :type string  :documentation "The name of the special block.")
  (start-point      nil :type integer :documentation "Point at start of '#+begin_' line.")
  (end-point        nil :type integer :documentation "Point at end of clause '#+end_⟨name⟩'.")
  ;; TODO: When refactors settle down, axe this? " header-start  =  start-point + (length name) "
  (header-start     nil :type integer :documentation "Point after block name; i.e., point at which block header & args begin.")
  (main-arg         nil :type string  :documentation "First non-keyword argument")
  (kwdargs          nil :type plist   :documentation "The key-value arguments for the header.")
  (column           nil :type integer :documentation "Indentation of '#+begin_' line")
  (body-start-point nil :type integer :documentation "Start of block body text")
  (contents         nil :type string  :documentation "Body text contents"))


(defun dash-expand:&org-special-block (key source)
  "Destructuring which works with `org-special-block', for use with `-let'.

For example,

       (let ((blk (make-org-special-block :name \"foo\" :main-arg \"hola\")))
         (-let (((&org-special-block 'name 'main-arg) blk))
           (message \"Block %s with arg %s\" name main-arg)))

"
  `(progn (assert (org-special-block-p ,source))
          (,(intern (s-replace "'" "" (format "org-special-block-%s" key))) ,source)))


(cl-defun org-special-block-after-point (&optional name)
  "Parses the first `org-special-block' after point.

Point should be at the start of the '#+begin_' clause.

If NAME is provided, then look for that kind of block;
otherwise look for any special block.

This assumes the block is well-formed and not nested."
  (interactive)
  (save-excursion
    (let (name-rx main-arg kwdargs contents start-point start-column end-point)
      (setq name-rx (if name `(literal ,(format "%s" name)) '(in "A-Za-z0-9_-")))
      ;; Look for #+begin_⟨name⟩
      (when (or (re-search-forward
                 (rx-to-string
                  `(seq  line-start (0+ space)
                         "#+begin_" (group (+ ,name-rx))          ; name
                         (optional (+ (any " \t")) (group (* (not (any "\n"))))) ; args (same line)
                         (* (any " \t")) (or "\n" "\r\n")
                         (group (*? anything))                               ; content (lazy)
                         line-start (0+ space) "#+end_" (backref 1) (* (any " \t")) line-end))
                 nil
                 t
                 )
                (error "'org-special-block-after-point': I had trouble parsing '#+begin_%s ⟨args⟩?\n⟨content⟩?\n#+end_%s'. Are the required pieces there? 🤔" name name))
                                        ;(debug)
        (setq name (match-string 1))
        (setq contents (match-string 3))
        (setq end-point (point))
        ;; Parse the header line into (main-arg . keyword-args)
        (thread-last
          (match-string 2) ;; All args, as a string
          (format "(%s)") ;; Wrap in list to ensure `read` parses args
          read           ;; We now have an honest to goodness Lisp list
          (--split-with (not (keywordp it)))
          (setq kwdargs))
        (setq main-arg (format "%s" (or (car (car kwdargs)) ""))
              kwdargs (cadr kwdargs))
        ;; Save indentation.
        (re-search-backward (format "\\#\\+begin_%s\\b" name))
        (setq start-point (point)
              start-column (current-column))
        ;; Return structured info
        (make-org-special-block
         :name (substring-no-properties name)
         :main-arg main-arg
         :kwdargs kwdargs
         :contents (s-trim (substring-no-properties contents))
         :start-point start-point
         :end-point end-point)))))


(cl-defmethod org-eval-replace-block ((block org-special-block) backend)
  "Replace a special Org block with the result of evaluating its handler.

This function replaces the region from START-POINT to END-POINT of BLOCK
with the result of evaluating the corresponding handler function:
  `org-block/NAME'

Here, NAME is the `name' slot of the BLOCK (i.e., the name used in the
#+begin_NAME / #+end_NAME delimiters).

Each special block can include:
- A *main argument*: The first (optional) positional argument after the block name.
- *Keyword arguments*: Zero or more ":key value" pairs.
- A *body*: The content between the begin and end block markers.

The corresponding handler function must be named `org-block/NAME' with signature:

  (org-block/NAME BACKEND CONTENTS MAIN-ARG &rest KWDARGS)

Example:

  #+begin_foo mainarg :x 1 :y 2
  block content
  #+end_foo

Will be replaced with the result of evaluating:

  (org-block/foo BACKEND \"block content\" \"mainarg\" '(:x . 1) '(:y . 2))

The handler's return value is inserted in place of the original block.
Indentation is preserved via `org-replace-text-while-preserving-indentation'.

This method is part of the Org export pipeline that processes supported
blocks listed in `org--supported-blocks', typically triggered during export
pre-processing steps."
  (-let [(&org-special-block 'name 'main-arg 'kwdargs 'contents 'start-point 'end-point) block]
    (org-replace-text-while-preserving-indentation
     start-point
     end-point
     (eval `(,(intern (format "org-block/%s" name))
             (quote ,backend)
             ,contents
             ,main-arg
             ;; The --map is so that args may be passed as "this" or just 'this' (raw symbols)
             ,@(--map (list 'quote it) kwdargs))))))


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

;;;;; org--rewrite-special-blocks-by-handlers

(defvar org--supported-blocks nil
  "Which special blocks, defined with `org-defblock', are supported.

Such blocks can be parsed using `org-special-block-after-point'.

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
  (setq org--current-backend backend)
  (cl-loop for blk in org--supported-blocks
           do (goto-char (point-min))
           (while (ignore-errors (re-search-forward (format "^\\s-*\\#\\+begin_%s\\b" blk)))
             (beginning-of-line)
             (org-eval-replace-block (org-special-block-after-point blk) backend))))

;;;;; org-defblock

(defun org--plist-remove (plist key)
  "Return PLIST with KEY (and its associated value) removed."
  (cl-loop for (k v) on plist by #'cddr
           unless (eq k key)
           append (list k v)))

(cl-defmacro org-defblock
    (name kwds &optional link-display docstring &rest body)
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
and a few punctuation with the exception of comma ',' since it is
a special Lisp operator. In doubt, enclose the main arg in
quotes.

Then, just as Org-mode src blocks consume key-value pairs, our
special blocks consume a number of KWDS, which is a list of the
form (key₀ value₀ … keyₙ valueₙ).

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

⇄ We use '@@html:⋯:@@' when altering CONTENTS, but otherwise use raw HTML *around* CONTENTS.
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
     (editor \"Editor Remark\" :face angry-red) (color \"red\" signoff \"\")
     \"Top level (HTML & LaTeX) editorial remarks; in Emacs they're angry red.\"
     (format (if (equal backend 'html)
               \"<strong style=\\\"color: %s;\\\">⟦%s:  %s%s⟧</strong>\"
               \"{\\color{%s}\\bfseries %s:  %s%s}\")
             color editor contents signoff))

   ;; I don't want to change the definition, but I'd like to have
   ;; the following as personalised defaults for the \"remark\" block.
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

  ;; TODO: Relocate
  (defvar org--block--link-display nil
    "Association list of block name symbols to link display vectors.")

  (cl-destructuring-bind (link-display docstring body)
      (lf-extract-optionals-from-rest link-display #'vectorp
                                      docstring    #'stringp
                                      body)
    ;; Pull :edit-indirect-mode out of kwds — it's a macro keyword, not a block header arg.
    ;; The extraction helpers live in org-special-block-indirect-edit.el.
    (let* ((edit-mode (org--indirect-edit-extract-mode kwds))
           (kwds      (org--indirect-edit-strip-kwds kwds)))
      `(progn
         ;; NOTE: We register the support at run-time and not earlier at macro-expansion time,
         ;; so that the global variable `org--supported-blocks' can be captured by any `let' clauses
         ;; that are around this `org-defblock' call.
         (add-to-list 'org--supported-blocks ',name)
         (when ,(not (null link-display)) (push (cons (quote ,name) ,link-display) org--block--link-display))
         (prog1
             (list
              (eval (backquote (org-defblock-only ,name ,kwds ,docstring ,@body)))
              ;; ⇨ The link type support
              (eval (backquote (org-deflink ,name
                                 ,(vconcat `[:help-echo (format "%s:%s\n\n%s" (quote ,name) o-label ,docstring)] (or link-display (cdr (assoc name org--block--link-display))))
                                 ;; s-replace-all `((,(format "@@%s:" backend) . "") ("#+end_export" . "") (,(format "#+begin_export %s" backend) . ""))
                                 (s-replace-regexp "@@" ""
                                                   (,(intern (format "org-block/%s" name)) o-backend (or o-description o-label) o-label :contents-occur-as-link-description t))))))
           ;; Make org-link/𝒮 have the same docs as org-block/𝒮
           (put ',(intern (format "org-link/%s" name)) 'function-documentation
                (documentation-property ',(intern (format "org-block/%s" name)) 'function-documentation)))
         ;; ⇨ Auto-generate C-c ' handler when :edit-indirect-mode is specified.
         (org-defblock-setup-indirect-edit ,name ,edit-mode)))))


;; WHERE ...


(cl-defmacro org-defblock-only (name args &optional docstring &rest body)
  "Generate a Lisp `org-block/NAME' export function from a `org-defblock' definition.


★ Like `org-defblock' but only supports blocks, not links. ★


- NAME         [Symbol]: The name of the block type.
- DOCSTRING    [Nullable String]: Documentation of the block.
- ARGS: Property list beginning with main arg binding and default value,
        followed by \"keyword default-value\" pairs.
- BODY: Code to be executed just before export.
  ⇒ May mention symbol \"backend\" and string \"raw-contents\".

Features:
+ Auto-defaults for main and keyword args via `org--header-args'.
  Default values can be set long after the associated handler is created.
+ Optional `contents-occur-as-link-description' flag for minimal output (used in links).
+ Automatic wrapping in export blocks (`org-export', `org-parse')."
  (declare (indent defun))

  ;; Account for possibly omitted docstring
  (cond
   ;; Mimicking (cl-defun f () "hi") then (f) ⇒ "hi"
   ((null body) (setq body docstring) (setq docstring ""))
   ;; Mimicking (cl-defun f () 'x 'y) then (f) ⇒ 'y
   ((not (stringp docstring)) (setq body (cons docstring body)) (setq docstring "")))

  (let ((defun-name (intern (format "org-block/%s" name)))
        (main-arg-name (or (cl-first args) 'main-arg))
        (main-arg-default-value (cl-second args))
        (keywords (cddr args)))
    (let* ((fn-name (intern (format "org-block/%s" name)))
           (should-generate-generic
            (or (null body)
                (not (fboundp fn-name))
                (null (documentation fn-name))
                (string-match-p "^\s*\n\s*\n.fn.*$" (documentation fn-name)))))
      ;; Save docs to the symbol variable, if I need them for anything in the
      ;; future; e.g., setting docs of the function symbol or using them in
      ;; custom special blocks (such as the one used to make the docs for this
      ;; package).
      (put (intern (format "org-block/%s" name)) 'variable-documentation docstring)
      `(prog1
           (cl-defun ,defun-name
               (backend      ;; Symbol
                raw-contents ;; String
                &optional ,main-arg-name
                &rest  _
                &key ,@(-partition 2 keywords) (contents-occur-as-link-description nil)
                &allow-other-keys)
             (cl-assert (and backend (symbolp backend)) nil "Org-special-block handler '%s' expects a non-null symbol for arg1" ',name)
             (cl-assert (stringp raw-contents) nil "Org-special-block handler '%s' expects a string for arg2" ',name)

             ;; Use default value for blank main argument
             (when (or (null ,main-arg-name) (s-blank-p ,main-arg-name))
               (setq ,main-arg-name
                     (or
                      (org--header-arg-of ',name ,(intern (format ":%s" "main-arg")))
                      ,main-arg-default-value)))

             ;; Use any headers for this block type, if no local value is passed
             ,@(cl-loop for (key default-value) in (-partition 2 keywords)
                        collect `(when (or (null ,key) (s-blank-p ,key))
                                   (setq ,key (or
                                               (org--header-arg-of
                                                ',name
                                                ,(intern (format ":%s" key)))
                                               ,default-value))))

             (cl-letf (((symbol-function 'org-export)
                        (lambda (x) "Wrap the given X in an export block for the current backend."
                          (if contents-occur-as-link-description x (format "#+begin_export %s \n%s\n#+end_export" backend x))))
                       ((symbol-function 'org-parse)
                        (lambda (x) "This should ONLY be called within an ORG-EXPORT call."
                          (if contents-occur-as-link-description x (format "\n#+end_export\n%s\n#+begin_export %s\n" x backend)))))
               (org-export
                (let ((contents (org-parse raw-contents))) ,@body))))
         ;; Now that the defun is created, let's modify its docstring to make it richer.
         (let ((user-docs
                (org--replace-examples-in-string
                 (or (documentation-property ',defun-name 'variable-documentation) "")
                 (lambda (example backend) (format "For example,\n\n%s\n\n%s-exports to\n\n%s"
                                                   (thread-last example (s-split "\n") (--map (concat "\t" it)) (s-join "\n"))
                                                   (pcase (or backend 'html)
                                                     ('html "HTML")
                                                     ('latex "LaTeX")
                                                     (else else))
                                                   (thread-last (org-export-string example (or backend 'html))
                                                                (s-split "\n") (--map (concat "\t" it)) (s-join "\n"))))))
               (lisp-docs
                ,(concat
                  "\nRegarding the Lisp function:"
                  "\n+ BACKEND refers to the current export backend."
                  "\n+ RAW-CONTENTS refers to the text as the user wrote it verbatim."
                  "\n  ⇒ You may mention CONTENTS to refer to the 'org parsed' version of user text."
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

To remove this support, simply add an \"un\" after the \"-\"
in the \"org-defblock\" declaration:

    (org-undefblock shout () (upcase contents))

Now, the original Org source exports to LaTeX as

    \\begin{shout}
    hello, world
    \\end{shout}

As such, \"shout\" now exports with the default Org behaviour
for special blocks.

Note: Link support is also removed, so that \"[[shout: hello, world]]\"
no longer LaTeX-exports to \"HELLO, WORLD\" but instead exports to
\"\\\\url{shout: hello, world}\".
"
  `(progn (fmakunbound ',(intern (format "org-block/%s" name)))
          (setq org--supported-blocks (cl-remove ',name org--supported-blocks))
          (fmakunbound  ',(intern (format "org-link/%s" name)))
          (-when-let (link-params (org-link-set-parameters ,(format "%s" name)))
            (--each '(:export :face :follow :display :keymap :help-echo)
              (setf (plist-get link-params it) nil)))
          (message ,(format "\"%s\" is no longer recognised by 'org-special-block-extras'." name))))

(require 'org-deflink)

;;;;; header args support
(defvar org--header-args nil
  "Alist (name plist) where `:main-arg' is a special plist key.

  It serves a similar role to that of Org's src 'header-args'.

  See doc of SET-BLOCK-HEADER-ARGS for more information.")

(defmacro org-set-block-header-args (blk &rest kvs)
  "Set default valuts for special block arguments.

This is similar to, and inspired by, Org-src block header-args.

Example src use:
    #+PROPERTY: header-args:Language :key value

Example block use:
    (set-block-header-args Block :main-arg mainvalue :key value)

A full, working, example can be seen by \"C-h o RET defblock\".
"
  `(add-to-list 'org--header-args (list (quote ,blk) ,@kvs)))

;; This is our 𝒳, "remark".
;; As a link, it should be shown angry-red;
;; it takes two arguments: "color" and "signoff"
;; with default values being "red" and "".
(org-defblock rremark
              (editor "Editor Remark" color "red" signoff "")
              [:face '(:foreground "red" :weight bold)]
                                        ; :please-preserve-new-lines
              "Top level (HTML & LaTeX) editorial remarks; in Emacs they're angry red."
              (format (if (equal backend 'html)
                          "<strong style=\"color: %s;\">⟦%s: %s%s⟧</strong>"
                        "{\\color{%s}\\bfseries %s:  %s%s}")
                      color editor contents signoff))

;; I don't want to change the definition, but I'd like to have
;; the following as personalised defaults for the "remark" block.
;; OR, I'd like to set this for links, which do not have argument options.
(org-set-block-header-args rremark :main-arg "Jasim Jameson" :signoff "( Aim for success! )")

;;;;; blockcall

(cl-defmacro org--blockcall (blk &optional main-arg &rest keyword-args-then-contents)
  "An anaologue to `funcall` but for blocks.

Usage: (blockcall blk-name main-arg even-many:key-values raw-contents)

One should rarely use this directly; instead use
o-thread-blockcall.
"
  `(concat "#+end_export\n" (,(intern (format "org-block/%s" blk))
                             backend ;; defblock internal
                             ,@(last keyword-args-then-contents) ;; contents
                             ,main-arg
                             ,@(-drop-last 1 keyword-args-then-contents)) "\n#+begin_export"))

;;;;; thread-blockcall

(defmacro org-thread-blockcall (body &rest forms)
  "Thread text through a number of blocks.

BODY is likely to be 'raw-contents', possibly with user manipulations.

Each FORMS is of the shape '(block-name main-argument :key-value-pairs)'

(thread-blockcall x)       = x
(thread-blockcall x (f a)) = (blockcall f a x)
(thread-blockcall x f₁ f₂) ≈ (f₂ (f₁ x))

The third is a '≈', and not '=', because the RHS contains
'blockcall's as well as massages the export matter
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
  "Yields a cons list of block type and language pairs.

The intent is that the block types are fontified using the given language name."
  (--map (cons (symbol-name it) "org") (-cons* 'tiny 'center 'quote  org--supported-blocks)))

(defvar osbe--original-match-string (symbol-function 'match-string))

(cl-defun osbe--match-string (n &optional str)
 (let* ((block-type (string-remove-prefix "_" (funcall osbe--original-match-string 4 str)))
             (fontification (cdr (assoc block-type (osbe--block-fontifications)))))
        (if (and (equal n 7) fontification)
            fontification
          (funcall osbe--original-match-string n str))))

;; TODO: This should only be enabled when org-special-blocks-mode is enabled and otherwise should be removed.
(advice-add 'org-fontify-meta-lines-and-blocks
       :around (lambda (fontify &rest args)
                      (cl-letf (((symbol-function 'match-string) #'osbe--match-string))
                        (apply fontify args))))

(require 'org-ctrl-c-ctrl-c-for-special-block)

(provide 'org-defblock)
;;; org-defblock.el ends here
