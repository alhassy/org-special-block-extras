;;; org-defblock.el --- Declare special blocks, links, and tags in the style of defun  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.18
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8") (lf "26.10.1") (seq "2.0"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

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

;; The `org-defblock' macro, and its kin `org-defblock-only' and `org-undefblock'.  The machinery
;; they stand on lives in modules of its own:
;;
;;   `org-special-block-parse'  the `org-special-block' struct, read from the buffer
;;   `org-block-switches'       :folded, :when, :let, …, which every block understands
;;   `org-block-export'         each block replaced by its handler's result; LaTeX & HTML heads
;;   `org-defblock-tag'         a section tagged :NAME: exports as a NAME block
;;   `org-block-fontify'        how blocks look in Org buffers
;;   `org-deflink', `org-deftag' the link and tag each block also makes
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

;;;; Imports

(require 's)
(require 'dash)
(require 'subr-x)
(require 'cl-lib)
(require 'cus-edit)
(require 'org)
(require 'seq)
(require 'lf)
(require 'org-special-block-indirect-edit)
(require 'org-special-block-split)
(require 'org-deftag)
(require 'org-deflink)
(require 'org-block-header-args)
(require 'org-special-block-parse)
(require 'org-block-switches)
(require 'org-block-export)
(require 'org-defblock-tag)
(require 'org-block-fontify)
(require 'org-ctrl-c-ctrl-c-for-special-block)

;;;; org-defblock-links-and-tags

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

(eval-and-compile ;; `org-defblock' calls these as it expands.
(defun org--defblock-other-keys-p (args)
  "The desugared ARGS ↦ whether a header line may carry keywords they do not declare.
  (title nil :allow-other-keys nil)  ↦  nil      (title nil color nil)  ↦  t
That is, unless `cl-defun'-style ARGS omit `&allow-other-keys'; see `org--defblock-desugar-args'."
  (or (not (plist-member args :allow-other-keys)) (plist-get args :allow-other-keys)))

(defun org--defblock-option (kwds option)
  "(KWDS, OPTION) ↦ the desugared KWDS' value for OPTION: as given; else t for :block, else `org-defblock-links-and-tags'.
  (title nil :tag nil), :tag  ↦  nil      (title nil), :block  ↦  t"
  (cond ((plist-member kwds option) (plist-get kwds option))
        ((eq option :block) t)
        (t org-defblock-links-and-tags)))

(defun org--defblock-link-form (name docstring link)
  "(NAME, DOCSTRING, LINK option) ↦ the `org-deflink' form whose NAME: links render as NAME blocks.
  [[NAME:LABEL][DESCRIPTION]]  ≈  #+begin_NAME LABEL ⋯ DESCRIPTION ⋯ #+end_NAME
LINK is t, or a display vector such as [:face …]; see `org-defblock'."
  `(org-deflink ,name
                ,(format "%s\n\nAs a link, [[%s:LABEL][DESCRIPTION]] ≈ #+begin_%s LABEL ⋯ DESCRIPTION ⋯ #+end_%s."
                         (or docstring "") name name name)
                ,(vconcat `[:help-echo (format "%s:%s\n\n%s" ',name o-label ,(or docstring ""))]
                          (and (vectorp link) link))
                (s-replace-regexp "@@" ""
                                  (,(intern (format "org-block/%s" name)) o-backend (or o-description o-label) o-label
                                   :contents-occur-as-link-description t))))

(defun org--defblock-tag-form (name args subsections)
  "(NAME, ARGS, SUBSECTIONS) ↦ the `org-deftag' form whose sections tagged :NAME: export as NAME blocks around their text.
ARGS is the desugared arglist; see `org--defblock-section-to-block' for SUBSECTIONS."
  `(org-deftag ,name ()
               ,(format "Export each section tagged :%s: as a %s block around its text; see `org-defblock'." name name)
               (org--defblock-section-to-block ',name o-heading o-drawer o-tag-args o-properties
                                               ',(or (car args) 'main-arg)
                                               ',(org--block-declared-keywords (org-plist-delete args :allow-other-keys))
                                               o-backend ',subsections ,(org--defblock-other-keys-p args)))))

;;;; What a handler's body calls: org-export, org-parse

(defun org--export-wrap (backend x)
  "(BACKEND, X) ↦ X within an export block for BACKEND; a result opening in Org opens sans an empty export block.
So, say, a named block's #+name: and #+caption: label what it begins with."
  ;; A result that opens in Org, as by `org-parse', does so sans an empty export block, which would be a stray
  ;; blank line; likewise one closing in Org with a block's #+end_ line; e.g., Org's own quote, handed back by
  ;; `org-builtin-block', is just as Org would have it.
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
    (if (eq backend 'latex) (org--latex-hug-braces wrapped) wrapped)))

(defun org--latex-hug-braces (wrapped)
  "WRAPPED LaTeX ↦ itself, its Org text within braces hugging them, as LaTeX reads each newline as a space.
  \\blockcquote{key}{%⏎ This…⏎\\ifhmode\\unskip\\fi} is \\blockcquote{key}{This…}; issue #27."
  (--> wrapped
       ;; We add % after { to suppress the newline that opens the contents …
       (replace-regexp-in-string "\\(^\\|[^\\]\\){\n#\\+end_export\n"
                                 "\\1{%\n#+end_export\n" it t)
       ;; … and \unskip before } to drop the space of the newline that closes them;
       ;; only amid text, as in vertical mode \unskip would eat the space after a list.
       (replace-regexp-in-string "\n#\\+begin_export latex\n}"
                                 "\n#+begin_export latex\n\\ifhmode\\unskip\\fi}"
                                 it t t)))

(defun org--export-escape (backend x)
  "(BACKEND, X) ↦ Org text X, escaping the export block for BACKEND that `org--export-wrap' puts it in."
  (format "\n#+end_export\n%s\n#+begin_export %s\n" x backend))

(defun org--block-refuse-unknown-keys (name handler-args known)
  "(NAME, HANDLER-ARGS, KNOWN) ↦ nil when each key of the plist HANDLER-ARGS is KNOWN; else an error naming the rest."
  (-when-let (unknown (cl-loop for key in handler-args by #'cddr
                               unless (memq key (cons :contents-occur-as-link-description known))
                               collect key))
    (error "‘%s’ block: unknown header argument%s %s; it takes %s" name
           (if (cdr unknown) "s" "") (mapconcat #'symbol-name unknown " ")
           (if known (mapconcat #'symbol-name known " ") "no header arguments"))))

;;;; The docstring C-h f shows

(defun org--docstring-example (example backend)
  "(EXAMPLE, BACKEND) ↦ the Org text EXAMPLE, indented, beside what it exports to along BACKEND, else HTML."
  (cl-flet ((indent (text) (s-join "\n" (--map (if (s-blank-p it) it (concat "    " it)) (s-lines text)))))
    (format "For example,\n\n%s\n\n%s-exports to\n\n%s"
            (indent example)
            (pcase (or backend 'html) ('html "HTML") ('latex "LaTeX") (other other))
            (indent (org-export-string example (or backend 'html))))))

(defun org--defblock-lisp-docs (name)
  "NAME ↦ how the Lisp function `org-block/NAME' reads its arguments, for its docstring."
  (concat
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
   (format "\n     (org-block/%s `latex \"Hello, world\")" name)))

(defun org--defblock-documentation (name)
  "NAME ↦ the docstring C-h f shows for `org-block/NAME': the user's, its examples exported, then `org--defblock-lisp-docs'.
A closing “(fn ARGS)” line is the signature C-h f shows, but only while it closes the docstring; so it stays last."
  (let ((user-docs (org--replace-examples-in-string
                    (or (documentation-property (intern (format "org-block/%s" name)) 'variable-documentation) "")
                    #'org--docstring-example))
        usage)
    (when (string-match "\n*\\((fn .*)\\)[ \t\n]*\\'" user-docs)
      (setq usage (match-string 1 user-docs)
            user-docs (substring user-docs 0 (match-beginning 0))))
    (format "%s%s%s%s" user-docs (if (s-ends-with? "\n" user-docs) "" "\n") (org--defblock-lisp-docs name)
            (if usage (concat "\n\n" usage) ""))))

;;;; org-defblock

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
  each REGEXP match in the contents in FACE; e.g., the `math' block's “:Fold:blue:” shows as a tag, in blue.
  And :language \"latex\" shows the contents as a #+begin_src latex block's.  Each part is optional; see
  `org--fontify-blocks'.
+ `:hover STRING' is prose that eldoc shows, on hovering the block's #+begin_ line, beneath its docstring's
  first line; e.g., a tip too long for that line.  See `org--special-block-eldoc'.
+ `:manual URL' is where the block is documented, which eldoc offers as “See the manual”; a URL of #ANCHOR is
  that section of `org-special-block-extras-manual'; e.g., :manual \"#math\".

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

   ;; Personalised defaults, without changing the definition, as Babel's for a src block of language remark;
   ;; or, per section, :header-args:remark: :editor \"Jasim Jameson\" in a heading's drawer.
   (setq org-babel-default-header-args:remark \='((:editor . \"Jasim Jameson\") (:signoff . \"( Aim for success! )\")))

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
  (let* ((kwds    (org--defblock-desugar-args kwds))
         (handler (intern (format "org-block/%s" name)))
         ;; The block is on unless declined via `:block nil'; the link and tag follow
         ;; `org-defblock-links-and-tags' unless the block says otherwise; e.g., `:tag nil'.
         (block?  (org--defblock-option kwds :block))
         (link    (org--defblock-option kwds :link)) ;; nil, t, or a display vector
         (tag?    (org--defblock-option kwds :tag))  ;; nil, t, or [:subsections MODE]
         (fontify (plist-get kwds :fontify))
         ;; The options configure the macro; they are not block arguments.
         (args    (org-plist-delete-all kwds '(:block :link :tag :edit-indirect-mode :fontify :latex-head :html-head
                                                   :hover :manual))))
    `(progn
       ;; NOTE: We register the support at run-time and not earlier at macro-expansion time,
       ;; so that the global variable `org--supported-blocks' can be captured by any `let' clauses
       ;; that are around this `org-defblock' call.
       ,@(when block? `((add-to-list 'org--supported-blocks ',name)))
       ;; Overriding one of Org's own blocks is fair game, yet worth a word.
       ,@(when (and block? (memq (intern (format "%s-block" name)) org--builtin-block-types))
           `((message "org-defblock: %s overrides Org's own #+begin_%s; a handler may hand some back by `org-builtin-block'"
                      ',name ',name)))
       (org-defblock-only ,name ,args ,docstring ,@body)
       ;; ⇨ Its definition, whereby `org-defblock-alias' makes another name do as this one does.
       (put ',handler 'org-defblock-definition '(,kwds ,docstring ,@body))
       ;; ⇨ The LaTeX packages it needs, loaded for it when unloaded; a redefinition sans :latex-head needs none.
       (put ',handler 'org-latex-head ',(plist-get kwds :latex-head))
       ;; ⇨ Likewise, the HTML its pages need in their <head>.
       (put ',handler 'org-html-head ',(plist-get kwds :html-head))
       ;; ⇨ What eldoc says of it beyond its docstring: more prose, and where its manual is.
       (put ',handler 'org-hover ,(plist-get kwds :hover))
       (put ',handler 'org-manual ,(plist-get kwds :manual))
       ;; ⇨ The link type support, unless declined via `:link nil'
       ,@(when link (list (org--defblock-link-form name docstring link)))
       ;; ⇨ Auto-generate C-c ' handler when :edit-indirect-mode is specified.
       (org-defblock-setup-indirect-edit ,name ,(plist-get kwds :edit-indirect-mode))
       ;; ⇨ How the block looks in Org buffers; a redefinition sans :fontify forgets any earlier look.
       (setf (alist-get ,(downcase (format "%s" name)) org--block-fontifications nil 'remove #'equal)
             ,(when fontify `(list ,@(append fontify nil))))
       ;; ⇨ The tag, unless declined via `:tag nil': a section tagged :NAME: exports as a NAME block around its text.
       ,@(when tag? (list (org--defblock-tag-form name args (org--defblock-tag-subsections name tag?)))))))

(defmacro org-defblock-alias (alias name)
  "Make ALIAS a block, link, and tag that do just as those of NAME, an `org-defblock', do; e.g., as LaTeX and HTML
have their own words for one thing:
  (org-defblock-alias tooltip margin)  ⇒  #+begin_tooltip ≡ #+begin_margin, and [[tooltip:x][…]] ≡ [[margin:x][…]]"
  (declare (indent defun))
  `(pcase-let ((`(,kwds ,docstring . ,body)
                (or (get ',(intern (format "org-block/%s" name)) 'org-defblock-definition)
                    (error "‘org-defblock-alias %s’: %s is no org-defblock" ',alias ',name))))
     (eval `(org-defblock ,',alias ,kwds ,(format "%s\n\nAn alias of `org-block/%s'." (or docstring "") ',name) ,@body) t)))

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
+ Defaults for main and keyword args, as a src block's header-args; see `org--block-header-args'.
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
  (let* ((allow-other-keys? (org--defblock-other-keys-p args))
         (args (org-plist-delete args :allow-other-keys))
         (defun-name (intern (format "org-block/%s" name)))
         (main-arg-name (or (cl-first args) 'main-arg))
         (main-arg-default-value (cl-second args))
         (keywords (cddr args))
         (keyword-names (org--block-declared-keywords args)))
    `(prog1
         (cl-defun ,defun-name
             (backend      ;; Symbol
              raw-contents ;; String
              &optional ,main-arg-name
              &rest  org--handler-args
              ;; No defaults here: an omitted keyword must arrive as nil, so the body below can
              ;; prefer any `org-babel-default-header-args:NAME' value over the declared default.
              &key ,@keyword-names (contents-occur-as-link-description nil)
              ;; Always allowed here: we refuse unknown keywords ourselves, below, naming only the user's own.
              &allow-other-keys)
           ;; Assertions are expanded here, once: the handler runs interpreted, which would re-expand them on every call.
           ,(macroexpand-all `(lf-assert ',name        t
                                         backend      (and it (symbolp it))
                                         raw-contents (stringp it)))

           ,@(unless allow-other-keys?
               `((org--block-refuse-unknown-keys ',name org--handler-args
                                                 ',(--map (intern (format ":%s" it)) (cons main-arg-name keyword-names)))))

           ;; A blank main argument may be given by its own name, as #+begin_satz :name N is #+begin_satz N
           ;; (issue #47); else it is the block's header-args default, else its declared default.
           (when (or (null ,main-arg-name) (equal ,main-arg-name ""))
             (setq ,main-arg-name
                   (or
                    (plist-get org--handler-args ,(intern (format ":%s" main-arg-name)))
                    (org--header-arg-of ',name ,(intern (format ":%s" main-arg-name)))
                    ,main-arg-default-value)))

           ;; Use any headers for this block type, if no local value is passed
           ,@(cl-loop for (key default-value) in (-partition 2 keywords)
                      collect `(when (or (null ,key) (equal ,key ""))
                                 (setq ,key (or (org--header-arg-of ',name ,(intern (format ":%s" key)))
                                                ,default-value))))

           ;; Lexical, since BODY is spliced right here; `cl-letf' would clobber any global `org-export'.
           (cl-flet ((org-export (x) "Wrap X in an export block for the current backend; see `org--export-wrap'."
                       (if contents-occur-as-link-description x (org--export-wrap backend x)))
                     (org-parse (x) "This should ONLY be called within an ORG-EXPORT call."
                       (if contents-occur-as-link-description x (org--export-escape backend x))))
             ;; The body must yield a string: otherwise, e.g. the nil of a stray `when',
             ;; would silently export as the text “nil”.
             (let ((result (let ((contents (org-parse raw-contents))) ,@body)))
               ,(macroexpand-all `(lf-assert ',name  t
                                             result (stringp it)))
               (org--block-note-heads ',defun-name backend result)
               (org-export result))))
       ;; The user's docstring, verbatim, for anything that wants it; e.g., custom special blocks
       ;; such as the one used to make the docs for this package.
       (put ',defun-name 'variable-documentation ,docstring)
       ;; The flat (MAIN DEFAULT KEY₁ DEFAULT₁ …) arglist, since the handler's own arglist hides it; see `org-block-signature'.
       (put ',defun-name 'org-defblock-args '(,main-arg-name ,main-arg-default-value ,@keywords))
       ;; Now that the defun is created, its docstring is made richer.
       (put ',defun-name 'function-documentation (org--defblock-documentation ',name)))))

(cl-defun org-export-string (string &optional (backend 'html))
  "Export Org STRING along BACKEND, with `org-special-block-extras' enabled."
  (with-temp-buffer
    (insert "\n") ;; Without the newline, we lose any initial string.
    (insert string)
    (let ((org-inhibit-startup t))
      (org-mode)
      (org-special-block-extras-mode)
      (org-export-as backend nil nil :body-only nil))))


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

;;;; org-undefblock

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
  (declare (indent defun))
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

;; This is our 𝒳, “remark”.
;; As a link, it should be shown angry-red;
;; it takes two arguments: “color” and “signoff”
;; with default values being "red" and "".
(org-defblock rremark
              (editor "Editor Remark" color "red" signoff "" :latex-head ("xcolor") :manual "#tooltip"
                      :link [:face '(:foreground "red" :weight bold)])
                                        ; :please-preserve-new-lines
              "Top level (HTML & LaTeX) editorial remarks; in Emacs they're angry red."
              (format (if (equal backend 'html)
                          "<strong style=\"color: %s;\">⟦%s: %s%s⟧</strong>"
                        "{\\color{%s}\\bfseries %s:  %s%s}")
                      color editor contents signoff))

;; I don't want to change the definition, but I'd like to have the following as personalised defaults for the
;; “rremark” block, as Babel's are for a src block; links, which have no header line, take them too.
(defvar org-babel-default-header-args:rremark '((:editor . "Jasim Jameson") (:signoff . "( Aim for success! )"))
  "The rremark block's defaults, as `org-babel-default-header-args:LANG' are a src block's.")

;;;; blockcall

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

(provide 'org-defblock)
;;; org-defblock.el ends here
