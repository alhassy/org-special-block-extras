;;; org-special-block-extras.el --- 46 new custom blocks & 56 link types for Org-mode   -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.18
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8") (lf "26.10.1") (seq "2.0"))
;; Keywords: org, blocks, colors, convenience
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

;; This library provides common desirable features using the Org interface for
;; blocks and links:
;;
;; 0. A unified interface, the `org-defblock' macro, for making new block and link types.
;;
;; 1. Colours: Regions of text and inline text can be coloured using 19 colours;
;;  easily extendable; below is an example.
;;
;;             #+begin_red org
;;             /This/
;;                   *text*
;;                          _is_
;;                               red!
;;             #+end_red
;;
;; 2. Multiple columns: Regions of text are exported into multiple side-by-side
;; columns
;;
;; 3. Remarks: First-class visible editor comments
;;
;; 4. Details: Regions of text can be folded away in HTML
;;
;; 5. Badges: SVG badges have the pleasant syntax
;; badge:key|value|colour|url|logo; only the first two are necessary.
;;
;; 6. Tooltips: Full access to Lisp documentation as tooltips, or any other
;; documentation-backend, including user-defined entries; e.g., doc:thread-first
;; retrives the documentation for thread-first and attachs it as a tooltip to
;; the text in the HTML export and as a glossary entry in the LaTeX export
;;
;; 7. Various other blocks: Solution, org-demo, spoiler (“fill in the blanks”).
;;
;; The .el files are the source of truth; org-special-block-extras.org is the
;; manual, with further examples demonstrating these blocks.
;;
;; Full documentation can be found at
;; https://alhassy.com/org-special-block-extras

;;; Design: Implementation strategy, trade-offs, and constraints

;; The purpose of this section is to add rigour to what are otherwise vague
;; intuitions regarding the overall structure of this system. (Which I will
;; forget in a month's time.)
;;
;;
;; 1. [Motivating Observation] People export Org to HTML and LaTeX complex
;; environments, such as folding text away in <details> blocks or enclosing
;; prose in \begin{theorem} environments.
;;
;; 2. [Observation of Burden] This requires users to learn about HTML/LaTeX, but
;; often there are common “blocks” that users would like to have, for which
;; there is no `cl-defun' like mechanism to define them.
;;
;; 3. [Insight] We propose to provide what we think are common blocks ---such as
;; details, box, parallel, kbd, tooltips, Lisp docs--- and to provide a
;; `cl-defun' like macro named `org-defblock' to allow users to provide support
;; for how new special blocks should be implemented, with respect to any
;; backend. Moreover, we want this to be “extensible” by end-users and so borrow
;; the existing “#+begin_src ⟨language⟩ :arg value” syntax to also work with our
;; special blocks. That is, each special block may now have an optional “main
;; argument” (such as ⟨language⟩) and may have optional additional keyword-value
;; pair arguments.
;;
;; 4. [Additional Problems] However, sometimes it is awkward to use a verbose
;; special block when you only want to invoke it on a single word. As such, we
;; propose that `org-defblock' also incorporates the support of Org links; so
;; that users may write “doc:thread-last” instead of “#+begin_doc \n thread-last
;; \n #+end_doc”. That is, we will provide a `org-deflink' and
;; `org-defblock-only' methods and, for the convenience of users, also provide
;; their combination in the method `org-defblock'.
;;
;; 5. [Improvement] Converse to ♯4, users may have swashes of text and it can be
;; awkward to phrase them in a large special block. As such, `org-deftag' lets
;; users write “* my section :my_block: \n contents” instead of
;; “#+begin_my_block \n contents \n #+end_block”; `org-defblock' declares such a
;; tag for each block, unless given “:tag nil”.  For instance, tagging a section
;; “:details:” exports it to HTML folded away in a <details> element.
;;
;; 6. [Implementation Overview] Special blocks “#+begin_foo ⟨args⟩ \n ⟨contents⟩
;; \n #+end_foo” are captured by the structure `org-special-block', which is
;; parsed by `org-special-block-after-point'.  Then methods that rewrite a
;; special block by its implementation (for a backend) are called to rewrite
;; such blocks as a pre-processing step before export actually occurs. This is
;; handled by `org--rewrite-special-blocks-by-handlers'.  To figure out what
;; should be rewritten and what should not be re-written, the global variable
;; `org--supported-blocks' maintains all names introduced by `org-defblock'.
;;
;; 7. [Code Organisation] `org-defblock', `org-deflink', and `org-deftag' each live in a file of their name, as do
;; `org-thread-blockcall' and, in org-block-header-args.el, the blocks' header-args.  The blocks and links built atop
;; them live in org-links.el, org-color-links.el, org-details-block.el, org-box-block.el, org-callout-blocks.el,
;; org-parallel-block.el, org-proof-block.el, org-align-block.el, org-margin-blocks.el, org-quote-block.el,
;; org-solution-block.el, org-demo-block.el, org-spoiler-block.el, org-remark-block.el, org-style-block.el,
;; org-substitution-blocks.el, and org-math-block.el ---one file per family of blocks; this file
;; requires them all and defines the mode.  The graph block, org-graph-block.el,
;; needs Graphviz and so is opt-in: (require 'org-graph-block); see graphs-README.org.
;;
;;
;; 🤔 Did you think “That was all straightforward, why even bother writing it down?”
;; If so, then I've succeeded in communicating my thoughts to you and my future-self.
;; If not, please reach out with feedback.

;;; TODO: Declare `indent' for org-defblock, org-defblock-only, and org-deflink
;; 
;; (declare (indent indent-spec))
;; 
;; Indent calls to this function or macro according to indent-spec. This is typically used for macros, though it works for functions too. See Indenting Macros.
;; 
;;; TODO: Declare which arg of org-defblock, org-defblock-only, and org-deflink are intended as docstrings
;;   (declare (doc-string 4) (indent defun))
;; This is used when defining a function or macro which itself will be used to define entities like functions, macros, or variables. It indicates that the nth argument, if any, should be considered as a documentation string.
;;
;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Declare-Form.html

;; MA: Perhaps this changes indentation? Or gives linting support?
;; TODO: Write a poor docstring for a `cl-defun' so that I see flycheck errors (eg extra dots, spaces, etc);
;; then, change it to be a `org-defblock-only' and see if I still get the errors.
;; THEN, register the declaration (doc-string …) and check if I get an errors now.

;;; Declareing type info: (ftype type)
;;
;; Declare “type” to be the type of this function. This type is used by
;; describe-function for documentation, and by the native compiler (see
;; Compilation of Lisp to Native Code) for optimizing code generation and
;; inferring types. Incorrect type declarations may cause crashes in natively
;; compiled code (see below). Functions with type declarations are shown by C-h
;; C-f as having a “declared type”.
;;
;; (defun my/positive-p (x)
;;   (declare (ftype (function (number) boolean)))
;;   (when (> x 0)
;;     t))
;;
;; HAS DOCS:
;;
;;     (my/positive-p X)
;;     
;;     Declared type: (function (number) boolean)
;;     
;;     Not documented.
;;
;;
;; “type” is a type specifier (see Type Specifiers) of the form (function
;; (arg-1-type … arg-n-type) RETURN-TYPE). Argument types can be interleaved
;; with &optional and &rest to reflect the function’s calling convention (see
;; Features of Argument Lists).
;;
;; For more details, see https://www.gnu.org/software/emacs/manual/html_node/elisp/Declare-Form.html.
;; Note: Function type /specifications/ can be sophisticated.
;;; TODO: Declare defuns as pure + side-effect-free, when approriate
;;
;; Evaluating a form may also make changes that persist; these changes are
;; called “side effects”. An example of a form that produces a side effect is
;; (setq foo 1). The computation can also have side effects, such as lasting
;; changes in the values of variables or the contents of data structures.
;;
;; A “pure function” is a function which, in addition to having no side effects,
;; always returns the same value for the same combination of arguments,
;; regardless of external factors such as machine type or system state.
;; 
;;;; (pure val)
;; If the value is non-nil, the named function is considered to be pure (see
;; What Is a Function?). Calls with constant arguments can be evaluated at
;; compile time.
;;
;;;; (side-effect-free val)
;; If val is non-nil, this function is free of side effects, so the byte compiler can ignore calls whose value is ignored.
;;
;; A non-nil value indicates that the named function is free of side effects
;; (see What Is a Function?), so the byte compiler may ignore a call whose value
;; is unused. If the property’s value is error-free, the byte compiler may even
;; delete such unused calls.
;;;; (important-return-value val)
;; If val is non-nil, the byte compiler will warn about calls to this function that do not use the returned value. 
;; 
;; A non-nil value makes the byte compiler warn about code that calls the named
;; function without using its returned value. This is useful for functions where
;; doing so is likely to be a mistake.
;;;; > When it comes to marking, what would be the practical benefit?
;;
;; You will know that this function is 'pure' & 'side-effect-free', it's easier
;; to comprehend such a code. If such a 'marker' would exist it would also
;; encourage others to make such a functions.
;;
;; Also, users (i.e., future-me) would get helpful warnings:
;;;;;;; Warn about more ignored function return values.
;;;;;;; The compiler now warns when the return value from certain functions is
;;;;;;; implicitly ignored.  Example:
;;;;;;; 
;;;;;;; (progn (nreverse my-list) my-list)
;;;;;;; 
;;;;;;; will elicit a warning because it is usually pointless to call
;;;;;;; 'nreverse' on a list without using the returned value.
;;;;;;; 
;;;;;;; To silence the warning, make use of the value in some way, such as
;;;;;;; assigning it to a variable.  You can also wrap the function call in
;;;;;;; '(ignore ...)', or use 'with-suppressed-warnings' with the warning
;;;;;;; name 'ignored-return-value'.
;;;;;;; 
;;;;;;; The warning will only be issued for calls to functions declared
;;;;;;; 'important-return-value' or 'side-effect-free' (but not 'error-free').
;;;;;;;
;;;;;;; See also: https://emacs.stackexchange.com/a/28341 
;;
;; If a pure, side-effect-free function is given constant arguments, the result
;; of the computation can be determined at compile time, as it's guaranteed not
;; to change. e.g. https://www.reddit.com/r/emacs/comments/134j38g/comment/jigs0ba/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
;;
;;

;;; Code:

(require 'org-defblock)
(require 'org-thread-blockcall)
(require 'org-links)
(require 'org-color-links)
(require 'org-details-block)
(require 'org-box-block)
(require 'org-callout-blocks)
(require 'org-parallel-block)
(require 'org-margin-blocks)
(require 'org-math-block)
(require 'org-quote-block)
(require 'org-solution-block)
(require 'org-demo-block)
(require 'org-substitution-blocks)
(require 'org-spoiler-block)
(require 'org-remark-block)
(require 'org-style-block)
(require 'org-align-block)
(require 'org-proof-block)
(require 'org-deftag)

;;;;; Version

(defconst org-special-block-extras-version (package-get-version))
(defun org-special-block-extras-version ()
  "Print the current version of the package in the minibuffer."
  (interactive)
  (message org-special-block-extras-version))

(defcustom org-special-block-add-html-extra t
  "Whether an HTML page using <kbd> or tooltips gets our CSS and JS for them in its head; see `org--load-html-head'.
They are inlined ---see tooltips.css and tooltips.js--- so an exported page loads nothing from elsewhere;
set this to nil for pages sans them, whose tooltips then never open."
  :type 'boolean
  :group 'org-special-block-extras)

;;;;; HTML head extras

(defconst org--kbd-html-head
  "
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;

            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>"
  "Styling for <kbd> elements, added to a page that uses <kbd>, by `org--load-html-head'.")

(defconst org--tooltip-html-head
  (let ((here (file-name-directory (or load-file-name buffer-file-name))))
    ;; A package archive must ship these files too; e.g., MELPA's recipe needs :files (:defaults "tooltips.*").
    (cl-flet ((slurp (file)
                (if (file-readable-p (expand-file-name file here))
                    (with-temp-buffer (insert-file-contents (expand-file-name file here)) (buffer-string))
                  (display-warning 'org-special-block-extras (format "%s is missing, so tooltips will not open" file))
                  "")))
      (format "\n<style>\n%s</style>\n<script>\n%s</script>\n" (slurp "tooltips.css") (slurp "tooltips.js"))))
  "tooltips.css and tooltips.js, inlined, for “.tooltip” elements; added to a page that has tooltips, by `org--load-html-head'.
Inlined, so an exported page makes no request for them; see https://github.com/alhassy/org-special-block-extras/issues/53.")

;;;;; Glossary libraries

(defvar org--docs-libraries-loaded nil
  "Whether `org-special-block-extras-mode' has loaded `org-docs-libraries' this session.")

;;;;; Eldoc: hover documentation for blocks and links

(defun org-block-signature (name)
  "NAME ↦ how its block is called, with defaults; e.g., #+begin_details TITLE=\"Details\" :title-color \"green\"; else nil."
  (-when-let (args (get (intern-soft (format "org-block/%s" name)) 'org-defblock-args))
    (-let [(main default . keys) args]
      (concat (format "#+begin_%s" name)
              ;; The anonymous main argument is omitted, unless it has a default to show.
              (if (and (eq main 'main-arg) (null default)) "" (format " %s" (upcase (symbol-name main))))
              (if default (format "=%S" default) "")
              (mapconcat (-lambda ((key default)) (format " :%s%s" key (if default (format " %S" default) "")))
                         (-partition 2 keys) "")))))

(defun org--block-keys (name)
  "NAME ↦ the keys that act on a NAME block, each beside the gist of what it does, as `org-def-action' and
`org-defblock''s :edit-indirect-mode declare; e.g., for math,
  (\"C-c C-c  Show the maths as an image atop the block; again, to remove it.  See …\"
   \"C-c '    Edit the block in `latex-mode'; C-c ' again writes it back.\")"
  ;; Raw, lest C-c ' read as C-c ’.
  (let ((gist (lambda (f) (car (split-string (or (documentation f t) "") "\n")))))
    (-non-nil
     (list (-some--> (intern-soft (format "org-action/%s" name))
             (and (fboundp it) (concat "C-c C-c  " (funcall gist it))))
           (concat "C-c '    " (-if-let (edit (alist-get name org--block-edit-special-handlers nil nil #'equal))
                                   (funcall gist edit)
                                 ;; Every special block is so edited, by `org-special-block-edit'.
                                 "Edit the block in `org-mode'; C-c ' again writes it back."))))))

(defcustom org-special-block-extras-manual "https://alhassy.com/org-special-block-extras-blocks-and-links"
  "The manual of our blocks and links, whose sections a block's, or link's, `:manual' of #ANCHOR names; see
`org--hover-footer'."
  :type 'string)

(defun org--hover-footer (symbol)
  "SYMBOL ↦ the buttons that close eldoc's word on it: “Docs”, describing SYMBOL; and, given its `:manual', “See the
manual”, browsing there.  E.g., for `org-block/math', “Docs · See the manual”, the latter leading to
https://alhassy.com/org-special-block-extras-blocks-and-links#math."
  (let ((manual (-some--> (get symbol 'org-manual)
                  (if (string-prefix-p "#" it) (concat org-special-block-extras-manual it) it)))
        (button (lambda (label action help) (make-text-button (copy-sequence label) nil
                                                               'action action 'follow-link t 'help-echo help))))
    (concat (funcall button "Docs" (lambda (_) (describe-function symbol)) (format "Describe `%s'" symbol))
            (when manual (concat " · " (funcall button "See the manual" (lambda (_) (browse-url manual)) manual))))))

(defun org--special-block-eldoc (callback &rest _)
  "Hand eldoc's CALLBACK the signature and gist of the block, or link, at point, and more; else nil.
On a #+begin_NAME or #+end_NAME line, that is the NAME block; on a NAME: link, the NAME block, else its `org-deflink'.
The gist is the first line, which the echo area shows; for a multi-line display, such as eldoc-box, there follow its
`:hover' prose, the keys that act on it, and its “Docs · See the manual” buttons; see `org--hover-footer'."
  (when (derived-mode-p 'org-mode)
    (let* ((delimiter? (save-excursion (beginning-of-line) (looking-at-p "[ \t]*#\\+\\(begin\\|end\\)_")))
           (name  (if delimiter? (org-special-block-name-at-point)
                    (-some->> (org--link-at-point) (org-element-property :type))))
           (block (and name (intern-soft (format "org-block/%s" name))))
           (link  (and name (not delimiter?) (intern-soft (format "org-link/%s" name))))
           (gist  (lambda (docstring) (car (split-string (or docstring "") "\n"))))
           (hover (lambda (symbol gist keys)
                    (string-join (-non-nil (append (list gist (get symbol 'org-hover)) keys
                                                   (list (org--hover-footer symbol))))
                                 "\n"))))
      (cond ((get block 'org-defblock-args)
             (funcall callback (funcall hover block (funcall gist (get block 'variable-documentation))
                                        (and delimiter? (org--block-keys name)))
                      :thing (org-block-signature name) :face 'font-lock-function-name-face))
            ((fboundp link)
             (funcall callback (funcall hover link (funcall gist (documentation link t))
                                        (and (get link 'org-deflink-follows) '("C-c C-o  Follow it.")))
                      :thing (format "%s:" name) :face 'font-lock-function-name-face))))))

;;;;; Completing #+begin_ lines

(defconst org--org-block-names '("src" "example" "export" "quote" "verse" "center" "comment")
  "Org's own block names, offered beside ours so completing #+begin_s still reaches src.")

(defun org--block-gist (name)
  "NAME ↦ the first line of the docstring of our block NAME; e.g., \"tip\" ↦ \"Helpful advice for doing things better…\"."
  (-some--> (intern-soft (format "org-block/%s" name)) (get it 'variable-documentation) (car (split-string it "\n"))))

(defun org--special-block-capf ()
  "On a #+begin_ line, complete the block's name, each of ours annotated by its gist; e.g., #+begin_ti ↦ tip, tooltip.
A doc buffer, for a completion popup's documentation pane, holds the block's signature and whole docstring."
  (let ((case-fold-search t))
    (when (looking-back "^[ \t]*#\\+begin_\\([^ \t\n]*\\)" (line-beginning-position))
      (list (match-beginning 1) (match-end 1)
            ;; Once each: quote is both Org's and, given a citation, ours.
            (delete-dups (append (mapcar (lambda (b) (format "%s" b)) org--supported-blocks) org--org-block-names))
            :exclusive 'no
            :annotation-function (lambda (name) (concat "  " (or (org--block-gist name) "Org's own")))
            :company-kind (lambda (_) 'keyword)
            :company-doc-buffer
            (lambda (name)
              (-when-let (doc (-some--> (intern-soft (format "org-block/%s" name)) (get it 'variable-documentation)))
                (with-current-buffer (get-buffer-create " *org-block-doc*")
                  (erase-buffer)
                  (insert (org-block-signature name) "\n\n" doc)
                  (current-buffer))))))))

(defun org--special-block-capf-setup ()
  "In this Org buffer, complete #+begin_ lines before Org's own pcomplete does."
  (add-hook 'completion-at-point-functions #'org--special-block-capf -50 t))

;;;;; The mode

;;;###autoload
(define-minor-mode org-special-block-extras-mode
  "Provide 46 new custom blocks & 56 link types for Org-mode.

All relevant Lisp functions are prefixed ‘org-’; e.g., `org-docs-insert'.

On a #+begin_NAME line, or a NAME: link, eldoc shows the block's arguments and gist;
C-c ' edits any special block's contents in an Org buffer, or one of its `:edit-indirect-mode';
C-c C-v C-d splits any special block in two at point;
and a heading tagged :red:, or :details_Hint:, exports as that block around its text.

HTML exports carry our tooltips' CSS and JS, inlined; see `org-special-block-add-html-extra'.
"
  :global t
  :group 'org-special-block-extras
  :lighter " OSBE"
  (if org-special-block-extras-mode
      (progn
        ;; https://orgmode.org/manual/Advanced-Export-Configuration.html
        (add-hook 'org-export-before-parsing-functions #'org--rewrite-special-blocks-by-handlers)
        ;; Switches on any block not of ours ---src, quote, any #+begin_𝒳---, before our own blocks are rewritten;
        ;; and before Babel too, so a src block's :let reaches the code it runs, and its :noexport stops it running.
        ;; The second pass serves the blocks of #+INCLUDE: files, which expand between the two.
        (add-hook 'org-export-before-processing-functions #'org--rewrite-any-block-switches)
        (add-hook 'org-export-before-parsing-functions #'org--rewrite-any-block-switches -10)
        (add-hook 'org-export-before-processing-functions #'org--html-export-style-setup)
        (advice-add 'org-html-footnote-reference :around #'org--footnote-as-tooltip)
        (add-hook 'org-export-filter-options-functions #'org--footnotes-in-margin)
        (add-hook 'org-export-filter-final-output-functions #'org--load-latex-head)
        (add-hook 'org-export-filter-final-output-functions #'org--load-html-head)
        (advice-add 'org-fontify-meta-lines-and-blocks :around #'osbe--fontify-blocks-as-org)
        (advice-add 'org-remove-font-lock-display-properties :after #'org--undisplay)
        ;; Appended, so a block's :fontify face lands atop Org's own fontification of its contents.
        (font-lock-add-keywords 'org-mode '((org--fontify-blocks)) 'append)
        ;; Prepended, so Org's own fontification lands atop a block's :language.
        (font-lock-add-keywords 'org-mode '((org--fontify-block-languages)))
        ;; Before the rewrite, which consumes the documentation blocks this reads.
        (add-hook 'org-export-before-parsing-functions #'org--docs-prepare -60)
        (add-hook 'eldoc-documentation-functions #'org--special-block-eldoc)
        (add-hook 'org-mode-hook #'org--special-block-capf-setup)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer (when (derived-mode-p 'org-mode) (org--special-block-capf-setup))))
        ;; Ensure user's documentation libraries have loaded
        (unless org--docs-libraries-loaded
          (setq org--docs-libraries-loaded t)
          (org-docs-load-libraries)))
    (remove-hook 'org-export-before-parsing-functions #'org--rewrite-special-blocks-by-handlers)
    (remove-hook 'org-export-before-processing-functions #'org--rewrite-any-block-switches)
    (remove-hook 'org-export-before-parsing-functions #'org--rewrite-any-block-switches)
    (advice-remove 'org-html-footnote-reference #'org--footnote-as-tooltip)
    (remove-hook 'org-export-filter-options-functions #'org--footnotes-in-margin)
    (remove-hook 'org-export-filter-final-output-functions #'org--load-latex-head)
    (remove-hook 'org-export-filter-final-output-functions #'org--load-html-head)
    (remove-hook 'org-export-before-processing-functions #'org--html-export-style-setup)
    (advice-remove 'org-fontify-meta-lines-and-blocks #'osbe--fontify-blocks-as-org)
    (advice-remove 'org-remove-font-lock-display-properties #'org--undisplay)
    (font-lock-remove-keywords 'org-mode '((org--fontify-blocks)))
    (font-lock-remove-keywords 'org-mode '((org--fontify-block-languages)))
    (remove-hook 'org-export-before-parsing-functions #'org--docs-prepare)
    (remove-hook 'eldoc-documentation-functions #'org--special-block-eldoc)
    (remove-hook 'org-mode-hook #'org--special-block-capf-setup)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer (remove-hook 'completion-at-point-functions #'org--special-block-capf t)))))

;;;;; Exporting sans the mode

(defun org--warn-when-mode-off (&rest _)
  "Before an export with `org-special-block-extras-mode' off, warn of the first of our blocks in the buffer, if any.
Our links are live once the package is loaded, yet our blocks only while the mode is on; so a #+begin_rremark would
otherwise export, silently, as Org's own \\begin{rremark}, while rremark:foo is red.  See issue #59."
  (unless org-special-block-extras-mode
    (let ((case-fold-search t))
      (save-excursion
        (goto-char (point-min))
        ;; A plain #+begin_quote is Org's own, as only a quote with arguments is ours; see `org-block/quote'.
        (when (re-search-forward (format "^[ \t]*#\\+begin_\\(%s\\_>\\|quote[ \t]+[^ \t\n]\\)"
                                         (regexp-opt (--remove (equal it "quote") (--map (format "%s" it) org--supported-blocks))))
                                 nil t)
          (display-warning 'org-special-block-extras
                           (format "#+begin_%s exports as Org's own special block, since `org-special-block-extras-mode' \
is off; turn it on, say by (add-hook 'org-mode-hook #'org-special-block-extras-mode)"
                                   (car (split-string (match-string 1))))))))))

;; Hooked once the package is loaded, as our links are, rather than by the mode it warns about.
(add-hook 'org-export-before-parsing-functions #'org--warn-when-mode-off)

(provide 'org-special-block-extras)

;;; org-special-block-extras.el ends here
