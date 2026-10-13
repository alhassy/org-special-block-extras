;;; org-special-block-extras.el --- 30 new custom blocks & 34 link types for Org-mode   -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.3
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "27.1") (org "9.1") (lf "1.0") (dad-joke "1.4") (seq "2.0") (lolcat "0"))
;; Keywords: org, blocks, colors, convenience
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

;; This library provides common desirable features using the Org interface for
;; blocks and links:
;;
;; 0. A unified interface, the ‘defblock’ macro, for making new block and link types.
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
;; This file has been tangled from a literate, org-mode, file; and so contains
;; further examples demonstrating the special blocks it introduces.
;;
;; Full documentation can be found at
;; https://alhassy.github.io/org-special-block-extras

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
;; awkward to phrase them in a large special block. As such, a future
;; improvement (already initiated) is to introduce `org-deftag' so that users
;; can write “* my section :my_block: \n contents” instead of “#+begin_my_block
;; \n contents \n #+end_block”. Then, for convenience, we can incorporate this
;; into `org-defblock'. Then, for instance, users may simplify tag a section
;; “:details:” to have it export to HTML folded away in a <details> element.
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
;; 7. [Code Organisation] The code is organised into sections using
;; `outshine-mode'.  Section “Core” contains `org-defblock' and its
;; associates. Section “Derived” contains the definitions of what we think are
;; common desirable special blocks, such as kbd as a nice way to render
;; keystrokes or doc as a way to attach tooltip/footnote documentation to
;; phrases.
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

;;; Commentary:

;; This library provides common desirable features using the Org interface for
;; blocks and links:
;;
;; 0. A unified interface, the ‘defblock’ macro, for making new block and link types.
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
;; This file has been tangled from a literate, org-mode, file; and so contains
;; further examples demonstrating the special blocks it introduces.
;;
;; Full documentation can be found at
;; https://alhassy.github.io/org-special-block-extras

;;; Code:

(require 'org-defblock)

;;;;; Version

(defconst org-special-block-extras-version (package-get-version))
(defun org-special-block-extras-version ()
  "Print the current version of the package in the minibuffer."
  (interactive)
  (message org-special-block-extras-version))

(defcustom org-special-block-add-html-extra t
  "Whether to let `org-special-block-extras' to add content to the `ox-html' head tag.

The `org-special-block-extras' mode adds a lot of extra HTML/JS code that
1. [Bloat] may not be needed by everyone using this package,
2. [Security Threat] loads stuff from foreign websites.

Since the extra stuff is for beautiful tooltips or styles,
for ease of use, the default behaviour is to use such
“untrusted data from untrusted websites”.

To avoid such behaviour, set this variable to `nil'.")

;;;###autoload
(define-minor-mode org-special-block-extras-mode
  "Provide 30 new custom blocks & 34 link types for Org-mode.

All relevant Lisp functions are prefixed ‘org-’; e.g., `org-docs-insert'.

This minor mode uses “untrusted data from untrusted websites” when exporting
to HTML, this is done for beautiful tooltips or styles.
Disable this behaviour by setting `org-special-block-add-html-extra' to `nil'.
"
  :lighter " OSPE"
  (if org-special-block-extras-mode
      (progn
 ;; https://orgmode.org/manual/Advanced-Export-Configuration.html
        (add-hook 'org-export-before-parsing-hook 'org--rewrite-special-blocks-by-handlers)
        (setq org-export-allow-bind-keywords t)
        (defvar org--ospe-kbd-html-setup nil
          "Has the necessary keyboard styling HTML beeen added?")

        ;; Append kbd styling to `org-html-head-extra' exactly once per
        ;; Emacs session.  The setup-guard and the injection must be
        ;; nested — previously they were siblings, so the injection
        ;; fired on every mode activation and batch exports accumulated
        ;; N copies of the <style> block in every page's <head>.
        (unless org--ospe-kbd-html-setup
          (setq org--ospe-kbd-html-setup t)
          (when org-special-block-add-html-extra
            (setq org-html-head-extra
                  (concat org-html-head-extra
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
          </style>"))))
        ;; Ensure user's documentation libraries have loaded
        (unless org--docs-from-libraries
          (org-docs-load-libraries))
        (defvar org--tooltip-html-setup nil
          "Has the necessary HTML beeen added?")

        ;; Append tooltipster CSS/JS to `org-html-head-extra' exactly
        ;; once per Emacs session.  Nest the injection inside the
        ;; setup-guard — previously siblings, so every activation
        ;; stacked a duplicate copy in every subsequent page's <head>
        ;; during batch exports.
        (unless org--tooltip-html-setup
          (setq org--tooltip-html-setup t)
          (when org-special-block-add-html-extra
            (setq org-html-head-extra
                  (concat org-html-head-extra
                          "
          <link rel=\"stylesheet\" type=\"text/css\" href=\"https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css\"/>

          <link rel=\"stylesheet\" type=\"text/css\" href=\"https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css\" />

          <script type=\"text/javascript\">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src=\"https://code.jquery.com/jquery-1.10.0.min.js\"%3E%3C/script%3E'));
              }
          </script>

           <script type=\"text/javascript\"            src=\"https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js\"></script>

            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>

          <style>
             abbr {color: red;}

             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          "))))
        (defvar org--docs-empty! (list nil t)
          "An indicator of when glossary entries should be erased.

        We erase the glossary not on the first export, but on the second export.
        The first export collects all citations, which are used in the second export.")
        (setcdr (last org--docs-empty!) org--docs-empty!) ;; It's an infinite cyclic list.

        ;; Actual used glossary entries depends on the buffer; so clean up after each export
        (advice-add #'org-export-dispatch
                    :after (lambda (&rest _)
                             (when (pop org--docs-empty!)
                               (setq org--docs-actually-used nil ;; The 𝒳 of each “doc:𝒳” that appears in the current buffer.
                                     org--docs nil))))           ;; The “#+begin_documentation ⋯ :label 𝒳” of the current buffer.
        ) ;; Must be on a new line; I'm using noweb-refs
    (remove-hook 'org-export-before-parsing-hook 'org--rewrite-special-blocks-by-handlers)
    )) ;; Must be on a new line; I'm using noweb-refs

(require 'org-links)
(require 'org-color-links)
(require 'org-details-block)
(require 'org-box-block)
(require 'org-parallel-block)
(require 'org-defblock-misc)
(require 'org-proof-block)



(provide 'org-special-block-extras)

;;; org-special-block-extras.el ends here
