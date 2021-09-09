;;; org-special-block-extras.el --- 30 new custom blocks & 34 link types for Org-mode   -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 3.0
;; Package-Requires: ((s "1.12.0") (dash "2.18.1") (emacs "26.1") (org "9.1") (lf "1.0"))
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

;;; Code:

;; String and list manipulation libraries
;; https://github.com/magnars/dash.el
;; https://github.com/magnars/s.el

(require 's)               ;; “The long lost Emacs string manipulation library”
(require 'dash)            ;; “A modern list library for Emacs”
(require 'subr-x)          ;; Extra Lisp functions; e.g., ‘when-let’.
(require 'cl-lib)          ;; New Common Lisp library; ‘cl-???’ forms.

(require 'cus-edit) ;; To get the custom-* faces

(require 'org)
(require 'ox-latex)
(require 'ox-html)

(require 'lf)

;;;###autoload
(define-minor-mode org-special-block-extras-mode
    "Provide 30 new custom blocks & 34 link types for Org-mode.

All relevant Lisp functions are prefixed ‘o-’; e.g., `o-docs-insert'."
  nil nil nil
  (if org-special-block-extras-mode
      (progn
        ;; https://orgmode.org/manual/Advanced-Export-Configuration.html
        (add-hook 'org-export-before-parsing-hook 'o--support-special-blocks-with-args)
        (setq org-export-allow-bind-keywords t)
        (defvar o--kbd-html-setup nil
          "Has the necessary keyboard styling HTML beeen added?")
        
        (unless o--kbd-html-setup
          (setq o--kbd-html-setup t)
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
        </style>")))
        ;; Ensure user's documentation libraries have loaded
        (unless o--docs-from-libraries
          (o-docs-load-libraries))
        (defvar o--tooltip-html-setup nil
          "Has the necessary HTML beeen added?")
        
        (unless o--tooltip-html-setup
          (setq o--tooltip-html-setup t)
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
        ")))
        (defvar o--docs-empty! (list nil t)
          "An indicator of when glossary entries should be erased.
        
        We erase the glossary not on the first export, but on the second export.
        The first export collects all citations, which are used in the second export.")
        (setcdr (last o--docs-empty!) o--docs-empty!) ;; It's an infinite cyclic list.
        
        ;; Actual used glossary entries depends on the buffer; so clean up after each export
        (advice-add #'org-export-dispatch
          :after (lambda (&rest _)
          (when (pop o--docs-empty!)
              (setq o--docs-actually-used nil ;; The 𝒳 of each “doc:𝒳” that appears in the current buffer.
                    o--docs nil))))           ;; The “#+begin_documentation ⋯ :label 𝒳” of the current buffer.
      ) ;; Must be on a new line; I'm using noweb-refs
    (remove-hook 'org-export-before-parsing-hook 'o--support-special-blocks-with-args)
    )) ;; Must be on a new line; I'm using noweb-refs

(cl-defmacro o-deflink
    (name &optional docstring display &rest body)
  "Make a new Org-link NAME that exports using form BODY.

Since Org links are essentially string-valued functions,
a function ‘o-link/NAME’ is created.

DOCSTRING is optional; it is visible with
   (documentation 'o-link/NAME)

BODY is a string-valued expression, that may make use of the names
o-label, o-description, o-backend. The final one refers to the export
backend, such as 'html or 'latex. The first two are obtained from uses:

   [[name:o-label][o-description]]

In particular, the use case “name:o-label” means that o-description is nil.

Example use:

   ;; In a Lisp buffer, press “C-x C-e” to load this definition
   (o-deflink shout (upcase (or o-description o-label)))

   ;; In an Org-buffer, press “C-c C-e h o” to see how this exports
   <shout: hello world!>

   ;; Or using the bracket format
   [[shout:][hello world!]]
   [[shout: hello world!]]

   ;; or using the plain format
   shout:hello_world

Here is a more complex, involved, example that makes use of
‘:let’ for local declarations. For instance, “define:hello”
renders as the word “hello” with a tooltip defining the word; the
definition is obtained from the command line tool ‘wn’.

  (o-deflink define
    \"Define the given word using WordNet, along with synonyms and coordinate terms.\"
    [:let (definition (shell-command-to-string (format \"wn %s -over -synsn -coorn\" o-label)))
     :help-echo definition]
    (--> definition
      (s-replace-regexp \"\\\\\\\"\" \"''\" it) ;; The presence of ‘\\\"’ in tooltips breaks things, so omit them.
      (s-replace-regexp \"\\n\" \"<br>\" it)
      (format \"<abbr class=\\\"tooltip\\\" title=\\\"%s\\\">%s</abbr>\" it o-label)))

For HTML tooltips, see `o-html-export-preserving-whitespace'.

More generally, org-special-block-extra's “doc” link type
supports, in order of precedence: User definitions, Emacs Lisp
documentation of functions & variables, and definitions of
English words. For example, “doc:existential_angst” for an entry
‘existential_angst’ whose associated documentation-glossary is
user-defined in a ‘#+documentation’ Org-block, or
“doc:thread-first” for the Emacs Lisp documentation of the
function `thread-first', or “doc:user-mail-address” for the Emacs
Lisp documentation of the variable `user-mail-address', or
“doc:hello” for the definition of the English word ‘hello’.

DISPLAY is a vector consisting of key-value pairs that affects how the link
is displayed in Emacs Org buffers. The keys are as follows.

+ :help-echo is a string-valued expression of the tooltip that should accompany
  the new link in Org buffers. It has access to o-format being one of ‘plain’,
  ‘angle’, ‘bracket’ which indicates the format of the link, as shown above.
  It also has access to o-label and o-description.

  By default, the tooltip is the link name followed by the documentation
  of the link, and, finally, the HTML export of the link.
  That way, upon hover, users can visually see the link contents,
  know what/how the link exports, and actually see the HTML export.

  That is to say, for the ‘shout’ example aboce, the default display is essentially:
  [:help-echo (o-link/shout o-label o-description 'html)]

  You may want to add the following to your Emacs init file:

    ;; Nearly instantaneous display of tooltips.
    (setq tooltip-delay 0)
    ;; Give user 30 seconds before tooltip automatically disappears.
    (setq tooltip-hide-delay 300)

+ :face specifies how should these links be displayed within Emacs.
   It is a list-valued expression.
   As usual, it may make use of O-LABEL (but O-DESCRIPTION has value nil).

+ [:display full] if you do not want bracket links to be
  folded away in Org buffers; i.e., “[[X][Y]]” does not render as just “Y”.

+ :follow is a form that is executed when you click on such links; e.g., to open
   another buffer, browser, or other action. It makes use of (an implicit argument) ‘o-label’.
   Be aware that ‘o-label’ is a string that may contain spaces; e.g., when the action is to open
   a URL in a browser.

   If you are in need of providing similar, related, actions on a single link
   then your :follow can condition on the current prefix argument via ‘o-prefix’
   (which is essentially `current-prefix-arg').
   For instance, a user presses “C-u RET” on your link to do one thing
   but “C-u 72 RET” to do another action.

+ :keymap is an alternating list of keys and actions to be
  performed when those keys are pressed while point is on the link.
  For example:
      [:keymap (C-h (message-box \"hola\"))]

  By default, C-n and C-p are for moving to next and previous occruances of the same link type.

+ :let is a list of alternating variable symbol name and value, which are then used to form
  a concrete `let*' clause. This is useful for introducing local variables for use in the DISPLAY
  as well as in the CONTENTS. Such local declarations may make use of O-LABEL and O-DESCRIPTION, as usual."
  (cl-destructuring-bind (docstring display body)
      (lf-extract-optionals-from-rest docstring #'stringp
                                      display   #'vectorp
                                      body)
    (setq display (seq--into-list display))
    (let ((o-link/NAME (intern (format "o-link/%s" name)))
          (navigation "Press “C-h” to see possible actions on this link type.")
          (lets (cl-loop for (variable value)
                      on (cl-getf display :let)
                      by #'cddr
                      collect (list variable value))))
      `(progn
       ;; Declare the underlying function and documentation
       (cl-defun ,o-link/NAME ;; function name
           (o-label o-description o-backend)         ;; function args
           ;; new function documentation
           ,docstring
           ;; function body
           (let* ,lets ,@body))
       ;; Construct the Org-link
       (org-link-set-parameters
        ,(format "%s" name)
        :export (quote ,o-link/NAME)
        ;; How should these links be displayed?
        ;; (We augment the namespace with the missing o-description that local variables may be using.)
        :face (lambda (o-label)  (let (o-description) (let* ,lets ,(cl-getf display :face))))
        ;; When you click on such links, what should happen?
        ;; (We augment the namespace with the missing o-description that local variables may be using.)
        :follow (lambda (o-label o-prefix) (let (o-description) (let* ,lets ,(cl-getf display :follow))))
         ;; These links should *never* be folded in descriptive display;
        ;; i.e., “[[example:lable][description]]” will always appear verbatim
        ;; and not hide the first pair […].
        :display (quote ,(cl-getf display :display)) ;; e.g.,: 'full
        ;; Any special keybindings when cursour is on this link type?
        ;; On ‘NAME:’ links, C-n/p to go to the next/previous such links.
        :keymap (let ((o-keymap (copy-keymap org-mouse-map))
                      (pattern (format "%s:" (quote ,name)))
                      (msg  (concat
                             (documentation (quote ,o-link/NAME))
                             "\nKEY BINDINGS:\n"
                             "\nUnless indicated below otherwise..."
                             "\n\tC-h: Shows this helpful message buffer"
                             "\n\tC-n/C-p on the link to jump to next/previous links of this type;"
                             "\n\tC-c C-x C-n/p for moving between arbitrary link types.\n\n"
                             (pp-to-string (quote ,(cl-getf display :keymap))))))
                  ;; Populate the keymap
                  (cl-loop for (key action)
                        on (-cons* 'C-p `(re-search-backward ,pattern nil t)
                                   'C-n `(re-search-forward ,pattern nil t)
                                   'C-h `(-let [max-mini-window-height 0] ;; i.e., insist on displaying in a dedicated buffer
                                           (display-message-or-buffer ,msg))
                                   (quote ,(cl-getf display :keymap)))
                        by #'cddr
                        do (define-key o-keymap (kbd (format "%s" key))
                             `(lambda () (interactive) ,action)))
                  ;; Return the keymap
                  o-keymap)
        ;; The tooltip alongside a link
        :help-echo (lambda (window object position)
                     (save-excursion
                       (goto-char position)
                       (-let* (((&plist :path :format :contents-begin :contents-end)
                                (cadr (org-element-context)))
                               (o-format format)
                               (o-label path)
                               (o-description
                                (when (equal format 'bracket)
                                  (copy-region-as-kill contents-begin contents-end)
                                  (substring-no-properties (car kill-ring)))))
                         (or (let* ,lets ,(cl-getf display :help-echo))
                             (format "%s:%s\n\n%s\nHTML Export:\n\n%s"
                                     (quote ,name)
                                     (or o-description o-label)
                                     ,(concat (or docstring "") "\n\n" navigation "\n")
                                     (,o-link/NAME o-label o-description 'html)))))))
        ;; Return value is the name of the underlying function.
        ;; We do this to be consistent with `defun'.
        (quote ,o-link/NAME)))))

(defvar o--supported-blocks nil
  "Which special blocks, defined with DEFBLOCK, are supported.")

(cl-defmacro o-defblock
  (name main-arg kwds &optional experimental docstring &rest body)
  "Declare a new special block, and link, in the style of DEFUN.

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

----------------------------------------------------------------------

TLDR for EXPERIMENTAL and DOCSTRING and BODY, the first two parts are
optional; they're a symbol, a string, then the main body.  The
symbol, O-RESPECT-NEWLINES?, when present enables a highly
experimental [i.e., do *not* use it!] feature: No new lines for
blocks in HTML export.  Its need rose from developing the MARGIN
block type.

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

   (defblock remark
     (editor \"Editor Remark\" :face angry-red) (color \"red\" signoff \"\")
     \"Top level (HTML & LaTeX)O-RESPECT-NEWLINES? editorial remarks; in Emacs they're angry red.\"
     (format (if (equal backend 'html)
               \"<strong style=\\\"color: %s;\\\">⟦%s:  %s%s⟧</strong>\"
               \"{\\color{%s}\\bfseries %s:  %s%s}\")
             color editor contents signoff))

   ;; I don't want to change the definition, but I'd like to have
   ;; the following as personalised defaults for the “remark” block.
   ;; OR, I'd like to set this for links, which do not have argument options.
   (defblock-header-args remark :main-arg \"Jasim Jameson\" :signoff \"( Aim for success! )\")

Three example uses:

    ;; ⟨0⟩ As a special blocks with arguments given.
    #+begin_remark Bobbert Barakallah :signoff \"Thank-you for pointing this out!\" :color green
    I was trying to explain that ${\large (n × (n + 1) \over 2}$ is always an integer.
    #+end_remark

    ;; ⟨1⟩ As a terse link, using default values for the args.
    ;;     Notice that Org-mode formatting is recoqgnised even in links.
    [ [remark:Jasim Jameson][Why are you taking about “$\mathsf{even}$” here?]]

    ;; ⟨2⟩ So terse that no editor name is provided.
    [ [remark:][Please improve your transition sentences.]]

    ;; ⟨★⟩ Unlike 0, examples 1 and 2 will have the default SIGNOFF
    ;; catenated as well as the default red color."
  ;; ⇨ The special block support
  ;;
  (add-to-list 'o--supported-blocks name) ;; global var

  ;; Identify which of the optional features is present...
  (cl-destructuring-bind (o-respect-newlines? docstring body)
      (lf-extract-optionals-from-rest experimental #'keywordp
                                      docstring    #'stringp
                                      body)
    `(progn
       ;; Produce an associated Lisp function
       ,(o-defblock---support-block-type
         name
         docstring
         (if (consp `,main-arg) (car main-arg) 'main-arg) ;; main argument's name
         (cadr main-arg) ;; main argument's value
         kwds
         body
         ;; MA: I'd like it to be always ‘true’, but it's experimental and breaks so much stuff.
         o-respect-newlines?
         )

       ;; ⇨ The link type support
       ;; The ‘main-arg’ may contain a special key ‘:link-type’ whose contents
       ;; are dumped here verbatim.
       ;; ‘(main-arg-name main-arg-val :face … :follow …)’
           (o-deflink ,name
             [:help-echo (format "%s:%s\n\n%s" (quote ,name) o-label ,docstring)
             ,@(cddr main-arg) ;; verbatim link extras
             ]
             ;; s-replace-all `(("#+end_export" . "") (,(format "#+begin_export %s" backend) . ""))
             (s-replace-all `(("@@" . "")) ;; (,(format "@@%s:" backend) . "")
                            (,(intern (format "o--%s" name))
                             o-backend (or o-description o-label) o-label :o-link? t))))))

;; WHERE ...

(cl-defmethod o-defblock---support-block-type
    (name docstring main-arg-name main-arg-value kwds body o-respect-newlines?)
  "Helper method for o-defblock.

This method creates an Org block type's associated Lisp function.

NAME, string: The name of the block type.
DOCSTRING, string: Documentation of block.
MAIN-ARG-NAME: Essentially main-arg's name
MAIN-ARG-VALUE: Essentially main-arg's value
KWDS, plist: Keyword-value pairs
BODY, list: Code to be executed"
  `(cl-defun ,(intern (format "o--%s" name))
       (backend raw-contents
                &optional ;; ,(car main-arg)
                ,main-arg-name
                &rest _
                &key (o-link? nil) ,@(-partition 2 kwds))
     ,docstring
     ;; Use default for main argument
     (when (and ',main-arg-name (s-blank-p ,main-arg-name))
       (--if-let (plist-get (cdr (assoc ',name o--header-args)) :main-arg)
           (setq ,main-arg-name it)
         (setq ,main-arg-name ,main-arg-value)))

     (cl-letf (((symbol-function 'org-export)
                 (lambda (x)
            "Wrap the given X in an export block for the current backend.

          One can think of this function as replacing the #+begin_𝒳⋯#+end_𝒳
          in-place in your Org document; but really that's done by the
          ⋯-support-blocks function.
          "
            (if o-link?
                x
            ;; o-respect-newlines? is super experimental: It's a bit ugly on the LaTeX side.
            (cond ((and ,o-respect-newlines? (member backend '(html reveal)))
                   (format "@@%s:%s@@" backend (s-replace "\n" (format "@@\n@@%s:" backend) x) backend))
                  (:else
                   (format "#+begin_export %s \n%s\n#+end_export"
                           backend x))))))

               ((symbol-function 'org-parse)
                (lambda (x)
                  "This should ONLY be called within an ORG-EXPORT call."
                  (if o-link?
                      x
                    (cond ((and ,o-respect-newlines? (member backend '(html reveal)))
                           (format "@@%s@@%s:" x backend))
                          (:else
                           (format "\n#+end_export\n%s\n#+begin_export %s\n" x
                                   backend)))))))

       ;; Use any headers for this block type, if no local value is passed
       ,@(cl-loop for k in (mapcar #'car (-partition 2 kwds))
                  collect `(--when-let (plist-get (cdr (assoc ',name o--header-args))
                                                  ,(intern (format ":%s" k)))
                             (when (s-blank-p ,k)
                               (setq ,k it))))

       (org-export
        (let ((contents (org-parse raw-contents))) ,@body)))))

(defun o--pp-list (xs)
  "Given XS as (x₁ x₂ … xₙ), yield the string “x₁ x₂ … xₙ”, no parens.
  When n = 0, yield the empty string “”."
  (s-chop-suffix ")" (s-chop-prefix "(" (format "%s" (or xs "")))))

(defvar o--current-backend nil
  "A message-passing channel updated by
o--support-special-blocks-with-args
and used by DEFBLOCK.")

(defun o--support-special-blocks-with-args (backend)
  "Remove all headlines in the current buffer.
BACKEND is the export back-end being used, as a symbol."
  (setq o--current-backend backend)
  (let (blk-start        ;; The point at which the user's block begins.
        header-start ;; The point at which the user's block header & args begin.
        kwdargs          ;; The actual key-value arguments for the header.
        main-arg         ;; The first (non-keyed) value to the block.
        blk-column       ;; The column at which the user's block begins.
        body-start       ;; The starting line of the user's block.
        blk-contents         ;; The actual body string.
        ;; ⟨blk-start/column⟩#+begin_⟨header-start⟩blk main-arg :key₀ val ₀ … :keyₙ valₙ  ;; ⟵ ⟨kwdargs⟩
        ;; ⟨body-start⟩ body
        ;; #+end_blk
        )
  (cl-loop for blk in o--supported-blocks
        do (goto-char (point-min))
        (while (ignore-errors (re-search-forward (format "^\s*\\#\\+begin_%s" blk)))
          ;; MA: HACK: Instead of a space, it should be any non-whitespace, optionally;
          ;; otherwise it may accidentlly rewrite blocks with one being a prefix of the other!
          (setq header-start (point))
          ;; Save indentation
          (re-search-backward (format "\\#\\+begin_%s" blk))
          (setq blk-start (point))
          (setq blk-column (current-column))
          ;; actually process body
          (goto-char header-start)
          (setq body-start (1+ (line-end-position)))
          (thread-last
              (buffer-substring-no-properties header-start (line-end-position))
            (format "(%s)")
            read
            (--split-with (not (keywordp it)))
            (setq kwdargs))
          (setq main-arg (o--pp-list (car kwdargs)))
          (setq kwdargs (cadr kwdargs))
          (forward-line -1)
          (re-search-forward (format "^\s*\\#\\+end_%s" blk))
          (setq blk-contents (buffer-substring-no-properties body-start (line-beginning-position)))
          (kill-region blk-start (point))
          (insert (eval `(,(intern (format "o--%s" blk))
                          (quote ,backend)
                          ,blk-contents
                          ,main-arg
                          ,@(--map (list 'quote it) kwdargs))))
          ;; See: https://github.com/alhassy/org-special-block-extras/issues/8
          ;; (indent-region blk-start (point) blk-column) ;; Actually, this may be needed...
          ;; (indent-line-to blk-column) ;; #+end...
          ;; (goto-char blk-start) (indent-line-to blk-column) ;; #+begin...
          ;; the --map is so that arguments may be passed
          ;; as "this" or just ‘this’ (raw symbols)
      ))))

(defvar o--header-args nil
  "Alist (name plist) where “:main-arg” is a special plist key.

  It serves a similar role to that of Org's src ‘header-args’.

  See doc of SET-BLOCK-HEADER-ARGS for more information.")

(defmacro o-set-block-header-args (blk &rest kvs)
  "Set default valuts for special block arguments.

This is similar to, and inspired by, Org-src block header-args.

Example src use:
    #+PROPERTY: header-args:Language :key value

Example block use:
    (set-block-header-args Block :main-arg mainvalue :key value)

A full, working, example can be seen by “C-h o RET defblock”.
"
  `(add-to-list 'o--header-args (list (quote ,blk) ,@kvs)))

;; This is our 𝒳, “remark”.
;; As a link, it should be shown angry-red;
;; it takes two arguments: “color” and “signoff”
;; with default values being "red" and "".
(o-defblock rremark
  (editor "Editor Remark" :face '(:foreground "red" :weight bold)) (color "red" signoff "")
  ; :please-preserve-new-lines
  "Top level (HTML & LaTeX) editorial remarks; in Emacs they're angry red."
  (format (if (equal backend 'html)
            "<strong style=\"color: %s;\">⟦%s: %s%s⟧</strong>"
            "{\\color{%s}\\bfseries %s:  %s%s}")
          color editor contents signoff))

;; I don't want to change the definition, but I'd like to have
;; the following as personalised defaults for the “remark” block.
;; OR, I'd like to set this for links, which do not have argument options.
(o-set-block-header-args rremark :main-arg "Jasim Jameson" :signoff "( Aim for success! )")

(cl-defmacro o--blockcall (blk &optional main-arg &rest keyword-args-then-contents)
  "An anaologue to `funcall` but for blocks.

Usage: (blockcall blk-name main-arg even-many:key-values raw-contents)

One should rarely use this directly; instead use
o-thread-blockcall.
"
  `(concat "#+end_export\n" (,(intern (format "o--%s" blk))
    backend ;; defblock internal
    ; (format "\n#+begin_export html\n\n%s\n#+end_export\n" ,(car (last keyword-args-then-contents))) ;; contents
    ,@(last keyword-args-then-contents) ;; contents
    ,main-arg
    ,@(-drop-last 1 keyword-args-then-contents)) "\n#+begin_export"))

(defmacro o-thread-blockcall (body &rest forms)
  "Thread text through a number of blocks.

BODY is likely to be ‘raw-contents’, possibly with user manipulations.

Each FORMS is of the shape “(block-name main-argument
:key-value-pairs)”

(thread-blockcall x)       = x
(thread-blockcall x (f a)) = (blockcall f a x)
(thread-blockcall x f₁ f₂) ≈ (f₂ (f₁ x))

The third is a ‘≈’, and not ‘=’, because the RHS contains
‘blockcall’s as well as massages the export matter
between conseqeuctive blockcalls.

A full example:

    (o-defblock nesting (name) nil
      \"Show text in a box, within details, which contains a box.\"

      (o-thread-blockcall raw-contents
                        (box name)
                        (details (upcase name) :title-color \"green\")
                        (box (format \"⇨ %s ⇦\" name) :background-color \"blue\")
                        ))
"
  (if (not forms) body
     `(-let [result (o--blockcall ,@(car forms) ,body)]
    ,@(cl-loop for b in (cdr forms)
          collect `(setq result (o--blockcall ,@b
                                     (concat
                                   "#+begin_export\n"
                                   result
                                   "\n#+end_export"
                                   )))) result)))

(o-defblock solution
  (title "Solution")
  (reprimand "Did you actually try? Maybe see the ‘hints’ above!"
   really "Solution, for real")
  "Show the answers to a problem, but with a reprimand in case no attempt was made."
  (o-thread-blockcall raw-contents
                    (details really :title-color "red")
                    (box reprimand :background-color "blue")
                    (details title)))

(o-defblock org-demo nil (source "Source" result "Result"
                        source-color "cyan" result-color "cyan"
                        style "parallel"
                        sep (if (equal backend 'html) "@@html:<p><br>@@" "\n\n\n\n")
                        )
  "Output the CONTENTS of the block as both parsed Org and unparsed.

Label the source text by SOURCE and the result text by RESULT

finally, the source-result fragments can be shown in a STYLE
that is either “parallel” (default) or “sequential”.

SEP is the separator; e.g., a rule ‘<hr>'.
"
  (-let [text (concat
               ;; Source
               (thread-last raw-contents
                 (format (if (equal backend 'html)
                             "<div ><pre class=\"src src-org\">%s</pre></div>"
                           "\n\\begin{verbatim}\n%s\n\\end{verbatim}"))
                 org-export
                 (o--blockcall box source :background-color source-color)
                 org-export)
               ;; Separator
               sep
               ;; Result
               (thread-last raw-contents
                 (o--blockcall box result :background-color result-color)
                 org-export))]

   (if (equal style "parallel")
       (o--blockcall parallel "2" :bar nil text)
       (concat "#+end_export\n" text "\n#+begin_export"))))

(o-defblock stutter (reps 2) nil
  "Output the CONTENTS of the block REPS many times"
  (-let [num (if (numberp reps) reps (string-to-number reps))]
    (s-repeat num contents)))

(o-defblock rename (list "") nil
  "Perform the given LIST of substitutions on the text.
The LIST is a comma separated list of ‘to’ separated symbols.
In a link, no quotes are needed."
  (s-replace-all
   (--map (cons (car it) (cadr it))
          (--map (s-split " to " (s-trim it))
                 (s-split "," list)))
   contents))

(o-defblock spoiler (color "grey") (left "((" right "))")
  "Hide text enclosed in double parens ((like this)) as if it were spoilers.
   LEFT and RIGHT may be other kinds of delimiters.
   The main argument, COLOR, indicates which color to use.

For LaTeX, this becomes “fill in the blanks”, with the answers
in the footnotes."
  (if (equal backend 'latex)
      (s-replace-regexp
       (concat (regexp-quote left) "\\(.*?\\)" (regexp-quote right))
       "@@latex:\\\\fbox{\\\\phantom{\\1}}\\\\footnote{\\1}@@"
       contents)
  (-let [id (gensym)]
    (concat
     ;; In HTML, a ‘style’ can be, technically, almost anywhere...
     (format
      "<style> #%s {color: %s; background-color:%s;}
       #%s:hover {color: black; background-color:white;} </style>
       " id color color id)
     (s-replace-regexp
      (concat (regexp-quote left) "\\(.*?\\)" (regexp-quote right))
      (format "@@html:<span id=\"%s\"> \\1 </span>@@" id)
      contents)))))

(defun o--list-to-math (lst)
  "Get a result LST from ORG-LIST-TO-LISP and render it as a proof tree."
  (cond
   ((symbolp lst) "")
   ((symbolp (car lst)) (o--list-to-math (cadr lst)))
   (t
    (-let* (((conclusion₀ children) lst)
            ((name named?) (s-split " :: " conclusion₀))
            (conclusion (or named? conclusion₀)))
      (if (not children)
          (if named? (format "\\frac{}{%s}[%s]" conclusion name) conclusion)
        (format "\\frac{\\displaystyle %s}{%s}%s"
                (s-join " \\qquad "
                        (mapcar #'o--list-to-math children))
                conclusion
                (if named? (format "[\\text{%s}]" name) "")))))))

(o-defblock tree (main-arg) nil
  "Write a proof tree using Org-lists.

To get

         premises₀  …   premisesₙ
       ────────────────────────────[ reason ]
               conclusion

You type

       #+begin_tree
       + reason :: conclusion
         - premises₀
         - premises₁
         ⋮
         - premisesₙ
       #+end_tree

Where each premisesᵢ may, recursively, also have named reasons
and (indented) child premises of its own.

If there are multiple trees, they are shown one after the other.

The text in this block should be considered LaTeX;
as such, Org markup is not recognised.

A proof tree, derivation, is then just a deeply nested
itemisation.  For instance, assuming P = Q(X), X = Y, Q(Y) = R,
the following proves P = R.

  #+begin_tree
  + Trans :: P = R
    - P = Q(X)
      + ✓
    - Trans :: Q(X) = R
      + Trans :: Q(X) = Q(Y)
        - Refl :: Q(X) = Q(X)
          + ✓
        - Leibniz :: Q(X) = Q(Y)
          + X = Y
            - ✓
      + Sym :: Q(Y) = R
        - R = Q(Y)
          - ✓
  #+end_tree"
  (s-join "" (--map (format "\\[%s\\]"
                                (o--list-to-math it))
                        (cdr (with-temp-buffer
                               (insert raw-contents)
                               (goto-char (point-min))
                               (org-list-to-lisp))))))

(defvar o-hide-editor-comments nil
  "Should editor comments be shown in the output or not.")

(o-defblock remark
      (editor "Editor Remark" :face '(:foreground "red" :weight bold)) (color "black" signoff "" strong nil)
; :inline-please__see_margin_block_for_a_similar_incantation ; ⇒ crashes!
"Format CONTENTS as an first-class editor comment according to BACKEND.

The CONTENTS string has an optional switch: If it contains a line
with having only ‘#+replacewith:’, then the text preceding this
clause should be replaced by the text after it; i.e., this is
what the EDITOR (the person editing) intends and so we fromat the
replacement instruction (to the authour) as such.

In Emacs, as links, editor remarks are shown with a bold red; but
the exported COLOR of a remark is black by default and it is not
STRONG ---i.e., bold---. There is an optional SIGNOFF message
that is appended to the remark.
"
  (-let* (;; Are we in the html backend?
          (tex? (equal backend 'latex))

          ;; fancy display style
          (boxed (lambda (x)
                   (if tex?
                       (concat "\\fbox{\\bf " x "}")
                     (concat "<span style=\"border-width:1px"
                             ";border-style:solid;padding:5px\">"
                             "<strong>" x "</strong></span>"))))

          ;; Is this a replacement clause?
          ((this that) (s-split "\\#\\+replacewith:" contents))
          (replacement-clause? that) ;; There is a ‘that’
          (replace-keyword (if tex?
                             "\\underline{Replace:}" "&nbsp;<u>Replace:</u>"))
          (with-keyword    (if tex? "\\underline{With:}" "<u>With:</u>"
                             ))
          (editor (format "[%s:%s" editor
                          (if replacement-clause?
                              replace-keyword
                            "")))
          (contents′ (if replacement-clause?
                         (format "%s %s %s" this
                                 (org-export (funcall boxed with-keyword))
                                 that)
                       contents))

          ;; “[Editor Comment:”
          (edcomm-begin (funcall boxed editor))
          ;; “]”
          (edcomm-end (funcall boxed "]")))

    (setq org-export-allow-bind-keywords t) ;; So users can use “#+bind” immediately
    (if o-hide-editor-comments
        ""
      (format (pcase backend
                ('latex (format "{\\color{%%s}%s %%s %%s %%s %%s}" (if strong "\\bfseries" "")))
                (_ (format "<%s style=\"color: %%s;\">%%s %%s %%s %%s</%s>" (if strong "strong" "p") (if strong "strong" "p"))))
              color edcomm-begin contents′ signoff edcomm-end))))

(org-link-set-parameters
 "edcomm"
  :follow (lambda (_))
  :export (lambda (label description backend)
            (o--edcomm
             backend
             (format ":ed:%s\n%s" label description)))
  :help-echo (lambda (_ __ position)
               (save-excursion
                 (goto-char position)
                 (-let [(&plist :path) (cadr (org-element-context))]
                   (format "%s made this remark" (s-upcase path)))))
  :face '(:foreground "red" :weight bold))

(o-defblock details (title "Details") (title-color "green")
  "Enclose contents in a folded up box, for HTML.

For LaTeX, this is just a boring, but centered, box.

By default, the TITLE of such blocks is “Details”
and its TITLE-COLOR is green.

In HTML, we show folded, details, regions with a nice greenish colour.

In the future ---i.e., when I have time---
it may be prudent to expose more aspects as arguments,
such as ‘background-color’.
"
  (format
   (pcase backend
     (`latex "\\begin{quote}
                  \\begin{tcolorbox}[colback=%s,title={%s},sharp corners,boxrule=0.4pt]
                    %s
                  \\end{tcolorbox}
                \\end{quote}")
     (_ "<details class=\"code-details\"
                 style =\"padding: 1em;
                          background-color: #e5f5e5;
                          /* background-color: pink; */
                          border-radius: 15px;
                          color: hsl(157 75% 20%);
                          font-size: 0.9em;
                          box-shadow: 0.05em 0.1em 5px 0.01em  #00000057;\">
                  <summary>
                    <strong>
                      <font face=\"Courier\" size=\"3\" color=\"%s\">
                         %s
                      </font>
                    </strong>
                  </summary>
                  %s
               </details>"))
   title-color title contents))

(o-defblock Details (title "Details") (title-color "green")
  "Enclose contents in a folded up box, for HTML.

For LaTeX, this is just a boring, but centered, box.

By default, the TITLE of such blocks is “Details”
and its TITLE-COLOR is green.

In HTML, we show folded, details, regions with a nice greenish colour.

In the future ---i.e., when I have time---
it may be prudent to expose more aspects as arguments,
such as ‘background-color’.
"
  (format
   (pcase backend
     (`latex "\\begin{quote}
                  \\begin{tcolorbox}[colback=%s,title={%s},sharp corners,boxrule=0.4pt]
                    %s
                  \\end{tcolorbox}
                \\end{quote}")
     (_ "<details class=\"code-details\"
                 style =\"padding: 1em;
                          background-color: #e5f5e5;
                          /* background-color: pink; */
                          border-radius: 15px;
                          color: hsl(157 75% 20%);
                          font-size: 0.9em;
                          box-shadow: 0.05em 0.1em 5px 0.01em  #00000057;\">
                  <summary>
                    <strong>
                      <font face=\"Courier\" size=\"3\" color=\"%s\">
                         %s
                      </font>
                    </strong>
                  </summary>
                  %s
               </details>"))
   title-color title contents))

(o-defblock box (title "") (background-color nil)
  "Enclose text in a box, possibly with a title.

By default, the box's COLOR is green for HTML and red for LaTeX,
and it has no TITLE.

The HTML export uses a padded div, whereas the LaTeX export
requires the tcolorbox pacakge.

In the future, I will likely expose more arguments.
"
  (apply #'concat
  (pcase backend
   (`latex `("\\begin{tcolorbox}[title={" ,title "}"
             ",colback=" ,(pp-to-string (or background-color 'red!5!white))
             ",colframe=red!75!black, colbacktitle=yellow!50!red"
             ",coltitle=red!25!black, fonttitle=\\bfseries,"
             "subtitle style={boxrule=0.4pt, colback=yellow!50!red!25!white}]"
             ,contents
             "\\end{tcolorbox}"))
    (_ `("<div style=\"padding: 1em; background-color: "
             ,(o-subtle-colors (format "%s" (or background-color "green")))
             ";border-radius: 15px; font-size: 0.9em"
             "; box-shadow: 0.05em 0.1em 5px 0.01em #00000057;\">"
             "<h3>" ,title "</h3>"
            ,contents "</div>")))))

(defun o-subtle-colors (c)
  "HTML codes for common colours.

Names are very rough approximates.

   Translations from: https://www.december.com/html/spec/softhues.html"
  (pcase c
    ("teal"    "#99FFCC") ;; close to aqua
    ("brown"   "#CCCC99") ;; close to moss
    ("gray"    "#CCCCCC")
    ("purple"  "#CCCCFF")
    ("lime"    "#CCFF99") ;; brighter than ‘green’
    ("green"   "#CCFFCC")
    ("blue"    "#CCFFFF")
    ("orange"  "#FFCC99")
    ("peach"   "#FFCCCC")
    ("pink"    "#FFCCFF")
    ("yellow"  "#FFFF99")
    ("custard" "#FFFFCC") ;; paler than ‘yellow’
    (c c)
  ))

(o-defblock parallel (cols 2) (bar nil)
  "Place ideas side-by-side, possibly with a separator.

There are COLS many columns, and they may be seperated by black
solid vertical rules if BAR is a non-nil value.

Writing “#+begin_parallel 𝓃 :bar (any text except ‘nil’)”
will produce a parallel of 𝓃 many columns, possibly
seperated by solid rules, or a “bar”.

The contents of the block may contain ‘#+columnbreak:’ to request
a columnbreak. This has no effect on HTML export since HTML
describes how text should be formatted on a browser, which can
dynamically shrink and grow and thus it makes no sense to have
hard columnbreaks. We do replace such declarations by ‘<p><br>’,
which sometimes accomplishes the desired goal.
"
  (let ((rule (pcase backend
               (`latex (if bar 2 0))
               (_  (if bar "solid" "none"))))
        (contents′  (s-replace "#+columnbreak:"
                               (if (equal 'latex backend) "\\columnbreak" "@@html:<p><br>@@")
                               contents)))
  (format (pcase backend
   (`latex "\\par \\setlength{\\columnseprule}{%s pt}
          \\begin{minipage}[t]{\\linewidth}
          \\begin{multicols}{%s}
          %s
          \\end{multicols}\\end{minipage}")
   (_ "<div style=\"column-rule-style: %s;column-count: %s;\">%s</div>"))
   rule cols contents′)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load support for 20 colour custom blocks and 20 colour link types

(defvar o--colors
  '(black blue brown cyan darkgray gray green lightgray lime
          magenta olive orange pink purple red teal violet white
          yellow)
  "Colours that should be available on all systems.")

(cl-loop for colour in o--colors
         do (eval `(o-defblock ,colour
                     (the-color "black" :face `(:foreground ,(format "%s" (quote ,colour))))
                     nil
                     ,(format "Show text in %s color." colour)
                     (let ()
                       (format (pcase backend
                                 (`latex "\\begingroup\\color{%s}%s\\endgroup\\,")
                                 (_  "<span style=\"color:%s;\">%s</span>"))
                               (quote ,colour) contents)))))

(o-defblock color
  (color black :face (lambda (colour) `(:foreground ,(format "%s" colour))))
  nil
  "Format text according to a given COLOR, which is black by default."
  (format (pcase backend
            (`latex "\\begingroup\\color{%s}%s\\endgroup\\,")
            (`html  "<span style=\"color:%s;\">%s</span>"))
          color contents))

(o-defblock latex-definitions nil nil
  "Declare but do not display the CONTENTS according to the BACKEND."
  (format (pcase backend
            ('html "<p style=\"display:none\">\\[%s\\]</p>")
            (_ "%s"))
          raw-contents))

(o-deflink kbd
  "Show keysequence O-LABEL in a nice grey button-like font, along with a tooltip of its documentation, if any.

Such links do not get folded in [[bracket]] style, and are rendered as buttons within Emacs.

Moreover, O-LABEL may use ‘_’ in-lieu of spaces or [[bracket]] link notation.

Examples:
    [[kbd:C-x C-s]]
  ≈ <kbd: C-x C-s>
  ≈ kbd:C-x_C-s"
  [:display 'full
   :let (the-label  (s-trim (s-replace "_" " " o-label))
         lisp-func  (ignore-errors (cl-second (help--analyze-key (kbd the-label) the-label)))
         tooltip    (or o-description (ignore-errors (documentation lisp-func)) "")
         tooltip?   (not (equal tooltip ""))
         style      (if tooltip? "border-color: red" "")
         keystrokes (format "<kbd style=\"%s\">%s</kbd>" style the-label))
   ;; o-description is always nil when it comes to deciding the :face.
   :face (list :inherit 'custom-button :box (if tooltip? "red" t))
   :help-echo (format "%s ∷ %s\n%s" the-label (or lisp-func "") tooltip)]
  (if (equal o-backend 'latex)
      (format "\\texttt{%s}" the-label)
    (if tooltip?
        ;; The style=⋯ is to remove the underlying caused by <abbr>.
        (format "<abbr class=\"tooltip\" style=\"border: none; text-decoration: none;\" title=\"%s ∷ %s<br>%s\">%s</abbr>"
                the-label (or lisp-func "") (o-html-export-preserving-whitespace tooltip)
                keystrokes)
      keystrokes)))

(defvar
 o--supported-octoicons
 (-partition 2
 '(
   home
   "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 16
   16\" width=\"16\" height=\"16\"><path fill-rule=\"evenodd\"
   d=\"M16 9l-3-3V2h-2v2L8 1 0 9h2l1 5c0 .55.45 1 1 1h8c.55 0
   1-.45 1-1l1-5h2zm-4 5H9v-4H7v4H4L2.81 7.69 8 2.5l5.19 5.19L12
   14z\"></path></svg>"

   link
   "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 16
   16\" width=\"16\" height=\"16\"><path fill-rule=\"evenodd\"
   d=\"M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69
   3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10
   5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0
   2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5
   0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55
   13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z\"></path></svg>"

   mail
   "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 14
   16\" width=\"14\" height=\"16\"><path fill-rule=\"evenodd\"
   d=\"M0 4v8c0 .55.45 1 1 1h12c.55 0 1-.45
   1-1V4c0-.55-.45-1-1-1H1c-.55 0-1 .45-1 1zm13 0L7 9 1 4h12zM1
   5.5l4 3-4 3v-6zM2 12l3.5-3L7 10.5 8.5 9l3.5 3H2zm11-.5l-4-3
   4-3v6z\"></path></svg>"

   report
   "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 16
   16\" width=\"16\" height=\"16\"><path fill-rule=\"evenodd\"
   d=\"M0 2a1 1 0 011-1h14a1 1 0 011 1v9a1 1 0 01-1 1H7l-4
   4v-4H1a1 1 0 01-1-1V2zm1 0h14v9H6.5L4 13.5V11H1V2zm6
   6h2v2H7V8zm0-5h2v4H7V3z\"></path></svg>"

   tag
   "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 15
   16\" width=\"15\" height=\"16\"><path fill-rule=\"evenodd\"
   d=\"M7.73 1.73C7.26 1.26 6.62 1 5.96 1H3.5C2.13 1 1 2.13 1
   3.5v2.47c0 .66.27 1.3.73 1.77l6.06 6.06c.39.39 1.02.39 1.41
   0l4.59-4.59a.996.996 0 000-1.41L7.73 1.73zM2.38
   7.09c-.31-.3-.47-.7-.47-1.13V3.5c0-.88.72-1.59
   1.59-1.59h2.47c.42 0 .83.16 1.13.47l6.14 6.13-4.73
   4.73-6.13-6.15zM3.01 3h2v2H3V3h.01z\"></path></svg>"

   clock
   "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 14
   16\" width=\"14\" height=\"16\"><path fill-rule=\"evenodd\"
   d=\"M8 8h3v2H7c-.55 0-1-.45-1-1V4h2v4zM7 2.3c3.14 0 5.7 2.56
   5.7 5.7s-2.56 5.7-5.7 5.7A5.71 5.71 0 011.3 8c0-3.14 2.56-5.7
   5.7-5.7zM7 1C3.14 1 0 4.14 0 8s3.14 7 7 7 7-3.14
   7-7-3.14-7-7-7z\"></path></svg>"))

"An association list of supported OctoIcons.

Usage: (cadr (assoc 'ICON o--supported-octoicons))")

(o-deflink octoicon
  "Show an OctoIcon: home, link, mail, report, tag, clock"
  [:help-echo "Show an OctoIcon: home, link, mail, report, tag, clock"]
  (unless (member (intern o-label) '(home link mail report tag clock))
    (error "octoicon:%s ⇒ This label is not supported!" o-label))
  (if (not (equal o-backend 'html))
      ""
    (s-collapse-whitespace
     (cadr (assoc (intern o-label)
                  o--supported-octoicons)))))

(o-deflink link-here
  "Export a link to the current location in an Org file."
  [:help-echo (format "This is a local anchor link named “%s”" path)]
  (if (not (equal o-backend 'html))
      ""
    (format (s-collapse-whitespace
     "<a class=\"anchor\" aria-hidden=\"true\" id=\"%s\"
          href=\"#%s\">%s</a>")
    o-label o-label (cadr (assoc 'link
                             o--supported-octoicons)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The badge link types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defmacro o-make-badge
  (name &optional social-shields-name social-url social-shields-url )
  "Make a link NAME whose export is presented as an SVG badge.

If the link is intend to be a social badge, then, adhering to
shield.io conventions, the appropriate SOCIAL-SHIELDS-NAME must
be given.

The SOCIAL-URL is a URL that the badge points to and it should
have a single ‘%s’ where the link label argument will go.

Some social badges have a uniform URL, but otherwise, such as
twitter, deviate and so need their own SOCIAL-SHIELDS-URL,
which has a single ‘%s’ for the link's label argument.

----------------------------------------------------------------------

E.g., to create a badge named “go”:

     (o-make-badge \"go\")

Then the following exports nicely from an Org file:

     go:key|value|blue|here|gnu-emacs"
  `(o-deflink ,(intern name)
    "Export a Shields.io badge, with general Syntax:  badge:key|value|colour|url|logo.
Precise details for each argument are shown in the Emacs tooltip for this badge."
    [:display 'full
     :follow (--> (s-split "|" path)
               (or (nth 3 it) o-label)
               (browse-url it))
    :help-echo
      (-let [ (key value color url logo)  (mapcar #'s-trim (s-split "|" o-label)) ]
        (lf-string "${o-label}
                    \nGeneral Syntax:  badge:key|value|colour|url|logo
                           Key    : ${key}
                           Value  : ${(or value \"\")}
                           Colour : ${(or color \"green\")}
                           URL    : ${(or url \"\")}
                           Logo   : ${(or logo \"\")}

                    This results in an SVG badge “[key∣value]”, where the ‘key’
                    is coloured grey and the ‘value’ is coloured ‘color’;
                    for the HTML backend, and otherwise are silently omitted.
                    Descriptions are ignored; i.e., ‘[[badge:label][description]]’
                    is the same as ‘[[badge:label]]’.

                    ► ‘key’ and ‘value’ have their underscores interpreted as spaces.
                       ⇒ ‘__’ is interpreted as an underscore;
                         Of course, you can write ‘<badge: ⋯>’, then ‘⋯’ may have multiword, spaced, content.
                       ⇒ ‘|’ is not a valid substring, but ‘-, %, ?’ are okay.

                    ► ‘|color|url|logo’ are optional;
                       ⇒ If ‘url’ is not present, the resulting badge is not a hyperlink.
                       ⇒ if ‘url’ is ‘here’ then we have a local link;
                          i.e., the resulting badge behaves like ‘link-here:key’.
                       ⇒ ‘color’ may be: ‘brightgreen’ or ‘success’,
                                         ‘red’         or ‘important’,
                                         ‘orange’      or ‘critical’,
                                         ‘lightgrey’   or ‘inactive’,
                                         ‘blue’        or ‘informational’,
                         or ‘green’, ‘yellowgreen’, ‘yellow’, ‘blueviolet’, ‘ff69b4’, etc.
                         Consult https://htmlcolorcodes.com/ to see the HEX codes of other colours.
                       ⇒ ‘logo’ examples and how they look can be found at
                          https://alhassy.github.io/org-special-block-extras/#Example-Badge-Icons

                   See also: https://alhassy.github.io/org-special-block-extras/#Common-Project-Badges"))]

    ;; :export #'o--link--badge
    (if (equal o-backend 'latex) ""
      (-let [ (key value color url logo)  (mapcar #'s-trim (s-split "|" o-label)) ]
        (format
         (pcase ,(if social-shields-name `(format ,social-url o-label) 'url)
           ("here" (format "<a id=\"%s\" href=\"#%s\">%%s</a>" (s-replace "%" "%%" key) (s-replace "%" "%%" key)))
           (""      "%s") ;; e.g., badge:key|value|color||logo
           ('nil    "%s") ;; e.g., badge:key|value|color
           (t (format "<a href=\"%s\">%%s</a>"
                      (s-replace "%" "%%"
                                 ,(if social-shields-name
                                      `(format ,social-url o-label)
                                    'url)))))
         ,(if social-shields-name
              (if social-shields-url
                  `(format ,social-shields-url o-label)
                `(format "<img src=\"https://img.shields.io/%s/%s?style=social\">"
                         ,social-shields-name o-label))
            '(format "<img src=\"https://img.shields.io/badge/%s-%s-%s?logo=%s\">"
                     (url-hexify-string (s-replace "-" "--" key))
                     (url-hexify-string (s-replace "-" "--" (or value "")))
                     color
                     logo)))))))

(o-make-badge "badge")

;; Since we're invoking a macro, the twitter-excitement is used lazily; i.e.,
;; consulted when needed instead of being evaluated once.
(defvar o-link-twitter-excitement
  "This looks super neat (•̀ᴗ•́)و:"
  "The string prefixing the URL being shared.")

(o-make-badge
 "tweet"
 "twitter/url?=url="
 (format
   "https://twitter.com/intent/tweet?text=%s:&url=%%s"
   o-link-twitter-excitement)
 "<img src=\"https://img.shields.io/twitter/url?url=%s\">"
               )

;; MA: I don't think this is ideal for long-term maintainability, see ‘:OLD’ below.
(cl-loop for (social url name)
         in '(("reddit/subreddit-subscribers" "https://www.reddit.com/r/%s" "reddit")
             ("github" "https://www.github.com/%s")
             ("github/stars" "https://www.github.com/%s/stars")
             ("github/watchers" "https://www.github.com/%s/watchers")
             ("github/followers" "https://www.github.com/%s?tab=followers")
             ("github/forks" "https://www.github.com/%s/fork")
             ("twitter/follow" "https://twitter.com/intent/follow?screen_name=%s"))
         for name′ = (or name (s-replace "/" "-" social))
         do (eval `(o-make-badge ,name′ ,social ,url)))

(defvar o--docs nil
  "An alist of (LABEL NAME DESCRIPTION) entries; our glossary.

Example setter:
0. (o-docs-set \"os\" \"Emacs\" \"A place wherein I do all of my computing.\")

Example getters:
0. (o-docs-get LABEL)
1. (-let [(name description) (cdr (assoc LABEL o--docs))] ⋯)

See also `o--docs-from-libraries' and `o-docs-load-libraries'.")

(cl-defun o-docs-set (label name description)
  "Add a new documentation-glossary entry, if it is not already present.

We associate LABEL to have title NAME and glossary value DESCRIPTION.

Example usage:
  (o-docs-set \"cat\"
              \"Category Theory\"
              \"A theory of typed  composition; e.g., typed monoids.\")"
  (add-to-list 'o--docs (list label name description)))

(cl-defun o-docs-get (label)
  "Return the name and documentation-glossary values associated with LABEL.

It returns a list of length 2.

Example uses:

  ;; Get the Lisp documentation of `thread-last'
  (o-docs-get \"thread-last\")

  ;; Get the English definition of ‘computing’
  (o-docs-get \"computing\")

We look for LABEL from within the current buffer first, using `o--docs',
and otherwise look among the loaded libraries, using `o--docs-from-libraries',
and, finally, look for the documentation entry using `o-docs-fallback'."
  (cdr (or (assoc label o--docs)
           (assoc label o--docs-from-libraries)
           (funcall o-docs-fallback label)
           (error "Error: No documentation-glossary entry for “%s”!" label))))

(cl-defun o-docs-insert ()
  "Insert a “doc:𝒳” link from user's documentation-glossary database.

It can be tricky to remember what you have, or what documentation entries mention, and so
this command gives a searchable way to insert doc links."
  (interactive)
  (thread-last
      (cl-remove-duplicates (-concat o--docs o--docs-from-libraries)
                            :test (lambda (x y) (cl-equalp (car x) (car y))))
    (--map (format "%s ∷ %s" (car it) (cl-third it)))
    (completing-read "Insert doc link ∷ ")
    (s-split "∷")
    car
    (concat "doc:")
    (insert)))

(defvar o-docs-fallback
  (lambda (label) (list
              ;; label
              label
              ;; name
              label
              ;; documentation
              (or
               (ignore-errors (documentation (intern label)))
               (ignore-errors (documentation-property
                               (intern label)
                               'variable-documentation))
               (-let [it (shell-command-to-string
                          (format "wn %s -over -synsn" label))]
                 (if (s-blank-p it)
                     (error "Error: No documentation-glossary entry for “%s”!" label)
                   it)))))

  "The fallback method to retriving documentation or glossary entries.

We try to retrive the Emacs Lisp function documentation of the
given LABEL, if possible, otherwise we try to retrive the Emacs
Lisp variable documentation, and if that fails then we look up
the word in the English dictionary.

The English definition is obtained from the command line tool ‘wn’, WordNet.")

(defvar o-docs-libraries nil
  "List of Org files that have ‘#+begin_documentation’ blocks that should be loaded
   for use with the ‘doc:𝒳’ link type.")

(cl-defun o-docs-load-libraries
    (&optional (libs o-docs-libraries))
"Load documentation-glossary libraries LIBS.

If no LIBS are provided, simply use those declared
o-docs-libraries.

See `o-docs-from-libraries'."
(interactive)
(cl-loop for lib in libs
      do (with-temp-buffer
           (insert-file-contents lib)
           ;; doc only activates after an export
           (-let [org-export-with-broken-links t] (org-html-export-as-html))
           (kill-buffer)
           (delete-window)
           (setq o--docs-from-libraries (-concat o--docs o--docs-from-libraries))
           (setq o--docs nil))))

(defvar o--docs-from-libraries nil

  "The alist of (label name description) entries loaded from the libraries.

The ‘doc:𝒳’ link will load the libraries, possibly setting this variable to ‘nil’,
then make use of this variable when looking for documentation strings.

Interactively call `o-docs-load-libraries'
to force your documentation libraries to be reloaded.

See also `o-docs-libraries'.")

(defvar o--docs-actually-used nil
  "Which words are actually cited in the current article.

We use this listing to actually print a glossary using
‘show:GLOSSARY’.")

(o-deflink doc
 "Export O-LABEL as itself, or as the provided O-DESCRIPTION,
 along with a tooltip that shows the user's
 documentation-glossary for o-LABEL and using that entry's name
 when no O-DESCRIPTION is provided."
 [:let (entry (o-docs-get o-label)
        name (cl-first entry)
        docs (cl-second entry)
        display-name (or o-description name))
  :help-echo (format "[%s] %s :: %s" o-label name docs)
  :face '(custom-button)]
   (add-to-list 'o--docs-actually-used (list o-label name docs))
   (pcase o-backend
     (`html  (format "<abbr class=\"tooltip\" title=\"%s\">%s</abbr>"
                     (o-html-export-preserving-whitespace docs)
                     display-name))
     ;; Make the current word refer to its glosary entry;
     ;; also declare the location that the glossary should refer back to.
     (`latex (format (concat "\\hyperref"
                             "[o-glossary-%s]{%s}"
                             "\\label{o-glossary"
                             "-declaration-site-%s}")
                     label display-name label))))

;; WHERE ...

(defun o-html-export-preserving-whitespace (s)
  "Make Org-markup'd string S ready for HTML presentation, preserving whitespace.

This is orthogonal to the  `org-export-string-as' command; i.e.,
(org-export-string-as s 'html :body-only-please) does something
else!  In particular, what this yields is an HTML rendition that
does not account for whitespace, such as indentation and
newlines.
"
  ;; Make it look pretty!
  (thread-last s
    (s-replace-regexp "\\#\\+begin_src [^\n]*\n" "<pre class='tooltip'>")
    (s-replace-regexp "\\( \\)*\\#\\+end_src\n" "</pre>")
    (s-replace-regexp "\\#\\+begin_export [^\n]*\n" "")
    (s-replace-regexp "\\( \\)*\\#\\+end_export" "")
    (s-replace "  " "&emsp;") ; Preserve newlines
    (s-replace "\n" "<br>")   ; Preserve whitespace
    (s-replace-regexp "\\#\\+begin_example<br>" "")
    (s-replace-regexp "\\#\\+end_example<br>" "")
    ; (s-replace-regexp "\\#\\+begin_src \\(.\\)*\\#\\+end_src)" "<pre>\1</pre>")
    ; (s-replace-regexp "\\#\\+begin_src [^<]*<br>" "<pre>")
    ; (s-replace-regexp "<br>\\( \\)*\\#\\+end_src<br>" "<br>\1</pre>")
    ;; Translate Org markup
    ;; Only replace /.*/ by <em>.*<em> when it does not have an alphanum,:,/,< before it.
    (s-replace-regexp "\\([^a-z0-9A-Z:/<]\\)/\\(.+?\\)/" "\\1<em>\\2</em>")
    (s-replace-regexp "\\*\\(.+?\\)\\*" "<strong>\\1</strong>")
    (s-replace-regexp "\\~\\([^ ].*?\\)\\~" "<code>\\1</code>")
    ;; No, sometimes we want equalities.
    ;; (s-replace-regexp "=\\([^ \n].*?\\)=" "<code>\\1</code>")
    (s-replace-regexp "\\$\\(.+?\\)\\$" "<em>\\1</em>")
    (s-replace-regexp "\\[\\[\\(.*\\)\\]\\[\\(.*\\)\\]\\]" "\\2 (\\1)")
    ;; 5+ dashes result in a horizontal line
    (s-replace-regexp "-----+" "<hr>")
    ;; Spacing in math mode
    (s-replace-regexp "\\\\quad" "&#x2000;")
    (s-replace-regexp "\\\\," "&#8194;") ;; en space
    (s-replace-regexp "\\\\;" "&#8195;") ;; em space
    ;; The presence of ‘\"’ in tooltips breaks things, so omit them.
    (s-replace-regexp "\\\"" "''")))

(o-defblock documentation
  (name (error "Documentation block: Name must be provided"))
  (label nil show nil color "green")
  "Register the dictionary entries in CONTENTS to the dictionary variable.

The dictionary variable is ‘o--docs’.

A documentation entry may have its LABEL, its primary identifier,
be:
1. Omitted
2. Given as a single symbol
3. Given as a many aliases '(lbl₀ lbl₁ … lblₙ)

The third case is for when there is no canonical label to refer to
an entry, or it is convenient to use multiple labels for the same
entry.

In all of the above cases, two additional labels are included:
The entry name with spaces replaced by underscores, and again but
all lower case.

Documentation blocks are not shown upon export;
unless SHOW is non-nil, in which case they are shown
using the ‘box’ block, with the provided COLOR passed to it.

In the futture, it may be nice to have an option to render tooltips.
That'd require the ‘doc:𝒳’ link construction be refactored via a ‘defun’."
  (unless (consp label) (setq label (list label)))
  (push (s-replace " " "_" name) label)
  (push (downcase (s-replace " " "_" name)) label)
  (cl-loop for l in label
        do  (add-to-list 'o--docs
                         (mapcar #'s-trim (list (format "%s" l) name (substring-no-properties raw-contents)))))
  ;; Should the special block show something upon export?
  (if show (o--blockcall box name :background-color color raw-contents) ""))

(o-deflink show
  "Yield the value of the expression O-LABEL, with =GLOSSARY= being a reserved name.

Example uses:

    show:user-full-name

    <show: (* 2 (+ 3 4 (- pi))) >


Note that there is `elisp' links with Emacs, out of the box.
However, they only serve to evaluate Lisp expressions; for example,
to make “link buttons” that do useful things, as follows.

   [[elisp:(find-file user-init-file)][Init]]

In particular, `elisp' links do not export the value of their expression.
That is what we accomplish with this new `show' link type."
  [:face '(:underline "green")
   :let (o-value (if (equal o-label "GLOSSARY")
                     (pp-to-string (mapcar #'cl-second o--docs-actually-used))
                   (pp-to-string (eval (car (read-from-string o-label)))))
         o-expr (if (equal o-label "GLOSSARY")
                    (concat "GLOSSARY ---i.e., o--docs-actually-used"
                            "\n\nWe erase the glossary not on the first export, but on the second export."
                            "\nThe first export collects all citations, which are used in the second export.")
                  o-label))
  :help-echo (format
              (concat "Upon export, the following will be placed literally"
                      "\n\t%s"
                      "\nWhich is the value of the expression:\n\t%s")
              o-value
              o-expr)]
  (cond ((not (equal o-label "GLOSSARY")) o-value)

       ;; Otherwise O-LABEL is glossary, which we print in HTML & LaTeX
       ((equal 'html o-backend)
          (s-join " "
                  (--map
                   (format "<abbr class=\"tooltip\" title=\"%s\">%s</abbr>"
                           (o-html-export-preserving-whitespace (cl-third it))
                           (cl-second it))
                   ;; Ignore duplicates; i.e., entries with the same name/title.
                   (cl-remove-duplicates o--docs-actually-used
                                         :test (lambda (x y) (cl-equalp (cl-second x) (cl-second y)))))))
       (t (s-join "\n\n"
                  (cl-loop for (label name doc)
                           in o--docs-actually-used
                           collect
                           (format
                           (concat "\\vspace{1em}\\phantomsection"
                                   "\\textbf{%s}\\quad"
                                   "\\label{o-glossary-%s}"
                                   "%s See page "
                                   "\\pageref{org-special-block-extras"
                                   "-glossary-declaration-site-%s}")
                           name
                           o-label
                           (when doc
                             (thread-last doc ;; preserve whitespace
                               (s-replace "&" "\\&") ;; Hack!
                               (s-replace "  " " \\quad ")
                               (s-replace "\n" " \\newline{\\color{white}.}")))
                           o-label))))))

(o-defblock margin
  (marker nil
          :display 'full
          :face '(:foreground "grey" :weight bold
          :underline "orange" :overline "orange"))
  (color "gray!80"
          counter "footnote"
          width "\\paperwidth - \\textwidth - \\oddsidemargin - 1in - 3ex")
          ;; Width: https://tex.stackexchange.com/a/101861/69371
  :please-inline__no-extra-newlines__k-thx-bye!
  "Produce an HTML tooltip or a LaTeX margin note.

The ‘margin’ block is intended for “one-off” (mostly optional) remarks.

For notes that you want to use repeatedly, in multiple articles
or in multiple locations in the same article, consider using
‘documentation’ to declare them and ‘doc’ to invoke them.

For LaTeX, place ‘#+resize:’ to have the remainder of a block be
resized, for now 1.3 the margin width ---requires \\usepackage{adjustbox}.

----------------------------------------------------------------------

WIDTH, COUNTER, and COLOR are LaTeX specfic.

When no label, marker, is used for a marginal note, we rely
on a COUNTER, such as ‘footnote’ (default) or ‘sidenote.’
Since HTML has no margin per se, we use “∘” as default marker:
Users hover over it to read the marginal note.

Marginal notes have their labels, markers, in black
and the notes themselves have COLOR being grey!80.
In Emacs, margin links appear grey with an orange tinted boarder.

Regarding LaTeX, since verbatim environments do not in general work well
as arguments to other commands, such as ‘\\marginpar’, we save the contents
of the special block in a ‘minipage’ within a LaTeX ‘box’; then we can unfold such
a box in the margin. Hence, ‘src’ blocks can appear within ‘margin’ blocks (•̀ᴗ•́)و

The WIDTH argument is the width of the margin; i.e., the width of the underlying
minipage.

One could use \\maxsizebox{.25\\textwidth}{\\textheight}{ ... }
which only resizes the content if its natural size is larger than
the given 〈width〉 or 〈height〉.  We don't use this, since
maxsizebox does not natively allow linebreaks
(e.g., one would wrap contents in a tabular environment then use
‘\\\\’, two backslashes, to request a line break; but this
crashes if one wants to also use verbatim environments.)

In LaTeX, it may be useful to invoke ‘\\dotfill’."
  (-let [stepcounter (if marker "" (format "\\stepcounter{%s}" counter))]
    (pcase backend
      (`latex
       (setq marker (or marker (format "{\\the%s}" counter))) ;; "\\circ"
       (format "\\!\\!${}^{\\textnormal{%s}}$
               \\newsavebox{\\OrgSpecialBlockExtrasMarginBox}
               \\begin{lrbox}{\\OrgSpecialBlockExtrasMarginBox}
               \\begin{minipage}{%s}
               \\raggedright \\iffalse Otherwise default alignment is fully justified. \\fi
               \\footnotesize
               \\setminted{fontsize=\\footnotesize, breaklines} \\iffalse HACK! \\fi
               \\color{%s}
               {\\color{black}${}^{\\textnormal{%s}}$}\n\\normalfont\n %s
               \\end{minipage}
               \\end{lrbox}
               \\marginpar{\\usebox{\\OrgSpecialBlockExtrasMarginBox}%s}
               \\hspace{-1.9ex}
               \\global\\let\\OrgSpecialBlockExtrasMarginBox\\relax"
               marker
               width
               color
               marker
               (if (s-contains? "#+resize:" contents)
                   (s-concat
                    (s-replace "#+resize:"
                               "#+latex: \\maxsizebox{1.3\\textwidth}{\\textheight}{\\begin{tabular}{l}\n"
                               (s-trim contents))
                    "\n\\end{tabular}}")
                 (s-trim contents))
               stepcounter))
      (_ (setq marker (or marker "°"))
         (format "<abbr class=\"tooltip\" title=\"%s\">%s</abbr>&emsp13;"
                 (o-html-export-preserving-whitespace contents)
                 ; MA: FIXME: (org-export-string-as contents 'html :body-only-please)
                 marker)))))

(defun o--list-to-calc (lst rel hint-format NL-length color)
  "Get a result from org-list-to-lisp and render it as a calculational proof.

LST is an expression, possibly with a hint and dedicated relation.
The expression may contain multiple lines, as may the hints.

REL is the default relation in the left-most column.

HINT_FORMAT is the formatting string for hints; e.g.,
\"\\color{maroon}{\\langle\\large\\substack{\\text{ %s }}⟩}\"

NL-length is how long the explicit vertical space, \\, should be.
The number refers to the vspace occupied nearly by the height of
a single normal sized letter.

COLOR is the colour of the hints."
  (cond
   ((symbolp lst) "")
   ((symbolp (car lst)) (o--list-to-calc (cadr lst)))
   (t (-let* (((conclusion₀ children) lst)
              ((expr₀ hint) (s-split "--" conclusion₀))
              ((op₀ expr₁) (cdr (s-match "^\\[\\(.*\\)\\]\\(.*\\)" expr₀)))
              (op (or op₀ rel))
              (expr (or expr₁ expr₀)))
        (if (not children)
            (if hint
                (format
                 "\n %s \\;\\; & \\qquad \\color{%s}{%s} \n \\\\ & \\begin{split}%s\\end{split}"
                 op
                 color
                 ;; the hfill is so that we do not use substack's default
                 ;; centering, but instead left-align justificatiion
                 ;; hints.
                 (format (s-replace "%s" "{\\large\\substack{\\text{ %s } \\hfill\\\\\n}}" hint-format)
                         (s-replace "\n" " } \\hfill\\\\\n\\text{ "
                                    (s-replace "\n\n" (s-repeat (* 6 NL-length) "\n $\\,$") (s-trim hint))))
                 expr)
              (format "\\begin{split}%s\\end{split} \n" expr))
       ;; MA: The following could be improved.
          (format "\n %s \\;\\; & \\qquad \\color{%s}{%s} \n \\\\ & \\begin{split}%s\\end{split}"
                  op color
                  ;; BEGIN similar as above
                  (format (s-replace
                           "%s"
                           "{\\large\\substack{\\text{ %s } \\hfill\\\\ \\begin{split} & %s \n\\end{split}\\hfill\\\\\n}}"
                           hint-format)
                          (s-replace "\n" " } \\hfill\\\\\n\\text{ "
                                     (s-replace "\n\n" (s-repeat (* 6 NL-length) "\n $\\,$") (s-trim hint)))
                          ;; END similar
                          (s-chop-prefix
                           "\\\\" (s-join
                                   "\\\\"
                                   (--map (format "%s" (o--list-to-calc it rel hint-format NL-length color))
                                          children))))
                  expr))))))

(o-defblock calc
  (main-arg)
  (rel "=" hint-format "\\left[ %s \\right." explicit-vspace 2 color "maroon")
  "Render an Org-list as an equational proof.

Sometimes the notation delimiting justification hints may clash
with the domain topic, so we can change the hint format, e.g., to
\"\\left\\langle %s \\right\\rangle\".  Set HINT-FORMAT to the
empty string, \"\", to comment-out all hints in the exported
version.

The hint is the text immediately after a “--”, if there are
multiple such delimiters only the first is shown; this can be
useful if we want to have multiple alternatives, say for extra
details in the source but not so much in the export.

Line breaks are taken literally in the hints, but not in the
math; where one uses \\\\.  For math with multiple lines, use ‘&’
as an alignment marker; otherwise math is right-justified.

For HTML, to use an TeX it must be enclosed in $, since that is
what is required by MathJaX."
  (thread-last (with-temp-buffer
                 (insert raw-contents)
                 (goto-char (point-min))
                 (org-list-to-lisp))
    cdr
    (--map (format "%s" (o--list-to-calc it rel hint-format explicit-vspace color)))
    (s-join "\\\\")
    (format "$$\\begin{align*} & %s \n\\end{align*}$$")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'org-special-block-extras)

;;; org-special-block-extras.el ends here
