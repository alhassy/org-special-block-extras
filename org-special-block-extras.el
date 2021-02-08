;;; org-special-block-extras.el --- 30 new custom blocks & 34 link types for Org-mode   -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 2.3
;; Package-Requires: ((s "1.12.0") (dash "2.16.0") (emacs "26.1") (dash-functional "1.2.0") (org "9.1"))
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
(require 'dash-functional) ;; Function library; ‘-const’, ‘-compose’, ‘-orfn’,
                           ;; ‘-not’, ‘-partial’, etc.


(require 'cus-edit) ;; To get the custom-* faces

(require 'org)
(require 'ox-latex)
(require 'ox-html)

;;;###autoload
(define-minor-mode org-special-block-extras-mode
    "Provide 30 new custom blocks & 34 link types for Org-mode."
  nil nil nil
  (if org-special-block-extras-mode
      (progn
        ;; https://orgmode.org/manual/Advanced-Export-Configuration.html
        (add-hook 'org-export-before-parsing-hook 'org-special-block-extras--support-special-blocks-with-args)
        (advice-add #'org-html-special-block
           :before-until (apply-partially #'org-special-block-extras--advice 'html))
        
        (advice-add #'org-latex-special-block
           :before-until (apply-partially #'org-special-block-extras--advice 'latex))
        (setq org-export-allow-bind-keywords t)
        (defvar org-special-block-extras--kbd-html-setup nil
          "Has the necessary keyboard styling HTML beeen added?")
        
        (unless org-special-block-extras--kbd-html-setup
          (setq org-special-block-extras--kbd-html-setup t)
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
        (unless org-special-block-extras--docs-from-libraries
          (org-special-block-extras-docs-load-libraries))
        (defvar org-special-block-extras--tooltip-html-setup nil
          "Has the necessary HTML beeen added?")
        
        (unless org-special-block-extras--tooltip-html-setup
          (setq org-special-block-extras--tooltip-html-setup t)
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
        ;; Actual used glossary entries depends on the buffer; so clean up after each export
        (advice-add #'org-export-dispatch
          :after (lambda (&rest _)
          (setq org-special-block-extras--docs-GLOSSARY nil
                org-special-block-extras--docs nil)))
        (cl-loop for lnk in org-special-block-extras-fancy-links
              do (highlight-phrase (format "%s:[^ \n]*" lnk)
                                   'custom-button))
        
        ;; Other faces to consider: custom-button-mouse, custom-button-unraised,
        ;; custom-button, custom-button-pressed, custom-link
      ) ;; Must be on a new line; I'm using noweb-refs
    (remove-hook 'org-export-before-parsing-hook 'org-special-block-extras--support-special-blocks-with-args)
    (advice-remove #'org-html-special-block
                   (apply-partially #'org-special-block-extras--advice 'html))
    
    (advice-remove #'org-latex-special-block
                   (apply-partially #'org-special-block-extras--advice 'latex))
    (cl-loop for lnk in org-special-block-extras-fancy-links
          do (unhighlight-regexp (format "%s:[^ \n]*" lnk)))
    )) ;; Must be on a new line; I'm using noweb-refs

(defvar org-special-block-extras--supported-blocks nil
  "Which special blocks, defined with DEFBLOCK, are supported.")

(cl-defmacro org-special-block-extras-defblock
  (name main-arg kwds &rest experimental&&docstring&&body)
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

TLDR for EXPERIMENTAL&&DOCSTRING&&BODY, the first two parts are
optional; they're a symbol, a string, then the main body.  The
symbol, OSPE-RESPECT-NEWLINES?, when present enables a highly
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
     \"Top level (HTML & LaTeX)OSPE-RESPECT-NEWLINES? editorial remarks; in Emacs they're angry red.\"
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
    ;; catenated as well as the default red color.
"
  ;; ⇨ The special block support
  ;;
  (add-to-list 'org-special-block-extras--supported-blocks name) ;; global var

  ;; Identify which of the optional features is present...
  (let (ospe-respect-newlines?
        docstring
        body)
    (if (symbolp (first experimental&&docstring&&body))
        ;; okay we have a newline declaration, but do we ALSO have a doc-string?
        (if (stringp (second experimental&&docstring&&body))
            (setq ospe-respect-newlines? t
                  docstring (second experimental&&docstring&&body)
                  body      (cddr   experimental&&docstring&&body))
          (setq ospe-respect-newlines? t
                body      (rest experimental&&docstring&&body)))
      ;; no newline declaration...
      ;; maybe we have a docstring?
      (if (stringp (first experimental&&docstring&&body))
          (setq docstring (first experimental&&docstring&&body)
                body      (rest experimental&&docstring&&body))
        ;; else neither newline-declaration now docstring
        (setq body experimental&&docstring&&body)))

    `(progn
       ;; Produce an associated Lisp function
       ,(org-special-block-extras-defblock---support-block-type
         name
         docstring
         (if (consp `,main-arg) (car main-arg) 'main-arg) ;; main argument's name
         (cadr main-arg) ;; main argument's value
         kwds
         body
         ;; MA: I'd like it to be always ‘true’, but it's experimental and breaks so much stuff.
         ospe-respect-newlines?
         )

       ;; ⇨ The link type support
       ;; The ‘main-arg’ may contain a special key ‘:link-type’ whose contents
       ;; are dumped here verbatim.
       ;; ‘(main-arg-name main-arg-val :face … :follow …)’
       ,(org-special-block-extras-defblock---support-link-type
         name (cddr main-arg) docstring))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WHERE ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defmethod org-special-block-extras-defblock---support-block-type
    (name docstring main-arg-name main-arg-value kwds body ospe-respect-newlines?)
  "Helper method for org-special-block-extras-defblock.

This method creates an Org block type's associated Lisp function.

NAME, string: The name of the block type.
DOCSTRING, string: Documentation of block.
MAIN-ARG-NAME: Essentially main-arg's name
MAIN-ARG-VALUE: Essentially main-arg's value
KWDS, plist: Keyword-value pairs
BODY, list: Code to be executed"
  `(cl-defun ,(intern (format "org-special-block-extras--%s" name))
       (backend raw-contents
                &optional ;; ,(car main-arg)
                ,main-arg-name
                &rest _
                &key (ospe-link? nil) ,@(-partition 2 kwds))
     ,docstring
     ;; Use default for main argument
     (when (and ',main-arg-name (s-blank-p ,main-arg-name))
       (--if-let (plist-get (cdr (assoc ',name org-special-block-extras--header-args)) :main-arg)
           (setq ,main-arg-name it)
         (setq ,main-arg-name ,main-arg-value)))

     (cl-letf (((symbol-function 'org-export)
                 (lambda (x)
            "Wrap the given X in an export block for the current backend.

          One can think of this function as replacing the #+begin_𝒳⋯#+end_𝒳
          in-place in your Org document; but really that's done by the
          ⋯-support-blocks function.
          "
            (if ospe-link?
                x
            ;; ospe-respect-newlines? is super experimental: It's a bit ugly on the LaTeX side.
            (cond ((and ,ospe-respect-newlines? (member backend '(html reveal)))
                   (format "@@%s:%s@@" backend (s-replace "\n" (format "@@\n@@%s:" backend) x) backend))
                  (:else
                   (format "#+begin_export %s \n%s\n#+end_export"
                           backend x))))))

               ((symbol-function 'org-parse)
                (lambda (x)
                  "This should ONLY be called within an ORG-EXPORT call."
                  (if ospe-link?
                      x
                    (cond ((and ,ospe-respect-newlines? (member backend '(html reveal)))
                           (format "@@%s@@%s:" x backend))
                          (:else
                           (format "\n#+end_export\n%s\n#+begin_export %s\n" x
                                   backend)))))))

       ;; Use any headers for this block type, if no local value is passed
       ,@(cl-loop for k in (mapcar #'car (-partition 2 kwds))
                  collect `(--when-let (plist-get (cdr (assoc ',name org-special-block-extras--header-args))
                                                  ,(intern (format ":%s" k)))
                             (when (s-blank-p ,k)
                               (setq ,k it))))

       (org-export
        (let ((contents (org-parse raw-contents))) ,@body)))))

(cl-defmethod org-special-block-extras-defblock---support-link-type
    (name verbatim tooltip)
  "Helper method for org-special-block-extras-defblock.

This method creates an Org link type.

NAME, string: The name of the link type.
VERBATIM, list: Code dumped into the org-link-set-parameters.
TOOLTIP, string: Tooltip text alongside link, for use in Emacs."
  `(org-link-set-parameters
    ,(format "%s" name)
    ,@verbatim
    :export (lambda (label description backend)
              ; s-replace-all `(("#+end_export" . "") (,(format "#+begin_export %s" backend) . ""))
              (s-replace-all `(("@@" . "")) ; (,(format "@@%s:" backend) . "")
               (,(intern (format "org-special-block-extras--%s" name))
                backend (or description label) label :ospe-link? t)))
    ;; The tooltip alongside a link
    :help-echo (lambda (window object position)
                 (save-excursion
                   (goto-char position)
                   (-let* (((&plist :path :format :raw-link :contents-begin :contents-end)
                            (cadr (org-element-context)))
                           (description
                            (when (equal format 'bracket)
                              (copy-region-as-kill contents-begin contents-end)
                              (substring-no-properties (car kill-ring)))))
                     (format "%s\n\n%s"
                             raw-link ,tooltip))))))

(defun org-special-block-extras--pp-list (xs)
  "Given XS as (x₁ x₂ … xₙ), yield the string “x₁ x₂ … xₙ”, no parens.
  When n = 0, yield the empty string “”."
  (s-chop-suffix ")" (s-chop-prefix "(" (format "%s" (or xs "")))))

(defvar org-special-block-extras--current-backend nil
  "A message-passing channel updated by
org-special-block-extras--support-special-blocks-with-args
and used by DEFBLOCK.")

(defun org-special-block-extras--support-special-blocks-with-args (backend)
  "Remove all headlines in the current buffer.
BACKEND is the export back-end being used, as a symbol."
  (setq org-special-block-extras--current-backend backend)
  (cl-loop for blk in org-special-block-extras--supported-blocks
        for kwdargs = nil
        for blk-start = nil
        do (goto-char (point-min))
        (while (ignore-errors (re-search-forward (format "^\s*\\#\\+begin_%s" blk)))
          ;; MA: HACK: Instead of a space, it should be any non-whitespace, optionally;
          ;; otherwise it may accidentlly rewrite blocks with one being a prefix of the other!
          ; (kill-line)
          ; (error (format "(%s)" (substring-no-properties (car kill-ring))))
          (setq blk-start (line-beginning-position))
          (setq header-start (point))
          (setq body-start (1+ (line-end-position)))
          (setq kwdargs (read (format "(%s)" (buffer-substring-no-properties header-start (line-end-position)))))
          (setq kwdargs (--split-with (not (keywordp it)) kwdargs))
          (setq main-arg (org-special-block-extras--pp-list (car kwdargs)))
          (setq kwdargs (cadr kwdargs))
          ; (beginning-of-line) (kill-line)
          (forward-line -1)
          (re-search-forward (format "^\s*\\#\\+end_%s" blk))
          (setq contents (buffer-substring-no-properties body-start (line-beginning-position)))
          ; (beginning-of-line)(kill-line) ;; Hack!
          (kill-region blk-start (point))
          (insert
             (eval `(,(intern (format "org-special-block-extras--%s" blk))
                     (quote ,backend)
                     contents
                     main-arg
                     ,@(--map (list 'quote it) kwdargs)))
             )
          ;; the --map is so that arguments may be passed
          ;; as "this" or just ‘this’ (raw symbols)
      )))

(defvar org-special-block-extras--header-args nil
  "Alist (name plist) where “:main-arg” is a special plist key.

  It serves a similar role to that of Org's src ‘header-args’.

  See doc of SET-BLOCK-HEADER-ARGS for more information.")

(defmacro org-special-block-extras-set-block-header-args (blk &rest kvs)
  "Set default valuts for special block arguments.

This is similar to, and inspired by, Org-src block header-args.

Example src use:
    #+PROPERTY: header-args:Language :key value

Example block use:
    (set-block-header-args Block :main-arg mainvalue :key value)

A full, working, example can be seen by “C-h o RET defblock”.
"
  `(add-to-list 'org-special-block-extras--header-args (list (quote ,blk) ,@kvs)))

(defun org-special-block-extras-short-names ()
  "Expose shorter names to the user.

Namely,

  org-special-block-extras-set-block-header-args   ↦  set-block-header-args
  org-special-block-extras-defblock                ↦  defblock
  org-special-block-extras-subtle-colors           ↦  subtle-colors
"
  (defalias 'defblock              'org-special-block-extras-defblock)
  (defalias 'set-block-header-args 'org-special-block-extras-set-block-header-args)
  (defalias 'thread-blockcall      'org-special-block-extras-thread-blockcall)
  (defalias 'subtle-colors         'org-special-block-extras-subtle-colors))

;; This is our 𝒳, “remark”.
;; As a link, it should be shown angry-red;
;; it takes two arguments: “color” and “signoff”
;; with default values being "red" and "".
;; (Assuming we already called org-special-block-extras-short-names. )
(org-special-block-extras-defblock rremark
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
(org-special-block-extras-set-block-header-args rremark :main-arg "Jasim Jameson" :signoff "( Aim for success! )")

(cl-defmacro org-special-block-extras--blockcall (blk &optional main-arg &rest keyword-args-then-contents)
  "An anaologue to `funcall` but for blocks.

Usage: (blockcall blk-name main-arg even-many:key-values raw-contents)

One should rarely use this directly; instead use
org-special-block-extras-thread-blockcall.
"
  `(concat "#+end_export\n" (,(intern (format "org-special-block-extras--%s" blk))
    backend ;; defblock internal
    ; (format "\n#+begin_export html\n\n%s\n#+end_export\n" ,(car (last keyword-args-then-contents))) ;; contents
    ,@(last keyword-args-then-contents) ;; contents
    ,main-arg
    ,@(-drop-last 1 keyword-args-then-contents)) "\n#+begin_export"))

(defmacro org-special-block-extras-thread-blockcall (body &rest forms)
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

    (org-special-block-extras-defblock nesting (name) nil
      \"Show text in a box, within details, which contains a box.\"

      (org-special-block-extras-thread-blockcall raw-contents
                        (box name)
                        (details (upcase name) :title-color \"green\")
                        (box (format \"⇨ %s ⇦\" name) :background-color \"blue\")
                        ))
"
  (if (not forms) body
     `(-let [result (org-special-block-extras--blockcall ,@(car forms) ,body)]
    ,@(cl-loop for b in (cdr forms)
          collect `(setq result (org-special-block-extras--blockcall ,@b
                                     (concat
                                   "#+begin_export\n"
                                   result
                                   "\n#+end_export"
                                   )))) result)))

(org-special-block-extras-defblock solution
  (title "Solution")
  (reprimand "Did you actually try? Maybe see the ‘hints’ above!"
   really "Solution, for real")
  "Show the answers to a problem, but with a reprimand in case no attempt was made."
  (org-special-block-extras-thread-blockcall raw-contents
                    (details really :title-color "red")
                    (box reprimand :background-color "blue")
                    (details title)))

(org-special-block-extras-defblock org-demo nil (source "Source" result "Result"
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
                 (org-special-block-extras--blockcall box source :background-color source-color)
                 org-export)
               ;; Separator
               sep
               ;; Result
               (thread-last raw-contents
                 (org-special-block-extras--blockcall box result :background-color result-color)
                 org-export))]

   (if (equal style "parallel")
       (org-special-block-extras--blockcall parallel "2" :bar nil text)
       (concat "#+end_export\n" text "\n#+begin_export"))))

(org-special-block-extras-defblock stutter (reps 2) nil
  "Output the CONTENTS of the block REPS many times"
  (-let [num (if (numberp reps) reps (string-to-number reps))]
    (s-repeat num contents)))

(org-special-block-extras-defblock rename (list "") nil
  "Perform the given LIST of substitutions on the text.
The LIST is a comma separated list of ‘to’ separated symbols.
In a link, no quotes are needed."
  (s-replace-all
   (--map (cons (car it) (cadr it))
          (--map (s-split " to " (s-trim it))
                 (s-split "," list)))
   contents))

(org-special-block-extras-defblock spoiler (color "grey") (left "((" right "))")
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

(defun org-special-block-extras--list-to-math (lst)
  "Get a result LST from ORG-LIST-TO-LISP and render it as a proof tree."
  (cond
   ((symbolp lst) "")
   ((symbolp (car lst)) (org-special-block-extras--list-to-math (cadr lst)))
   (t
    (-let* (((conclusion₀ children) lst)
            ((name named?) (s-split " :: " conclusion₀))
            (conclusion (or named? conclusion₀)))
      (if (not children)
          (if named? (format "\\frac{}{%s}[%s]" conclusion name) conclusion)
        (format "\\frac{\\displaystyle %s}{%s}%s"
                (s-join " \\qquad "
                        (mapcar #'org-special-block-extras--list-to-math children))
                conclusion
                (if named? (format "[\\text{%s}]" name) "")))))))

(org-special-block-extras-defblock tree (main-arg) nil
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
                                (org-special-block-extras--list-to-math it))
                        (cdr (with-temp-buffer
                               (insert raw-contents)
                               (goto-char (point-min))
                               (org-list-to-lisp))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core utility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-special-block-extras--advice (backend blk contents _)
  "Invoke the appropriate custom block handler, if any.

A given custom block BLK has a TYPE extracted from it, then we
send the block CONTENTS along with the current export BACKEND to
the formatting function ORG-SPECIAL-BLOCK-EXTRAS--TYPE if it is
defined, otherwise, we leave the CONTENTS of the block as is.

We also support the seemingly useless blocks that have no
contents at all, not even an empty new line."
  (let* ((type    (nth 1 (nth 1 blk)))
         (handler (intern (format "org-special-block-extras--%s" type))))
    (ignore-errors (apply handler backend (or contents "") nil))))

(defun org-special-block-extras--extract-arguments (contents &rest args)
"Get list of CONTENTS string with ARGS lines stripped out and values of ARGS.

Example usage:

    (-let [(contents′ . (&alist 'k₀ … 'kₙ))
           (…extract-arguments contents 'k₀ … 'kₙ)]
          body)

Within ‘body’, each ‘kᵢ’ refers to the ‘value’ of argument
‘:kᵢ:’ in the CONTENTS text and ‘contents′’ is CONTENTS
with all ‘:kᵢ:’ lines stripped out.

+ If ‘:k:’ is not an argument in CONTENTS, then it is assigned value NIL.
+ If ‘:k:’ is an argument in CONTENTS but is not given a value in CONTENTS,
  then it has value the empty string."
  (let ((ctnts contents)
        (values (cl-loop for a in args
                         for regex = (format ":%s:\\(.*\\)" a)
                         for v = (cadr (s-match regex contents))
                         collect (cons a v))))
    (cl-loop for a in args
             for regex = (format ":%s:\\(.*\\)" a)
             do (setq ctnts (s-replace-regexp regex "" ctnts)))
    (cons ctnts values)))

(defvar org-special-block-extras-hide-editor-comments nil
  "Should editor comments be shown in the output or not.")

(org-special-block-extras-defblock remark
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
    (if org-special-block-extras-hide-editor-comments
        ""
      (format (pcase backend
                ('latex (format "{\\color{%%s}%s %%s %%s %%s %%s}" (if strong "\\bfseries" "")))
                (_ (format "<%s style=\"color: %%s;\">%%s %%s %%s %%s</%s>" (if strong "strong" "p") (if strong "strong" "p"))))
              color edcomm-begin contents′ signoff edcomm-end))))

(org-link-set-parameters
 "edcomm"
  :follow (lambda (_))
  :export (lambda (label description backend)
            (org-special-block-extras--edcomm
             backend
             (format ":ed:%s\n%s" label description)))
  :help-echo (lambda (_ __ position)
               (save-excursion
                 (goto-char position)
                 (-let [(&plist :path) (cadr (org-element-context))]
                   (format "%s made this remark" (s-upcase path)))))
  :face '(:foreground "red" :weight bold))

(org-special-block-extras-defblock details (title "Details") (title-color "green")
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

(org-special-block-extras-defblock Details (title "Details") (title-color "green")
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

(org-special-block-extras-defblock box (title "") (background-color nil)
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
             ,(org-special-block-extras-subtle-colors (format "%s" (or background-color "green")))
             ";border-radius: 15px; font-size: 0.9em"
             "; box-shadow: 0.05em 0.1em 5px 0.01em #00000057;\">"
             "<h3>" ,title "</h3>"
            ,contents "</div>")))))

(defun org-special-block-extras-subtle-colors (c)
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

(org-special-block-extras-defblock parallel (cols 2) (bar nil)
  "Place ideas side-by-side, possibly with a seperator.

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

(defvar org-special-block-extras--colors
  '(black blue brown cyan darkgray gray green lightgray lime
          magenta olive orange pink purple red teal violet white
          yellow)
  "Colours that should be available on all systems.")

(cl-loop for colour in org-special-block-extras--colors
         do (eval `(org-special-block-extras-defblock ,colour
                     (the-color "black" :face `(:foreground ,(format "%s" (quote ,colour))))
                     nil
                     ,(format "Show text in %s color." colour)
                     (let ()
                       (format (pcase backend
                                 (`latex "\\begingroup\\color{%s}%s\\endgroup\\,")
                                 (_  "<span style=\"color:%s;\">%s</span>"))
                               (quote ,colour) contents)))))

(org-special-block-extras-defblock color
  (color black :face (lambda (colour) `(:foreground ,(format "%s" colour))))
  nil
  "Format text according to a given COLOR, which is black by default."
  (format (pcase backend
            (`latex "\\begingroup\\color{%s}%s\\endgroup\\,")
            (`html  "<span style=\"color:%s;\">%s</span>"))
          color contents))

(org-special-block-extras-defblock latex-definitions nil nil
  "Declare but do not display the CONTENTS according to the BACKEND."
  (format (pcase backend
            ('html "<p style=\"display:none\">\\[%s\\]</p>")
            (_ "%s"))
          raw-contents))

(org-link-set-parameters
 "kbd"
  :follow (lambda (_))
  :export (lambda (label description backend)
            (format (pcase backend
                      ('latex "\\texttt{%s}")
                      (_ "<kbd> %s </kbd>") )
                    (or description (s-replace "_" " " label)))))

(defvar
 org-special-block-extras--supported-octoicons
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

Usage: (cadr (assoc 'ICON org-special-block-extras--supported-octoicons))")

;; Show an OctoIcon: home, link, mail, report, tag, clock
(org-link-set-parameters
  "octoicon"
  :follow (lambda (_))
  :export (lambda (icon _ backend)
    (pcase backend
      (`html  (format
               (s-collapse-whitespace
                (cadr (assoc (intern icon)
                             org-special-block-extras--supported-octoicons)))))
      (_ ""))))

;; Export a link to the current location in an Org file.
(org-link-set-parameters
  "link-here"
  :follow (lambda (path) (message "This is a local anchor link named “%s”" path))
  :export  (lambda (label _ backend)
    (pcase backend
      (`html  (format (s-collapse-whitespace
          "<a class=\"anchor\" aria-hidden=\"true\" id=\"%s\"
          href=\"#%s\">%s</a>")
                      label label (cadr (assoc 'link
                              org-special-block-extras--supported-octoicons))))
      (_ ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The badge link types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defmacro org-special-block-extras-make-badge
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

     (org-special-block-extras-make-badge \"go\")

Then the following exports nicely from an Org file:

     go:key|value|blue|here|gnu-emacs

--------------------------------------------------------------------------------

The LABEL should be of the shape ‘key|value|color|url|logo’
resulting in a badge “|key|value|” where the ‘key’
is coloured grey and the ‘value’ is coloured ‘color’.

+ Only the syntax ‘badge:key|value|color|url’ is supported.
  - ‘key’ and ‘value’ have their underscores interpreted as spaces.
     ⇒ Underscores are interpreted as spaces;
     ⇒ ‘__’ is interpreted as an underscore;
     ⇒ ‘|’ is not a valid substring, but ‘-, %, ?’ are okay.
  - ‘|color|url|logo’ are optional;
     if ‘url’ is ‘here’ then the resulting badge behaves
     like ‘link-here:key’.
  - ‘color’ may be: ‘brightgreen’ or ‘success’,
                    ‘red’         or ‘important’,
                    ‘orange’      or ‘critical’,
                    ‘lightgrey’   or ‘inactive’,
                    ‘blue’        or ‘informational’,
            or ‘green’, ‘yellowgreen’, ‘yellow’, ‘blueviolet’, ‘ff69b4’, etc.
+ Of course, you can write ‘⟦badge: ⋯⟧’, then ‘⋯’ may have multiword, spaced, content.
+ Such links are displayed using a SVG badges
  and so do not support the DESCRIPTION syntax
  ‘⟦link:label][description⟧’.
+ Besides the HTML BACKEND, such links are silently omitted.
"

  `(org-link-set-parameters ,name
    :follow (lambda (path) (--> (s-split "|" path)
                         (or (nth 3 it) path)
                         (browse-url it)))
    ;; :export #'org-special-block-extras--link--badge
    :export (lambda (label description backend)
              (if (equal backend 'latex) ""
               (-let [ (key value color url logo)  (s-split "|" label) ]
                (format
                 (pcase ,(if social-shields-name `(format ,social-url label) 'url)
                   ("here" (format "<a id=\"%s\" href=\"#%s\">%%s</a>" (s-replace "%" "%%" key) (s-replace "%" "%%" key)))
                   (""      "%s") ;; e.g., badge:key|value|color||logo
                   ('nil    "%s") ;; e.g., badge:key|value|color
                   (t      (format "<a href=\"%s\">%%s</a>"  (s-replace "%" "%%" ,(if social-shields-name `(format ,social-url label) 'url))))
                   )
                 ,(if social-shields-name
                     (if social-shields-url
                         `(format ,social-shields-url label)
                       `(format "<img src=\"https://img.shields.io/%s/%s?style=social\">"
                                ,social-shields-name label))
                   '(format "<img src=\"https://img.shields.io/badge/%s-%s-%s?logo=%s\">"
                           (url-hexify-string (s-replace "-" "--" key))
                           (url-hexify-string (s-replace "-" "--" (or value "")))
                           color logo)))
                 )))
    ;; The tooltip alongside a link
    :help-echo (lambda (window object position)
                 (save-excursion
                   (goto-char position)
                   (-let* (((&plist :path :format :raw-link :contents-begin :contents-end)
                            (cadr (org-element-context)))
                           (description
                            (when (equal format 'bracket)
                              (copy-region-as-kill contents-begin contents-end)
                              (substring-no-properties (car kill-ring)))))
                     (format "%s\n\n General Syntax:\n\t badge:key|value|colour|url|logo"
                             raw-link))))))

(org-special-block-extras-make-badge "badge")

;; Since we're invoking a macro, the twitter-excitement is used lazily; i.e.,
;; consulted when needed instead of being evaluated once.
(defvar org-special-block-extras-link-twitter-excitement
  "This looks super neat (•̀ᴗ•́)و:"
  "The string prefixing the URL being shared.")

(org-special-block-extras-make-badge
 "tweet"
 "twitter/url?=url="
 (format
   "https://twitter.com/intent/tweet?text=%s:&url=%%s"
   org-special-block-extras-link-twitter-excitement)
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
         do (eval `(org-special-block-extras-make-badge ,name′ ,social ,url)))

(defvar org-special-block-extras--docs nil
  "An alist of (label name description) entries; our glossary.

Example use: (-let [(name description) (cdr (assoc 'label docs))] ⋯)")

(defvar org-special-block-extras--docs-fallback
  (lambda (label) (list label label (documentation (intern label))))
  "The fallback method to retriving documentation or glossary entries.")

(defvar org-special-block-extras--docs-libraries nil
  "List of Org files that have ‘#+begin_documentation’ blocks that should be loaded
   for use with the ‘doc:𝒳’ link type.")

(cl-defun org-special-block-extras-docs-load-libraries
    (&optional (libs org-special-block-extras--docs-libraries))
"Load user's personal documentation libraries.

If no LIBS are provided, simply use those declared
org-special-block-extras--docs-libraries.

See org-special-block-extras--docs-from-libraries.
"
(interactive)
(cl-loop for lib in libs
      do (with-temp-buffer
           (insert-file-contents lib)
           ;; doc only activates after an export
           (-let [org-export-with-broken-links t] (org-html-export-as-html))
           (kill-buffer)
           (delete-window)
           (setq org-special-block-extras--docs-from-libraries (-concat org-special-block-extras--docs org-special-block-extras--docs-from-libraries))
           (setq org-special-block-extras--docs nil))))

(defvar org-special-block-extras--docs-from-libraries nil

  "The alist of (label name description) entries loaded from the libraries.

The initial value ‘-1’ is used to indicate that no libraries have been loaded.
The ‘doc:𝒳’ link will load the libraries, possibly setting this variable to ‘nil’,
then make use of this variable when looking for documentation strings.

Interactively call org-special-block-extras-docs-load-libraries
to force your documentation libraries to be reloaded.

See also org-special-block-extras--docs-libraries.")

(defvar org-special-block-extras--docs-GLOSSARY nil
  "Which words are actually cited in the current article.

We use this listing to actually print a glossary using
‘show:GLOSSARY’.")

(org-link-set-parameters
 "doc"
 :follow (lambda (_) ())
 :export
   `(lambda (label description backend)
     (-let [(name docs) (org-special-block-extras--name&doc label)]
       (add-to-list 'org-special-block-extras--docs-GLOSSARY
                    (list label name docs))
       (setq name (or description name))
       (pcase backend
         (`html  (format "<abbr class=\"tooltip\" title=\"%s\">%s</abbr>"
                         (org-special-block-extras--poor-mans-html-org-export docs)
                         name))
         ;; Make the current word refer to its glosary entry;
         ;; also declare the location that the glossary should refer back to.
         (`latex (format (concat "\\hyperref"
                                 "[org-special-block-extras-glossary-%s]{%s}"
                                "\\label{org-special-block-extras-glossary"
                                "-declaration-site-%s}")
                         label name label)))))
  :help-echo
  `(lambda (_ __ position)
    (save-excursion
      (goto-char position)
      (-let* (((&plist :path) (cadr (org-element-context)))
              ((name doc) (org-special-block-extras--name&doc path)))
        (format "[%s] %s :: %s" path name doc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WHERE ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-special-block-extras--poor-mans-html-org-export (s)
  "Make Org-markup'd string S ready for HTML presentation.

Strangely produces: Lisp nesting exceeds ‘max-lisp-eval-depth’
(org-export-string-as s 'html :body-only-please)"

  ;; Make it look pretty!
  (thread-last s
    ;; Strangely produces: Lisp nesting exceeds ‘max-lisp-eval-depth’
    ;; (org-export-string-as docs 'html :body-only-please)
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

(defun org-special-block-extras--name&doc (lbl)
  "Look for ‘lbl’ from within the current buffer first, otherwise look among the loaded libraries."
  (let* ((wit (or (assoc lbl org-special-block-extras--docs)
                  (assoc lbl org-special-block-extras--docs-from-libraries)))
         (name (cl-second wit))
         (doc (cl-third wit)))
    ;; If there is no documentation, try the fallback.
    (unless doc
      (setq doc
            (condition-case nil
                (funcall org-special-block-extras--docs-fallback lbl)
              (error
               (error "Error: No documentation-glossary entry for “%s”!"
                      lbl))))
      (setq name (nth 1 doc))
      (setq doc (nth 2 doc)))
    (list name doc)))

(org-special-block-extras-defblock documentation
  (name (error "Documentation block: Name must be provided"))
  (label nil show nil color "green")
  "Register the dictionary entries in CONTENTS to the dictionary variable.

The dictionary variable is ‘org-special-block-extras--docs’.

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
        do  (add-to-list 'org-special-block-extras--docs
                         (mapcar #'s-trim (list (format "%s" l) name (substring-no-properties raw-contents)))))
  ;; Should the special block show something upon export?
  (if show (org-special-block-extras--blockcall box name :background-color color raw-contents) ""))

(let ((whatdo (lambda (x)
                (message
                          (concat "The value of variable  %s  will be placed "
                                  "here literally upon export, "
                                  "which is: \n\n %s")
                          (s-upcase x)
                          (if (equal x "GLOSSARY")
                              (format "A cleaned up presentation of ...\n%s"
                                      org-special-block-extras--docs-GLOSSARY)
                          (pp (eval (intern x))))))))
  (org-link-set-parameters
    "show"
    :face '(:underline "green")
    :follow whatdo
    :help-echo `(lambda (_ __ position)
                  (save-excursion
                    (goto-char position)
                    (-let [(&plist :path) (cadr (org-element-context))]
                      (funcall ,whatdo path))))
    :export
     (lambda (label _description backend)
      (cond ((not (equal label "GLOSSARY")) (prin1 (eval (intern label))))
            ((equal 'html backend) "") ;; Do not print glossary in HTML
            (t
             (-let ((fstr (concat "\\vspace{1em}\\phantomsection"
                                 "\\textbf{%s}\\quad"
                                 "\\label{org-special-block-extras-glossary-%s}"
                                 "%s See page "
                                 "\\pageref{org-special-block-extras"
                                 "-glossary-declaration-site-%s}"))
                    (preserve ;; preserve whitespace
                     (lambda (x)
                       (s-replace "\n" " \\newline{\\color{white}.}"
                                  (s-replace "  " " \\quad "
                                             ;; Hack!
                                             (s-replace "&" "\\&" x))))))
               (s-join "\n\n"
                       (cl-loop for (label name doc)
                             in org-special-block-extras--docs-GLOSSARY
                             collect (format fstr name label
                                             (when doc (funcall preserve doc))
                                             label)))))))))

(org-special-block-extras-defblock margin
  (marker nil
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
                 (org-special-block-extras--poor-mans-html-org-export contents)
                 ; MA: FIXME: (org-export-string-as contents 'html :body-only-please)
                 marker)))))

(defun org-special-block-extras--list-to-calc (lst rel hint-format NL-length color)
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
   ((symbolp (car lst)) (org-special-block-extras--list-to-calc (cadr lst)))
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
                                   (--map (format "%s" (org-special-block-extras--list-to-calc it rel hint-format NL-length color))
                                          children))))
                  expr))))))

(org-special-block-extras-defblock calc
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
    (--map (format "%s" (org-special-block-extras--list-to-calc it rel hint-format explicit-vspace color)))
    (s-join "\\\\")
    (format "$$\\begin{align*} & %s \n\\end{align*}$$")))

(defvar org-special-block-extras-fancy-links
  '(badge kbd link-here doc tweet)
  "The links, regexps, that should be shown with a boxed face within Emacs.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'org-special-block-extras)

;;; org-special-block-extras.el ends here
