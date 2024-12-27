;;; org-special-block-extras.el --- 30 new custom blocks & 34 link types for Org-mode   -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 4.1.1
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
(require 'seq)

(require 'lf)

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
        (add-hook 'org-export-before-parsing-hook 'org--support-special-blocks-with-args)
        (setq org-export-allow-bind-keywords t)
          (defvar org--ospe-kbd-html-setup nil
            "Has the necessary keyboard styling HTML beeen added?")
        
          (unless org--ospe-kbd-html-setup
            (setq org--ospe-kbd-html-setup t))
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
          </style>")))
        ;; Ensure user's documentation libraries have loaded
        (unless org--docs-from-libraries
          (org-docs-load-libraries))
          (defvar org--tooltip-html-setup nil
            "Has the necessary HTML beeen added?")
        
          (unless org--tooltip-html-setup
            (setq org--tooltip-html-setup t))
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
          ")))
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
    (remove-hook 'org-export-before-parsing-hook 'org--support-special-blocks-with-args)
    )) ;; Must be on a new line; I'm using noweb-refs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We define a parent keymap that org-deflink keymaps inherit from.
;; We also define a few useful functions that we then bind to this parent map.

(defvar org-special-block-extras-mode-map (make-keymap)
  "A keymap of actions, on link types, that is inherited by all `org-deflink' link keymaps.

To learn about keymap inheritance, run:  C-h i m elisp RETURN m Inheritance and Keymaps RETURN.

This keymap has the following bindings setup:

    (define-key org-special-block-extras-mode-map (kbd \"C-n\") #'org-this-link-next)
    (define-key org-special-block-extras-mode-map (kbd \"C-p\") #'org-this-link-previous)
    (define-key org-special-block-extras-mode-map (kbd \"C-h\") #'org-this-link-show-docs)

The use of `C-n' and `C-p' may be a nuisance to some users, since they override `forward-line'
and `previous-line' when the cursor is on an org-link type. As such, place something like the following
in your initialisation file.

    ;; Use  C-c C-f  to move to the next link of the same link-type as the one under the cursor
    (define-key org-special-block-extras-mode-map (kbd \"C-c C-f\") #'org-this-link-next)

Alternatively, if you don't find much value in these basic bindings, you can remove them all:

    ;; Disable basic org-special-block-extras link keybindings
    (setcdr org-special-block-extras-mode-map nil)

    ;; Or, remove a single binding
    (define-key org-special-block-extras-mode-map (kbd \"C-n\") nil)")

(defvar org-special-block-extras-mode-map--link-keymap-docs nil
  "An alist referencing key bindings for Org links; used in `org-this-link-show-docs'.")

(defun org-link-at-point ()
  "Get the Org link type at point, with suffix colon."
  (interactive)
  (let ((working-line (line-number-at-pos)))
    (save-excursion
      ;; Account for cursour being on anywhere on the links “name:key”.
      (backward-word 2)
      (unless (= working-line (line-number-at-pos))
        (goto-line working-line))
      (let* ((here-to-eol (buffer-substring-no-properties (point) (point-at-eol)))
             ;; E.g., “kbd:”, the name part of an Org link
             (link-name (cl-second (s-match "\\([^ ]+:\\).+" here-to-eol))))
        link-name))))

(defun org-this-link-next ()
  "Go to the next Org link that is similar to the link at point."
  (interactive)
  (re-search-forward (org-link-at-point) nil t))
(defun org-this-link-previous ()
  "Go to the previous Org link that is similar to the link at point."
  (interactive)
  (re-search-backward (org-link-at-point) nil t))
(defun org-this-link-show-docs ()
  "Show documentation for the Org link at point in a read-only buffer.

                     Press ‘q’ to kill the resulting buffer and window."
  (interactive)
  (let* ((link (s-chop-suffix ":" (org-link-at-point)))
         (msg (ignore-errors
                (concat
                 (documentation (intern (format "org-link/%s" link)))
                 "\nKEY BINDINGS:\n"
                 "\nUnless indicated below otherwise..."
                 "\n\tC-h: Shows this helpful message buffer"
                 "\n\tC-n/C-p on the link to jump to next/previous links of this type;"
                 "\n\tC-c C-x C-n/p for moving between arbitrary link types.\n\n"
                 (pp-to-string
                  (cdr (assoc link org-special-block-extras-mode-map--link-keymap-docs))))))
         ;; i.e., insist on displaying in a dedicated buffer
         (max-mini-window-height 0))
    (display-message-or-buffer msg)
    (switch-to-buffer-other-window "*Message*")
    (rename-buffer (format "Help: Org Link “%s”" link))
    (read-only-mode)
    (local-set-key "q" #'kill-buffer-and-window)
    (message "Read-only; “q” to kill buffer and window.")))

(define-key org-special-block-extras-mode-map (kbd "C-n") #'org-this-link-next)
(define-key org-special-block-extras-mode-map (kbd "C-p") #'org-this-link-previous)
(define-key org-special-block-extras-mode-map (kbd "C-h") #'org-this-link-show-docs)

(cl-defmacro org-deflink
    (name &optional docstring display &rest body)
  "Make a new Org-link NAME that exports using form BODY.

Since Org links are essentially string-valued functions,
a function ‘org-link/NAME’ is created.

DOCSTRING is optional; it is visible with
   (documentation 'org-link/NAME)

BODY is a string-valued expression, that may make use of the names
o-label, o-description, o-backend. The final one refers to the export
backend, such as 'html or 'latex. The first two are obtained from uses:

   [[name:o-label][o-description]]

In particular, the use case “name:o-label” means that o-description is nil.

---------------------------------------------------------------------------

Example use:

   ;; In a Lisp buffer, press “C-x C-e” to load this definition
   (org-deflink shout (upcase (or o-description o-label)))

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

  (org-deflink define
    \"Define the given word using WordNet, along with synonyms and coordinate terms.\"
    [:let (definition (shell-command-to-string (format \"wn %s -over -synsn -coorn\" o-label)))
     :help-echo definition]
    (--> definition
      (s-replace-regexp \"\\\\\\\"\" \"''\" it) ;; The presence of ‘\\\"’ in tooltips breaks things, so omit them.
      (s-replace-regexp \"\\n\" \"<br>\" it)
      (format \"<abbr class=\\\"tooltip\\\" title=\\\"%s\\\">%s</abbr>\" it o-label)))

For HTML tooltips, see `org-ospe-html-export-preserving-whitespace'.

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
  [:help-echo (org-link/shout o-label o-description 'html)]

  You may want to add the following to your Emacs init file:

    ;; Nearly instantaneous display of tooltips.
    (setq tooltip-delay 0)
    ;; Give user 30 seconds before tooltip automatically disappears.
    (setq tooltip-hide-delay 300)

+ :face specifies how should these links be displayed within Emacs.
   It is a list-valued expression.
   As usual, it may make use of O-LABEL (but O-DESCRIPTION has value nil).
   Example:
               :face '(:underline \"green\")

   See https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html

+ [:display 'full] if you do not want bracket links to be
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
    (let ((org-link/NAME (intern (format "org-link/%s" name)))
          (navigation "Press “C-h” to see possible actions on this link type.")
          (lets (cl-loop for (variable value)
                      on (cl-getf display :let)
                      by #'cddr
                      collect (list variable value))))
      `(progn
       ;; Declare the underlying function and documentation
       (cl-defun ,org-link/NAME ;; function name
           (o-label o-description o-backend)         ;; function args
           ;; new function documentation
           ,docstring
           ;; function body
           (let* ,lets ,@body))
       ;; Construct the Org-link
       (org-link-set-parameters
        ,(format "%s" name)
        :export (quote ,org-link/NAME)
        ;; How should these links be displayed?
        ;; (We augment the namespace with the missing o-description that local variables may be using.)
        :face (lambda (o-label)  (let (o-description) (let* ,lets ,(cl-getf display :face))))
        ;; When you click on such links, what should happen?
        ;; (We augment the namespace with the missing o-description that local variables may be using.)
        :follow (lambda (o-label o-prefix) (let (o-description) (let* ,lets ,(cl-getf display :follow))))
         ;; These links should *never* be folded in descriptive display;
        ;; i.e., “[[example:lable][description]]” will always appear verbatim
        ;; and not hide the first pair […].
        :display (cl-the symbol ,(cl-getf display :display)) ;; e.g.,: 'full
        ;; Any special keybindings when cursour is on this link type?
        ;; On ‘NAME:’ links, C-n/p to go to the next/previous such links.
        :keymap (let ((o-keymap (copy-keymap org-mouse-map))
                      (pattern (format "%s:" (quote ,name))))

                  ;; If this Org-link has additional key bindings, then save
                  ;; them in an alist for reference in `org-this-link-show-docs'.
                  (when (quote ,(cl-getf display :keymap))
                    (push (cons (format "%s" (quote ,name)) (quote ,(cl-getf display :keymap)))
                          org-special-block-extras-mode-map--link-keymap-docs))

                  ;; Let's inherit some possibly useful key bindings.
                  (set-keymap-parent o-keymap org-special-block-extras-mode-map)

                  ;; Populate the keymap
                  (cl-loop for (key action) on (quote ,(cl-getf display :keymap))
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
                               (org-format format)
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
                                     (,org-link/NAME o-label o-description 'html)))))))
        ;; Return value is the name of the underlying function.
        ;; We do this to be consistent with `defun'.
        (quote ,org-link/NAME)))))

(org-deflink melpa
 "Produce a Melpa badge for a given pacakge O-LABEL, which links to the Melpa page.
We try to get the package's version from a constant “⟨O-LABEL⟩-version” if it exists."
 [:face '(:box "purple" :foreground "purple")]
 (format (concat "<a href=\"https://melpa.org/#/%s\">"
                 "<img alt=\"MELPA\" src=\"https://img.shields.io/badge/%s-%s-green?logo=Gnu-Emacs\"></img>"
                 "</a>")
         o-label
         (s-replace "_" "__" (s-replace "-" "--" o-label)) ;; shields.io conventions
         (or (ignore-errors (eval (intern (concat o-label "-version")))) "Melpa")))

(defvar org--supported-blocks nil
  "Which special blocks, defined with DEFBLOCK, are supported.")

(cl-defmacro org-defblock
  (name kwds &optional link-display docstring &rest body)
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
     (editor \"Editor Remark\" :face angry-red) (color \"red\" signoff \"\")
     \"Top level (HTML & LaTeX) editorial remarks; in Emacs they're angry red.\"
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
  (add-to-list 'org--supported-blocks name) ;; global var

  ;; TODO: Relocate
  (defvar org--block--link-display nil
    "Association list of block name symbols to link display vectors.")

  ;; Identify which of the optional features is present...
  (cl-destructuring-bind (link-display docstring body)
      (lf-extract-optionals-from-rest link-display #'vectorp
                                      docstring    #'stringp
                                      body)
      `(progn
        (when ,(not (null link-display)) (push (cons (quote ,name) ,link-display) org--block--link-display))
       (list
      ,(org--create-defmethod-of-defblock name docstring (plist-get kwds :backend) kwds body)
       ;; ⇨ The link type support
      (eval (backquote (org-deflink ,name
                   ,(vconcat `[:help-echo (format "%s:%s\n\n%s" (quote ,name) o-label ,docstring)] (or link-display (cdr (assoc name org--block--link-display))))
                   ;; s-replace-all `((,(format "@@%s:" backend) . "") ("#+end_export" . "") (,(format "#+begin_export %s" backend) . ""))
                   (s-replace-regexp "@@" ""
                                     (,(intern (format "org-block/%s" name)) o-backend (or o-description o-label) o-label :o-link? t)))))))))

;; WHERE ...

(cl-defmethod org--create-defmethod-of-defblock ((name symbol) docstring backend-type (kwds list) (body list))
  "Helper method to produce an associated Lisp function for org-defblock.

+ NAME: The name of the block type.
+ DOCSTRING, string|null: Documentation of block.
+ KWDS: Keyword-value pairs
+ BODY: Code to be executed"
  (cl-assert (or (stringp docstring) (null docstring)))
  (cl-assert (or (symbolp backend-type) (null backend-type)))

  (let ((main-arg-name (or (cl-first kwds) 'main-arg))
        (main-arg-value (cl-second kwds))
        (kwds (cddr kwds)))
       ;; Unless we've already set the docs for the generic function, don't re-declare it.
       `(if ,(null body)
         (cl-defgeneric ,(intern (format "org-block/%s" name)) (backend raw-contents &rest _)
           ,docstring)

       (cl-defmethod ,(intern (format "org-block/%s" name))
     ((backend ,(if backend-type `(eql ,backend-type) t))
      (raw-contents string)
      &optional
      ,main-arg-name
      &rest _
      &key (o-link? nil) ,@(--reject (keywordp (car it)) (-partition 2 kwds))
      &allow-other-keys)
     ,docstring
     ;; Use default for main argument
     (when (and ',main-arg-name (s-blank-p ,main-arg-name))
       (--if-let (plist-get (cdr (assoc ',name org--header-args)) :main-arg)
           (setq ,main-arg-name it)
         (setq ,main-arg-name ,main-arg-value)))

     (cl-letf (((symbol-function 'org-export)
                 (lambda (x) "Wrap the given X in an export block for the current backend."
                   (if o-link? x (format "#+begin_export %s \n%s\n#+end_export" backend x))))
         ((symbol-function 'org-parse)
          (lambda (x) "This should ONLY be called within an ORG-EXPORT call."
            (if o-link? x (format "\n#+end_export\n%s\n#+begin_export %s\n" x backend)))))

       ;; Use any headers for this block type, if no local value is passed
       ,@(cl-loop for k in (mapcar #'car (-partition 2 kwds))
                  collect `(--when-let (plist-get (cdr (assoc ',name org--header-args))
                                                  ,(intern (format ":%s" k)))
                             (when (s-blank-p ,k)
                               (setq ,k it))))

       (org-export
        (let ((contents (org-parse raw-contents))) ,@body)))))))

(defun org--pp-list (xs)
  "Given XS as (x₁ x₂ … xₙ), yield the string “x₁ x₂ … xₙ”, no parens.
  When n = 0, yield the empty string “”."
  (s-chop-suffix ")" (s-chop-prefix "(" (format "%s" (or xs "")))))

(defvar org--current-backend nil
  "A message-passing channel updated by
org--support-special-blocks-with-args
and used by DEFBLOCK.")

(defun org--support-special-blocks-with-args (backend)
  "Remove all headlines in the current buffer.
BACKEND is the export back-end being used, as a symbol."
  (setq org--current-backend backend)
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
  (cl-loop for blk in org--supported-blocks
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
          (setq main-arg (org--pp-list (car kwdargs)))
          (setq kwdargs (cadr kwdargs))
          (forward-line -1)
          (re-search-forward (format "^\s*\\#\\+end_%s" blk))
          (setq blk-contents (buffer-substring-no-properties body-start (line-beginning-position)))
          (kill-region blk-start (point))
          (insert (eval `(,(intern (format "org-block/%s" blk))
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

(defvar org--header-args nil
  "Alist (name plist) where “:main-arg” is a special plist key.

  It serves a similar role to that of Org's src ‘header-args’.

  See doc of SET-BLOCK-HEADER-ARGS for more information.")

(defmacro org-set-block-header-args (blk &rest kvs)
  "Set default valuts for special block arguments.

This is similar to, and inspired by, Org-src block header-args.

Example src use:
    #+PROPERTY: header-args:Language :key value

Example block use:
    (set-block-header-args Block :main-arg mainvalue :key value)

A full, working, example can be seen by “C-h o RET defblock”.
"
  `(add-to-list 'org--header-args (list (quote ,blk) ,@kvs)))

;; This is our 𝒳, “remark”.
;; As a link, it should be shown angry-red;
;; it takes two arguments: “color” and “signoff”
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
;; the following as personalised defaults for the “remark” block.
;; OR, I'd like to set this for links, which do not have argument options.
(org-set-block-header-args rremark :main-arg "Jasim Jameson" :signoff "( Aim for success! )")

(cl-defmacro org--blockcall (blk &optional main-arg &rest keyword-args-then-contents)
  "An anaologue to `funcall` but for blocks.

Usage: (blockcall blk-name main-arg even-many:key-values raw-contents)

One should rarely use this directly; instead use
o-thread-blockcall.
"
  `(concat "#+end_export\n" (,(intern (format "org-block/%s" blk))
    backend ;; defblock internal
    ; (format "\n#+begin_export html\n\n%s\n#+end_export\n" ,(car (last keyword-args-then-contents))) ;; contents
    ,@(last keyword-args-then-contents) ;; contents
    ,main-arg
    ,@(-drop-last 1 keyword-args-then-contents)) "\n#+begin_export"))

(defmacro org-thread-blockcall (body &rest forms)
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

(org-defblock solution
  (title "Solution" reprimand "Did you actually try? Maybe see the ‘hints’ above!"
   really "Solution, for real")
  "Show the answers to a problem, but with a reprimand in case no attempt was made."
  (org-thread-blockcall raw-contents
                    (details really :title-color "red")
                    (box reprimand :background-color "blue")
                    (details title)))

(org-defblock org-demo (nil nil source "Source" result "Result"
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
                 (org--blockcall box source :background-color source-color)
                 org-export)
               ;; Separator
               sep
               ;; Result
               (thread-last raw-contents
                 (org--blockcall box result :background-color result-color)
                 org-export))]

   (if (equal style "parallel")
       (org--blockcall parallel "2" :bar nil text)
       (concat "#+end_export\n" text "\n#+begin_export"))))

(org-defblock stutter (reps 2)
  "Output the CONTENTS of the block REPS many times"
  (-let [num (if (numberp reps) reps (string-to-number reps))]
    (s-repeat num contents)))

(org-defblock rename (list "")
  "Perform the given LIST of substitutions on the text.
The LIST is a comma separated list of ‘to’ separated symbols.
In a link, no quotes are needed."
  (s-replace-all
   (--map (cons (car it) (cadr it))
          (--map (s-split " to " (s-trim it))
                 (s-split "," list)))
   contents))

(org-defblock spoiler (color "grey" left "((" right "))")
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

(defun org--list-to-math (lst)
  "Get a result LST from ORG-LIST-TO-LISP and render it as a proof tree."
  (cond
   ((symbolp lst) "")
   ((symbolp (car lst)) (org--list-to-math (cadr lst)))
   (t
    (-let* (((conclusion₀ children) lst)
            ((name named?) (s-split " :: " conclusion₀))
            (conclusion (or named? conclusion₀)))
      (if (not children)
          (if named? (format "\\frac{}{%s}[%s]" conclusion name) conclusion)
        (format "\\frac{\\displaystyle %s}{%s}%s"
                (s-join " \\qquad "
                        (mapcar #'org--list-to-math children))
                conclusion
                (if named? (format "[\\text{%s}]" name) "")))))))

(org-defblock tree (main-arg)
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
                                (org--list-to-math it))
                        (cdr (with-temp-buffer
                               (insert raw-contents)
                               (goto-char (point-min))
                               (org-list-to-lisp))))))

(defun osbe--block-fontifications ()
"Yields a cons list of block type and language pairs.

The intent is that the block types are fontified using the given language name."
    (--map (cons (symbol-name it) "org") (-cons* 'tiny 'center 'quote  org--supported-blocks)))

(defvar osbe--original-match-string (symbol-function 'match-string))

(cl-defun osbe--match-string (n &optional str)
          (let* ((block-type (string-remove-prefix "_" (funcall osbe--original-match-string 4 str)))
             (fontification (cdr (assoc block-type (osbe--block-fontifications)))))
        ;; (message "%s - %s -> %s" n block-type fontification) ;; For debugging.
        (if (and (equal n 7) fontification)
            fontification
          (funcall osbe--original-match-string n str))))

;; TODO: This should only be enabled when org-special-blocks-mode is enabled and otherwise should be removed.
(advice-add 'org-fontify-meta-lines-and-blocks
            :around (lambda (fontify &rest args)
                  (cl-letf (((symbol-function 'match-string) #'osbe--match-string))
                    (apply fontify args))))

(org-defblock details (title "Details"
              background-color "#e5f5e5" title-color "green")
  "Enclose contents in a folded up box, for HTML.

For LaTeX, this is just a boring, but centered, box.

By default, the TITLE of such blocks is “Details”,
its TITLE-COLOR is green, and BACKGROUND-COLOR is “#e5f5e5”.

In HTML, we show folded, details, regions with a nice greenish colour.

In the future ---i.e., when I have time---
it may be prudent to expose more aspects as arguments.
"
   (pcase backend
     (`latex (concat (pcase (substring background-color 0 1)
                       ("#" (format "\\definecolor{osbe-bg}{HTML}{%s}" (substring background-color 1)))
                       (_ (format "\\colorlet{osbe-bg}{%s}" background-color)))
                     (pcase (substring title-color 0 1)
                       ("#" (format "\\definecolor{osbe-fg}{HTML}{%s}" (substring title-color 1)))
                       (_ (format "\\colorlet{osbe-fg}{%s}" title-color)))
                     (format "\\begin{quote}
                              \\begin{tcolorbox}[colback=osbe-bg,colframe=osbe-fg,title={%s},sharp corners,boxrule=0.4pt]
                                   %s
                               \\end{tcolorbox}
                \\end{quote}" title contents)))
     (_ (format "<details class=\"code-details\"
                 style =\"padding: 1em;
                          background-color: %s;
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
               </details>" background-color title-color title contents))))


(org-defblock box (title "" background-color nil shadow nil frame-color nil title-background-color nil)
  "Enclose text in a box, possibly with a title.

By default, the box's COLOR is green for HTML and red for LaTeX,
and it has no TITLE.

SHADOW is an alternate style of boxing: It shows the contents in a centred
box with a shadow colour being the given non-nil value of SHADOW.
If SHADOW is a hexidecimal colour, it should be a string enclosed in double quotes.
More accurately, the following are all valid uses:

    #+begin_box title :shadow t
    #+begin_box title :shadow inset

    #+begin_box title :shadow \"pink\"
    #+begin_box title :shadow pink

    #+begin_box title :shadow (cyan pink orange yellow)
    #+begin_box title :shadow (cyan \"inset pink\" orange)
    #+begin_box title :shadow (:left cyan :right pink :deep-right orange :deep-left yellow)

Notice that prefixing colours with ‘inset’ causes the colour to be within the box,
rather than spread, with blur, around the box. The use of ‘inset’ swaps left and right.

The HTML export uses a padded div, whereas the LaTeX export
requires the tcolorbox package.

In the future, I will likely expose more arguments."

  (pcase backend
    (`latex
     (apply #'concat
            `("\\begin{tcolorbox}[title={" ,title "}"
              ",colback=" ,(pp-to-string (or background-color 'red!5!white))
              ",colframe=" ,(pp-to-string (or frame-color 'red!75!black))
              ",colbacktitle=" ,(pp-to-string (or title-background-color 'yellow!50!red))
              ",coltitle=red!25!black, fonttitle=\\bfseries,"
              "subtitle style={boxrule=0.4pt, colback=yellow!50!red!25!white}]"
              ,contents
              "\\end{tcolorbox}")))
    ;; CSS syntax: “box-shadow: specification, specification, ...”
    ;; where a specification is of the shape “[inset] x_offset y_offset [blur [spread]] color”.
    (_ (-let [haze (lambda (left right deep-right deep-left)
                     (format "width: 50%%; margin: auto; box-shadow: %s"
                             (thread-last (list (cons right      "8px 6px 13px 8px %s")
                                                (cons left       "-16px 12px 20px 16px %s")
                                                (cons deep-right "48px 36px 71px 28px %s")
                                                (cons deep-left  "-48px -20px 71px 28px %s"))
                               (--filter (car it))
                               (--map (format (cdr it) (car it)))
                               (s-join ","))))]
         (format "<div style=\"%s\"> <h3>%s</h3> %s </div>"
                 (s-join ";" `("padding: 1em"
                               ,(format "background-color: %s" (org-subtle-colors (format "%s" (or background-color "green"))))
                               "border-radius: 15px"
                               "font-size: 0.9em"
                               ,(when shadow
                                  (cond
                                   ((equal shadow t)
                                    (funcall haze "hsl(60, 100%, 50%)" "hsl(1, 100%, 50%)" "hsl(180, 100%, 50%)" nil))
                                   ((equal shadow 'inset)
                                    (funcall haze "inset hsl(60, 100%, 50%)" "inset hsl(1, 100%, 50%)" "inset hsl(180, 100%, 50%)" nil))
                                   ((or (stringp shadow) (symbolp shadow))
                                    (format "box-shadow: 10px 10px 20px 0px %s; width: 50%%; margin: auto" (pp-to-string shadow)))
                                   ((json-plist-p shadow)
                                    (-let [(&plist :left X :right Y :deep-right Z :deep-left W) shadow]
                                      (funcall haze X Y Z W)))
                                   (:otherwise (-let [(X Y Z W) shadow]
                                        (funcall haze X Y Z W)))))))
                 title contents)))))


(defun org-subtle-colors (c)
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


(org-defblock parallel (cols "2" bar nil)
  "Place ideas side-by-side, possibly with a separator.

There are COLS many columns, and they may be seperated by solid
vertical rules if BAR is a non-nil (colour) value.

+ COLS is either a number or a sequence of the shape: 10% 20% 30%.
+ BAR is either `t', `nil', or a colour such as `red' or `blue'.

----------------------------------------------------------------------

“Soft Columns”: Writing “#+begin_parallel 𝓃 :bar t” will produce
𝒏-many parallel columns, possibly separated by solid rules, or a
“bar”. This style allows text to freely move between columns,
depending on the size of the browser, which may dynamically
shrink and grow.  BAR can either be `t', `nil', or
any (backend)-valid colour specification; such as `red' or
`green'.

“Hard Columns”: Alternatively, for non-uniform column widths,
COLS may instead be a specification of the widths of the
columns. However, this extra flexibility comes at an additional
cost: The contents of the block must now contain 𝒏-1 lines
consisting of ‘#+columnbreak:’, when the specification determines
𝒏 columns, as shown in the following example.

   #+begin_parallel 20% 60% 20% :bar green
   Hello, to the left!

   #+columnbreak:
   A super duper wide middle margin!

   #+columnbreak:
   Goodbye (“God-be-with-ye”) to the right!
   #+end_parallel

The specification is 𝒏 measurements denoting widths; which may be
in any HTML recognisable units; e.g., “5em 20px 30%” is valid.
I personally advise only the use of percentage measurements.

In the Soft Columns style above, any ‘#+columnbreak:’ are merely ignored.
With LaTeX export, the use of ‘#+columnbreak:’ is used to request a column break."
  (let ((rule (pcase backend
               (`latex (if bar 2 0))
               (_  (format "%s %s" (if bar "solid" "none") (if (string= bar "t") "black" bar)))))
        (contents′ (s-replace "#+columnbreak:" "\\columnbreak" contents)))
    (pcase backend
      (`latex   (format  "\\par \\setlength{\\columnseprule}{%s pt}
          \\begin{minipage}[t]{\\linewidth}
          \\begin{multicols}{%s}
          %s
          \\end{multicols}\\end{minipage}"    rule cols contents′))
      (_ (if (not (s-contains-p "%" cols))
             (format "<div style=\"column-rule-style: %s;column-count: %s;\">%s</div>"
                     rule cols contents)
           ;; Otherwise: cols ≈ "10% 40% 50%", for example.
           (let ((spec (s-split " " (s-collapse-whitespace (s-trim cols))))
                 (columnBreak (lambda (width omit-rule?)
                                (format "<div style=\"width: %s; margin: 10px; border-right:4px %s; float:  left;\">" width
                                        (if omit-rule? "none" rule)))) )
             (format "<div style=\"display: flex; justify-content: space-between; align-items: flex-start;\">%s%s%s</div>"
                     (funcall columnBreak (pop spec) nil)
                     (s-replace-regexp (regexp-quote "#+columnbreak:")
                                       ;; ‘λ’ since we need the “pop” evaluated for each find-replace instance.
                                       ;; We use “not spec” to omit the rule separator when there is NOT anymore elements in SPEC.
                                       (lambda (_) (format "@@html:</div>%s@@" (funcall columnBreak (pop spec) (not spec))))
                                       contents)
		     (if (s-contains-p " " cols) "</div>" ""))))))))


(defvar org--html-export-style-choice "default"
  "This variable holds the link label declared by users.
  It is used in the hook to Org's reprocessing; `org--html-export-style-setup'.")

(defvar org-html-export-styles
      `((default . "")
        (bigblow . "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-bigblow.setup")
        (readtheorg . "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup")
        (rose . "#+HTML_HEAD: <link href=\"https://taopeng.me/org-notes-style/css/notes.css\" rel=\"stylesheet\" type=\"text/css\" />")
        (latexcss . "#+HTML_HEAD: <link rel=\"stylesheet\" href=\"https://latex.now.sh/style.min.css\" />"))
      "An alist of theme-to-setup pairs, symbols-to-strings, used by `org-link/html-export-style'.

  For live examples of many of the themes, see
  https://olmon.gitlab.io/org-themes/.

  In due time, I would like to add more, such as those linked from
  the discussion https://news.ycombinator.com/item?id=23130104.
  A nice, simple, opportunity for someone else to contribute.")
;;
;; Add a bunch more
(cl-loop for theme in '(comfy_inline imagine_light
                        rethink_inline simple_whiteblue
                        retro_dark simple_gray solarized_dark
                        solarized_light stylish_white)
         do (push (cons theme (format "#+SETUPFILE: https://gitlab.com/OlMon/org-themes/-/raw/master/src/%s/%s.theme" theme theme)) org-html-export-styles))

(defun org--html-export-style-setup (_backend)
  "Insert an HTML theme link setup, according to `org-html-export-styles'."
  (save-excursion
    (goto-char (point-min))
    (-> (or (assoc org--html-export-style-choice org-html-export-styles)
           (error  "Error: Unknown html-export-style ∷ %s ∉ '%s"
                   org--html-export-style-choice
                   (-cons* 'random 'default (mapcar 'car org-html-export-styles))))
       cdr
       (format "\n %s \n")
       insert)))

(org-deflink html-export-style
  "Add a dedicated style theme, from `org-html-export-styles'."
  [:face '(:underline "green")
   :keymap ("?" ;; Let use select new choice of style with the “?” key.
            (-let [choice (completing-read
                           "New HTML style: "
                           (cons "random" (--map (pp-to-string (car it)) org-html-export-styles)))]
              (save-excursion
                (beginning-of-line)
                (while (re-search-forward
                        (rx (seq "html-export-style:" (one-or-more word))) nil t)
                  (replace-match (concat "html-export-style:" choice))))))
   :help-echo (thread-last (cons "random" (--map (pp-to-string (car it)) org-html-export-styles))
                (-partition 4)
                (--map (s-join "     " it))
                (s-join "\n")
                (format "Press “?” to change the theme. \n\n Supported themes include:\n\n%s"))
   :let (whatdo (progn
                  (setq org--html-export-style-choice
                        (if (equal "random" o-label)
                            (seq-random-elt (mapcar 'car org-html-export-styles))
                          (intern o-label)))
                  (pcase o-label
                    ;; TODO: Move this to when the mode is enabled/disabled?
                    ("default" (remove-hook 'org-export-before-processing-hook
                                            'org--html-export-style-setup))
                    (_   (add-hook 'org-export-before-processing-hook
                                   'org--html-export-style-setup)))))
   ]
  ;; Result string, nothing.
  "")

(org-deflink fortune
   "Print an ASCII animal saying the given link's description, a fortune, or a joke.

This is essentially a wrapper around the following command-line incantation

    fortune | cowsay | lolcat -f

We show colourful sayings both in Emacs (when you click on the link) and in HTML export.

This requires you have the command-line packages ‘fortune’, ‘cowsay’, ‘lolcat’,
and ‘aha’ for converting coloured terminal output into HTML.
On MacOS, all of these can be installed with `brew install 𝒳';
better-yet use the Emacs Lisp `system-packages-install' package.

The help-echo, hover tooltip, provides useful information on possibly link types and their effects.

Example uses:

   fortune:joke    ;; Show a random animal saying a punny joke

   fortune:random  ;; Show a random animal saying a random fortune/phrase

   fortune:dragon  ;; Show a dragon saying a random fortune/phrase

   ;; Show a the given animal saying the given phrase
   [[fortune:mutilated][Opps, I broke the thing!]]

   ;; The ‘fortune’ link grew out of the following link in my work journal
   [[elisp:(dad-joke-get)][I wanna smile!]]

For HTML export, the resulting HTML element has class ‘org-fortune’,
to which users may adorn CSS styling. For instance, in an Org file:

  #+html: <style> .org-fortune {font-style: italic; font-family: Monaco} </style>
  # Chalkduster font-family is good for one-line sayings, phrases.

We intentionally do not fold-up such links when they have associated descriptions.

When you click, it takes a seconds to fetch jokes; so await a moment when hovering over joke fortunes."
   [:display 'full
    :face '(:box (:color "orange" :style released-button) :underline "green" :overline "green")
    :let (animals '(blowfish bud-frogs cower default dragon
                    dragon-and-cow flaming-sheep ghostbusters
                    moose mutilated sheep stegosaurus turkey
                    turtle tux)
          animal (if (string-equal "cow" (s-trim o-label))
                     'default
                   (seq-random-elt animals))
          animal₀ (if (equal 'default animal) 'cow animal)
          _loads (progn (require 'dad-joke) (require 'seq) (require 'lolcat))
          saying (cond
                  (o-description (format "echo %s" (pp-to-string o-description)))
                  ((equal "joke" (s-trim o-label)) (format "echo %s" (pp-to-string (dad-joke-get))))
                  (:otherwise "fortune"))
          result (format "%s\t\t\t%s"
                         (shell-command-to-string (format "%s | cowsay -f %s" saying animal))
                         animal₀)
          buf-name (format "fortune:%s" animal₀))
   :follow (progn (display-message-or-buffer result)
                  (ignore-errors (kill-buffer buf-name))
                  (switch-to-buffer-other-window "*Message*")
                  (rename-buffer buf-name)
                  (highlight-regexp (format "%s" animal₀) 'hi-green-b)
                  (lolcat-this-buffer)
                  (local-set-key "q" #'kill-buffer-and-window)
                  (message "“q” to kill buffer and window."))
   :help-echo (s-join "\n" (list "“fortune:𝒳” or “[[fortune:𝒳][description 𝒟]]” where 𝒳 is "
                                 "⇢ joke   ⟦A random animal that says a punny joke⟧"
                                 "⇢ random ⟦A random animal that says 𝒟, or a random fortune phrase⟧"
                                 "⇢ ⟦This animal says 𝒟, or a random fortune phrase⟧"
                                 "\t cow, blowfish, bud-frogs, cower, dragon, dragon-and-cow,"
                                 "\t flaming-sheep, ghostbusters, moose, mutilated, sheep,"
                                 "\t stegosaurus, turkey, turtle tux"
                                 "\n"
                                 "\n" result))
   ]
   (if (equal o-backend 'html)
       (--> (shell-command-to-string (format "%s | cowsay -f %s | lolcat -f | aha -n" saying animal))
          (if (s-starts-with? "/System/Library" it)
              (s-join "\n" (cdr (s-split "\n" it)))
            it)
          (format "%s\t\t\t%s" it animal₀)
          (format "<pre class=\"org-fortune\"> %s </pre>" it))
     result))

(defvar org-hide-editor-comments nil
  "Should editor comments be shown in the output or not.")

(org-defblock remark
      (editor "Editor Remark" color "black" signoff "" strong nil)
; :inline-please__see_margin_block_for_a_similar_incantation ; ⇒ crashes!
[:face '(:foreground "red" :weight bold)]
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
    (if org-hide-editor-comments
        ""
      (format (pcase backend
                ('latex (format "{\\color{%%s}%s %%s %%s %%s %%s}" (if strong "\\bfseries" "")))
                (_ (format "<%s style=\"color: %%s;\">%%s %%s %%s %%s</%s>" (if strong "strong" "p") (if strong "strong" "p"))))
              color edcomm-begin contents′ signoff edcomm-end))))

(org-link-set-parameters
 "edcomm"
  :follow (lambda (_))
  :export (lambda (label description backend)
            (org--edcomm
             backend
             (format ":ed:%s\n%s" label description)))
  :help-echo (lambda (_ __ position)
               (save-excursion
                 (goto-char position)
                 (-let [(&plist :path) (cadr (org-element-context))]
                   (format "%s made this remark" (s-upcase path)))))
  :face '(:foreground "red" :weight bold))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load support for 20 colour custom blocks and 20 colour link types

(defvar org--ospe-colors
  '(black blue brown cyan darkgray gray green lightgray lime
          magenta olive orange pink purple red teal violet white
          yellow)
  "Colours that should be available on all systems.")

(cl-loop for colour in org--ospe-colors
         do (eval `(org-defblock ,colour
                     (the-color "black")
                     (vector :face `(:foreground ,(format "%s" (quote ,colour))))
                     ,(format "Show text in %s color." colour)
                     (let ()
                       (format (pcase backend
                                 (`latex "\\begingroup\\color{%s}%s\\endgroup\\,")
                                 (_  "<span style=\"color:%s;\">%s</span>"))
                               (quote ,colour) contents)))))

(org-defblock color
  (color black)
  (vector :face (lambda (colour) `(:foreground ,(format "%s" colour))))
  "Format text according to a given COLOR, which is black by default."
  (format (pcase backend
            (`latex "\\begingroup\\color{%s}%s\\endgroup\\,")
            (`html  "<span style=\"color:%s;\">%s</span>"))
          color contents))

(org-defblock latex-definitions nil
  "Declare but do not display the CONTENTS according to the BACKEND."
  (format (pcase backend
            ('html "<p style=\"display:none\">\\[%s\\]</p>")
            (_ "%s"))
          raw-contents))

(org-deflink kbd
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
        ;; The style=⋯ is to remove the underlining caused by <abbr>.
        (format "<abbr class=\"tooltip\" style=\"border: none; text-decoration: none;\" title=\"%s ∷ %s<br>%s\">%s</abbr>"
                the-label (or lisp-func "") (org-ospe-html-export-preserving-whitespace tooltip)
                keystrokes)
      keystrokes)))

(defvar
 org--supported-octoicons
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

Usage: (cadr (assoc 'ICON org--supported-octoicons))")

(org-deflink octoicon
  "Show an OctoIcon: home, link, mail, report, tag, clock"
  [:help-echo "Show an OctoIcon: home, link, mail, report, tag, clock"]
  (unless (member (intern o-label) '(home link mail report tag clock))
    (error "octoicon:%s ⇒ This label is not supported!" o-label))
  (if (not (equal o-backend 'html))
      ""
    (s-collapse-whitespace
     (cadr (assoc (intern o-label)
                  org--supported-octoicons)))))

(org-deflink link-here
  "Export a link to the current location in an Org file."
  [:help-echo (format "This is a local anchor link named “%s”" path)]
  (if (not (equal o-backend 'html))
      ""
    (format (s-collapse-whitespace
     "<a class=\"anchor\" aria-hidden=\"true\" id=\"%s\"
          href=\"#%s\">%s</a>")
    o-label o-label (cadr (assoc 'link
                             org--supported-octoicons)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The badge link types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defmacro org-make-badge
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

     (org-make-badge \"go\")

Then the following exports nicely from an Org file:

     go:key|value|blue|here|gnu-emacs"
  `(org-deflink ,(intern name)
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

    ;; :export #'org--link--badge
    (if (equal o-backend 'latex) ""
      (-let [ (key value color url logo)  (mapcar #'s-trim (s-split "|" o-label)) ]
        (format
         (pcase ,(if social-shields-name `(format ,social-url o-label) 'url)
           ("here" (format "<a id=\"%s\" href=\"#%s\">%%s</a>" (s-replace "%" "%%" key) (s-replace "%" "%%" key)))
           (""      "%s") ;; e.g., badge:key|value|color||logo
           ('nil    "%s") ;; e.g., badge:key|value|color
           (_ (format "<a href=\"%s\">%%s</a>"
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

(org-make-badge "badge")

;; Since we're invoking a macro, the twitter-excitement is used lazily; i.e.,
;; consulted when needed instead of being evaluated once.
(defvar org-link-twitter-excitement
  "This looks super neat (•̀ᴗ•́)و:"
  "The string prefixing the URL being shared.")

(org-make-badge
 "tweet"
 "twitter/url?=url="
 (format
   "https://twitter.com/intent/tweet?text=%s:&url=%%s"
   org-link-twitter-excitement)
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
         do (eval `(org-make-badge ,name′ ,social ,url)))

(defvar org--docs nil
  "An alist of (LABEL NAME DESCRIPTION) entries; our glossary.

Example setter:
0. (org-docs-set \"os\" \"Emacs\" \"A place wherein I do all of my computing.\")

Example getters:
0. (org-docs-get LABEL)
1. (-let [(name description) (cdr (assoc LABEL o--docs))] ⋯)

See also `org--docs-from-libraries' and `org-docs-load-libraries'.")

(cl-defun org-docs-set (label name description)
  "Add a new documentation-glossary entry, if it is not already present.

We associate LABEL to have title NAME and glossary value DESCRIPTION.

Example usage:
  (org-docs-set \"cat\"
              \"Category Theory\"
              \"A theory of typed  composition; e.g., typed monoids.\")"
  (add-to-list 'org--docs (list label name description)))

(cl-defun org-docs-get (label)
  "Return the name and documentation-glossary values associated with LABEL.

It returns a list of length 2.

Example uses:

  ;; Get the Lisp documentation of `thread-last'
  (org-docs-get \"thread-last\")

  ;; Get the English definition of ‘computing’
  (org-docs-get \"computing\")

We look for LABEL from within the current buffer first, using `org--docs',
and otherwise look among the loaded libraries, using `org--docs-from-libraries',
and, finally, look for the documentation entry using `org-docs-fallback'."
  (cdr (or (assoc label org--docs)
           (assoc label org--docs-from-libraries)
           (funcall org-docs-fallback label)
           (error "Error: No documentation-glossary entry for “%s”!" label))))

(cl-defun org-docs-insert ()
  "Insert a “doc:𝒳” link from user's documentation-glossary database.

It can be tricky to remember what you have, or what documentation entries mention, and so
this command gives a searchable way to insert doc links."
  (interactive)
  (thread-last
      (cl-remove-duplicates (-concat org--docs org--docs-from-libraries)
                            :test (lambda (x y) (cl-equalp (car x) (car y))))
    (--map (format "%s ∷ %s" (car it) (cl-third it)))
    (completing-read "Insert doc link ∷ ")
    (s-split "∷")
    car
    (concat "doc:")
    (insert)))

(defvar org-docs-fallback
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
                 (if (and (s-blank-p it) (not org-export-with-broken-links))
                     (error "Error: No documentation-glossary entry for “%s”!" label)
                   it)))))

  "The fallback method to retriving documentation or glossary entries.

We try to retrive the Emacs Lisp function documentation of the
given LABEL, if possible, otherwise we try to retrive the Emacs
Lisp variable documentation, and if that fails then we look up
the word in the English dictionary.

The English definition is obtained from the command line tool ‘wn’, WordNet.")

(defvar org-docs-libraries nil
  "List of Org files that have ‘#+begin_documentation’ blocks that should be loaded
   for use with the ‘doc:𝒳’ link type.")

(cl-defun org-docs-load-libraries
    (&optional (libs org-docs-libraries))
"Load documentation-glossary libraries LIBS.

If no LIBS are provided, simply use those declared
org-docs-libraries.

See `org-docs-from-libraries'."
(interactive)
(cl-loop for lib in libs
      do (with-temp-buffer
           (insert-file-contents lib)
           ;; doc only activates after an export
           (-let [org-export-with-broken-links t] (org-html-export-as-html))
           (kill-buffer)
           (delete-window)
           (setq org--docs-from-libraries (-concat org--docs org--docs-from-libraries))
           (setq org--docs nil))))

(defvar org--docs-from-libraries nil

  "The alist of (label name description) entries loaded from the libraries.

The ‘doc:𝒳’ link will load the libraries, possibly setting this variable to ‘nil’,
then make use of this variable when looking for documentation strings.

Interactively call `org-docs-load-libraries'
to force your documentation libraries to be reloaded.

See also `org-docs-libraries'.")

(defvar org--docs-actually-used nil
  "Which words are actually cited in the current article.

We use this listing to actually print a glossary using
‘show:GLOSSARY’.")

(org-deflink doc
 "Export O-LABEL as itself, or as the provided O-DESCRIPTION,
 along with a tooltip that shows the user's
 documentation-glossary for o-LABEL and using that entry's name
 when no O-DESCRIPTION is provided."
 [:let (entry (org-docs-get o-label)
        name (cl-first entry)
        docs (cl-second entry)
        display-name (or o-description name))
  :help-echo (format "[%s] %s :: %s" o-label name docs)
  :face '(custom-button)]
   (add-to-list 'org--docs-actually-used (list o-label name docs))
   (pcase o-backend
     (`html  (format "<abbr class=\"tooltip\" title=\"%s\">%s</abbr>"
                     (org-ospe-html-export-preserving-whitespace docs)
                     display-name))
     ;; Make the current word refer to its glosary entry;
     ;; also declare the location that the glossary should refer back to.
     (`latex (format (concat "\\hyperref"
                             "[o-glossary-%s]{%s}"
                             "\\label{o-glossary"
                             "-declaration-site-%s}")
                     label display-name label))))

;; WHERE ...

(defun org-ospe-html-export-preserving-whitespace (s)
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

(org-defblock documentation
  (name (error "Documentation block: Name must be provided")
       label nil show nil color "green")
  "Register the dictionary entries in CONTENTS to the dictionary variable.

The dictionary variable is ‘org--docs’.

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
        do  (add-to-list 'org--docs
                         (mapcar #'s-trim (list (format "%s" l) name (substring-no-properties raw-contents)))))
  ;; Should the special block show something upon export?
  (if show (org--blockcall box name :background-color color raw-contents) ""))

(org-deflink show
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
                     (pp-to-string (mapcar #'cl-second org--docs-actually-used))
                   (pp-to-string (eval (car (read-from-string o-label)))))
         o-expr (if (equal o-label "GLOSSARY")
                    (concat "GLOSSARY ---i.e., org--docs-actually-used"
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
                           (org-ospe-html-export-preserving-whitespace (cl-third it))
                           (cl-second it))
                   ;; Ignore duplicates; i.e., entries with the same name/title.
                   (cl-remove-duplicates org--docs-actually-used
                                         :test (lambda (x y) (cl-equalp (cl-second x) (cl-second y)))))))
       (:otherwise (s-join "\n\n"
                  (cl-loop for (label name doc)
                           in org--docs-actually-used
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

(org-defblock margin
  (marker nil
  color "gray!80"
          counter "footnote"
          width "\\paperwidth - \\textwidth - \\oddsidemargin - 1in - 3ex")
          ;; Width: https://tex.stackexchange.com/a/101861/69371
  (vector :display 'full
          :face '(:foreground "grey" :weight bold :underline "orange" :overline "orange"))
  "Produce an HTML tooltip or a LaTeX margin note.

The ‘margin’ block is intended for “one-off” (mostly optional) remarks.

It is essentially “inline footnotes”.

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
      (_ (format "<abbr class=\"tooltip\" title=\"%s\">%s</abbr>&emsp13;"
                 (org-ospe-html-export-preserving-whitespace contents)
                 ; MA: FIXME: (org-export-string-as contents 'html :body-only-please)
                 marker)))))


(org-defblock tooltip (marker "°")
 "Produce an HTML tooltip."
 (format "<abbr class=\"tooltip\" title=\"%s\">%s</abbr>&emsp13;"
         (org-ospe-html-export-preserving-whitespace contents)
         marker))


(defun org--list-to-calc (lst rel hint-format NL-length color)
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
   ((symbolp (car lst)) (org--list-to-calc (cadr lst)))
   (:otherwise (-let* (((conclusion₀ children) lst)
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
                                   (--map (format "%s" (org--list-to-calc it rel hint-format NL-length color))
                                          children))))
                  expr))))))

(org-defblock calc
  (main-arg nil
  rel "=" hint-format "\\left[ %s \\right." explicit-vspace 2 color "maroon")
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
    (--map (format "%s" (org--list-to-calc it rel hint-format explicit-vspace color)))
    (s-join "\\\\")
    (format "$$\\begin{align*} & %s \n\\end{align*}$$")))

(setq my/theorem-counter 0)

(org-defblock theorem (title)
  "Show block contents prefixed with “Theorem 𝒏”, where the 𝒏umbering automatically increments."
  (format "<div class=\"theorem\"><b>Theorem %s%s.</b>&nbsp;%s</div>"
          (cl-incf my/theorem-counter)
          (if title (format " [“%s”]" title) "")
          (org-parse raw-contents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'org-special-block-extras)

;;; org-special-block-extras.el ends here
