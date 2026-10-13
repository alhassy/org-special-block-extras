;;; org-deflink.el --- org-deflink macro, keymap, and nav helpers  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 4.1.1
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "27.1") (org "9.1") (lf "1.0") (seq "2.0"))
;; Keywords: org, links, convenience
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

;; The `org-deflink' macro for defining new Org link types, along with the
;; parent keymap `org-special-block-extras-mode-map', nav helpers
;; (`org-link-at-point', `org-this-link-next', `org-this-link-previous',
;; `org-this-link-show-docs'), and the default C-n/C-p/C-h bindings.
;;
;; Full documentation at https://alhassy.github.io/org-special-block-extras

;;; Code:

(require 's)
(require 'dash)
(require 'cl-lib)
(require 'org)
(require 'ox)
(require 'lf)

(defun org--base-backend (backend)
  "BACKEND ↦ `html' or `latex' when it is, or derives from, one of them; else BACKEND.

Block and link bodies compare (equal backend 'html); normalising once, here,
lets a derived backend (say, a blog's `org-export-define-derived-backend' of
`html') get the HTML rendering without every body learning about derivation."
  (cond ((org-export-derived-backend-p backend 'html) 'html)
        ((org-export-derived-backend-p backend 'latex) 'latex)
        (t backend)))

;;;;; org-deflink

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
      ;; Account for cursour being on anywhere on the links "name:key".
      (backward-word 2)
      (unless (= working-line (line-number-at-pos))
  (goto-line working-line))
      (let* ((here-to-eol (buffer-substring-no-properties (point) (point-at-eol)))
             ;; E.g., "kbd:", the name part of an Org link
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

                     Press 'q' to kill the resulting buffer and window."
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
    (rename-buffer (format "Help: Org Link \"%s\"" link))
    (read-only-mode)
    (local-set-key "q" #'kill-buffer-and-window)
    (message "Read-only; \"q\" to kill buffer and window.")))

(define-key org-special-block-extras-mode-map (kbd "C-n") #'org-this-link-next)
(define-key org-special-block-extras-mode-map (kbd "C-p") #'org-this-link-previous)
(define-key org-special-block-extras-mode-map (kbd "C-h") #'org-this-link-show-docs)

(cl-defmacro org-deflink
    (name &optional docstring display &rest body)
  "Make a new Org-link NAME that exports using form BODY.

Since Org links are essentially string-valued functions,
a function 'org-link/NAME' is created.

DOCSTRING is optional; it is visible with
   (documentation 'org-link/NAME)

BODY is a string-valued expression, that may make use of the names
o-label, o-description, o-backend. The final one refers to the export
backend, such as 'html or 'latex. The first two are obtained from uses:

   [[name:o-label][o-description]]

In particular, the use case \"name:o-label\" means that o-description is nil.

---------------------------------------------------------------------------

Example use:

   ;; In a Lisp buffer, press \"C-x C-e\" to load this definition
   (org-deflink shout (upcase (or o-description o-label)))

   ;; In an Org-buffer, press \"C-c C-e h o\" to see how this exports
   <shout: hello world!>

   ;; Or using the bracket format
   [[shout:][hello world!]]
   [[shout: hello world!]]

   ;; or using the plain format
   shout:hello_world

Here is a more complex, involved, example that makes use of
':let' for local declarations. For instance, \"define:hello\"
renders as the word \"hello\" with a tooltip defining the word; the
definition is obtained from the command line tool 'wn'.

  (org-deflink define
    \"Define the given word using WordNet, along with synonyms and coordinate terms.\"
    [:let (definition (shell-command-to-string (format \"wn %s -over -synsn -coorn\" o-label)))
     :help-echo definition]
    (--> definition
      (s-replace-regexp \"\\\\\\\"\" \"''\" it) ;; The presence of '\\\"' in tooltips breaks things, so omit them.
      (s-replace-regexp \"\\n\" \"<br>\" it)
      (format \"<abbr class=\\\"tooltip\\\" title=\\\"%s\\\">%s</abbr>\" it o-label)))

For HTML tooltips, see `org-ospe-html-export-preserving-whitespace'.

More generally, org-special-block-extra's \"doc\" link type
supports, in order of precedence: User definitions, Emacs Lisp
documentation of functions & variables, and definitions of
English words. For example, \"doc:existential_angst\" for an entry
'existential_angst' whose associated documentation-glossary is
user-defined in a '#+documentation' Org-block, or
\"doc:thread-first\" for the Emacs Lisp documentation of the
function `thread-first', or \"doc:user-mail-address\" for the Emacs
Lisp documentation of the variable `user-mail-address', or
\"doc:hello\" for the definition of the English word 'hello'.

DISPLAY is a vector consisting of key-value pairs that affects how the link
is displayed in Emacs Org buffers. The keys are as follows.

+ :help-echo is a string-valued expression of the tooltip that should accompany
  the new link in Org buffers. It has access to o-format being one of 'plain',
  'angle', 'bracket' which indicates the format of the link, as shown above.
  It also has access to o-label and o-description.

  By default, the tooltip is the link name followed by the documentation
  of the link, and, finally, the HTML export of the link.
  That way, upon hover, users can visually see the link contents,
  know what/how the link exports, and actually see the HTML export.

  That is to say, for the 'shout' example above, the default display is essentially:
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
  folded away in Org buffers; i.e., \"[[X][Y]]\" does not render as just \"Y\".

+ :follow is a form that is executed when you click on such links; e.g., to open
   another buffer, browser, or other action. It makes use of (an implicit argument) 'o-label'.
   Be aware that 'o-label' is a string that may contain spaces; e.g., when the action is to open
   a URL in a browser.

   If you are in need of providing similar, related, actions on a single link
   then your :follow can condition on the current prefix argument via 'o-prefix'
   (which is essentially `current-prefix-arg').
   For instance, a user presses \"C-u RET\" on your link to do one thing
   but \"C-u 72 RET\" to do another action.

+ :keymap is an alternating list of keys and actions to be
  performed when those keys are pressed while point is on the link.
  For example:
      [:keymap (C-h (message-box \"hola\"))]

  By default, C-n and C-p are for moving to next and previous occruances of the same link type.

+ :let is a list of alternating variable symbol name and value, which are then used to form
  a concrete `let*' clause. This is useful for introducing local variables for use in the DISPLAY
  as well as in the CONTENTS. Such local declarations may make use of O-LABEL and O-DESCRIPTION, as usual.

The preferred syntax supplies all keys — including :export for the export body — as a
flat top-level plist with no enclosing vector:

  (org-deflink shout
    \"Shout the label.\"
    :face \\='(:weight bold)
    :export (upcase o-label))"
  (cl-destructuring-bind (docstring display body)
      (lf-extract-optionals-from-rest docstring #'stringp
                               display   #'vectorp
                                      body)
    ;; Flat plist style: no vector supplied; body is (KEY VAL … :export FORM).
    ;; Split :export out as the body; remaining keys become the display plist.
    (when (and (null display) (keywordp (car body)))
      (let* ((all    body)
             (export (cl-getf all :export))
             (dkeys  (cl-loop for (k v) on all by #'cddr
                              unless (eq k :export)
                              append (list k v))))
        (setq display dkeys
              body    (when export (list export)))))
    ;; DEPRECATED: vector style ([:display … :face … :let … :follow …] BODY…).
    ;; Supported for now; will be removed in a future release. Use the flat
    ;; plist style with :export instead.
    (setq display (if (vectorp display) (seq--into-list display) display))
    (let ((org-link/NAME (intern (format "org-link/%s" name)))
          (navigation "Press \"C-h\" to see possible actions on this link type.")
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
           (let ((o-backend (org--base-backend o-backend)))
             (let* ,lets ,@body)))
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
          ;; i.e., "[[example:lable][description]]" will always appear verbatim
          ;; and not hide the first pair […].
          :display (cl-the symbol ,(cl-getf display :display)) ;; e.g.,: 'full
          ;; Any special keybindings when cursour is on this link type?
          ;; On 'NAME:' links, C-n/p to go to the next/previous such links.
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

(provide 'org-deflink)
;;; org-deflink.el ends here
