;;; org-deflink.el --- org-deflink macro, keymap, and nav helpers  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.16
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "27.1") (org "9.6") (lf "26.10.1") (seq "2.0"))
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

(defun org-export-markdown-p ()
  "↦ non-nil while exporting to Markdown; i.e., the export's backend is, or derives from, `md' ---so `gfm' too.
Since `md' derives from `html', block and link bodies see `backend' as `html' then; see `org--base-backend'."
  (and org-export-current-backend (org-export-derived-backend-p org-export-current-backend 'md)))

(defun org--markdown-alert (type title body)
  "(TYPE, TITLE, BODY) ↦ Org quote that ox-md renders as GitHub's alert “> [!TYPE]”, opening with TITLE in bold, if any.
The marker is a paragraph of its own, lest ox-gfm, which unfills paragraphs, join it to the next line."
  (format "#+begin_quote\n[!%s]\n\n%s%s\n#+end_quote" (upcase type) (if title (format "*%s*\n\n" title) "") body))

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

(defun org--link-at-point ()
  "The Org link element at point; else nil."
  (org-element-lineage (org-element-context) '(link) t))

(defun org-link-at-point ()
  "The type of the Org link at point, with suffix colon; e.g., \"kbd:\"; else nil."
  (interactive)
  (-some--> (org--link-at-point) (concat (org-element-property :type it) ":")))

(defun org--this-link-move (search edge)
  "Move, by SEARCH, to another Org link of the same type as the one at point, starting from its EDGE.
SEARCH is `re-search-forward' or `re-search-backward'; EDGE is :end or :begin.  The type is matched as
a whole symbol; e.g., \"kbd:\" but not \"xkbd:\"."
  (let ((link (or (org--link-at-point) (user-error "Not on an Org link"))))
    (goto-char (org-element-property edge link))
    (funcall search (concat "\\_<" (regexp-quote (org-element-property :type link)) ":") nil t)))

(defun org-this-link-next ()
  "Go to the next Org link that is similar to the link at point."
  (interactive)
  (org--this-link-move #'re-search-forward :end))

(defun org-this-link-previous ()
  "Go to the previous Org link that is similar to the link at point."
  (interactive)
  (org--this-link-move #'re-search-backward :begin))

(defun org-this-link-show-docs ()
  "Show documentation for the Org link at point in a help buffer; press ‘q’ to quit it."
  (interactive)
  (let ((link (s-chop-suffix ":" (or (org-link-at-point) (user-error "Not on an Org link")))))
    (with-help-window (format "*Help: Org Link “%s”*" link)
      (princ (concat
              (documentation (intern (format "org-link/%s" link)))
              "\nKEY BINDINGS:\n"
              "\nUnless indicated below otherwise..."
              "\n\tC-h: Shows this helpful message buffer"
              "\n\tC-n/C-p on the link to jump to next/previous links of this type;"
              "\n\tC-c C-x C-n/p for moving between arbitrary link types.\n\n"
              (pp-to-string (cdr (assoc link org-special-block-extras-mode-map--link-keymap-docs))))))))

(define-key org-special-block-extras-mode-map (kbd "C-n") #'org-this-link-next)
(define-key org-special-block-extras-mode-map (kbd "C-p") #'org-this-link-previous)
(define-key org-special-block-extras-mode-map (kbd "C-h") #'org-this-link-show-docs)

(defun org--deflink-arglist-p (form rest)
  "Non-nil iff FORM, followed by the forms REST, is an `org-deflink' arglist such as (label &optional description).

An arglist is a list of at most 3 plain symbols ---besides `&optional'--- or a list mentioning `&optional';
a lone form, or one whose head is a function, is the link's body instead; e.g., (upcase o-label)."
  (and (consp form) rest (proper-list-p form)
       (or (memq '&optional form)
           (and (<= (length form) 3)
                (cl-every (lambda (it) (or (consp it)
                                           (and (symbolp it) it (not (eq it t)) (not (keywordp it)) (not (fboundp it)))))
                          form)))))

(defun org--deflink-mentions-p (form names)
  "Non-nil iff FORM mentions any of the symbols NAMES, anywhere within it."
  (cond ((symbolp form) (memq form names))
        ((consp form) (or (org--deflink-mentions-p (car form) names) (org--deflink-mentions-p (cdr form) names)))
        ((vectorp form) (cl-some (lambda (it) (org--deflink-mentions-p it names)) form))))

(defun org--deflink-parse-arglist (arglist)
  "ARGLIST ↦ ((NAME ANAPHOR DEFAULT) …), pairing the names of (label &optional description backend) with o-label, o-description, o-backend.

  (label &optional (description \"…\"))  ↦  ((label o-label nil) (description o-description \"…\"))"
  (cl-loop for arg in (remq '&optional arglist)
           for anaphor in '(o-label o-description o-backend)
           collect (list (if (consp arg) (car arg) arg) anaphor (when (consp arg) (cadr arg)))))

(cl-defmacro org-deflink
    (name &optional docstring display &rest body)
  "Make a new Org-link NAME that exports using form BODY.

An optional ARGLIST may come first, naming the link's label, description,
and export backend ---in that order--- for use in BODY and DISPLAY:

   (org-deflink shout (label &optional description)
     \"Shout the description, else the label.\"
     :face \='(:weight bold)
     :export (upcase (or description label)))

As a convenience, ARGLIST may be omitted; then BODY and DISPLAY refer to the
anaphoric names o-label, o-description, and o-backend, which are always
available.  An argument written (NAME DEFAULT) falls back to DEFAULT when
absent; e.g., (label &optional (description \"…\")).

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

  (org-deflink define (word)
    \"Define the given word using WordNet, along with synonyms and coordinate terms.\"
    :let (definition (shell-command-to-string (format \"wn %s -over -synsn -coorn\" word)))
    :help-echo definition
    :export (org--html-tooltip definition word :plain))

See `org--html-tooltip' for HTML tooltips: their text is Org, or, as here, plain.

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

Besides :export, the following keys affect how the link is displayed in Emacs
Org buffers.  They are given alongside :export, as above; in the deprecated
style, they form a vector DISPLAY between the docstring and BODY, such as
[:help-echo (format \"%s\" o-label)].

+ :help-echo is a string-valued expression of the tooltip that should accompany
  the new link in Org buffers. It has access to o-format being one of ‘plain’,
  ‘angle’, ‘bracket’ which indicates the format of the link, as shown above.
  It also has access to o-label and o-description.

  By default, the tooltip is the link name followed by the documentation
  of the link, and, finally, the HTML export of the link.
  That way, upon hover, users can visually see the link contents,
  know what/how the link exports, and actually see the HTML export.

  That is to say, for the ‘shout’ example above, the default display is essentially:
  :help-echo (org-link/shout o-label o-description 'html)

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

+ :display 'full if you do not want bracket links to be
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
      :keymap (C-h (message-box \"hola\"))

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
  ;; An optional leading ARGLIST names the anaphors; see `org--deflink-arglist-p'.
  (let (arglist)
    (when (org--deflink-arglist-p docstring (or display body))
      (setq arglist   docstring
            docstring display
            display   (car body)
            body      (cdr body)))
    (cl-destructuring-bind (docstring display body)
        (lf-extract-optionals-from-rest docstring #'stringp display #'vectorp body)
      ;; Flat plist style: no vector supplied; body is (KEY VAL … :export FORM).
      ;; Split :export out as the body; remaining keys become the display plist.
      (when (and (null display) (keywordp (car body)))
        (setq display (org-plist-delete (copy-sequence body) :export)
              body    (-some-> (plist-get body :export) list)))
      ;; DEPRECATED: vector style ([:display … :face … :let … :follow …] BODY…).
      ;; Supported for now; will be removed in a future release. Use the flat
      ;; plist style with :export instead.
      (setq display (append display nil))
      (let* ((org-link/NAME (intern (format "org-link/%s" name)))
             (navigation "Press “C-h” to see possible actions on this link type.")
             ;; The ARGLIST's names, each paired with the anaphor it names; e.g., (o-label label o-label).
             (arg-lets (cl-loop for (arg anaphor default) in (org--deflink-parse-arglist arglist)
                                collect (list anaphor arg (if default `(or ,anaphor ,default) anaphor))))
             ;; Names for the label and description, usable everywhere o-label is; then the user's :let.
             (lets (append (mapcar #'cdr (--remove (eq (car it) 'o-backend) arg-lets))
                           (cl-loop for (variable value) on (plist-get display :let) by #'cddr
                                    collect (list variable value))))
             ;; The name for the backend exists only where o-backend does: the export body.
             (backend-lets (mapcar #'cdr (--filter (eq (car it) 'o-backend) arg-lets)))
             (keymap (plist-get display :keymap))
             (help-echo-default
              `(format "%s:%s\n\n%s\nHTML Export:\n\n%s"
                       ,(symbol-name name)
                       (or o-description o-label)
                       ,(concat (or docstring "") "\n\n" navigation "\n")
                       (,org-link/NAME o-label o-description 'html))))
        (cl-flet ((scoped (form)
                    ;; FORM, seeing the :let names when it mentions them; so a constant :face, say, costs no :let
                    ;; on each redisplay.  The :let names come as a whole, since each may refer to the ones before.
                    (if (org--deflink-mentions-p form (mapcar #'car lets)) `(let* ,lets ,form) form)))
          `(progn
             ;; The underlying function, of label, description, and backend.
             (defun ,org-link/NAME (o-label o-description o-backend)
               ,docstring
               (let* ((o-backend (org--base-backend o-backend)) ,@backend-lets ,@lets)
                 ,@body))
             ;; Which bindings this link type adds, for `org-this-link-show-docs'.
             ,@(when keymap
                 `((setf (alist-get ,(symbol-name name) org-special-block-extras-mode-map--link-keymap-docs nil nil #'equal)
                         ',keymap)))
             (org-link-set-parameters
              ,(symbol-name name)
              :export #',org-link/NAME
              ;; How these links look, and what clicking them does, within Emacs; there is no description here.
              :face (lambda (o-label) (let ((o-description nil)) ,(scoped (plist-get display :face))))
              :follow (lambda (o-label o-prefix) (let ((o-description nil)) ,(scoped (plist-get display :follow))))
              ;; E.g., 'full: such links are never folded in descriptive display; i.e.,
              ;; “[[example:label][description]]” appears verbatim.
              :display ,(plist-get display :display)
              ;; On NAME: links, C-n/C-p go to the next/previous such links; see `org-special-block-extras-mode-map'.
              :keymap (let ((o-keymap (copy-keymap org-mouse-map)))
                        (set-keymap-parent o-keymap org-special-block-extras-mode-map)
                        ,@(cl-loop for (key action) on keymap by #'cddr
                                   collect `(define-key o-keymap (kbd ,(format "%s" key)) (lambda () (interactive) ,action)))
                        o-keymap)
              ;; The tooltip alongside a link.
              :help-echo (lambda (_window _object position)
                           (save-excursion
                             (goto-char position)
                             (let* ((link (org--link-at-point))
                                    (o-format (org-element-property :format link))
                                    (o-label (org-element-property :path link))
                                    (o-description (and (eq o-format 'bracket)
                                                        (org-element-property :contents-begin link)
                                                        (buffer-substring-no-properties
                                                         (org-element-property :contents-begin link)
                                                         (org-element-property :contents-end link)))))
                               (ignore o-format)
                               ,(if (plist-get display :help-echo)
                                    `(or ,(scoped (plist-get display :help-echo)) ,help-echo-default)
                                  help-echo-default)))))
             ;; Return value is the name of the underlying function, as with `defun'.
             ',org-link/NAME))))))

(provide 'org-deflink)
;;; org-deflink.el ends here
