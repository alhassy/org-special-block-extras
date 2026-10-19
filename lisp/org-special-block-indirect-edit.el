;;; org-special-block-indirect-edit.el --- C-c ' dispatch for org special blocks  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Provides a generic C-c ' (org-edit-special) dispatch mechanism for org
;; special blocks, atop `org-special-block-at-point', and a macro used by
;; `org-defblock' to auto-generate per-block edit handlers from the
;; :edit-indirect-mode keyword.
;;
;; Manual registration:
;;
;;   (org-defblock-add-edit-special-handler "myblock" #'my-edit-handler)
;;
;; Auto-generation via org-defblock:
;;
;;   (org-defblock myblock (&optional main-arg :edit-indirect-mode some-mode) ...)
;;
;; The handler is called with no arguments when C-c ' is pressed inside the
;; named block and must return non-nil to claim the event.
;;
;; Any other special block, of ours or not, is edited in an `org-mode' buffer,
;; wherever Org itself knows no special environment; see `org-special-block-edit'.

;;; Code:

(require 'org)
(require 'org-special-block-at-point)

(defvar org--block-fontifications)

(defvar org-special-block-edit-functions nil
  "Functions called with a block's NAME in its C-c ' buffer, once its mode is set; e.g., the math block's
`org--math-edit-setup', by which TAB folds its hints there too.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Runtime dispatch

(defvar org--block-edit-special-handlers nil
  "Alist of (block-name-string . handler-function) for C-c ' dispatch.
Use `org-defblock-add-edit-special-handler' to register handlers.")

(defun org-defblock-add-edit-special-handler (block-name handler)
  "Register HANDLER as the C-c ' action for blocks named BLOCK-NAME (string).
HANDLER is called with no arguments when point is inside the named block
and should return non-nil to claim the event."
  (setf (alist-get (downcase block-name) org--block-edit-special-handlers nil nil #'equal) handler))

(defun org--block-edit-special-dispatcher (&optional _arg)
  "Dispatch C-c ' to the registered handler for the special block at point."
  (org--special-block-dispatch org--block-edit-special-handlers))

(advice-add 'org-edit-special :before-until #'org--block-edit-special-dispatcher)

(defun org-special-block-edit (&optional mode)
  "Edit the contents of the special block at point in a buffer of MODE, `org-mode' by default; C-c ' writes them back."
  (require 'org-src)
  ;; An empty block has no contents for `org-src--edit-element' to edit, so we give it a blank line.
  (let ((element (org-special-block-at-point)))
    (unless (org-element-property :contents-begin element)
      (save-excursion (goto-char (org-element-property :post-affiliated element)) (end-of-line) (insert "\n"))))
  (let* ((element (org-special-block-at-point))
         (name (org-element-property :type element)))
    (org-src--edit-element
     element
     (org-src--construct-edit-buffer-name (buffer-name) name)
     (lambda ()
       (funcall (or mode #'org-mode))
       (org--block-fontify-edit-buffer name)
       (run-hook-with-args 'org-special-block-edit-functions name)
       (setq-local header-line-format
                   (substitute-command-keys (format "%s block ---\\[org-edit-src-exit] to apply & exit." name))))
     t)
    t))

(defun org--block-fontify-edit-buffer (name)
  "Show this edit buffer's text as the NAME block's :fontify :keywords show it in Org buffers; see `org--fontify-blocks'.
A keyword's function face is called for the face, as there."
  (-when-let (keywords (plist-get (cdr (assoc (downcase name) org--block-fontifications)) :keywords))
    (font-lock-add-keywords
     nil (--map (-let [(matcher group face) it]
                  (list matcher (list group (if (functionp face) (list face) (list 'quote face)) 'prepend)))
                keywords)
     'append)))

(defun org--special-block-edit-fallback (edit-special &rest args)
  "Call EDIT-SPECIAL with ARGS; where Org knows no special environment, yet point is in a special block, edit its contents.
So Org keeps all it does ---a table, a link, a src block, even within a special block--- and every other block is editable."
  (condition-case err
      (apply edit-special args)
    (user-error
     (if (and (equal (cadr err) "No special environment to edit here") (org-special-block-at-point))
         (org-special-block-edit)
       (signal (car err) (cdr err))))))

(advice-add 'org-edit-special :around #'org--special-block-edit-fallback)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macro-expansion helper (used by org-defblock)

(defmacro org-defblock-setup-indirect-edit (name edit-mode)
  "Emit defun + registration for the C-c ' handler of block NAME.
NAME is a symbol (the block name); EDIT-MODE is a mode-function symbol.
Expands to nothing when EDIT-MODE is nil."
  (declare (indent defun))
  (when edit-mode
    (let ((handler-name (intern (format "org-%s--do-edit-special" name))))
      `(progn
         (defun ,handler-name ()
           ,(format "Edit the block in `%s'; C-c ' again writes it back.\nAuto-generated by `org-defblock' via :edit-indirect-mode, for #+begin_%s blocks."
                    edit-mode name)
           (org-special-block-edit #',edit-mode))
         (org-defblock-add-edit-special-handler ,(symbol-name name) #',handler-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'org-special-block-indirect-edit)
;;; org-special-block-indirect-edit.el ends here
