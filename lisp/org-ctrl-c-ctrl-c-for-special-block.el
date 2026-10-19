;;; org-ctrl-c-ctrl-c-for-special-block.el --- C-c C-c dispatch for org special blocks  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; By way of example,
;;
;;     (org-def-action shout (&optional greeting &key (volume 1))
;;       "Announce the shout's GREETING, at VOLUME."
;;       (message "%s, at %s: %s" greeting volume raw-contents))
;;
;; makes C-c C-c within “#+begin_shout Hello :volume 11 … #+end_shout” say “Hello, at 11: …”.
;; That is, `org-def-action' is to C-c C-c what `org-defblock' is to export: the arglist is the
;; block's, in either of `org-defblock''s styles, its values ---inherited ones too--- bound by name.
;; A body yielding nil declines the key press; the block's :C-c_C-c switch, if any, then has it.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'org)
(require 'org-special-block-at-point)

(declare-function org--block-let "org-block-switches")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar org--block-cc-handlers nil
  "Alist of (block-name-string . handler-function) for C-c C-c dispatch; `org-def-action' adds to it.")

(declare-function org--read-header "org-block-header-args")
(declare-function org--block-eval "org-block-switches")
(declare-function org--block-name "org-special-block-parse")
(declare-function org--defblock-desugar-args "org-defblock")
(declare-function org--block-header-args "org-block-header-args")
(declare-function org--plist-merge "org-block-header-args")

(defmacro org-def-action (name args &optional docstring &rest body)
  "Make C-c C-c within a NAME block run BODY, with the block's ARGS bound, as `org-defblock' does for export.
  (org-def-action shout (&optional greeting &key (volume 1)) \"Announce it.\" (message \"%s, %s\" greeting volume))
ARGS take either style of `org-defblock''s; BODY sees them by name, and the block's text as RAW-CONTENTS, with point
within the block.  The function is `org-action/NAME'.  A non-nil value claims the key press; nil declines it."
  (declare (indent defun) (doc-string 3))
  (unless (stringp docstring) (setq body (cons docstring body) docstring nil))
  (let* ((flat (cl-loop for (key value) on (org--defblock-desugar-args args) by #'cddr
                        unless (keywordp key) append (list key value)))
         (main (or (car flat) 'main-arg))
         (function (intern (format "org-action/%s" name))))
    `(progn
       ;; All arguments by keyword, the main one too: so an absent one takes its default, as a block's does.
       (cl-defun ,function (raw-contents &key (,main ,(cadr flat)) ,@(-partition 2 (cddr flat)) &allow-other-keys)
         ,@(when docstring (list docstring))
         (ignore raw-contents ,main)
         ,@body)
       (setf (alist-get ,(downcase (format "%s" name)) org--block-cc-handlers nil nil #'equal)
             (lambda () (org--block-action-call #',function ',main)))
       #',function)))

(defun org--block-action-call (function main)
  "(FUNCTION, MAIN) ↦ FUNCTION's value on the block around point: its text, its main argument as MAIN, and the rest.
The block's keyword arguments include those it inherits; see `org--block-header-args'.  Its text has its :let
shorthands applied, as export renders it; see `org--block-let'."
  (let* ((elem (org-special-block-at-point))
         (block (save-excursion (goto-char (org-element-property :post-affiliated elem))
                                (org-special-block-after-point (org--block-name elem))))
         (main-arg (org-string-nw-p (org-special-block-main-arg block)))
         (kwdargs (org-special-block-kwdargs block))
         (contents (org-special-block-contents block)))
    ;; The written main argument first, so it wins over one inherited by its name.
    (apply function (-if-let (bindings (plist-get kwdargs :let)) (org--block-let bindings contents nil) contents)
           (append (when main-arg (list (intern (format ":%s" main)) main-arg)) kwdargs))))

(defun org--block-cc-switch ()
  "Run the :C-c_C-c switch of the special block around point; nil when it has none.

  #+begin_quiz :C-c_C-c (message \"Score: 3/4\")     ;; A form is evaluated
  #+begin_quiz :C-c_C-c my/check-answers           ;; A function is called; interactively, if a command

The switch may be inherited, as a src block's header-args are; e.g., :header-args:quiz: :C-c_C-c my/check-answers
in a heading's drawer; see `org--block-header-args'.  The Lisp runs as `org-defblock-evaluate-switches' permits."
  (-when-let* ((blk (org-special-block-at-point))
               (action (plist-get (org--plist-merge (org--block-header-args blk (downcase (org--block-name blk)))
                                                    (-drop-while (-not #'keywordp)
                                                                 (org--read-header (org-element-property :parameters blk))))
                                  :C-c_C-c)))
    (if (and (symbolp action) (fboundp action))
        (if (commandp action) (call-interactively action) (funcall action))
      (org--block-eval action nil))
    t))

(defun org--block-cc-dispatcher ()
  "Dispatch C-c C-c to the `org-def-action' of the special block at point; should it decline, to its :C-c_C-c switch.
Within a table, C-c C-c is Org's own, aligning it; on its #+TBLFM: line, computing its formulas, as in any block."
  (unless (or (org-at-table-p) (org-at-TBLFM-p))
    (or (org--special-block-dispatch org--block-cc-handlers)
        (org--block-cc-switch))))

(add-hook 'org-ctrl-c-ctrl-c-hook #'org--block-cc-dispatcher)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'org-ctrl-c-ctrl-c-for-special-block)
;;; org-ctrl-c-ctrl-c-for-special-block.el ends here
