;;; org-ctrl-c-ctrl-c-for-special-block.el --- C-c C-c dispatch for org special blocks  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.github.io/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Provides a generic C-c C-c dispatch mechanism for org special blocks,
;; atop `org-special-block-at-point'.
;;
;; Any block type can register a handler via:
;;
;;   (org-defblock-add-cc-handler "myblock" #'my-cc-handler)
;;
;; The handler is called with no arguments when C-c C-c is pressed inside
;; a #+begin_myblock … #+end_myblock block.  It should return non-nil to
;; claim the event (preventing further hook processing).

;;; Code:

(require 'org)
(require 'org-special-block-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar org--block-cc-handlers nil
  "Alist of (block-name-string . handler-function) for C-c C-c dispatch.
Use `org-defblock-add-cc-handler' to register handlers.")

(defun org-defblock-add-cc-handler (block-name handler)
  "Register HANDLER as the C-c C-c action for blocks named BLOCK-NAME (string).
HANDLER is called with no arguments when point is inside the named block
and should return non-nil to claim the event."
  (setf (alist-get (downcase block-name) org--block-cc-handlers nil nil #'equal) handler))

(declare-function org--read-header "org-defblock")
(declare-function org--block-eval "org-defblock")

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
  "Dispatch C-c C-c to the registered handler for the special block at point; else to its :C-c_C-c switch."
  (if (assoc (org-special-block-name-at-point) org--block-cc-handlers)
      (org--special-block-dispatch org--block-cc-handlers)
    (org--block-cc-switch)))

(add-hook 'org-ctrl-c-ctrl-c-hook #'org--block-cc-dispatcher)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'org-ctrl-c-ctrl-c-for-special-block)
;;; org-ctrl-c-ctrl-c-for-special-block.el ends here
