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

;; Provides a generic C-c C-c dispatch mechanism for org special blocks.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar org--block-cc-handlers nil
  "Alist of (block-name-string . handler-function) for C-c C-c dispatch.
Use `org-defblock-add-cc-handler' to register handlers.")

(defun org-special-block-at-point-p (name)
  "Return non-nil if point is inside a #+begin_NAME … #+end_NAME block."
  (save-excursion
    (let ((pos (point)) found)
      (goto-char (point-min))
      (while (and (not found)
                  (re-search-forward
                   (format "^[ \t]*#\\+begin_%s\\b" (regexp-quote name)) nil t))
        (let ((block-start (match-beginning 0)))
          (when (re-search-forward
                 (format "^[ \t]*#\\+end_%s\\b" (regexp-quote name)) nil t)
            (when (and (>= pos block-start) (<= pos (match-end 0)))
              (setq found t)))))
      found)))

(defun org-defblock-add-cc-handler (block-name handler)
  "Register HANDLER as the C-c C-c action for blocks named BLOCK-NAME (string).
HANDLER is called with no arguments when point is inside the named block
and should return non-nil to claim the event."
  (setf (alist-get block-name org--block-cc-handlers nil nil #'equal) handler))

(defun org--block-cc-dispatcher ()
  "Dispatch C-c C-c to the first matching registered block handler."
  (when (fboundp 'org-special-block-after-point)
    (cl-some (lambda (entry)
               (when (org-special-block-at-point-p (car entry))
                 (funcall (cdr entry))))
             org--block-cc-handlers)))

(add-hook 'org-ctrl-c-ctrl-c-hook #'org--block-cc-dispatcher)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'org-ctrl-c-ctrl-c-for-special-block)
;;; org-ctrl-c-ctrl-c-for-special-block.el ends here
