;;; org-special-block-at-point.el --- The special block at point, and dispatch on its name  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; The shared base of per-block key bindings, such as C-c C-c (see
;; `org-ctrl-c-ctrl-c-for-special-block') and C-c ' (see
;; `org-special-block-indirect-edit'): which special block is point in,
;; and which registered handler, if any, should act on it.

;;; Code:

(require 'org)
(require 'org-element)
(require 'dash)

(defun org-special-block-at-point ()
  "The innermost special block element around point, via `org-element'; else nil."
  (org-element-lineage (org-element-at-point) '(special-block) t))

(defun org-special-block-name-at-point ()
  "The name of the innermost special block around point, downcased; e.g., \"graph\" for #+begin_graph; else nil."
  (-some--> (org-special-block-at-point) (downcase (org-element-property :type it))))

(defun org-special-block-at-point-p (name)
  "Non-nil iff point is inside a #+begin_NAME … #+end_NAME block; the innermost, when blocks nest."
  (equal (downcase name) (org-special-block-name-at-point)))

(defun org--special-block-dispatch (handlers)
  "(HANDLERS, point) ↦ the result of calling the handler, in the alist HANDLERS, for the special block around point.
HANDLERS maps downcased block names to functions of no arguments; nil when none applies.
With no handlers at all, we skip parsing: this runs on every keypress of its binding."
  (when handlers
    (-some--> (org-special-block-name-at-point)
      (alist-get it handlers nil nil #'equal)
      (funcall it))))

(provide 'org-special-block-at-point)
;;; org-special-block-at-point.el ends here
