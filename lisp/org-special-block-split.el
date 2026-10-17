;;; org-special-block-split.el --- C-c C-v C-d splits a special block, as it does a src block  -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; In a src block, C-c C-v C-d (`org-babel-demarcate-block') splits it in two at point; elsewhere, it
;; makes a new src block.  We teach it special blocks too, after https://scripter.co/splitting-an-org-block-into-two/:
;;
;;   #+begin_tip Mind                       #+begin_tip Mind
;;   One.                                   One.
;;   ▮Two.                     C-c C-v C-d  #+end_tip
;;   #+end_tip                     ↦
;;                                          #+begin_tip Mind
;;                                          ▮Two.
;;                                          #+end_tip
;;
;; Each piece keeps the block's whole #+begin_ line, header arguments and all.

;;; Code:

(require 'org)
(require 'ob-core)
(require 'dash)
(require 'org-special-block-at-point)

(defun org-special-block-split (&optional arg)
  "Split the special block at point in two at point, or in three around the active region; with ARG, a heading between.
A point at a line's end splits after that line, and one mid-line splits it there, sans the space around it.
Point ends at the start of the piece after the (first) split; as with `org-babel-demarcate-block' for src blocks."
  (interactive "P")
  (when (org-in-src-block-p) (user-error "In a src block, which `org-babel-demarcate-block' splits"))
  (let* ((block (or (org-special-block-at-point) (user-error "Not in a special block")))
         (line (lambda (pos) (save-excursion (goto-char pos) (buffer-substring (line-beginning-position) (line-end-position)))))
         (begin-line (funcall line (org-element-property :post-affiliated block)))
         (end-line (funcall line (save-excursion (goto-char (org-element-property :end block))
                                                 (skip-chars-backward " \t\n")
                                                 (point))))
         (separator (if arg (concat (make-string (or (org-current-level) 1) ?*) " \n") "\n"))
         (points (if (org-region-active-p) (list (region-beginning) (region-end)) (list (point)))))
    (unless (--every? (and (org-element-property :contents-begin block)
                           (<= (org-element-property :contents-begin block) it (org-element-property :contents-end block)))
                      points)
      (user-error "Select within the block's contents to split it"))
    (deactivate-mark)
    ;; From the last split to the first, so the earlier positions stay put; point ends after the first.
    (dolist (p (reverse points))
      (goto-char p)
      (cond ((bolp))
            ((eolp) (forward-char 1))
            (t (delete-horizontal-space) (insert "\n")))
      (insert end-line "\n" separator begin-line "\n"))
    t))

(defun org--special-block-demarcate (&optional arg)
  "Split the special block at point, with ARG as `org-special-block-split' takes it; nil in, or out of, a src block."
  (when (and (not (org-in-src-block-p)) (org-special-block-at-point))
    (org-special-block-split arg)))

(advice-add 'org-babel-demarcate-block :before-until #'org--special-block-demarcate)

(provide 'org-special-block-split)
;;; org-special-block-split.el ends here
