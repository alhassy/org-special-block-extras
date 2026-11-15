;;; org-special-block-insert-arg.el --- C-c C-v C-j inserts a special block's argument, as it does a src block's  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.11.14
;; Package-Requires: ((dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; In a src block, C-c C-v C-j (`org-babel-insert-header-arg') completes a header argument, then its
;; value, and appends both to the #+begin_src line.  We teach it special blocks too:
;;
;;   #+begin_details Hint          C-c C-v C-j  title-color ⏎ red ⏎      #+begin_details Hint :title-color red
;;
;; It offers the block's own keywords, as `org-defblock' declares them, and the switches every block
;; understands, such as :folded; in a src block, those switches join Babel's own header arguments.

;;; Code:

(require 'org)
(require 'ob-core)
(require 'dash)
(require 'org-special-block-at-point)
(require 'org-block-switches)

(defun org--block-arg-candidates (name)
  "Returns (KEY . DEFAULT) for each argument a NAME block takes: its `org-defblock' keywords, then the switches it
does not shadow; e.g., for details, ((:title-color . \"green\") … (:folded . nil) …)."
  (let ((own (--map (cons (intern (format ":%s" (car it))) (cadr it))
                    (plist-get (get (intern-soft (format "org--%s-block" name)) 'org-defblock-args) :keys))))
    (append own (cl-loop for (key) in org-block-switches unless (assq key own) collect (list key)))))

(defun org-special-block-insert-arg (&optional key value)
  "Append KEY VALUE to the #+begin_ line of the special block at point, completing KEY among the block's arguments.
  #+begin_details Hint   ⟶   #+begin_details Hint :title-color red
VALUE is read with the declared default, if any, on offer; an empty VALUE, as for a switch such as :folded, is t."
  (interactive)
  (let* ((block (or (org-special-block-at-point) (user-error "Not in a special block")))
         (name (downcase (org-element-property :type block)))
         (candidates (org--block-arg-candidates name))
         (key (or key (intern (completing-read (format "Argument of %s: " name)
                                               (--map (symbol-name (car it)) candidates) nil nil ":"))))
         (default (cdr (assq key candidates)))
         (value (or value (read-string (format "%s value%s: " key (if default (format " (default %s)" default) ""))
                                       nil nil (and default (format "%s" default))))))
    (save-excursion
      (goto-char (org-element-property :post-affiliated block))
      (end-of-line)
      (skip-chars-backward " \t")
      (delete-region (point) (line-end-position))
      (insert (format " %s%s" key (if (org-string-nw-p (format "%s" value)) (format " %s" value) ""))))
    t))

(defun org--special-block-insert-arg (&optional key value)
  "Insert KEY VALUE into the special block at point, as `org-special-block-insert-arg'; nil in, or out of, a src block."
  (when (and (not (org-in-src-block-p)) (org-special-block-at-point))
    (org-special-block-insert-arg key value)))

(defun org--babel-insert-switch-too (insert &rest args)
  "Calls INSERT, `org-babel-insert-header-arg', on ARGS, with our switches among the header arguments it completes."
  (let ((org-babel-common-header-args-w-values
         (append org-babel-common-header-args-w-values
                 (--map (cons (intern (substring (symbol-name (car it)) 1)) :any) org-block-switches))))
    (apply insert args)))

(advice-add 'org-babel-insert-header-arg :before-until #'org--special-block-insert-arg)
(advice-add 'org-babel-insert-header-arg :around #'org--babel-insert-switch-too)

(provide 'org-special-block-insert-arg)
;;; org-special-block-insert-arg.el ends here
