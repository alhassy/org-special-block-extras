;;; org-block-header-args.el --- Default arguments for special blocks, as header-args are for src blocks  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.16
;; Package-Requires: ((dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; By way of example, every details block of this section is titled in red, and the second also
;; folds a box around itself:
;;
;;     * Exercises
;;       :PROPERTIES:
;;       :header-args:details: :title-color red
;;       :END:
;;
;;       #+begin_details Hint
;;       …
;;       #+end_details
;;
;;       #+header: :boxed t
;;       #+begin_details Answer
;;       …
;;       #+end_details
;;
;; That is, a NAME block takes its arguments' defaults just as a src block of language NAME takes
;; its header-args ---there is nothing new to learn.  From weakest to strongest:
;;
;;   1. `org-babel-default-header-args:NAME', an alist; e.g., ((:title-color . "red")).
;;   2. #+PROPERTY: header-args:NAME :key value, for the whole file.
;;   3. :header-args:NAME: in a heading's drawer, for its subtree; :header-args:NAME+: adds to it.
;;   4. #+header: :key value lines just above the block.
;;   5. The block's own #+begin_NAME line.
;;
;; A block's main argument is given by its name, as with #+begin_details :title Hint; and switches,
;; such as :let and :folded, are inherited alike ---even by Org's own blocks: a src block of
;; language LANG inherits switches from header-args:LANG, as it does Babel's own header arguments.
;; Babel's language-agnostic header-args are left to src blocks, being about evaluation.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'org)
(require 'org-element)

(declare-function org--read-header "org-defblock" (header))

(defun org--header-arg-of (name key)
  "(NAME, KEY) ↦ the default `org-babel-default-header-args:NAME' gives KEY, as Babel's does for a src block.
  (setq org-babel-default-header-args:details \\='((:title-color . \"red\")))  ⇒  (org--header-arg-of \\='details :title-color) = \"red\""
  (let ((defaults (intern (format "org-babel-default-header-args:%s" name))))
    (and (boundp defaults) (alist-get key (symbol-value defaults)))))

(defun org--plist-merge (&rest plists)
  "PLISTS ↦ one plist of all their keys, a later plist's value winning; (:a 1 :b 2), (:b 3) ↦ (:a 1 :b 3)."
  (let (merged)
    (dolist (plist plists merged)
      (cl-loop for (key value) on plist by #'cddr
               do (setq merged (plist-put merged key value))))))

(defun org--block-header-args (elem name)
  "(ELEM, NAME) ↦ the keyword arguments block ELEM inherits as a src block of language NAME would; see the Commentary.
  :header-args:details: :title-color red, on an enclosing heading  ↦  (:title-color red)
The line's own arguments are not among them; they win over these."
  (let ((defaults (intern (format "org-babel-default-header-args:%s" name)))
        (keywords (lambda (header) (-drop-while (-not #'keywordp) (org--read-header header)))))
    (apply #'org--plist-merge
           (and (boundp defaults) (--mapcat (list (car it) (cdr it)) (symbol-value defaults)))
           ;; Org's inheritance gives #+PROPERTY: lines, then each enclosing heading's, a `+' adding rather than replacing.
           (funcall keywords (org-entry-get (org-element-property :begin elem) (format "header-args:%s" name) 'inherit))
           (mapcar keywords (org-element-property :header elem)))))

(provide 'org-block-header-args)
;;; org-block-header-args.el ends here
