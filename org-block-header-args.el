;;; org-block-header-args.el --- Default arguments for special blocks, as header-args are for src blocks  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.16
;; Package-Requires: ((emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.github.io/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A block's arguments may be given defaults once, for every such block, much as a src block's
;; header-args are; see `org-set-block-header-args'.
;;
;; Full documentation at https://alhassy.github.io/org-special-block-extras

;;; Code:

(require 'cl-lib)

(defvar org--header-args nil
  "Alist (name plist) where `:main-arg' is a special plist key.

  It serves a similar role to that of Org's src ‘header-args’.

  See doc of `org-set-block-header-args' for more information.")

(defmacro org-set-block-header-args (blk &rest kvs)
  "Set default values for special block arguments.

This is similar to, and inspired by, Org-src block header-args.

Example src use:
    #+PROPERTY: header-args:Language :key value

Example block use:
    (org-set-block-header-args Block :main-arg mainvalue :key value)

A full, working, example can be seen by “C-h o org-defblock”.
"
  `(add-to-list 'org--header-args (list (quote ,blk) ,@kvs)))

(cl-defun org--header-arg-of (block-name arg-name)
  "Gets the header value for parameter ARG-NAME used with BLOCK-NAME blocks.

ARG-NAME is a keyword, whereas BLOCK-NAME is a symbol."
  (plist-get (cdr (assoc block-name org--header-args)) arg-name))

(provide 'org-block-header-args)
;;; org-block-header-args.el ends here
