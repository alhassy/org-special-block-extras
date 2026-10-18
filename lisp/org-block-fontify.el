;;; org-block-fontify.el --- How blocks look in Org buffers, by their :fontify  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.18
;; Package-Requires: ((dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; By way of example, (org-defblock proof … :fontify [:begin "Proof." :end "⏹" :face italic] …)
;; shows “#+begin_proof” as “Proof.”, “#+end_proof” as “⏹”, and the contents in italics.
;; Moreover, every supported block's contents are fontified as Org.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'org)
(require 'org-special-block-parse)

(defvar org--block-fontifications nil
  "Alist of (NAME . PLIST): how each block NAME, declared with `:fontify', looks in Org buffers; see `org--fontify-blocks'.")

(defun org--display-as (from to string)
  "Display the text from FROM to TO as STRING; marked as ours, so `org--undisplay' clears it as Org refontifies."
  (when (< from to)
    (put-text-property from to 'display (propertize string 'org-special-block-extras-display t))))

(defun org--undisplay (beg end)
  "Remove, between BEG and END, the display strings of `org--display-as'; Org's unfontifying leaves foreign ones be."
  (let ((pos beg))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'display nil end)))
        (when (-some--> (get-text-property pos 'display) (and (stringp it) (get-text-property 0 'org-special-block-extras-display it)))
          (put-text-property pos next 'display nil))
        (setq pos next)))))

(defun org--fontify-block-contents (name spec)
  "Show the contents of the NAME block, whose #+begin_ line point is on, in SPEC's :face, then its :keywords' faces."
  (let ((contents-start (min (1+ (line-end-position)) (point-max))))
    (save-excursion
      (when (re-search-forward (format "^[ \t]*#\\+end_%s[ \t]*$" (regexp-quote name)) nil t)
        (let ((contents-end (line-beginning-position)))
          (-when-let (face (plist-get spec :face))
            (font-lock-append-text-property contents-start contents-end 'face face))
          (pcase-dolist (`(,regexp ,group ,face) (plist-get spec :keywords))
            (goto-char contents-start)
            (while (re-search-forward regexp contents-end t)
              (font-lock-prepend-text-property (match-beginning group) (match-end group) 'face
                                               (if (functionp face) (funcall face) face)))))))))

(defun org--fontify-blocks (limit)
  "Font-lock matcher: up to LIMIT, show each block in `org--block-fontifications' as its PLIST says; then nil.
  :begin STRING  ↦  “#+begin_NAME” shows as STRING; the header's arguments stay as they are
  :end STRING    ↦  “#+end_NAME” shows as STRING
  :face FACE     ↦  the contents show in FACE too, atop their Org fontification
  :keywords ((REGEXP GROUP FACE) …)  ↦  each REGEXP match in the contents shows its GROUP in FACE, atop all else;
                                       a function FACE is called, with the match data at hand, for the face"
  (when org--block-fontifications
    (let ((case-fold-search t)
          (delimiter (format "^[ \t]*\\(#\\+\\(begin\\|end\\)_\\(%s\\)\\)\\(?:[ \t]\\|$\\)"
                             (regexp-opt (mapcar #'car org--block-fontifications)))))
      (while (re-search-forward delimiter limit t)
        (let* ((name (downcase (match-string 3)))
               (spec (cdr (assoc name org--block-fontifications)))
               (begin? (equal (downcase (match-string 2)) "begin")))
          (-when-let (shown (plist-get spec (if begin? :begin :end)))
            (org--display-as (match-beginning 1) (match-end 1) shown))
          (when (and begin? (or (plist-get spec :face) (plist-get spec :keywords)))
            (org--fontify-block-contents name spec))))))
  nil)

(defun osbe--block-fontifications ()
  "The block names whose contents are fontified as Org: tiny, center, quote, and every supported block."
  (append '("tiny" "center" "quote") (mapcar #'symbol-name org--supported-blocks)))

(defun osbe--fontify-blocks-as-org (fontify &rest args)
  "Call FONTIFY on ARGS, reading the language of each block in `osbe--block-fontifications' as \"org\".
`org-fontify-meta-lines-and-blocks' reads a block's language as group 7, and its name as group 4."
  (let ((match-string (symbol-function 'match-string))
        (org-blocks (osbe--block-fontifications)))
    (cl-letf (((symbol-function 'match-string)
               (lambda (n &optional string)
                 (or (and (eq n 7)
                          (member (-some->> (funcall match-string 4 string) (string-remove-prefix "_")) org-blocks)
                          "org")
                     (funcall match-string n string)))))
      (apply fontify args))))

(provide 'org-block-fontify)
;;; org-block-fontify.el ends here
