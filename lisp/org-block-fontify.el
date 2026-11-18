;;; org-block-fontify.el --- How blocks look in Org buffers, by their :fontify  -*- lexical-binding: t; -*-

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
            (while (if (functionp regexp) (funcall regexp contents-end) (re-search-forward regexp contents-end t))
              (font-lock-prepend-text-property (match-beginning group) (match-end group) 'face
                                               (if (functionp face) (funcall face) face)))))))))

(defun org--fontify-blocks (limit)
  "Font-lock matcher: up to LIMIT, show each block in `org--block-fontifications' as its PLIST says; then nil.
  :begin STRING  ↦  “#+begin_NAME” shows as STRING; the header's arguments stay as they are
  :end STRING    ↦  “#+end_NAME” shows as STRING
  :face FACE     ↦  the contents show in FACE too, atop their Org fontification
  :keywords ((REGEXP GROUP FACE) …)  ↦  each REGEXP match in the contents shows its GROUP in FACE, atop all else;
                                       a function FACE is called, with the match data at hand, for the face;
                                       a function REGEXP is a font-lock matcher, called with the contents' end
  :language LANG ↦  the contents show as a #+begin_src LANG block's do; a function LANG fontifies them itself;
                    see `org--fontify-block-languages'
  :code t        ↦  the contents are code, not Org: they show as their :language alone, sans Org's links and
                    emphasis; see `org--fontify-block-code'"
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
            (org--fontify-block-contents name spec))
          (when (and begin? (plist-get spec :code))
            (org--fontify-block-code name spec))))))
  nil)

(defconst org--fontify-org-markup '(org-emphasis nil invisible nil mouse-face nil keymap nil help-echo nil
                                    htmlize-link nil org-linked-text nil)
  "The text properties, besides faces, that Org's links and emphasis leave; `org--fontify-block-code' axes them.")

(defun org--fontify-block-code (name spec)
  "Show the contents of the NAME block, whose #+begin_ line point is on, as SPEC's :language alone: Org's links and
emphasis, fontified before, are axed; so, in code, a*b*c is no bold b, nor [[x]] a link, nor are markers hidden."
  (let ((argument (save-excursion (beginning-of-line)
                                     (and (looking-at "[ \t]*#\\+begin_[^ \t\n]+[ \t]+\\([^ \t\n]+\\)")
                                          (match-string-no-properties 1))))
        (contents-start (min (1+ (line-end-position)) (point-max))))
    (save-excursion
      (when (re-search-forward (format "^[ \t]*#\\+end_%s[ \t]*$" (regexp-quote name)) nil t)
        (let ((contents-end (line-beginning-position)))
          (remove-text-properties contents-start contents-end org--fontify-org-markup)
          (org--fontify-block-language (plist-get spec :language) argument contents-start contents-end))))))

(declare-function org--deftag-regexp "org-deftag")

(defun org--fontify-tagged-sections (limit)
  "Font-lock matcher: up to LIMIT, show the text of each section tagged :NAME: or :NAME_ARGUMENT:, for a NAME block
whose `:fontify' says :code, as that block's contents show; then nil.  The text runs from past the heading's drawer
up to the next heading, as the tag's block holds it; so in * Our page :image_html:, it shows as HTML.  A chunk that
begins within such a text starts at its heading, lest the text show as code only when its heading is in view."
  (-when-let (names (--keep (and (plist-get (cdr it) :code) (car it)) org--block-fontifications))
    (let ((case-fold-search t)
          (tags (--map (cons it (org--deftag-regexp it)) names)))
      (unless (org-before-first-heading-p) (org-back-to-heading t))
      (while (and (< (point) limit) (re-search-forward org-outline-regexp-bol limit t))
        (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
               (name (car (--first (string-match (cdr it) line) tags))))
          (when name
            (string-match (org--deftag-regexp name) line)
            (let ((argument (cadr (split-string (or (match-string 2 line) "") "_")))
                  (beg (save-excursion (org-end-of-meta-data) (point)))
                  (end (save-excursion (if (re-search-forward org-outline-regexp-bol nil t) (match-beginning 0)
                                         (point-max)))))
              (when (< beg end)
                (remove-text-properties beg end org--fontify-org-markup)
                (org--fontify-block-language (plist-get (cdr (assoc name org--block-fontifications)) :language)
                                             argument beg end))))))))
  nil)

(defun org--fontify-block-language (language argument beg end)
  "Show BEG to END as a #+begin_src LANGUAGE block's contents; a function LANGUAGE does so itself, by
(LANGUAGE ARGUMENT BEG END), ARGUMENT being the first word after the block's name."
  (save-match-data
    (if (functionp language) (funcall language argument beg end)
      (org-src-font-lock-fontify-block language beg end))))

(defun org--fontify-block-languages (limit)
  "Font-lock matcher: up to LIMIT, show the contents of each block whose `:fontify' names a :language LANG as a
#+begin_src LANG block shows; then nil.  It runs ahead of Org's own fontification, whose links and
emphasis so land atop; by way of example, a math block's \\def shows as in `latex-mode', and its red:x in red.
A function LANG fontifies the contents itself; see `org--fontify-block-language'.  A :code block is left to
`org--fontify-block-code', which runs after Org's own fontification instead."
  (-when-let (names (--keep (and (plist-get (cdr it) :language) (not (plist-get (cdr it) :code)) (car it))
                            org--block-fontifications))
    (let ((case-fold-search t)
          (begin (format "^[ \t]*#\\+begin_\\(%s\\)\\(?:[ \t]+\\([^ \t\n]+\\)?.*\\)?$" (regexp-opt names))))
      (while (re-search-forward begin limit t)
        (let ((name (downcase (match-string 1)))
              (argument (match-string-no-properties 2))
              (contents-start (min (1+ (line-end-position)) (point-max))))
          (save-excursion
            (when (re-search-forward (format "^[ \t]*#\\+end_%s[ \t]*$" (regexp-quote name)) nil t)
              (org--fontify-block-language (plist-get (cdr (assoc name org--block-fontifications)) :language)
                                           argument contents-start (line-beginning-position))))))))
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
