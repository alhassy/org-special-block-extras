;;; org-defblock-tag.el --- A section tagged :NAME: exports as a NAME block around its text  -*- lexical-binding: t; -*-

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

;; The bridge between `org-deftag' and `org-defblock': each block's tag rewrites a section as
;;
;;     * Why bother? :details_Spoiler_alert:        * Why bother?
;;     Body text…                            ↦      #+begin_details "Spoiler alert"
;;                                                  Body text…
;;                                                  #+end_details
;;
;; then renders that block at once.  See `org--defblock-section-to-block'.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'org)
(require 'org-deflink)
(require 'org-deftag)
(require 'org-special-block-parse)
(require 'org-block-export)

(defun org--defblock-tag-subsections (name tag)
  "(NAME, TAG option) ↦ what NAME's tag does with a section's subsections: `ignore', `error', or `recurse'.

  t                        ↦  ignore
  [:subsections recurse]   ↦  recurse
  [:subsections sideways]  ↦  an error, at once, naming NAME

See `org--defblock-section-to-block' for what each mode does."
  (let* ((spec (if (vectorp tag) (append tag nil) nil))
         (mode (or (plist-get spec :subsections) 'ignore)))
    (unless (and (cl-evenp (length spec)) (cl-loop for key in spec by #'cddr always (eq key :subsections)))
      (error "‘org-defblock %s’: :tag takes t, nil, or [:subsections MODE], but saw %S" name tag))
    (unless (memq mode '(ignore error recurse))
      (error "‘org-defblock %s’: :subsections takes ignore, error, or recurse, but saw %S" name mode))
    mode))

;;;; The block's header, from the section's properties

(defun org--section-property (properties key)
  "(PROPERTIES, KEY) ↦ the value the section's PROPERTIES give KEY, regardless of case; (:color \"pink\"), COLOR ↦ \"pink\"."
  (plist-get properties (intern (format ":%s" (downcase (symbol-name key))))))

(defun org--section-block-keywords (properties main keywords other-keys?)
  "(PROPERTIES, MAIN, KEYWORDS, OTHER-KEYS?) ↦ the keywords a section's PROPERTIES may supply its block.
KEYWORDS; and, when OTHER-KEYS?, every other property bar MAIN and Org's own, such as CATEGORY and CUSTOM_ID."
  (if (not other-keys?) keywords
    (let ((orgs (--map (intern (downcase it)) (append org-special-properties org-default-properties))))
      (-union keywords (cl-loop for key in properties by #'cddr
                                for name = (intern (substring (symbol-name key) 1))
                                unless (or (memq name orgs) (eq name main)) collect name)))))

(defun org--section-block-header (properties main-arg keywords)
  "(PROPERTIES, MAIN-ARG, KEYWORDS) ↦ the #+begin_ line's text after the name, each argument after a space.
  (:color \"pink\"), \"Spoiler alert\", (color)  ↦  \" \\\"Spoiler alert\\\" :color \\\"pink\\\"\""
  (concat (if main-arg (format " %S" main-arg) "")
          (mapconcat (lambda (key) (-if-let (value (org--section-property properties key)) (format " :%s %S" key value) ""))
                     keywords "")))

;;;; Walking a section's text and subsections

(defun org--next-subheading (level)
  "LEVEL ↦ the depth of the next heading, point at its start, when it is deeper than LEVEL; else nil."
  (and (re-search-forward org-outline-regexp-bol nil t)
       (progn (beginning-of-line)
              (let ((depth (- (save-excursion (skip-chars-forward "*") (point)) (point))))
                (and (> depth level) depth)))))

(defun org--wrap-section-text (name header backend)
  "From the start of a section's text, wrap it, up to the next heading, in a NAME block of HEADER; then render it for BACKEND.
Point ends after the result."
  (let ((start (point)) (end (make-marker)))
    (insert (format "#+begin_%s%s\n" name header))
    (goto-char (if (re-search-forward org-outline-regexp-bol nil t) (match-beginning 0) (point-max)))
    (unless (bolp) (insert "\n"))
    (insert (format "#+end_%s" name))
    (set-marker end (point))
    (set-marker-insertion-type end t)
    ;; Keep #+end_NAME on a line of its own, apart from any following heading.
    (save-excursion (insert "\n"))
    (goto-char start)
    ;; As in `org--rewrite-special-blocks-by-handlers': blocks render for backends derived from html or latex.
    (org-eval-replace-block (org-special-block-after-point name) (org--base-backend backend))
    (goto-char end)
    (set-marker end nil)))

(defun org--refuse-subsections (name heading level)
  "(NAME, HEADING, LEVEL) ↦ nil when the section HEADING, of LEVEL, has no subsection after point; else a `user-error'."
  (save-excursion
    (-when-let (depth (org--next-subheading level))
      (user-error "Section “%s” is tagged :%s:, whose block cannot hold its subsection “%s”; move the subsection out, or declare `:tag [:subsections ignore]' or `[:subsections recurse]' in ‘org-defblock %s’"
                  (string-trim (substring heading level)) name
                  (string-trim (buffer-substring (+ (point) depth) (line-end-position)))
                  name))))

(defun org--wrap-subsections (name header backend level)
  "Wrap each subsection's own text, deeper than LEVEL, in a NAME block of HEADER for BACKEND; see `org--wrap-section-text'.
A subsection tagged :NAME: itself is left to its own tag."
  (let ((own-tag (org--deftag-regexp name)))
    (while (org--next-subheading level)
      (let ((case-fold-search t))
        (if (looking-at (concat ".*" own-tag))
            (forward-line 1)
          (forward-line 1)
          (when (looking-at org-property-drawer-re)
            (goto-char (match-end 0))
            (forward-line 1))
          (org--wrap-section-text name header backend))))))

(defun org--defblock-section-to-block (name heading drawer tag-args properties main keywords backend
                                            &optional subsections other-keys?)
  "Rewrite the section at point ---its heading line and DRAWER erased by `org-deftag'--- as HEADING atop a NAME block.

  * Why bother? :details_Spoiler_alert:        * Why bother?
  :PROPERTIES:                                 :PROPERTIES:
  :COLOR: pink                          ↦      :COLOR: pink
  :END:                                        :END:
  Body text…                                   #+begin_details \"Spoiler alert\" :color \"pink\"
                                               Body text…
                                               #+end_details

TAG-ARGS, joined by spaces, form the main argument, else the section's MAIN property; each of the
KEYWORDS found among the section's PROPERTIES becomes a keyword argument ---and, when OTHER-KEYS? since
the block allows other keys, so does every other property bar Org's own, such as CATEGORY and CUSTOM_ID.
The section's own text is
wrapped, up to the next heading of any depth; a block cannot hold a heading, so SUBSECTIONS says what
becomes of the section's subsections:
+ `ignore' (the default): they stay outside the block, exported as usual.
+ `error': a section with a subsection is an error, naming both; e.g., for a block whose
  content would be silently cut short.
+ `recurse': each subsection's own text, at any depth, is wrapped likewise ---same arguments---
  in a NAME block of its own; a subsection tagged :NAME: itself is left to its own tag.

The block is then at once replaced by the result of `org-block/NAME' for BACKEND; so a tag works even
when NAME is not registered as a block, via `:block nil'.  Point ends after the last result."
  (let* ((main-arg (if tag-args (string-join tag-args " ") (org--section-property properties main)))
         (header (org--section-block-header properties main-arg
                                             (org--section-block-keywords properties main keywords other-keys?)))
         (level (progn (string-match "\\`\\*+" heading) (match-end 0))))
    ;; Restore the heading, and its property drawer, so the section keeps its properties; e.g., CUSTOM_ID.
    (insert heading "\n" (or drawer ""))
    (unless (bolp) (insert "\n"))
    (when (eq subsections 'error) (org--refuse-subsections name heading level))
    (org--wrap-section-text name header backend)
    (when (eq subsections 'recurse) (org--wrap-subsections name header backend level))))

(provide 'org-defblock-tag)
;;; org-defblock-tag.el ends here
