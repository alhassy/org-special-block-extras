;;; org-deftag.el --- org-deftag: tags as functions on Org sections  -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 4.1.1
;; Package-Requires: ((dash "2.18.1") (emacs "27.1") (org "9.6"))
;; Keywords: org, tags, convenience
;; URL: https://alhassy.github.io/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Just as a special block is a function on a region of text, a tag is a
;; function on an Org section: `org-deftag' declares one.
;;
;; Full documentation at https://alhassy.github.io/org-special-block-extras

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'org)

(defun org--deftag-parse-args (args)
  "ARGS ↦ ((NAME DEFAULT POSITIONAL?) …), for either style of `org-deftag' arglist.

  (&optional (color \"pink\") size &key note)  ↦  ((color \"pink\" t) (size nil t) (note nil nil))
  (color \"pink\" size nil)                    ↦  ((color \"pink\" t) (size nil t))

`&optional' names are positional: bound by the tag's underscore-separated items, in order.
`&key' names are bound only by the section's properties.  The flattened convenience form
lists NAME DEFAULT pairs, all positional; a missing final DEFAULT is nil."
  (if (not (or (memq '&optional args) (memq '&key args)))
      (cl-loop for (name default) on args by #'cddr collect (list name default t))
    (let (section result)
      (unless (memq (car args) '(&optional &key))
        (error "Nothing may precede &optional or &key in an arglist, but saw %S" args))
      (dolist (it args (nreverse result))
        (if (memq it '(&optional &key))
            (setq section it)
          (push (list (if (consp it) (car it) it) (when (consp it) (cadr it)) (eq section '&optional))
                result))))))

(defmacro org-deftag (name args docstring &rest body)
  "Re-render each Org section tagged :NAME: by running BODY at export time.

A tag is to a section what a special block is to a region: a function on it.
For example, the following renders a section as a <details> element.

   (org-deftag fold (&optional (color \"pink\"))
     \"HTML export a section as a <details> block, tinted COLOR.\"
     (insert \"\\n#+html:\"
             (format \"<details style=\\\"background-color: %s\\\">\" color)
             \"<summary>\" (string-trim-left o-heading \"\\\\*+ \")
             \"</summary>\")
     (org-next-visible-heading 1)
     (insert \"#+html: </details>\"))

Then a heading tagged :fold_teal: is folded away, on a teal background;
with a bare :fold: tag, the section's :COLOR: property is used, else pink.

To export a section as a special block, there is no need for a tag of
its own: declare the block with `org-defblock' and its `:tag t' option.

ARGS is a `cl-defun'-style arglist, such as

   (&optional (color \"pink\") size &key note)

The `&optional' arguments are positional: they are bound to the
underscore-separated items after NAME in the tag, in order; an item missing
from the tag falls back to the section's property of the same name ---e.g.,
:COLOR: teal--- and then to its default.  The `&key' arguments are bound only
by properties, then defaults.  As a convenience, ARGS may instead be flattened
into NAME DEFAULT pairs, all positional; the above, sans NOTE, is then
(color \"pink\" size nil).  See `org--deftag-parse-args'.

BODY runs with point at the start of the heading's line, which has been
erased along with any property drawer beneath it; BODY may anaphorically mention
- O-BACKEND:    the export backend, such as `html' or `latex';
- O-HEADING:    the heading's text, sans the :NAME…: tag;
- O-TAG-ARGS:   the tag's underscore-separated items after NAME, e.g. (\"pink\");
- O-PROPERTIES: the section's properties as a plist, e.g. (:color \"pink\"); and
- O-DRAWER:     the erased property drawer, as text, or nil; reinsert it with
                O-HEADING to keep the section's properties, such as CUSTOM_ID.

The result is the function `org-deftag/NAME', added to
`org-export-before-parsing-functions' at depth -50, so that tags run before
special blocks are rewritten; a tag may thereby produce special blocks."
  (declare (indent defun) (doc-string 3))
  (let ((func-name (intern (format "org-deftag/%s" name)))
        ;; The tag, such as :fold: or :fold_pink_big:, among the heading's trailing tags: group 1 is the tag
        ;; sans its closing colon ---so axing it leaves its neighbours' colons intact--- and group 2 its items.  Org tags cannot hold hyphens,
        ;; whereas block names often do ---e.g., latex-definitions--- so we match the line ourselves.
        (tag-re (format "\\(:%s\\(_[^: \t]*\\)?\\):\\(?:[^ \t\n]*:\\)?[ \t]*$" (regexp-quote (symbol-name name)))))
    `(progn
       (defun ,func-name (o-backend)
         ,docstring
         (let ((case-fold-search t))
           ;; One quick scan of the heading lines, so that exports without this tag skip walking every heading.
           (when (save-excursion
                   (goto-char (point-min))
                   (re-search-forward ,(concat "^\\*+ .*" tag-re) nil t))
             (outline-show-all)
             (org-map-entries
              (lambda ()
                (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
                  (when (string-match ,tag-re line)
                    ;; Read the match before anything else matches ---`split-string' included.
                    (let* ((items (match-string 2 line))
                           ;; The heading's line, sans this tag; a lone ":" left behind means it was the only tag.
                           (o-heading (replace-regexp-in-string "[ \t]+:[ \t]*$" "" (replace-match "" t t line 1)))
                           (o-tag-args (cdr (split-string (or items "") "_")))
                           (o-properties (cl-loop for (key . value) in (org-entry-properties nil 'standard)
                                                  append (list (intern (concat ":" (downcase key))) value)))
                           ;; The property drawer goes with the heading: its contents live on in o-properties.
                           (o-drawer (-when-let (body-start (car (org-get-property-block)))
                                       (save-excursion
                                         (goto-char body-start)
                                         (forward-line -1)
                                         (when (looking-at org-property-drawer-re)
                                           (prog1 (concat (match-string 0) "\n")
                                             (delete-region (match-beginning 0) (min (1+ (match-end 0)) (point-max))))))))
                           ;; Wherever BODY leaves point, the next heading must still begin a line.
                           (next-heading (save-excursion (outline-next-heading) (copy-marker (point) t)))
                           ,@(cl-loop with position = -1
                                      for (arg default positional?) in (org--deftag-parse-args args)
                                      collect `(,arg (or ,@(when positional? `((nth ,(cl-incf position) o-tag-args)))
                                                         (plist-get o-properties ,(intern (format ":%s" (downcase (symbol-name arg)))))
                                                         ,default))))
                      (ignore o-backend o-heading o-properties o-drawer)
                      (delete-region (line-beginning-position) (line-end-position))
                      ,@body
                      (goto-char next-heading)
                      (unless (bolp) (insert "\n"))
                      (set-marker next-heading nil)))))))))
       (add-hook 'org-export-before-parsing-functions #',func-name -50)
       #',func-name)))

(provide 'org-deftag)
;;; org-deftag.el ends here
