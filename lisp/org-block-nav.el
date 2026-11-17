;;; org-block-nav.el --- C-h, C-n, and C-p on a block's delimiters, as on a link  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.11.14
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

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

;; Links are lightweight blocks; so, as on a NAME: link, on the “#+begin_NAME” or “#+end_NAME” of one of our
;; blocks, C-h shows its documentation and C-n/C-p go to the next/previous NAME block.  Within its contents, and
;; after its name, these keys keep their usual meaning.  The bindings live in `org-defblock-map', a text property
;; of the delimiters, as a link's live in `org-deflink-map'.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 's)
(require 'dash)
(require 'org)

(defvar org--supported-blocks)

(defvar org-defblock-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") #'org-this-block-next)
    (define-key map (kbd "C-p") #'org-this-block-previous)
    (define-key map (kbd "C-h") #'org-this-block-show-docs)
    map)
  "A keymap of actions on the “#+begin_NAME” and “#+end_NAME” of each `org-defblock' block; see `org-deflink-map'.

By way of example, to move to the next block of the same name by C-c C-f, rather than C-n:

    (define-key org-defblock-map (kbd \"C-n\") nil)
    (define-key org-defblock-map (kbd \"C-c C-f\") #\\='org-this-block-next)")

(defun org--block-delimiter-regexp (&optional name)
  "Returns the regexp of a “#+begin_NAME” or “#+end_NAME”, group 1 being it and group 2 its name; any of ours sans NAME."
  (format "^[ \t]*\\(#\\+\\(?:begin\\|end\\)_\\(%s\\)\\)\\_>"
          (if name (regexp-quote name) (regexp-opt (mapcar #'symbol-name org--supported-blocks)))))

(defun org--activate-block-delimiters (limit)
  "Binds, up to LIMIT, `org-defblock-map' on each delimiter of our blocks; Org's unfontifying axes it anew; then nil."
  (when org--supported-blocks
    (let ((case-fold-search t)
          (delimiter (org--block-delimiter-regexp)))
      (while (re-search-forward delimiter limit t)
        (put-text-property (match-beginning 1) (match-end 1) 'keymap org-defblock-map))))
  nil)

(defun org--block-delimiter-at-point ()
  "Returns the name of our block whose “#+begin_NAME” or “#+end_NAME” point is on, downcased; else a `user-error'."
  (or (save-excursion
        (let ((case-fold-search t)
              (here (point)))
          (beginning-of-line)
          (and org--supported-blocks
               (looking-at (org--block-delimiter-regexp))
               (<= (match-beginning 1) here (match-end 1))
               (downcase (match-string-no-properties 2)))))
      (user-error "Not on a #+begin_ or #+end_ line of a special block")))

(defun org--this-block-move (search)
  "Moves, by SEARCH, to the “#+begin_NAME” of another block named as the one whose delimiter point is on.
SEARCH is `re-search-forward' or `re-search-backward'; the search starts beyond the current line."
  (let ((name (org--block-delimiter-at-point))
        (case-fold-search t))
    (if (eq search #'re-search-forward) (end-of-line) (beginning-of-line))
    (when (funcall search (format "^[ \t]*\\(#\\+begin_%s\\)\\_>" (regexp-quote name)) nil t)
      (goto-char (match-beginning 1)))))

(defun org-this-block-next ()
  "Goes to the next block of the same name as the one whose delimiter point is on."
  (interactive)
  (org--this-block-move #'re-search-forward))

(defun org-this-block-previous ()
  "Goes to the previous block of the same name as the one whose delimiter point is on."
  (interactive)
  (org--this-block-move #'re-search-backward))

(defun org-this-block-show-docs ()
  "Shows the documentation of the block whose delimiter point is on, in a help buffer; press ‘q’ to quit it."
  (interactive)
  (let ((name (org--block-delimiter-at-point)))
    (with-help-window (format "*Help: Org Block “%s”*" name)
      (princ (concat
              (documentation (intern (format "org--%s-block" name)))
              "\n\nKEY BINDINGS, on its #+begin_ and #+end_ lines; see `org-defblock-map':\n"
              "\n\tC-h: Shows this helpful message buffer"
              "\n\tC-n/C-p: Jump to the next/previous blocks of this name")))))

(provide 'org-block-nav)

;;; org-block-nav.el ends here
