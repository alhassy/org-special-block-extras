;;; org-parallel-block.el --- The `parallel' multi-column block  -*- lexical-binding: t; -*-

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

;;; Commentary:

;; The `parallel' special block for multi-column layouts.
;; HTML uses CSS columns or flexbox; LaTeX uses the multicols environment.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'org-defblock)

;;;;;; parallel

(defun org--parallel-columns (raw)
  "Returns (PREAMBLE COUNT . COLUMNS): RAW's text before its first heading; how many headings are its shallowest; and
the Org that sets each as a column, #+columnbreak: between them.  Each heading becomes its title in bold, atop its
text, its property drawer axed; a deeper one's, a paragraph of its own.  \"Intro\\n** Pros\\nFast.\\n** Cons\\nBig.\" gives (\"Intro\\n\" 2 . \"*Pros*\\nFast.\\n#+columnbreak:\\n*Cons*\\nBig.\")."
  (with-temp-buffer
    (insert raw)
    (org-mode)
    (goto-char (point-min))
    (let* ((first (if (re-search-forward org-outline-regexp-bol nil t) (match-beginning 0) (point-max)))
           (preamble (buffer-substring-no-properties (point-min) first))
           (top (progn (goto-char first) (cl-loop while (re-search-forward org-outline-regexp-bol nil t)
                                                  minimize (- (match-end 0) (match-beginning 0) 1))))
           (count 0))
      (delete-region (point-min) first)
      (goto-char (point-min))
      (while (re-search-forward org-complex-heading-regexp nil t)
        (let ((level (- (match-end 1) (match-beginning 1))) (title (or (match-string-no-properties 4) "")))
          ;; A column's first heading begins it, after a break; a deeper one, a paragraph of its own.
          (replace-match (format "%s*%s*" (cond ((> level top) "\n") ((> (cl-incf count) 1) "#+columnbreak:\n") (t ""))
                                 title)
                         t t)
          (forward-line 1)
          (when (looking-at org-property-drawer-re)
            (delete-region (match-beginning 0) (min (point-max) (1+ (match-end 0)))))))
      (cons preamble (cons count (buffer-string))))))

(org-defblock parallel (&optional cols &key bar &allow-other-keys :latex-head ("multicol") :manual "#Parallel"
                       :tag [:subsections contents] &context backend raw-contents contents)
              "Place ideas side-by-side, possibly with a separator.

There are COLS many columns, and they may be separated by solid
vertical rules if BAR is a non-nil (color) value.

+ COLS is either a number or a sequence of the shape: 10% 20% 30%.
+ BAR is either `t', `nil', or a color such as `red' or `blue'.

----------------------------------------------------------------------

“Soft Columns”: Writing “#+begin_parallel 𝓃 :bar t” will produce
𝒏-many parallel columns, possibly separated by solid rules, or a
“bar”. This style allows text to freely move between columns,
depending on the size of the browser, which may dynamically
shrink and grow.  BAR can either be `t', `nil', or
any (backend)-valid color specification; such as `red' or
`green'.

“Hard Columns”: Alternatively, for non-uniform column widths,
COLS may instead be a specification of the widths of the
columns. However, this extra flexibility comes at an additional
cost: The contents of the block must now contain 𝒏-1 lines
consisting of ‘#+columnbreak:’, when the specification determines
𝒏 columns, as shown in the following example.

   #+begin_parallel 20% 60% 20% :bar green
   Hello, to the left!

   #+columnbreak:
   A super duper wide middle margin!

   #+columnbreak:
   Goodbye (“God-be-with-ye”) to the right!
   #+end_parallel

The specification is 𝒏 measurements denoting widths; which may be
in any HTML recognisable units; e.g., “5em 20px 30%” is valid.
I personally advise only the use of percentage measurements.

A section tagged :parallel: sets each subheading as a column, its title in bold atop its text; COLS defaults to
how many there are, and its drawer's :COLS: and :BAR: are the block's arguments.  Its own text sits above them.

   * Lisp or Haskell? :parallel:          ≡   #+begin_parallel 2
   ** Lisp                                     *Lisp*
   Code is data.                              Code is data.
   ** Haskell                                  #+columnbreak:
   Types are propositions.                    *Haskell*
                                              Types are propositions.
                                              #+end_parallel

In the Soft Columns style above, any ‘#+columnbreak:’ are merely ignored.
With LaTeX export, the use of ‘#+columnbreak:’ is used to request a column break."
              (-let* (((preamble count . columns) (and (string-match-p org-outline-regexp-bol raw-contents)
                                                        (org--parallel-columns raw-contents)))
                      (contents (if columns (org-parse columns) contents))
                      (cols (format "%s" (or cols (and columns count) 2)))
                      (widths (split-string cols))
                     ;; Hard columns have their widths given; e.g., 20% 60% 20%, or 5em 20px.
                     (hard? (--any? (not (string-match-p "\\`[0-9]+\\'" it)) widths))
                     (color (if (equal (format "%s" bar) "t") "black" bar))
                     ;; A line of its own, so that mentions such as ~#+columnbreak:~ are left alone.
                     (column-break "^[ \t]*#\\+columnbreak:[ \t]*$"))
                (concat
                 (and columns (org-parse preamble))
                 (pcase backend
                  ('latex (format (concat "\\par \\setlength{\\columnseprule}{%s pt}\n"
                                          "\\begin{minipage}[t]{\\linewidth}\n"
                                          "\\begin{multicols}{%s}\n%s\n"
                                          "\\end{multicols}\\end{minipage}")
                                  (if bar 2 0)
                                  (if hard? (length widths) cols)
                                  (replace-regexp-in-string column-break "\\\\columnbreak" contents)))
                  ((guard (not hard?))
                   (format "<div style=\"column-count: %s;%s\">%s</div>"
                           cols (if bar (format " column-rule: 1px solid %s;" color) "") contents))
                  ;; Each width begins a column, separated by a bar unless it is the last; CONTENTS is Org
                  ;; between export fences, so the columns after the first begin within @@html:…@@.
                  (_ (let ((columns (cl-loop for (width . more) on widths
                                             collect (format "<div style=\"width: %s; margin: 10px; border-right: %s;\">"
                                                             width (if (and bar more) (format "4px solid %s" color) "none"))))
                           (chunks (split-string contents column-break)))
                       (format "<div style=\"display: flex; justify-content: space-between; align-items: flex-start;\">%s</div>"
                               (concat (car columns)
                                       (car chunks)
                                       (cl-loop for column in (cdr columns)
                                                for chunk in (cdr chunks)
                                                concat (format "@@html:</div>%s@@%s" column chunk))
                                       "</div>"))))))))

(org-defblock-attribute parallel :example "
# Two columns, by default; :bar rules them apart, in a color; #+columnbreak: ends a column.
#+begin_parallel 2 :bar green
*Lisp:* code is data.

#+columnbreak:
*Haskell:* types are propositions.
#+end_parallel

# Widths, rather than a count, fix each column's width: #+begin_parallel 30% 70%.")

(provide 'org-parallel-block)
;;; org-parallel-block.el ends here
