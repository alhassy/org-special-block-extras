;;; org-parallel-block.el --- The `parallel' multi-column block  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.16
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "27.1") (org "9.6"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.github.io/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; The `parallel' special block for multi-column layouts.
;; HTML uses CSS columns or flexbox; LaTeX uses the multicols environment.
;;
;; Full documentation at https://alhassy.github.io/org-special-block-extras

;;; Code:

(require 'org-defblock)

;;;;;; parallel

(org-defblock parallel (cols "2" bar nil)
              "Place ideas side-by-side, possibly with a separator.

There are COLS many columns, and they may be separated by solid
vertical rules if BAR is a non-nil (colour) value.

+ COLS is either a number or a sequence of the shape: 10% 20% 30%.
+ BAR is either `t', `nil', or a colour such as `red' or `blue'.

----------------------------------------------------------------------

“Soft Columns”: Writing “#+begin_parallel 𝓃 :bar t” will produce
𝒏-many parallel columns, possibly separated by solid rules, or a
“bar”. This style allows text to freely move between columns,
depending on the size of the browser, which may dynamically
shrink and grow.  BAR can either be `t', `nil', or
any (backend)-valid colour specification; such as `red' or
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

In the Soft Columns style above, any ‘#+columnbreak:’ are merely ignored.
With LaTeX export, the use of ‘#+columnbreak:’ is used to request a column break."
              (let* ((widths (split-string cols))
                     ;; Hard columns have their widths given; e.g., 20% 60% 20%, or 5em 20px.
                     (hard? (--any? (not (string-match-p "\\`[0-9]+\\'" it)) widths))
                     (colour (if (equal (format "%s" bar) "t") "black" bar))
                     ;; A line of its own, so that mentions such as ~#+columnbreak:~ are left alone.
                     (column-break "^[ \t]*#\\+columnbreak:[ \t]*$"))
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
                           cols (if bar (format " column-rule: 1px solid %s;" colour) "") contents))
                  ;; Each width begins a column, separated by a bar unless it is the last; CONTENTS is Org
                  ;; between export fences, so the columns after the first begin within @@html:…@@.
                  (_ (let ((columns (cl-loop for (width . more) on widths
                                             collect (format "<div style=\"width: %s; margin: 10px; border-right: %s;\">"
                                                             width (if (and bar more) (format "4px solid %s" colour) "none"))))
                           (chunks (split-string contents column-break)))
                       (format "<div style=\"display: flex; justify-content: space-between; align-items: flex-start;\">%s</div>"
                               (concat (car columns)
                                       (car chunks)
                                       (cl-loop for column in (cdr columns)
                                                for chunk in (cdr chunks)
                                                concat (format "@@html:</div>%s@@%s" column chunk))
                                       "</div>")))))))

(provide 'org-parallel-block)
;;; org-parallel-block.el ends here
