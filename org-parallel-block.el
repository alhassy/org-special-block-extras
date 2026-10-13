;;; org-parallel-block.el --- The `parallel' multi-column block  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 4.1.1
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "27.1") (org "9.1"))
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

There are COLS many columns, and they may be seperated by solid
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
              (let ((rule (pcase backend
                            (`latex (if bar 2 0))
                            (_  (format "%s %s" (if bar "solid" "none") (if (string= bar "t") "black" bar)))))
                    (contents′ (s-replace "#+columnbreak:" "\\columnbreak" contents)))
                (pcase backend
                  (`latex   (format  "\\par \\setlength{\\columnseprule}{%s pt}
          \\begin{minipage}[t]{\\linewidth}
          \\begin{multicols}{%s}
          %s
          \\end{multicols}\\end{minipage}"    rule cols contents′))
                  (_ (if (not (s-contains-p "%" cols))
                         (format "<div style=\"column-rule-style: %s;column-count: %s;\">%s</div>"
                                 rule cols contents)
                       ;; Otherwise: cols ≈ "10% 40% 50%", for example.
                       (let ((spec (s-split " " (s-collapse-whitespace (s-trim cols))))
                             (columnBreak (lambda (width omit-rule?)
                                            (format "<div style=\"width: %s; margin: 10px; border-right:4px %s; float:  left;\">" width
                                                    (if omit-rule? "none" rule)))) )
                         (format "<div style=\"display: flex; justify-content: space-between; align-items: flex-start;\">%s%s%s</div>"
                                 (funcall columnBreak (pop spec) nil)
                                 (s-replace-regexp (regexp-quote "#+columnbreak:")
                                                   ;; 'λ' since we need the "pop" evaluated for each find-replace instance.
                                                   ;; We use “not spec” to omit the rule separator when there is NOT anymore elements in SPEC.
                                                   (lambda (_) (format "@@html:</div>%s@@" (funcall columnBreak (pop spec) (not spec))))
                                                   contents)
		                         (if (s-contains-p " " cols) "</div>" ""))))))))

(provide 'org-parallel-block)
;;; org-parallel-block.el ends here
