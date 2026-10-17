;;; org-substitution-blocks.el --- Textual substitution: stutter and rename  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.16
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.github.io/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A stutter block repeats its contents; a rename block performs substitutions on them.  For substitution
;; on any block at all, see the :let switch, `org--block-let'.
;;
;; Full documentation at https://alhassy.github.io/org-special-block-extras

;;; Code:

(require 'org-defblock)

;;;;;; stutter
(org-defblock stutter (reps 2)
              "Output the CONTENTS of the block REPS many times"
              (-let [num (if (numberp reps) reps (string-to-number reps))]
                (s-repeat num contents)))

;;;;;; rename
(org-defblock rename (list "")
              "Perform the given LIST of substitutions on the text.
The LIST is a comma separated list of ‘to’ separated symbols.
In a link, no quotes are needed.

Substitution is handy enough that every block has it: the :let switch, much as a src block's :var, replaces
whole words and operators by the values of Lisp forms; e.g., #+begin_box :let (Yusuf \"Joseph\" -> \"⇒\")."
              (s-replace-all
               (--map (cons (car it) (cadr it))
                      (--map (s-split " to " (s-trim it))
                             (s-split "," list)))
               contents))

(provide 'org-substitution-blocks)
;;; org-substitution-blocks.el ends here
