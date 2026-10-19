;;; org-substitution-blocks.el --- Textual substitution: stutter  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.18
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A stutter block repeats its contents.  For substitution on any block at all, see the :let switch,
;; `org--block-let'.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'org-defblock)

;;;;;; stutter
(org-defblock stutter (reps 2 :manual "https://alhassy.com/org-special-block-extras#what-does-it-do")
              "Output the CONTENTS of the block REPS many times"
              (-let [num (if (numberp reps) reps (string-to-number reps))]
                (s-repeat num contents)))

(provide 'org-substitution-blocks)
;;; org-substitution-blocks.el ends here
