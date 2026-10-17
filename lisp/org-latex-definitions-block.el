;;; org-latex-definitions-block.el --- Declare LaTeX macros, shown in no export  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.17
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A latex-definitions block declares macros: in LaTeX, in the preamble; in HTML, for MathJax, hidden.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'org-defblock)

(org-defblock latex-definitions nil
              "Declare but do not display the CONTENTS according to the BACKEND.
In LaTeX, they are declarations, so they go to the preamble, as #+latex_header: lines; within a beamer frame, which
is read into a definition of its own, a macro's #1 would otherwise be an error."
              (cond
               ;; Markdown renderers strip the HTML's display:none, which would leave the definitions on show.
               ((org-export-markdown-p) "")
               ((equal backend 'latex)
                (org-parse (mapconcat (lambda (line) (concat "#+latex_header: " line)) (split-string raw-contents "\n" t) "\n")))
               (t (format "<p style=\"display:none\">\\[%s\\]</p>" raw-contents))))

(provide 'org-latex-definitions-block)
;;; org-latex-definitions-block.el ends here
