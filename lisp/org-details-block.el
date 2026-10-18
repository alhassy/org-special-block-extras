;;; org-details-block.el --- The `details' foldable block  -*- lexical-binding: t; -*-

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

;; The `details' special block: a foldable HTML <details> element,
;; and a tcolorbox for LaTeX.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'org-defblock)

;;;;;; details

(org-defblock details (title "Details" background-color "#e5f5e5" title-color "green"
                              :latex-head ("xcolor" "tcolorbox"))
              "Enclose contents in a folded up box, for HTML.

For LaTeX, this is just a boring, but centered, box.

By default, the TITLE of such blocks is “Details”,
its TITLE-COLOR is green, and BACKGROUND-COLOR is “#e5f5e5”.

In HTML, we show folded, details, regions with a nice greenish colour."
              (pcase backend
                ('latex (concat (org--latex-define-color "osbe-bg" background-color)
                                (org--latex-define-color "osbe-fg" title-color)
                                (format (concat "\\begin{quote}\n"
                                                "\\begin{tcolorbox}[colback=osbe-bg,colframe=osbe-fg,title={%s},sharp corners,boxrule=0.4pt]\n"
                                                "%s\n"
                                                "\\end{tcolorbox}\n"
                                                "\\end{quote}")
                                        title contents)))
                (_ (format (concat "<details class=\"code-details\" style=\"padding: 1em; background-color: %s; "
                                   "border-radius: 15px; color: hsl(157 75%% 20%%); font-size: 0.9em; "
                                   "box-shadow: 0.05em 0.1em 5px 0.01em #00000057;\">\n"
                                   "<summary><strong><font face=\"Courier\" size=\"3\" color=\"%s\">%s</font></strong></summary>\n"
                                   "%s\n"
                                   "</details>")
                           background-color title-color title contents))))

;;;;;; Toggle all details blocks

(org-deflink toggle-details (_label &optional (description "Unfold all details"))
  "A button, labelled DESCRIPTION, that unfolds every <details> on the page; pressed again, it folds them all.
Only for HTML; elsewhere, nothing.

  [[toggle-details:][Show me everything]]"
  :face '(:box t)
  :export (if (not (equal o-backend 'html)) ""
            (format (concat "<button onclick=\"let all = [...document.querySelectorAll('details')], open = all.some(d => !d.open);"
                            " all.forEach(d => d.open = open);\">%s</button>")
                    description)))

(provide 'org-details-block)
;;; org-details-block.el ends here
