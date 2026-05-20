;;; org-details-block.el --- The `details' foldable block  -*- lexical-binding: t; -*-

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

;; The `details' special block: a foldable HTML <details> element,
;; and a tcolorbox for LaTeX.
;;
;; Full documentation at https://alhassy.github.io/org-special-block-extras

;;; Code:

(require 'org-defblock)

;;;;;; details
(org-defblock details (title "Details"
                             background-color "#e5f5e5" title-color "green")
              "Enclose contents in a folded up box, for HTML.

For LaTeX, this is just a boring, but centered, box.

By default, the TITLE of such blocks is 'Details',
its TITLE-COLOR is green, and BACKGROUND-COLOR is '#e5f5e5'.

In HTML, we show folded, details, regions with a nice greenish colour.

In the future ---i.e., when I have time---
it may be prudent to expose more aspects as arguments.
"
              (pcase backend
                (`latex (concat (pcase (substring background-color 0 1)
                                  ("#" (format "\\definecolor{osbe-bg}{HTML}{%s}" (substring background-color 1)))
                                  (_ (format "\\colorlet{osbe-bg}{%s}" background-color)))
                                (pcase (substring title-color 0 1)
                                  ("#" (format "\\definecolor{osbe-fg}{HTML}{%s}" (substring title-color 1)))
                                  (_ (format "\\colorlet{osbe-fg}{%s}" title-color)))
                                (format "\\begin{quote}
                              \\begin{tcolorbox}[colback=osbe-bg,colframe=osbe-fg,title={%s},sharp corners,boxrule=0.4pt]
                                   %s
                               \\end{tcolorbox}
                \\end{quote}" title contents)))
                (_ (format "<details class=\"code-details\"
                 style =\"padding: 1em;
                          background-color: %s;
                          border-radius: 15px;
                          color: hsl(157 75% 20%);
                          font-size: 0.9em;
                          box-shadow: 0.05em 0.1em 5px 0.01em  #00000057;\">
                  <summary>
                    <strong>
                      <font face=\"Courier\" size=\"3\" color=\"%s\">
                         %s
                      </font>
                    </strong>
                  </summary>
                  %s
               </details>" background-color title-color title contents))))

(provide 'org-details-block)
;;; org-details-block.el ends here
