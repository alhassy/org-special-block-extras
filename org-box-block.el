;;; org-box-block.el --- The `box' block and org-subtle-colors  -*- lexical-binding: t; -*-

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

;; The `box' special block (styled div in HTML, tcolorbox in LaTeX)
;; and the `org-subtle-colors' colour-name→hex helper.
;;
;; Full documentation at https://alhassy.github.io/org-special-block-extras

;;; Code:

(require 'org-defblock)

;;;;;; box
(org-defblock box (title "" background-color nil shadow nil frame-color nil title-background-color nil)
              "Enclose text in a box, possibly with a title.

By default, the box's COLOR is green for HTML and red for LaTeX,
and it has no TITLE.

SHADOW is an alternate style of boxing: It shows the contents in a centred
box with a shadow colour being the given non-nil value of SHADOW.
If SHADOW is a hexidecimal colour, it should be a string enclosed in double quotes.
More accurately, the following are all valid uses:

    #+begin_box title :shadow t
    #+begin_box title :shadow inset

    #+begin_box title :shadow \"pink\"
    #+begin_box title :shadow pink

    #+begin_box title :shadow (cyan pink orange yellow)
    #+begin_box title :shadow (cyan \"inset pink\" orange)
    #+begin_box title :shadow (:left cyan :right pink :deep-right orange :deep-left yellow)

Notice that prefixing colours with 'inset' causes the colour to be within the box,
rather than spread, with blur, around the box. The use of 'inset' swaps left and right.

The HTML export uses a padded div, whereas the LaTeX export
requires the tcolorbox package.

In the future, I will likely expose more arguments."

              (pcase backend
                (`latex
                 (apply #'concat
                        `("\\begin{tcolorbox}[title={" ,title "}"
                          ",colback=" ,(pp-to-string (or background-color 'red!5!white))
                          ",colframe=" ,(pp-to-string (or frame-color 'red!75!black))
                          ",colbacktitle=" ,(pp-to-string (or title-background-color 'yellow!50!red))
                          ",coltitle=red!25!black, fonttitle=\\bfseries,"
                          "subtitle style={boxrule=0.4pt, colback=yellow!50!red!25!white}]"
                          ,contents
                          "\\end{tcolorbox}")))
                ;; CSS syntax: "box-shadow: specification, specification, ..."
                ;; where a specification is of the shape "[inset] x_offset y_offset [blur [spread]] color".
                (_ (-let [haze (lambda (left right deep-right deep-left)
                                 (format "width: 50%%; margin: auto; box-shadow: %s"
                                         (thread-last (list (cons right      "8px 6px 13px 8px %s")
                                                            (cons left       "-16px 12px 20px 16px %s")
                                                            (cons deep-right "48px 36px 71px 28px %s")
                                                            (cons deep-left  "-48px -20px 71px 28px %s"))
                                                      (--filter (car it))
                                                      (--map (format (cdr it) (car it)))
                                                      (s-join ","))))]
                     (format "<div style=\"%s\"> <h3>%s</h3> %s </div>"
                             (s-join ";" `("padding: 1em"
                                           ,(format "background-color: %s" (org-subtle-colors (format "%s" (or background-color "green"))))
                                           "border-radius: 15px"
                                           "font-size: 0.9em"
                                           ,(when shadow
                                              (cond
                                               ((equal shadow t)
                                                (funcall haze "hsl(60, 100%, 50%)" "hsl(1, 100%, 50%)" "hsl(180, 100%, 50%)" nil))
                                               ((equal shadow 'inset)
                                                (funcall haze "inset hsl(60, 100%, 50%)" "inset hsl(1, 100%, 50%)" "inset hsl(180, 100%, 50%)" nil))
                                               ((or (stringp shadow) (symbolp shadow))
                                                (format "box-shadow: 10px 10px 20px 0px %s; width: 50%%; margin: auto" (pp-to-string shadow)))
                                               ((json-plist-p shadow)
                                                (-let [(&plist :left X :right Y :deep-right Z :deep-left W) shadow]
                                                  (funcall haze X Y Z W)))
                                               (:otherwise (-let [(X Y Z W) shadow]
                                                             (funcall haze X Y Z W)))))))
                             title contents)))))

;;;;;;; org-subtle-colors
(defun org-subtle-colors (c)
  "HTML codes for common colours.

Names are very rough approximates.

   Translations from: https://www.december.com/html/spec/softhues.html"
  (pcase c
    ("teal"    "#99FFCC") ;; close to aqua
    ("brown"   "#CCCC99") ;; close to moss
    ("gray"    "#CCCCCC")
    ("purple"  "#CCCCFF")
    ("lime"    "#CCFF99") ;; brighter than 'green'
    ("green"   "#CCFFCC")
    ("blue"    "#CCFFFF")
    ("orange"  "#FFCC99")
    ("peach"   "#FFCCCC")
    ("pink"    "#FFCCFF")
    ("yellow"  "#FFFF99")
    ("custard" "#FFFFCC") ;; paler than 'yellow'
    (c c)
    ))

(provide 'org-box-block)
;;; org-defblock-box.el ends here
