;;; org-box-block.el --- The `box' block and org-subtle-colors  -*- lexical-binding: t; -*-

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

;; The `box' special block (styled div in HTML, tcolorbox in LaTeX)
;; and the `org-subtle-colors' colour-name→hex helper.
;;
;; Full documentation at https://alhassy.github.io/org-special-block-extras

;;; Code:

(require 'org-defblock)

;;;;;; box

(defun org--box-shadow (shadow)
  "SHADOW, as given to the `box' block, ↦ its CSS; nil when SHADOW is.

A single colour sits behind the box; otherwise up to four colours, as a list or a
plist with keys :left, :right, :deep-right and :deep-left, spread with blur around
it ---or within it, when prefixed with ‘inset’.  In CSS, “box-shadow: SPEC, …”
has each SPEC of the shape “[inset] x_offset y_offset [blur [spread]] color”."
  (let ((centred "width: 50%; margin: auto"))
    (cond
     ((null shadow) nil)
     ((and (atom shadow) (not (memq shadow '(t inset))))
      (format "box-shadow: 10px 10px 20px 0px %s; %s" shadow centred))
     (t
      (-let [(left right deep-right deep-left)
             (cond ((memq shadow '(t inset))
                    (--map (concat (if (eq shadow 'inset) "inset " "") it)
                           '("hsl(60, 100%, 50%)" "hsl(1, 100%, 50%)" "hsl(180, 100%, 50%)")))
                   ((keywordp (car shadow)) (--map (plist-get shadow it) '(:left :right :deep-right :deep-left)))
                   (t shadow))]
        (format "%s; box-shadow: %s" centred
                (s-join "," (-non-nil (list (and right      (format "8px 6px 13px 8px %s" right))
                                            (and left       (format "-16px 12px 20px 16px %s" left))
                                            (and deep-right (format "48px 36px 71px 28px %s" deep-right))
                                            (and deep-left  (format "-48px -20px 71px 28px %s" deep-left)))))))))))

(org-defblock box (title "" background-color nil shadow nil frame-color nil title-background-color nil)
              "Enclose text in a box, possibly with a TITLE.

By default, the box's BACKGROUND-COLOR is green for HTML and red for LaTeX;
for LaTeX, there are also FRAME-COLOR and TITLE-BACKGROUND-COLOR.

SHADOW is an alternate style of boxing: It shows the contents in a centred
box with a shadow colour being the given non-nil value of SHADOW.
If SHADOW is a hexadecimal colour, it should be a string enclosed in double quotes.
More accurately, the following are all valid uses:

    #+begin_box title :shadow t
    #+begin_box title :shadow inset

    #+begin_box title :shadow \"pink\"
    #+begin_box title :shadow pink

    #+begin_box title :shadow (cyan pink orange yellow)
    #+begin_box title :shadow (cyan \"inset pink\" orange)
    #+begin_box title :shadow (:left cyan :right pink :deep-right orange :deep-left yellow)

Notice that prefixing colours with ‘inset’ causes the colour to be within the box,
rather than spread, with blur, around the box. The use of ‘inset’ swaps left and right.

The HTML export uses a padded div, whereas the LaTeX export
requires the tcolorbox package; Markdown, sans colours, gets a quote opening with TITLE in bold."
              (pcase (if (org-export-markdown-p) 'md backend)
                ('md (org-parse (format "#+begin_quote\n%s%s\n#+end_quote"
                                        (if (org-string-nw-p (format "%s" title)) (format "*%s*\n\n" title) "")
                                        raw-contents)))
                ;; Colours are named first, since tcolorbox options cannot take hex codes such as #CCFFCC.
                ('latex
                 (concat (org--latex-define-color "osbe-box-back" (or background-color "red!5!white"))
                         (org--latex-define-color "osbe-box-frame" (or frame-color "red!75!black"))
                         (org--latex-define-color "osbe-box-title" (or title-background-color "yellow!50!red"))
                         (format (concat "\\begin{tcolorbox}[title={%s},colback=osbe-box-back,colframe=osbe-box-frame,"
                                         "colbacktitle=osbe-box-title,coltitle=red!25!black, fonttitle=\\bfseries,"
                                         "subtitle style={boxrule=0.4pt, colback=yellow!50!red!25!white}]"
                                         "%s\\end{tcolorbox}")
                                 title contents)))
                (_ (format "<div style=\"%s\"> <h3>%s</h3> %s </div>"
                           (s-join ";" (list "padding: 1em"
                                             (format "background-color: %s" (org-subtle-colors (format "%s" (or background-color "green"))))
                                             "border-radius: 15px"
                                             "font-size: 0.9em"
                                             (org--box-shadow shadow)))
                           title contents))))

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
    ("lime"    "#CCFF99") ;; brighter than ‘green’
    ("green"   "#CCFFCC")
    ("blue"    "#CCFFFF")
    ("orange"  "#FFCC99")
    ("peach"   "#FFCCCC")
    ("pink"    "#FFCCFF")
    ("yellow"  "#FFFF99")
    ("custard" "#FFFFCC") ;; paler than ‘yellow’
    (_ c)))

(provide 'org-box-block)
;;; org-box-block.el ends here
