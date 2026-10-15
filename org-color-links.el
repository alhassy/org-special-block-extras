;;; org-color-links.el --- Color blocks for org-special-block-extras  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.15
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "27.1") (org "9.6"))
;; Keywords: org, blocks, colors, convenience
;; URL: https://alhassy.github.io/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; The generic `color' block and 19 named colour blocks (red, blue, green, …).
;;
;; Full documentation at https://alhassy.github.io/org-special-block-extras

;;; Code:

(require 'org-defblock)

;;;;;; Color  ---Load support for 19 named colour blocks and links, and a generic one

(defvar org--ospe-colors
  '(black blue brown cyan darkgray gray green lightgray lime
     magenta olive orange pink purple red teal violet white
          yellow)
  "Colours that should be available on all systems.")

(defun org--color-text (backend colour text)
  "(BACKEND, COLOUR, TEXT) ↦ TEXT shown in COLOUR; LaTeX via \\color, and HTML (the default) via a styled span."
  (format (pcase backend
            ('latex "\\begingroup\\color{%s}%s\\endgroup\\,")
            (_      "<span style=\"color:%s;\">%s</span>"))
          colour text))

(dolist (colour org--ospe-colors)
  (eval `(org-defblock ,colour (:link [:face '(:foreground ,(symbol-name colour))])
           ,(format "Show text in %s color." colour)
           (org--color-text backend ',colour contents))))

(org-defblock color
              (color "black" :link [:face (list :foreground (or (org-string-nw-p o-label) "black"))])
              "Format text according to a given COLOR, which is black by default."
              (org--color-text backend color contents))

;;;;;; Highlight ---colour the background, rather than the text

(org-defblock highlight
              (color "yellow" :link [:face (list :background (or (org-string-nw-p o-label) "yellow"))])
              "Highlight text with a background of COLOR, which is yellow by default; as a link, it is so highlighted in Emacs too.

  [[highlight:pink][Read this twice.]]

For LaTeX, a hex COLOR such as #e5f5e5 needs no quotes; a block's text is a full-width paragraph box."
              (pcase backend
                ('latex (concat (org--latex-define-color "osbe-highlight" color)
                                (if contents-occur-as-link-description
                                    (format "\\colorbox{osbe-highlight}{%s}" contents)
                                  (format "\\colorbox{osbe-highlight}{\\parbox{\\dimexpr\\linewidth-2\\fboxsep}{%s}}" contents))))
                ;; A block's text is paragraphs, which a <span> may not hold.
                (_ (format (if contents-occur-as-link-description
                               "<span style=\"background-color:%s;\">%s</span>"
                             "<div style=\"background-color:%s;\">%s</div>")
                           color contents))))

;;;;;; Palette ---let readers pick the page's background colour

(defvar org-palette-colors
  '(snow ghostwhite whitesmoke floralwhite oldlace linen antiquewhite papayawhip blanchedalmond bisque
    cornsilk ivory lemonchiffon seashell honeydew mintcream azure aliceblue lavender lavenderblush
    mistyrose beige lightyellow lightcyan white)
  "The pale CSS colour names that a palette: link offers as page backgrounds.")

(org-deflink palette (_label &optional (description "🎨"))
  "A button, labelled DESCRIPTION, that unfolds a palette of `org-palette-colors'; clicking one makes it the page's background.
Only for HTML; elsewhere, nothing.

  [[palette:][Pick a background]]"
  :face (list :background (car (last org-palette-colors)) :box t)
  :export (if (not (equal o-backend 'html)) ""
            (format (concat "<button onclick=\"let p = this.nextElementSibling; p.style.display = p.style.display === 'none' ? 'block' : 'none';\">%s</button>"
                            "<div style=\"display: none\">%s</div>")
                    description
                    (mapconcat (lambda (c) (format "<button style=\"background-color: %s\" onclick=\"document.body.style.background = '%s'\">%s</button>" c c c))
                               org-palette-colors ""))))

(provide 'org-color-links)
;;; org-color-links.el ends here
