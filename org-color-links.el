;;; org-color-links.el --- Color blocks for org-special-block-extras  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 4.1.1
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "27.1") (org "9.1"))
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

(provide 'org-color-links)
;;; org-color-links.el ends here
