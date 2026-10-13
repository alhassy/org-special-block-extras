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

;; The generic `color' block and 20 named colour blocks (red, blue, green, …).
;;
;; Full documentation at https://alhassy.github.io/org-special-block-extras

;;; Code:

(require 'org-defblock)

;;;;;; Color  ---Load support for 20 colour custom blocks and 20 colour link types

(defvar org--ospe-colors
  '(black blue brown cyan darkgray gray green lightgray lime
     magenta olive orange pink purple red teal violet white
          yellow)
  "Colours that should be available on all systems.")

(cl-loop for colour in org--ospe-colors
   do (eval `(org-defblock ,colour
                                 (the-color "black")
                                 (vector :face `(:foreground ,(format "%s" (quote ,colour))))
                                 ,(format "Show text in %s color." colour)
                                 (let ()
                                   (format (pcase backend
                                             (`latex "\\begingroup\\color{%s}%s\\endgroup\\,")
                                             (_  "<span style=\"color:%s;\">%s</span>"))
                                           (quote ,colour) contents)))))

(org-defblock color
              (color "black")
              (vector :face (lambda (colour) `(:foreground ,(format "%s" colour))))
              "Format text according to a given COLOR, which is black by default."
              (format (pcase backend
                        (`latex "\\begingroup\\color{%s}%s\\endgroup\\,")
                        (`html  "<span style=\"color:%s;\">%s</span>"))
                      color contents))

(provide 'org-color-links)
;;; org-color-links.el ends here
