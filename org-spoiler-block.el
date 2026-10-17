;;; org-spoiler-block.el --- Spoilers: text hidden until hovered, or blanks to fill in  -*- lexical-binding: t; -*-

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

;; A spoiler block hides its ((parenthesised)) text: in HTML until hovered; in LaTeX as blanks to fill in.
;;
;; Full documentation at https://alhassy.github.io/org-special-block-extras

;;; Code:

(require 'org-defblock)

;;;;;; spoiler
(org-defblock spoiler (color "grey" left "((" right "))")
              "Hide text enclosed in double parens ((like this)) as if it were spoilers.
   LEFT and RIGHT may be other kinds of delimiters.
   The main argument, COLOR, indicates which color to use.

For LaTeX, this becomes “fill in the blanks”, with the answers
in the footnotes; for Markdown, whose renderers strip <style>, numbered
blanks with the answers folded away beneath them."
              (cond
               ((org-export-markdown-p)
                (let* ((answers nil)
                       (blanked (replace-regexp-in-string
                                 (concat (regexp-quote left) "\\(.*?\\)" (regexp-quote right))
                                 (lambda (it)
                                   (push (match-string 1 it) answers)
                                   (format "@@html:<ins>&emsp;%s&emsp;</ins>@@" (length answers)))
                                 raw-contents t t)))
                  (org-parse (format "%s\n\n#+html: <details><summary>Answers</summary>\n\n%s\n\n#+html: </details>"
                                     blanked
                                     (s-join "\n" (--map-indexed (format "%s. %s" (1+ it-index) it) (reverse answers)))))))
               ((equal backend 'latex)
                (s-replace-regexp
                 (concat (regexp-quote left) "\\(.*?\\)" (regexp-quote right))
                 "@@latex:\\\\fbox{\\\\phantom{\\1}}\\\\footnote{\\1}@@"
                 contents))
               (t
                ;; The styling depends only on COLOR, so a class per colour serves every spoiler of that colour.
                (-let [class (format "spoiler-%s" color)]
                  (concat
                   ;; In HTML, a ‘style’ can be, technically, almost anywhere...
                   (format "<style> .%s {color: %s; background-color:%s;} .%s:hover {color: black; background-color:white;} </style>\n"
                           class color color class)
                   (s-replace-regexp
                    (concat (regexp-quote left) "\\(.*?\\)" (regexp-quote right))
                    (format "@@html:<span class=\"%s\"> \\1 </span>@@" class)
                    contents))))))

(provide 'org-spoiler-block)
;;; org-spoiler-block.el ends here
