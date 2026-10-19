;;; org-color-links.el --- Color blocks for org-special-block-extras  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.18
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, colors, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; The generic `color' block and 19 named colour blocks (red, blue, green, …).
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'org-defblock)

;;;;;; Color  ---Load support for 19 named colour blocks and links, and a generic one

(defvar org--named-colors
  '(black blue brown cyan darkgray gray green lightgray lime
     magenta olive orange pink purple red teal violet white
          yellow)
  "The colours that are each a block and a link; e.g., #+begin_red and red:𝒳.  Each is named alike in HTML and LaTeX.")

(defun org--color-text (backend colour text)
  "(BACKEND, COLOUR, TEXT) ↦ TEXT shown in COLOUR; LaTeX via \\color, and HTML (the default) via a styled span.
In LaTeX, a COLOUR beyond xcolor's own `org--named-colors', such as #ff0066 or navy, is named first."
  (pcase backend
    ('latex (if (memq (intern (format "%s" colour)) org--named-colors)
                (format "\\begingroup\\color{%s}%s\\endgroup\\," colour text)
              (format "%s\\begingroup\\color{osbe-color}%s\\endgroup\\,"
                      (org--latex-define-color "osbe-color" colour) text)))
    (_ (format "<span style=\"color:%s;\">%s</span>" colour text))))

(dolist (colour org--named-colors)
  (eval `(org-defblock ,colour (:link [:face '(:foreground ,(symbol-name colour))] :latex-head ("xcolor")
                                 :manual "#Colours-links" &context backend contents)
           ,(format "Show text in %s color." colour)
           (org--color-text backend ',colour contents)))
  (org-link-set-parameters (symbol-name colour) :activate-func #'org--color-link-hide-type))

(defun org--color-link-hide-type (start end _path _bracket?)
  "Hide the “red:” of the colour link from START to END, since its colour says as much.
  [[red: hello world]]  ↦  hello world, in red; with point in it, `org--reveal-link-at-point' shows it in full.
As with Org's hidden brackets, `org-toggle-link-display' shows it all."
  (save-excursion
    (goto-char start)
    (when (and org-link-descriptive (re-search-forward "[a-z]+:[ \t]*" end t))
      (put-text-property start (point) 'invisible 'org-link)
      (org-rear-nonsticky-at (point)))))

(org-defblock color
              (color "black" :latex-head ("xcolor") :manual "#Colours-links" :link [:face (list :foreground (or (org-string-nw-p o-label) "black"))]
               &context backend contents)
              "Format text according to a given COLOR, which is black by default."
              (org--color-text backend color contents))

;;;;;; Highlight ---colour the background, rather than the text

(org-defblock highlight
              (color "yellow" :latex-head ("xcolor") :manual "#Highlights" :link [:face (list :background (or (org-string-nw-p o-label) "yellow"))]
               &context backend raw-contents contents link?)
              "Highlight text with a background of COLOR, which is yellow by default; as a link, it is so highlighted in Emacs too.

  [[highlight:pink][Read this twice.]]

For LaTeX, a hex COLOR such as #e5f5e5 needs no quotes; a block's text is a full-width paragraph box.
In Markdown, the text is <mark>ed, paragraph by paragraph, which GitHub shows yellow whatever COLOR is."
              (pcase (if (org-export-markdown-p) 'md backend)
                ('md (if link?
                         (format "<mark>%s</mark>" contents)
                       (org-parse (format "@@html:<mark>@@%s@@html:</mark>@@"
                                          (s-replace-regexp "\n\n+" "@@html:</mark>@@\n\n@@html:<mark>@@" (s-trim raw-contents))))))
                ('latex (concat (org--latex-define-color "osbe-highlight" color)
                                (if link?
                                    (format "\\colorbox{osbe-highlight}{%s}" contents)
                                  (format "\\colorbox{osbe-highlight}{\\parbox{\\dimexpr\\linewidth-2\\fboxsep}{%s}}" contents))))
                ;; A block's text is paragraphs, which a <span> may not hold.
                (_ (format (if link?
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
  :face (list :background (symbol-name (car (last org-palette-colors))) :box t)
  :manual "https://alhassy.com/org-special-block-extras#what-does-it-do"
  :export (if (not (equal o-backend 'html)) ""
            (format (concat "<button onclick=\"let p = this.nextElementSibling; p.style.display = p.style.display === 'none' ? 'inline' : 'none';\">%s</button>"
                            "<span style=\"display: none\">%s</span>")
                    description
                    (mapconcat (lambda (c) (format "<button style=\"background-color: %s\" onclick=\"document.body.style.background = '%s'\">%s</button>" c c c))
                               org-palette-colors ""))))

(provide 'org-color-links)
;;; org-color-links.el ends here
