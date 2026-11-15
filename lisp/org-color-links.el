;;; org-color-links.el --- Color blocks for org-special-block-extras  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.11.14
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, colors, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; The generic `color' block and 19 named color blocks (red, blue, green, …).
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'org-defblock)

;;;;;; Color  ---Load support for 19 named color blocks and links, and a generic one

(defvar org--named-colors
  '(black blue brown cyan darkgray gray green lightgray lime
     magenta olive orange pink purple red teal violet white
          yellow)
  "The colors that are each a block and a link; e.g., #+begin_red and red:𝒳.  Each is named alike in HTML and LaTeX.")

(defun org--color-text (backend color text)
  "(BACKEND, COLOR, TEXT) ↦ TEXT shown in COLOR; LaTeX via \\color, and HTML (the default) via a styled span.
In LaTeX, a COLOR beyond xcolor's own `org--named-colors', such as #ff0066 or navy, is named first."
  (pcase backend
    ('latex (if (memq (intern (format "%s" color)) org--named-colors)
                (format "\\begingroup\\color{%s}%s\\endgroup\\," color text)
              (format "%s\\begingroup\\color{osbe-color}%s\\endgroup\\,"
                      (org--latex-define-color "osbe-color" color) text)))
    (_ (format "<span style=\"color:%s;\">%s</span>" color text))))

(dolist (color org--named-colors)
  (eval `(org-defblock ,color (&allow-other-keys :link [:face '(:foreground ,(symbol-name color))]
                                 :latex-head ("xcolor") :manual "#Colours-links" &context backend contents)
           ,(format "Show text in %s color." color)
           (org--color-text backend ',color contents)))
  (eval `(org-defblock-attribute ,color :example ,(format "
# A block colors its every paragraph, *markup* and all.
#+begin_%1$s
Lo, a %1$s paragraph.
#+end_%1$s

# A link colors a word, %1$s:word; or a phrase, as [[%1$s:][these few words]].
Only the %1$s:key words are %1$s; Emacs hides “%1$s:”, until point is in the link." color)))
  (org-link-set-parameters (symbol-name color) :activate-func #'org--color-link-hide-type))

(defun org--color-link-hide-type (start end _path _bracket?)
  "Hide the “red:” of the color link from START to END, since its color says as much.
  [[red: hello world]]  ↦  hello world, in red; with point in it, `org--reveal-link-at-point' shows it in full.
As with Org's hidden brackets, `org-toggle-link-display' shows it all."
  (save-excursion
    (goto-char start)
    (when (and org-link-descriptive (re-search-forward "[a-z]+:[ \t]*" end t))
      (put-text-property start (point) 'invisible 'org-link)
      (org-rear-nonsticky-at (point)))))

(org-defblock color
              (&optional (color "black") &allow-other-keys :latex-head ("xcolor") :manual "#Colours-links"
               :link [:face (list :foreground (or (org-string-nw-p label) "black"))] &context backend contents)
              "Format text according to a given COLOR, which is black by default."
              (org--color-text backend color contents))

(org-defblock-attribute color :example "
# COLOR is any color HTML and LaTeX know by name, or a hex #RRGGBB.
#+begin_color #ff0066
A paragraph in hot pink.
#+end_color

# As a link, the color is its path, shown so in Emacs too: [[color:navy][in navy]].")

;;;;;; Highlight ---color the background, rather than the text

(org-defblock highlight
              (&optional (color "yellow") &allow-other-keys :latex-head ("xcolor") :manual "#Highlights"
               :link [:face (list :background (or (org-string-nw-p label) "yellow"))]
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

(org-defblock-attribute highlight :example "
# Sans COLOR, the background is yellow; here, a pale green.
#+begin_highlight #e5f5e5
A paragraph we should not miss.
#+end_highlight

# As a link, it highlights a phrase, in Emacs too: [[highlight:pink][Read this twice.]]")

;;;;;; Palette ---let readers pick the page's background color

(defvar org-palette-colors
  '(snow ghostwhite whitesmoke floralwhite oldlace linen antiquewhite papayawhip blanchedalmond bisque
    cornsilk ivory lemonchiffon seashell honeydew mintcream azure aliceblue lavender lavenderblush
    mistyrose beige lightyellow lightcyan white)
  "The pale CSS color names that a palette: link offers as page backgrounds.")

(org-deflink palette (_label &optional (description "🎨") backend)
  "A button, labelled DESCRIPTION, that unfolds a palette of `org-palette-colors'; clicking one makes it the page's background.
Only for HTML; elsewhere, nothing.

  [[palette:][Pick a background]]"
  :face (list :background (symbol-name (car (last org-palette-colors))) :box t)
  :manual "https://alhassy.com/org-special-block-extras#what-does-it-do"
  :export (if (not (equal backend 'html)) ""
            (format (concat "<button onclick=\"let p = this.nextElementSibling; p.style.display = p.style.display === 'none' ? 'inline' : 'none';\">%s</button>"
                            "<span style=\"display: none\">%s</span>")
                    description
                    (mapconcat (lambda (c) (format "<button style=\"background-color: %s\" onclick=\"document.body.style.background = '%s'\">%s</button>" c c c))
                               org-palette-colors ""))))

(provide 'org-color-links)
;;; org-color-links.el ends here
