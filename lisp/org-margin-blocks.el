;;; org-margin-blocks.el --- Notes beside the text: margin notes, tooltips, and footnotes as either  -*- lexical-binding: t; -*-

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

;; The margin block typesets its contents in the margin in LaTeX, and as a tooltip in HTML; the
;; tooltip block, the other way about; and Org's own footnotes become the one or the other.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'org-defblock)
(require 'org-links)  ;; for org--html-tooltip

;;;;;; Margin (LaTeX)

(defun org--latex-margin-note (marker width color contents &optional step sans-mark)
  "(MARKER, WIDTH, COLOR, CONTENTS, STEP, SANS-MARK) ↦ LaTeX for MARKER in the text, and CONTENTS in the margin under
MARKER, WIDTH wide and COLOR coloured; STEP, such as \\stepcounter{footnote}, runs first, so a MARKER of \\thefootnote
is the new number.  SANS-MARK omits MARKER from the text, which has marked the spot itself; e.g., within maths.
CONTENTS is saved in a minipage, then unfolded in the margin, since verbatim environments, such as src blocks', fail
as an argument of \\marginpar."
  ;; Outside the minipage, a newline is a space in the running text; so the commands there abut.  Within it, the
  ;; contents are \\raggedright, not fully justified, and minted, when loaded, is shrunk to fit ---Hack!
  (apply #'format
         (concat "%s" (unless sans-mark "${}^{\\textnormal{%s}}$")
                 "\\newsavebox{\\OrgSpecialBlockExtrasMarginBox}"
                 "\\begin{lrbox}{\\OrgSpecialBlockExtrasMarginBox}\\begin{minipage}{%s}\n"
                 "\\raggedright\\footnotesize\\ifdefined\\setminted\\setminted{fontsize=\\footnotesize, breaklines}\\fi\\color{%s}\n"
                 "{\\color{black}${}^{\\textnormal{%s}}$}\n\\normalfont\n%s\n"
                 "\\end{minipage}\\end{lrbox}\\marginpar{\\usebox{\\OrgSpecialBlockExtrasMarginBox}}"
                 ;; The {} keeps the space after us, which TeX would otherwise skip as it follows a command.
                 "\\global\\let\\OrgSpecialBlockExtrasMarginBox\\relax{}")
         (append (list (or step "")) (unless sans-mark (list marker)) (list width color marker contents))))

(org-defblock margin
              (marker nil
                      color "gray!80"
                      counter "footnote"
                      ;; The margin's own width; arithmetic such as “\\paperwidth - \\textwidth” needs the calc package.
                      width "\\marginparwidth"
                      :latex-head ("xcolor") :manual "#tooltip"
                      :link [:display 'full
                             :face '(:foreground "grey" :weight bold :underline "orange" :overline "orange")])
              "A remark, CONTENTS, on MARKER in the text: in HTML, a tooltip on MARKER, hovered on a desktop, tapped on a
phone; in LaTeX, MARKER followed by CONTENTS as a margin note, numbered by COUNTER, as footnotes are; in beamer, whose
slides have no margins, a footnote.  Sans MARKER, the note's number alone in LaTeX, and “°” in HTML.

  It's a                                   /Allah[[margin:][The God of Abraham]] does not burden a soul …/
  #+begin_tooltip simple
  Squint, and Maybe ℕ is ℕ.
  #+end_tooltip
  exercise.

The block is one-off, “inline footnotes”; Org's own [fn::…] are such remarks too.  For a note used repeatedly, consider
declaring it with ‘documentation’ and citing it with ‘doc’.

WIDTH, COUNTER, and COLOR are LaTeX's: the note is WIDTH wide, coloured COLOR, its number in black.  Since verbatim
environments fail as arguments to \\marginpar, CONTENTS is saved in a minipage within a LaTeX box, then unfolded in the
margin; hence, src blocks may appear within margin notes (•̀ᴗ•́)و  For LaTeX, a ‘#+resize:’ line has the rest of the
note resized, to 1.3 the margin's width ---requiring \\usepackage{adjustbox}.

`tooltip' is an alias, for when we think in HTML rather than LaTeX."
              (let ((marker (org-string-nw-p marker)))
                (pcase backend
                  ((guard (org-export-beamer-p)) (format "%s\\footnote{%s}" (or marker "") (s-trim contents)))
                  (`latex
                   (concat
                    ;; Sans MARKER, a block follows the newline, a space, after the word it remarks on; we swallow it.
                    (if (or marker contents-occur-as-link-description) marker "\\!\\!")
                    (org--latex-margin-note (format "{\\the%s}" counter) width color
                                           (if (s-contains? "#+resize:" contents)
                                               (s-concat
                                                (s-replace "#+resize:"
                                                           "#+latex: \\maxsizebox{1.3\\textwidth}{\\textheight}{\\begin{tabular}{l}\n"
                                                           (s-trim contents))
                                                "\n\\end{tabular}}")
                                             (s-trim contents))
                                           (format "\\refstepcounter{%s}" counter))))
                  ;; HTML has no margin per se, so the note is a tooltip on its marker.
                  (_ (concat (org--html-tooltip raw-contents (or marker "°")) "&emsp13;")))))

(org-defblock-alias tooltip margin)

;;;;;; Footnotes, as tooltips

(defun org--footnote-as-tooltip (footnote-reference &rest args)
  "Call FOOTNOTE-REFERENCE, `org-html-footnote-reference', with ARGS; then make its number a tooltip of the definition.
  A remark[fn::An aside.]  ↦  A remark<sup><abbr class=\"tooltip\" … id=\"fnr.1\">1<template>An aside.</template></abbr></sup>
The definition is exported by the export at hand, so its links and markup resolve as in the footnotes section, which
stays, for print and for readers sans script; the id stays too, so that section's links back still land.  Markdown,
which runs no script, keeps Org's own footnotes."
  (let ((html (apply footnote-reference args)))
    (-let [(reference _ info) args]
      (if (or (org-export-markdown-p)
              (not (string-match "<a id=\"\\([^\"]*\\)\" class=\"footref\"[^>]*>\\([^<]*\\)</a>" html)))
          html
        ;; Exporting the definition runs searches of its own, so we take this match's parts first.
        (let ((before (substring html 0 (match-beginning 0))) (after (substring html (match-end 0)))
              (id (match-string 1 html)) (number (match-string 2 html)))
          (concat before
                  (org--tooltip-abbr number
                                     (org-trim (org-export-data
                                                (org-element-contents (org-export-get-footnote-definition reference info))
                                                info))
                                     (format "id=\"%s\"" id))
                  after))))))

(defun org--footnotes-in-margin (info backend)
  "(INFO, BACKEND) ↦ INFO whose LaTeX footnotes are margin notes, as the margin block's are; e.g., A remark[fn::An aside.]
is typeset with “An aside.” in the margin, numbered as footnotes are.  A footnote command of the user's own, such as a
#+LATEX_FOOTNOTE_COMMAND: line, wins; as do Org's \\footnotemark, for footnotes in tables, and its references back."
  (when (and (org-export-derived-backend-p backend 'latex)
             (not (org-export-derived-backend-p backend 'beamer)) ; Slides have no margins.
             (equal (plist-get info :latex-default-footnote-command) "\\footnote{%s%s}"))
    ;; Org fills the two %s with the definition and its \label; stepping with \refstepcounter lets that \label name it.
    (plist-put info :latex-default-footnote-command
               (org--latex-margin-note "{\\thefootnote}" "\\marginparwidth" "gray!80" "%s%s" "\\refstepcounter{footnote}"))
    ;; A margin note is coloured; so, when there are footnotes, xcolor is loaded, as a block's :latex-head are.
    (when (save-excursion (goto-char (point-min)) (re-search-forward "\\[fn:" nil t))
      (cl-pushnew "xcolor" org--latex-head-used :test #'equal)))
  info)

(provide 'org-margin-blocks)
;;; org-margin-blocks.el ends here
