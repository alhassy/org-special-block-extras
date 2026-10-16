;;; org-defblock-misc.el --- Miscellaneous special blocks  -*- lexical-binding: t; -*-

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

;; Miscellaneous special blocks: margin, tooltip, latex-definitions,
;; solution, org-demo, stutter, rename, spoiler, remark.
;;
;; Full documentation at https://alhassy.github.io/org-special-block-extras

;;; Code:

(require 'org-defblock)
(require 'org-links)  ;; for org--html-tooltip
(require 'org-box-block)    ;; solution uses box
(require 'org-details-block) ;; solution uses details
(require 'org-parallel-block) ;; org-demo uses parallel

;;;;;; Margin (LaTeX)

(defun org--latex-margin-note (marker width color contents &optional step)
  "(MARKER, WIDTH, COLOR, CONTENTS, STEP) ↦ LaTeX for MARKER in the text, and CONTENTS in the margin under MARKER, WIDTH
wide and COLOR coloured; STEP, such as \\stepcounter{footnote}, runs first, so a MARKER of \\thefootnote is the new number.
CONTENTS is saved in a minipage, then unfolded in the margin, since verbatim environments, such as src blocks', fail
as an argument of \\marginpar."
  ;; Outside the minipage, a newline is a space in the running text; so the commands there abut.  Within it, the
  ;; contents are \\raggedright, not fully justified, and minted, when loaded, is shrunk to fit ---Hack!
  (format (concat "%s${}^{\\textnormal{%s}}$"
                  "\\newsavebox{\\OrgSpecialBlockExtrasMarginBox}"
                  "\\begin{lrbox}{\\OrgSpecialBlockExtrasMarginBox}\\begin{minipage}{%s}\n"
                  "\\raggedright\\footnotesize\\ifdefined\\setminted\\setminted{fontsize=\\footnotesize, breaklines}\\fi\\color{%s}\n"
                  "{\\color{black}${}^{\\textnormal{%s}}$}\n\\normalfont\n%s\n"
                  "\\end{minipage}\\end{lrbox}\\marginpar{\\usebox{\\OrgSpecialBlockExtrasMarginBox}}"
                  ;; The {} keeps the space after us, which TeX would otherwise skip as it follows a command.
                  "\\global\\let\\OrgSpecialBlockExtrasMarginBox\\relax{}")
          (or step "") marker width color marker contents))

(org-defblock margin
              (marker nil
                      color "gray!80"
                      counter "footnote"
                      ;; The margin's own width; arithmetic such as “\\paperwidth - \\textwidth” needs the calc package.
                      width "\\marginparwidth"
                      :link [:display 'full
                             :face '(:foreground "grey" :weight bold :underline "orange" :overline "orange")])
              "Produce an HTML tooltip or a LaTeX margin note.

The ‘margin’ block is intended for “one-off” (mostly optional) remarks.

It is essentially “inline footnotes”.

For notes that you want to use repeatedly, in multiple articles
or in multiple locations in the same article, consider using
‘documentation’ to declare them and ‘doc’ to invoke them.

For LaTeX, place ‘#+resize:’ to have the remainder of a block be
resized, for now 1.3 the margin width ---requires \\usepackage{adjustbox}.

----------------------------------------------------------------------

WIDTH, COUNTER, and COLOR are LaTeX specific.

When no label, marker, is used for a marginal note, we rely
on a COUNTER, such as ‘footnote’ (default) or ‘sidenote.’
Since HTML has no margin per se, we use “∘” as default marker:
Users hover over it to read the marginal note.

Marginal notes have their labels, markers, in black
and the notes themselves have COLOR being grey!80.
In Emacs, margin links appear grey with an orange tinted border.

Regarding LaTeX, since verbatim environments do not in general work well
as arguments to other commands, such as ‘\\marginpar’, we save the contents
of the special block in a ‘minipage’ within a LaTeX ‘box’; then we can unfold such
a box in the margin. Hence, ‘src’ blocks can appear within ‘margin’ blocks (•̀ᴗ•́)و

The WIDTH argument is the width of the margin; i.e., the width of the underlying
minipage.

One could use \\maxsizebox{.25\\textwidth}{\\textheight}{ ... }
which only resizes the content if its natural size is larger than
the given 〈width〉 or 〈height〉.  We don't use this, since
maxsizebox does not natively allow linebreaks
(e.g., one would wrap contents in a tabular environment then use
‘\\\\’, two backslashes, to request a line break; but this
crashes if one wants to also use verbatim environments.)

In LaTeX, it may be useful to invoke ‘\\dotfill’."
              (-let [stepcounter (if marker "" (format "\\stepcounter{%s}" counter))]
                (pcase backend
                  ((guard (org-export-beamer-p)) (format "\\footnote{%s}" (s-trim contents)))
                  (`latex
                   ;; A block follows the newline, a space, after the word it remarks on; we swallow it.
                   (concat
                    "\\!\\!"
                    (org--latex-margin-note (or marker (format "{\\the%s}" counter)) width color
                                           (if (s-contains? "#+resize:" contents)
                                               (s-concat
                                                (s-replace "#+resize:"
                                                           "#+latex: \\maxsizebox{1.3\\textwidth}{\\textheight}{\\begin{tabular}{l}\n"
                                                           (s-trim contents))
                                                "\n\\end{tabular}}")
                                             (s-trim contents))
                                           stepcounter)))
                  ;; HTML has no margin per se, so the note is a tooltip on its marker.
                  (_ (concat (org--html-tooltip raw-contents (or marker "∘"))
                             "&emsp13;")))))

;;;;;; Tooltip (HTML)
(org-defblock tooltip (marker "°")
              "Show CONTENTS, as Org, in a tooltip on MARKER; hovered on a desktop, tapped on a phone.
In LaTeX, MARKER is followed by CONTENTS as a margin note, numbered as footnotes are, as the margin block's are;
in beamer, whose slides have no margins, as a footnote."
              (if (equal backend 'latex)
                  (concat marker (if (org-export-beamer-p)
                                     (format "\\footnote{%s}" contents)
                                   (org--latex-margin-note "{\\thefootnote}" "\\marginparwidth" "gray!80" contents
                                                           "\\refstepcounter{footnote}")))
                (concat (org--html-tooltip raw-contents marker) "&emsp13;")))

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
               (org--latex-margin-note "{\\thefootnote}" "\\marginparwidth" "gray!80" "%s%s" "\\refstepcounter{footnote}")))
  info)

(org-defblock latex-definitions nil
              "Declare but do not display the CONTENTS according to the BACKEND.
In LaTeX, they are declarations, so they go to the preamble, as #+latex_header: lines; within a beamer frame, which
is read into a definition of its own, a macro's #1 would otherwise be an error."
              (cond
               ;; Markdown renderers strip the HTML's display:none, which would leave the definitions on show.
               ((org-export-markdown-p) "")
               ((equal backend 'latex)
                (org-parse (mapconcat (lambda (line) (concat "#+latex_header: " line)) (split-string raw-contents "\n" t) "\n")))
               (t (format "<p style=\"display:none\">\\[%s\\]</p>" raw-contents))))

;;;;; Blocks
;;;;;; solution
(org-defblock solution
              (title "Solution" reprimand "Did you actually try? Maybe see the ‘hints’ above!"
                     really "Solution, for real")
              "Show the answers to a problem, but with a reprimand in case no attempt was made."
              (org-thread-blockcall raw-contents
                                    (details really :title-color "red")
                                    (box reprimand :background-color "blue")
                                    (details title)))

(org-defblock org-demo (nil nil source "Source" result "Result"
                      source-color "cyan" result-color "cyan"
                      style "parallel"
                            sep (if (equal backend 'html) "@@html:<p><br>@@" "\n\n\n\n")
                            )
              "Output the CONTENTS of the block as both parsed Org and unparsed.

Label the source text by SOURCE and the result text by RESULT

finally, the source-result fragments can be shown in a STYLE
that is either “parallel” (default) or “sequential”.

SEP is the separator; e.g., a rule ‘<hr>'.

In Markdown, the source is a fenced org block above its result, each under its label in bold.
"
              (if (org-export-markdown-p)
                  (org-parse (format "*%s*\n#+begin_src org\n%s\n#+end_src\n\n*%s*\n\n%s"
                                     source (org-escape-code-in-string raw-contents) result raw-contents))
                  (-let [text (concat
                               ;; Source; in LaTeX an Org example block, which Org typesets, and for which beamer's
                               ;; frame is marked fragile, as raw verbatim text would not be.
                               (thread-last (if (equal backend 'html)
                                                (org-export (format "<div ><pre class=\"src src-org\">%s</pre></div>"
                                                                    raw-contents))
                                              (format "#+begin_example\n%s\n#+end_example"
                                                      (org-escape-code-in-string raw-contents)))
                                            (org--blockcall box source :background-color source-color)
                                            org-export)
                               ;; Separator
                               sep
                               ;; Result
                               (thread-last raw-contents
                                            (org--blockcall box result :background-color result-color)
                                            org-export))]

                    (if (equal style "parallel")
                        (org--blockcall parallel "2" :bar nil text)
                      (concat "#+end_export\n" text "\n#+begin_export")))))

;;;;;; stutter
(org-defblock stutter (reps 2)
              "Output the CONTENTS of the block REPS many times"
              (-let [num (if (numberp reps) reps (string-to-number reps))]
                (s-repeat num contents)))

;;;;;; rename
(org-defblock rename (list "")
              "Perform the given LIST of substitutions on the text.
The LIST is a comma separated list of ‘to’ separated symbols.
In a link, no quotes are needed."
              (s-replace-all
               (--map (cons (car it) (cadr it))
                      (--map (s-split " to " (s-trim it))
                             (s-split "," list)))
               contents))

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

;;;;;; Remark

(defvar org-hide-editor-comments nil
  "Should editor comments be shown in the output or not.")

(org-defblock remark
              (editor "Editor Remark" color "black" signoff "" strong nil
                      :link [:face '(:foreground "red" :weight bold)])
              "Format CONTENTS as a first-class editor comment according to BACKEND.

The CONTENTS string has an optional switch: If it contains a line
with having only ‘#+replacewith:’, then the text preceding this
clause should be replaced by the text after it; i.e., this is
what the EDITOR (the person editing) intends and so we format the
replacement instruction (to the author) as such.

In Emacs, as links, editor remarks are shown with a bold red; but
the exported COLOR of a remark is black by default and it is not
STRONG ---i.e., bold---. There is an optional SIGNOFF message
that is appended to the remark.

Remarks are hidden from the export when `org-hide-editor-comments' is
non-nil.  For just one file, write “#+bind: org-hide-editor-comments t”
in it; that needs `org-export-allow-bind-keywords' set to t, which we
leave to you, since #+bind lets a document set any variable.

In Markdown, a remark block is GitHub's “> [!NOTE]” alert, titled EDITOR; as a link, it stays HTML."
              (cond
               (org-hide-editor-comments "")
               ((and (org-export-markdown-p) (not contents-occur-as-link-description))
                (-let [(this that) (s-split "\\#\\+replacewith:" raw-contents)]
                  (org-parse (org--markdown-alert "note" editor
                                                  (concat (if that (format "*Replace:* %s\n\n*With:* %s" (s-trim this) (s-trim that)) raw-contents)
                                                          (if (org-string-nw-p signoff) (format "\n\n%s" signoff) ""))))))
               (t
                (-let* (;; Are we in the html backend?
                        (tex? (equal backend 'latex))

                        ;; fancy display style
                        (boxed (lambda (x)
                                 (if tex?
                                     (concat "\\fbox{\\bf " x "}")
                                   (concat "<span style=\"border-width:1px"
                                           ";border-style:solid;padding:5px\">"
                                           "<strong>" x "</strong></span>"))))

                        ;; Is this a replacement clause?
                        ((this that) (s-split "\\#\\+replacewith:" contents))
                        (replacement-clause? that) ;; There is a ‘that’
                        (replace-keyword (if tex?
                                             "\\underline{Replace:}" "&nbsp;<u>Replace:</u>"))
                        (with-keyword    (if tex? "\\underline{With:}" "<u>With:</u>"
                                             ))
                        (editor (format "[%s:%s" editor
                                        (if replacement-clause?
                                            replace-keyword
                                          "")))
                        (contents′ (if replacement-clause?
                                       (format "%s %s %s" this
                                               (org-export (funcall boxed with-keyword))
                                               that)
                                     contents))

                        ;; “[Editor Comment:”
                        (edcomm-begin (funcall boxed editor))
                        ;; “]”
                        (edcomm-end (funcall boxed "]")))

                       (format (pcase backend
                                 ('latex (format "{\\color{%%s}%s %%s %%s %%s %%s}" (if strong "\\bfseries" "")))
                                 (_ (format "<%s style=\"color: %%s;\">%%s %%s %%s %%s</%s>" (if strong "strong" "p") (if strong "strong" "p"))))
                               color edcomm-begin contents′ signoff edcomm-end)))))

;;;;;; Style ---CSS for the exported page

(org-defblock style (main-arg nil :link nil :tag nil)
  "Style the exported HTML page with the block's CSS, taken verbatim; elsewhere, nothing.

  #+begin_style
  h2 { color: teal; }
  #+end_style"
  (if (equal backend 'html) (format "<style>\n%s\n</style>" raw-contents) ""))

(provide 'org-defblock-misc)
;;; org-defblock-misc.el ends here
