;;; org-defblock-misc.el --- Miscellaneous special blocks  -*- lexical-binding: t; -*-

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

;; Miscellaneous special blocks: margin, tooltip, latex-definitions,
;; solution, org-demo, stutter, rename, spoiler, remark.
;;
;; Full documentation at https://alhassy.github.io/org-special-block-extras

;;; Code:

(require 'org-defblock)
(require 'org-links)  ;; for org-ospe-html-export-preserving-whitespace
(require 'org-box-block)    ;; solution uses box
(require 'org-details-block) ;; solution uses details

;;;;;; Margin (LaTeX)

(org-defblock margin
              (marker nil
                      color "gray!80"
                      counter "footnote"
                      width "\\paperwidth - \\textwidth - \\oddsidemargin - 1in - 3ex")
              ;; Width: https://tex.stackexchange.com/a/101861/69371
              (vector :display 'full
                      :face '(:foreground "grey" :weight bold :underline "orange" :overline "orange"))
              "Produce an HTML tooltip or a LaTeX margin note.

The ‘margin’ block is intended for “one-off” (mostly optional) remarks.

It is essentially “inline footnotes”.

For notes that you want to use repeatedly, in multiple articles
or in multiple locations in the same article, consider using
‘documentation’ to declare them and ‘doc’ to invoke them.

For LaTeX, place ‘#+resize:’ to have the remainder of a block be
resized, for now 1.3 the margin width ---requires \\usepackage{adjustbox}.

----------------------------------------------------------------------

WIDTH, COUNTER, and COLOR are LaTeX specfic.

When no label, marker, is used for a marginal note, we rely
on a COUNTER, such as ‘footnote’ (default) or ‘sidenote.’
Since HTML has no margin per se, we use “∘” as default marker:
Users hover over it to read the marginal note.

Marginal notes have their labels, markers, in black
and the notes themselves have COLOR being grey!80.
In Emacs, margin links appear grey with an orange tinted boarder.

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
                  (`latex
                   (setq marker (or marker (format "{\\the%s}" counter))) ;; "\\circ"
                   (format "\\!\\!${}^{\\textnormal{%s}}$
               \\newsavebox{\\OrgSpecialBlockExtrasMarginBox}
               \\begin{lrbox}{\\OrgSpecialBlockExtrasMarginBox}
               \\begin{minipage}{%s}
               \\raggedright \\iffalse Otherwise default alignment is fully justified. \\fi
               \\footnotesize
               \\setminted{fontsize=\\footnotesize, breaklines} \\iffalse HACK! \\fi
               \\color{%s}
               {\\color{black}${}^{\\textnormal{%s}}$}\n\\normalfont\n %s
               \\end{minipage}
               \\end{lrbox}
               \\marginpar{\\usebox{\\OrgSpecialBlockExtrasMarginBox}%s}
               \\hspace{-1.9ex}
               \\global\\let\\OrgSpecialBlockExtrasMarginBox\\relax"
                           marker
                           width
                           color
                           marker
                           (if (s-contains? "#+resize:" contents)
                               (s-concat
                                (s-replace "#+resize:"
                                           "#+latex: \\maxsizebox{1.3\\textwidth}{\\textheight}{\\begin{tabular}{l}\n"
                                           (s-trim contents))
                                "\n\\end{tabular}}")
                             (s-trim contents))
                           stepcounter))
                  (_ (format "<abbr class=\"tooltip\" title=\"%s\">%s</abbr>&emsp13;"
                             (org-ospe-html-export-preserving-whitespace contents)
                             marker)))))

;;;;;; Tooltip (HTML)
(org-defblock tooltip (marker "°")
              "Produce an HTML tooltip."
              (format "<abbr class=\"tooltip\" title=\"%s\">%s</abbr>&emsp13;"
                      (org-ospe-html-export-preserving-whitespace contents)
                      marker))

(org-defblock latex-definitions nil
              "Declare but do not display the CONTENTS according to the BACKEND."
              (format (pcase backend
                        ('html "<p style=\"display:none\">\\[%s\\]</p>")
                        (_ "%s"))
                      raw-contents))

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
"
              (-let [text (concat
                           ;; Source
                           (thread-last raw-contents
                                        (format (if (equal backend 'html)
                                                    "<div ><pre class=\"src src-org\">%s</pre></div>"
                                                  "\n\\begin{verbatim}\n%s\n\\end{verbatim}"))
                                        org-export
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
                  (concat "#+end_export\n" text "\n#+begin_export"))))

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
in the footnotes."
              (if (equal backend 'latex)
                  (s-replace-regexp
                   (concat (regexp-quote left) "\\(.*?\\)" (regexp-quote right))
                   "@@latex:\\\\fbox{\\\\phantom{\\1}}\\\\footnote{\\1}@@"
                   contents)
                (-let [id (gensym)]
                  (concat
                   ;; In HTML, a ‘style’ can be, technically, almost anywhere...
                   (format
                    "<style> #%s {color: %s; background-color:%s;}
       #%s:hover {color: black; background-color:white;} </style>
       " id color color id)
                   (s-replace-regexp
                    (concat (regexp-quote left) "\\(.*?\\)" (regexp-quote right))
                    (format "@@html:<span id=\"%s\"> \\1 </span>@@" id)
                    contents)))))

;;;;;; Remark

(defvar org-hide-editor-comments nil
  "Should editor comments be shown in the output or not.")

(org-defblock remark
              (editor "Editor Remark" color "black" signoff "" strong nil)
                                        ; :inline-please__see_margin_block_for_a_similar_incantation ; ⇒ crashes!
              [:face '(:foreground "red" :weight bold)]
              "Format CONTENTS as an first-class editor comment according to BACKEND.

The CONTENTS string has an optional switch: If it contains a line
with having only ‘#+replacewith:’, then the text preceding this
clause should be replaced by the text after it; i.e., this is
what the EDITOR (the person editing) intends and so we fromat the
replacement instruction (to the authour) as such.

In Emacs, as links, editor remarks are shown with a bold red; but
the exported COLOR of a remark is black by default and it is not
STRONG ---i.e., bold---. There is an optional SIGNOFF message
that is appended to the remark.
"
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

                (setq org-export-allow-bind-keywords t) ;; So users can use “#+bind” immediately
                (if org-hide-editor-comments
                    ""
                  (format (pcase backend
                            ('latex (format "{\\color{%%s}%s %%s %%s %%s %%s}" (if strong "\\bfseries" "")))
                            (_ (format "<%s style=\"color: %%s;\">%%s %%s %%s %%s</%s>" (if strong "strong" "p") (if strong "strong" "p"))))
                          color edcomm-begin contents′ signoff edcomm-end))))

(provide 'org-defblock-misc)
;;; org-defblock-misc.el ends here
