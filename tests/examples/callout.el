;;; callout.el --- Snapshot of the callout blocks showcase example  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

(define-org-example-test "callout"
  :input "
#+begin_tip
=C-h k= followed by any key tells us what that key does.
#+end_tip

#+begin_warning Mind the gap
=C-x C-c= quits Emacs, unsaved buffers and all ---it asks first, though.
#+end_warning"
  :html "<div class=\"callout\" style=\"border-left: 0.25em solid #1a7f37; padding: 0.5em 1em; margin: 1em 0;\">
<p style=\"color: #1a7f37; font-weight: bold; margin: 0;\">💡 Tip</p>
<p>
<code>C-h k</code> followed by any key tells us what that key does.
</p>

</div>

<div class=\"callout\" style=\"border-left: 0.25em solid #9a6700; padding: 0.5em 1em; margin: 1em 0;\">
<p style=\"color: #9a6700; font-weight: bold; margin: 0;\">⚠️ Mind the gap</p>
<p>
<code>C-x C-c</code> quits Emacs, unsaved buffers and all &mdash;it asks first, though.
</p>

</div>"
  :latex "\\definecolor{osbe-callout}{HTML}{1a7f37}\\begin{tcolorbox}[colframe=osbe-callout,
colback=osbe-callout!5!white,colbacktitle=osbe-callout!5!white,coltitle=osbe-callout,
fonttitle=\\bfseries,title={Tip},sharp corners,leftrule=3pt,toprule=0pt,rightrule=0pt,bottomrule=0pt]
\\texttt{C-h k} followed by any key tells us what that key does.

\\end{tcolorbox}

\\definecolor{osbe-callout}{HTML}{9a6700}\\begin{tcolorbox}[colframe=osbe-callout,
colback=osbe-callout!5!white,colbacktitle=osbe-callout!5!white,coltitle=osbe-callout,
fonttitle=\\bfseries,title={Mind the gap},sharp corners,leftrule=3pt,toprule=0pt,rightrule=0pt,
bottomrule=0pt]
\\texttt{C-x C-c} quits Emacs, unsaved buffers and all ---it asks first, though.

\\end{tcolorbox}")

(provide 'callout)
;;; callout.el ends here
