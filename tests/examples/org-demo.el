;;; org-demo.el --- Snapshot of the `org-demo' showcase example  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

(define-org-example-test "org-demo"
  :input "Sometimes, we want to show verbatim source and its resulting rendition ---which
is a major part of this article! So, let's make a block to mitigate such an
error-prone tedium.

#+begin_org-demo
/italics/ and _underline_

$e^{i \\times \\pi} + 1 = 0$
#+end_org-demo

Output the CONTENTS of the block as both parsed Org and unparsed.

Label the source text by SOURCE and the result text by RESULT"
  :html "<p>
Sometimes, we want to show verbatim source and its resulting rendition &mdash;which
is a major part of this article! So, let's make a block to mitigate such an
error-prone tedium.
</p>

<div style=\"column-count: 2;\">
<div style=\"padding: 1em;background-color: cyan;border-radius: 15px;font-size: 0.9em;\"> <h3>Source</h3> 
<div ><pre class=\"src src-org\">/italics/ and _underline_

$e^{i \\times \\pi} + 1 = 0$</pre></div>
</div>
<div style=\"padding: 1em;background-color: cyan;border-radius: 15px;font-size: 0.9em;\"> <h3>Result</h3> 
<p>
<i>italics</i> and <span class=\"underline\">underline</span>
</p>

<p>
\\(e^{i \\times \\pi} + 1 = 0\\)
</p>
</div>
</div>
<p>
Output the CONTENTS of the block as both parsed Org and unparsed.
</p>

<p>
Label the source text by SOURCE and the result text by RESULT
</p>"
  :latex "Sometimes, we want to show verbatim source and its resulting rendition ---which
is a major part of this article! So, let's make a block to mitigate such an
error-prone tedium.

\\par \\setlength{\\columnseprule}{0 pt}
\\begin{minipage}[t]{\\linewidth}
\\begin{multicols}{2}
\\colorlet{osbe-box-back}{cyan}\\colorlet{osbe-box-frame}{red!75!black}\\colorlet{osbe-box-title}{yellow!50!red}\\begin{tcolorbox}[title={Source},colback=osbe-box-back,colframe=osbe-box-frame,colbacktitle=osbe-box-title,coltitle=red!25!black, fonttitle=\\bfseries,subtitle style={boxrule=0.4pt, colback=yellow!50!red!25!white}]

\\begin{verbatim}
/italics/ and _underline_

$e^{i \\times \\pi} + 1 = 0$
\\end{verbatim}
\\end{tcolorbox}
\\colorlet{osbe-box-back}{cyan}\\colorlet{osbe-box-frame}{red!75!black}\\colorlet{osbe-box-title}{yellow!50!red}\\begin{tcolorbox}[title={Result},colback=osbe-box-back,colframe=osbe-box-frame,colbacktitle=osbe-box-title,coltitle=red!25!black, fonttitle=\\bfseries,subtitle style={boxrule=0.4pt, colback=yellow!50!red!25!white}]
\\emph{italics} and \\uline{underline}

\\(e^{i \\times \\pi} + 1 = 0\\)
\\end{tcolorbox}

\\end{multicols}\\end{minipage}
Output the CONTENTS of the block as both parsed Org and unparsed.

Label the source text by SOURCE and the result text by RESULT")
