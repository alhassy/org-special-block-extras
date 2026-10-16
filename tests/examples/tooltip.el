;;; tooltip.el --- Snapshot of the `tooltip' showcase example  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

(define-org-example-test "tooltip"
  :input "It can be annoying to make readers jump to the bottom of the page to
read a footnote.  Instead, I prefer to write an /inline [[doc:org-block/tooltip][remark]]/
containing, say, references or additional explanation.

   /[[tooltip:Allah][The God of Abraham; known as Elohim in the Bible]] does not burden a
       soul beyond what it can bear./ --- Quran 2:286

Or, without specifying an explicit label:

   /Allah[[tooltip:][The God of Abraham; known as Elohim in the Bible]] does not
       burden a soul beyond what it can bear./ --- Quran 2:286"
  :html "<p>
It can be annoying to make readers jump to the bottom of the page to
read a footnote.  Instead, I prefer to write an <i>inline <abbr class=\"tooltip\"
tabindex=\"0\">remark<template><pre>Show CONTENTS, as Org, in a tooltip on MARKER; hovered on a desktop, tapped on a phone.
In LaTeX, MARKER is followed by CONTENTS as a margin note, numbered as footnotes are, as the margin block’s are;
in beamer, whose slides have no margins, as a footnote.

Regarding the Lisp function:
+ BACKEND refers to the current export backend.
+ RAW-CONTENTS refers to the text as the user wrote it verbatim.
  ⇒ You may mention CONTENTS to refer to the ‘org parsed’ version of user text.
  ⇒ CONTENTS and RAW-CONTENTS are identical whenever key CONTENTS-OCCUR-AS-LINK-DESCRIPTION is non-nil.

For example, upon LaTeX export, the Org special block

     #+begin_tooltip
     Hello, world
     #+end_tooltip

 is rewritten to the result of the call

     (org-block/tooltip ‘latex \"Hello, world\")</pre></template></abbr></i>
containing, say, references or additional explanation.
</p>

<p>
<i><abbr class=\"tooltip\" tabindex=\"0\">Allah<template><p>
The God of Abraham; known as Elohim in the Bible</p></template></abbr>&emsp13; does not burden a
    soul beyond what it can bear.</i> &mdash; Quran 2:286
</p>

<p>
Or, without specifying an explicit label:
</p>

<p>
<i>Allah<abbr class=\"tooltip\" tabindex=\"0\">°<template><p>
The God of Abraham; known as Elohim in the Bible</p></template></abbr>&emsp13; does not
    burden a soul beyond what it can bear.</i> &mdash; Quran 2:286
</p>"
  :latex "It can be annoying to make readers jump to the bottom of the page to
read a footnote.  Instead, I prefer to write an \\emph{inline
\\hyperref[o-glossary-org-block/tooltip]{remark}
\\label{o-glossary-declaration-site-org-block/tooltip}}
containing, say, references or additional explanation.

\\emph{Allah\\refstepcounter{footnote}${}^{\\textnormal{{\\thefootnote}}}$\\newsavebox{\\OrgSpecialBlockExtrasMarginBox}\\begin{lrbox}{\\OrgSpecialBlockExtrasMarginBox}\\begin{minipage}{\\marginparwidth}
\\raggedright\\footnotesize\\ifdefined\\setminted\\setminted{fontsize=\\footnotesize, breaklines}
\\fi\\color{gray!80}
{\\color{black}${}^{\\textnormal{{\\thefootnote}}}$}
\\normalfont
The God of Abraham; known as Elohim in the Bible
\\end{minipage}\\end{lrbox}\\marginpar{\\usebox{\\OrgSpecialBlockExtrasMarginBox}}
\\global\\let\\OrgSpecialBlockExtrasMarginBox\\relax{} does not burden a
    soul beyond what it can bear.} --- Quran 2:286

Or, without specifying an explicit label:

\\emph{Allah°\\refstepcounter{footnote}${}^{\\textnormal{{\\thefootnote}}}$\\newsavebox{\\OrgSpecialBlockExtrasMarginBox}\\begin{lrbox}{\\OrgSpecialBlockExtrasMarginBox}\\begin{minipage}{\\marginparwidth}
\\raggedright\\footnotesize\\ifdefined\\setminted\\setminted{fontsize=\\footnotesize, breaklines}
\\fi\\color{gray!80}
{\\color{black}${}^{\\textnormal{{\\thefootnote}}}$}
\\normalfont
The God of Abraham; known as Elohim in the Bible
\\end{minipage}\\end{lrbox}\\marginpar{\\usebox{\\OrgSpecialBlockExtrasMarginBox}}
\\global\\let\\OrgSpecialBlockExtrasMarginBox\\relax{} does not
    burden a soul beyond what it can bear.} --- Quran 2:286")
