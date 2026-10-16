;;; footnote.el --- Snapshot of Org's footnotes as tooltips in HTML, and as margin notes in LaTeX  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

(define-org-example-test "footnote"
  :input "
The Quran was revealed over 23 years[fn::From 610 to 632 CE, in Mecca then Medina.], and the
Torah[fn:torah] and the Gospel are honoured in it as earlier scriptures.

[fn:torah] /Tawrat/, given to Moses; see [[https://en.wikipedia.org/wiki/Tawrat][Tawrat]]."
  :html "<p>
The Quran was revealed over 23 years<sup><abbr class=\"tooltip\" tabindex=\"0\"
id=\"fnr.1\">1<template>From 610 to 632 CE, in Mecca then Medina.</template></abbr></sup>, and the
Torah<sup><abbr class=\"tooltip\" tabindex=\"0\" id=\"fnr.torah\">2<template><p class=\"footpara\">
<i>Tawrat</i>, given to Moses; see <a href=\"https://en.wikipedia.org/wiki/Tawrat\">Tawrat</a>.
</p></template></abbr></sup> and the Gospel are honoured in it as earlier scriptures.
</p>
<div id=\"footnotes\">
<h2 class=\"footnotes\">Footnotes: </h2>
<div id=\"text-footnotes\">

<div class=\"footdef\"><sup><a id=\"fn.1\" class=\"footnum\" href=\"#fnr.1\"
role=\"doc-backlink\">1</a></sup> <div class=\"footpara\" role=\"doc-footnote\"><p class=\"footpara\">From
610 to 632 CE, in Mecca then Medina.</p></div></div>

<div class=\"footdef\"><sup><a id=\"fn.torah\" class=\"footnum\" href=\"#fnr.torah\"
role=\"doc-backlink\">2</a></sup> <div class=\"footpara\" role=\"doc-footnote\"><p class=\"footpara\">
<i>Tawrat</i>, given to Moses; see <a href=\"https://en.wikipedia.org/wiki/Tawrat\">Tawrat</a>.
</p></div></div>


</div>
</div>"
  :latex "The Quran was revealed over 23
years\\refstepcounter{footnote}${}^{\\textnormal{{\\thefootnote}}}$\\newsavebox{\\OrgSpecialBlockExtrasMarginBox}\\begin{lrbox}{\\OrgSpecialBlockExtrasMarginBox}\\begin{minipage}{\\marginparwidth}
\\raggedright\\footnotesize\\setminted{fontsize=\\footnotesize, breaklines}\\color{gray!80}
{\\color{black}${}^{\\textnormal{{\\thefootnote}}}$}
\\normalfont
From 610 to 632 CE, in Mecca then Medina.
\\end{minipage}\\end{lrbox}\\marginpar{\\usebox{\\OrgSpecialBlockExtrasMarginBox}}
\\global\\let\\OrgSpecialBlockExtrasMarginBox\\relax{}, and the
Torah\\refstepcounter{footnote}${}^{\\textnormal{{\\thefootnote}}}$\\newsavebox{\\OrgSpecialBlockExtrasMarginBox}\\begin{lrbox}{\\OrgSpecialBlockExtrasMarginBox}\\begin{minipage}{\\marginparwidth}
\\raggedright\\footnotesize\\setminted{fontsize=\\footnotesize, breaklines}\\color{gray!80}
{\\color{black}${}^{\\textnormal{{\\thefootnote}}}$}
\\normalfont
\\emph{Tawrat}, given to Moses; see \\href{https://en.wikipedia.org/wiki/Tawrat}{Tawrat}.
\\end{minipage}\\end{lrbox}\\marginpar{\\usebox{\\OrgSpecialBlockExtrasMarginBox}}
\\global\\let\\OrgSpecialBlockExtrasMarginBox\\relax{} and the Gospel are honoured in it as earlier
scriptures.")

(provide 'footnote)
;;; footnote.el ends here
