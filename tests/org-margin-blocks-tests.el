;;; org-margin-blocks-tests.el --- Tests for margin notes, tooltips, and footnotes as either  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(define-org-call-test "tooltip block produces an <abbr> tooltip in HTML."
  :call (org-block/tooltip 'html "tip text" "°")
  :returns "#+begin_export html
<abbr class=\"tooltip\" tabindex=\"0\">°<template><p>
tip text</p></template></abbr>&emsp13;
#+end_export")

(define-org-call-test "margin block produces an <abbr> tooltip for HTML."
  :call (org-block/margin 'html "note text" nil :marker "†" :counter "footnote")
  :returns "#+begin_export html
<abbr class=\"tooltip\" tabindex=\"0\">†<template><p>
note text</p></template></abbr>&emsp13;
#+end_export")

(define-org-call-test "margin block produces a \\marginpar for LaTeX."
  :call (org-block/margin 'latex "note" nil :marker "†" :counter "footnote")
  :returns "#+begin_export latex
\\!\\!${}^{\\textnormal{†}}$\\newsavebox{\\OrgSpecialBlockExtrasMarginBox}
\\begin{lrbox}{\\OrgSpecialBlockExtrasMarginBox}\\begin{minipage}{\\marginparwidth}
\\raggedright\\footnotesize\\ifdefined\\setminted\\setminted{fontsize=\\footnotesize, breaklines}
\\fi\\color{gray!80}
{\\color{black}${}^{\\textnormal{†}}$}
\\normalfont
#+end_export
note
#+begin_export latex
\\end{minipage}\\end{lrbox}\\marginpar{\\usebox{\\OrgSpecialBlockExtrasMarginBox}}
\\global\\let\\OrgSpecialBlockExtrasMarginBox\\relax{}
#+end_export")

(deftest "a footnote reference is a tooltip of its definition, exported as the rest of the page is; the id stays" [footnote]
  (let ((html (org-export-string "
A remark[fn::An /aside/ with a [[https://x.y][link]].] and another[fn:two].

[fn:two] Named *note*.")))
    (should (s-contains-p "<sup><abbr class=\"tooltip\" tabindex=\"0\" id=\"fnr.1\">1<template>An <i>aside</i> with a <a href=\"https://x.y\">link</a>.</template></abbr></sup>" html))
    (should (string-match-p "id=\"fnr.two\">2<template><p class=\"footpara\">\\s-*Named <b>note</b>.</p></template>" html))
    ;; The footnotes section stays, its links back landing on the tooltips.
    (should (s-contains-p "href=\"#fnr.1\"" html))))

(deftest "Markdown, which runs no script, keeps Org's own footnote references" [footnote]
  (require 'ox-md)
  (should (s-contains-p "<a id=\"fnr.1\" class=\"footref\" href=\"#fn.1\"" (org-export-string "Md[fn::plain]." 'md))))

(provide 'org-margin-blocks-tests)
;;; org-margin-blocks-tests.el ends here
