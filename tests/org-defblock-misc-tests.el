;;; org-defblock-misc-tests.el --- Tests for miscellaneous special blocks  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(define-org-call-test "tooltip block produces an <abbr> tooltip in HTML."
  :call (org-block/tooltip 'html "tip text" "°")
  :returns "#+begin_export html
<abbr class=\"tooltip\" tabindex=\"0\">°<template><p>
tip text</p></template></abbr>&emsp13;
#+end_export")

(define-org-call-test "latex-definitions block hides content in HTML."
  :call (org-block/latex-definitions 'html "\\newcommand{\\foo}{bar}" nil)
  :returns "#+begin_export html
<p style=\"display:none\">\\[\\newcommand{\\foo}{bar}\\]</p>
#+end_export")

(define-org-call-test "latex-definitions block sends its content to the LaTeX preamble, as #+latex_header: lines."
  :call (org-block/latex-definitions 'latex "\\newcommand{\\foo}{bar}" nil)
  :returns "#+begin_export latex

#+end_export
#+latex_header: \\newcommand{\\foo}{bar}
#+begin_export latex

#+end_export")

(define-org-call-test "stutter block repeats the content the given number of times."
  :call (org-block/stutter 'html "ha " "3")
  :returns "#+begin_export html

#+end_export
ha 
#+begin_export html

#+end_export
ha 
#+begin_export html

#+end_export
ha 
#+begin_export html

#+end_export")

(define-org-call-test "rename block performs comma-separated 'to' substitutions."
  :call (org-block/rename 'html "Hello World" "Hello to Hi, World to Earth")
  :returns "#+begin_export html

#+end_export
Hi Earth
#+begin_export html

#+end_export")

(deftest "spoiler block wraps hidden text in a styled <span> for HTML."
  (let ((result (org-block/spoiler 'html "The answer is ((42))." nil)))
    (should (string-match-p "span" result))
    (should (string-match-p "42" result))))

(define-org-call-test "spoiler block produces \\fbox{\\phantom{…}} for LaTeX."
  :call (org-block/spoiler 'latex "Answer: ((x = 5))." nil)
  :returns "#+begin_export latex

#+end_export
Answer: @@latex:\\fbox{\\phantom{x = 5}}\\footnote{x = 5}@@.
#+begin_export latex

#+end_export")

(deftest "remark block renders content when org-hide-editor-comments is nil."
  (let ((org-hide-editor-comments nil)
        (result (org-block/remark 'html "This is a remark." nil)))
    (should (string-match-p "remark" (downcase result)))))

(deftest "remark block produces no visible content when org-hide-editor-comments is t."
  (let ((org-hide-editor-comments nil))
    (setq org-hide-editor-comments t)
    (let ((result (org-block/remark 'html "This is a remark." nil)))
      (setq org-hide-editor-comments nil)
      (should-not (string-match-p "This is a remark" result)))))

(define-org-call-test "margin block produces an <abbr> tooltip for HTML."
  :call (org-block/margin 'html "note text" nil :marker "†" :counter "footnote")
  :returns "#+begin_export html
<abbr class=\"tooltip\" tabindex=\"0\">∘<template><p>
note text</p></template></abbr>&emsp13;
#+end_export")

(define-org-call-test "margin block produces a \\marginpar for LaTeX."
  :call (org-block/margin 'latex "note" nil :marker "†" :counter "footnote")
  :returns "#+begin_export latex
\\!\\!\\stepcounter{footnote}${}^{\\textnormal{{\\thefootnote}}}$\\newsavebox{\\OrgSpecialBlockExtrasMarginBox}\\begin{lrbox}{\\OrgSpecialBlockExtrasMarginBox}\\begin{minipage}{\\marginparwidth}
\\raggedright\\footnotesize\\ifdefined\\setminted\\setminted{fontsize=\\footnotesize, breaklines}
\\fi\\color{gray!80}
{\\color{black}${}^{\\textnormal{{\\thefootnote}}}$}
\\normalfont
#+end_export
note
#+begin_export latex
\\end{minipage}\\end{lrbox}\\marginpar{\\usebox{\\OrgSpecialBlockExtrasMarginBox}}
\\global\\let\\OrgSpecialBlockExtrasMarginBox\\relax{}
#+end_export")

;;; style

(define-org-export-test "a style block's CSS is a <style> element, taken verbatim" [style]
  :input "
          #+begin_style
          h2 { color: teal; }
          #+end_style
          Prose."
  :equals "<style>
           h2 { color: teal; }
           </style>
           <p>
           Prose.</p>
           ")

(define-org-export-test "a style block exports to nothing in LaTeX" [style]
  :input "
          #+begin_style
          h2 { color: teal; }
          #+end_style
          Prose."
  :to latex
  :equals "
           Prose.
           ")


;;; Footnotes, as tooltips

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

(provide 'org-defblock-misc-tests)
;;; org-defblock-misc-tests.el ends here
