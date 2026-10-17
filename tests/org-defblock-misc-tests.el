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
  :returns "#+latex_header: \\newcommand{\\foo}{bar}
#+begin_export latex

#+end_export")

(define-org-call-test "stutter block repeats the content the given number of times."
  :call (org-block/stutter 'html "ha " "3")
  :returns "ha
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
  :returns "Hi Earth
#+begin_export html

#+end_export")

(deftest "spoiler block wraps hidden text in a styled <span> for HTML."
  (let ((result (org-block/spoiler 'html "The answer is ((42))." nil)))
    (should (string-match-p "span" result))
    (should (string-match-p "42" result))))

(define-org-call-test "spoiler block produces \\fbox{\\phantom{…}} for LaTeX."
  :call (org-block/spoiler 'latex "Answer: ((x = 5))." nil)
  :returns "Answer: @@latex:\\fbox{\\phantom{x = 5}}\\footnote{x = 5}@@.
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

;;; quote, given a citation

(deftest "a block's Org contents within LaTeX braces hug them, sans the spaces of their newlines; issue #27" [quote]
  (should (s-contains-p "\\blockcquote[20-22]{zermelo1908}{%
\\textbf{This} is a \\emph{try}.
\\ifhmode\\unskip\\fi}"
                        (org-export-string "
#+begin_quote zermelo1908 :pages 20-22
*This* is a /try/.
#+end_quote" 'latex)))
  ;; Outside braces, a newline is a space the handler may well want.
  (org-defblock takeaway () "A takeaway: CONTENTS." (format "Takeaway: %s" contents))
  (should (s-contains-p "Takeaway: \nRemarkable."
                        (org-export-string "
#+begin_takeaway
Remarkable.
#+end_takeaway" 'latex))))

(deftest "a quote given a key is csquotes' \\blockcquote in LaTeX, as the issue spells it, :key and all; issue #24" [quote]
  (dolist (header '(":key zermelo1908 :pages 20-22" "zermelo1908 :pages 20-22"))
    (should (s-contains-p "\\blockcquote[20-22]{zermelo1908}{%\nThis is a \\emph{blockquote}.\n\\ifhmode\\unskip\\fi}"
                          (org-export-string (format "#+begin_quote %s\n\n    This is a /blockquote/.\n\n#+end_quote" header)
                                             'latex))))
  ;; Sans pages, sans the optional argument.
  (should (s-contains-p "\\blockcquote{zermelo1908}{"
                        (org-export-string "#+begin_quote zermelo1908\nQuoted.\n#+end_quote" 'latex))))

(deftest "a quote given a key is a blockquote attributed to its key, at its pages, in HTML and Markdown" [quote]
  (require 'ox-md)
  (let ((input "#+begin_quote zermelo1908 :pages 20-22\nQuoted.\n#+end_quote"))
    (should (s-contains-p "<footer>&mdash; <cite>zermelo1908</cite>, 20-22</footer></blockquote>"
                          (org-export-string input 'html)))
    (should (s-contains-p "> Quoted.\n> \n> &mdash; *zermelo1908*, 20-22" (org-export-string input 'md)))))

(deftest "a plain quote, sans arguments, stays Org's own, in every backend, and warns of nothing" [quote]
  (require 'ox-md)
  (let ((input "
- item
  #+begin_quote
  To be, or not to be.
  #+end_quote
- next")
        warnings)
    (dolist (backend '(latex html md))
      (let ((ours (org-export-string input backend))
            (orgs (unwind-protect
                      (cl-letf (((symbol-function 'display-warning) (lambda (&rest args) (push args warnings))))
                        (org-special-block-extras-mode -1)
                        (org-export-string-as input backend t))
                    (org-special-block-extras-mode 1))))
        (should (equal ours orgs))))
    (should-not warnings)))

(deftest "a quote: link is an inline quotation, cited when keyed; a section tagged :quote: is Org's own quote of its text" [quote]
  (let ((latex (org-export-string "
Hamlet says [[quote:][to be]]; Zermelo writes [[quote:zermelo1908][set theory is …]].

* Credo :quote:
Less is more." 'latex)))
    (should (s-contains-p "Hamlet says ``to be''; Zermelo writes \\textcquote{zermelo1908}{set theory is …}." latex))
    (should (string-match-p "\\\\begin{quote}\nLess is more\\.\n\\\\end{quote}" latex))))

(deftest "a cited quote, or quote: link, loads csquotes in LaTeX, unless the document already does" [quote]
  (cl-flet ((csquotes (input) (s-count-matches "{csquotes}" (org-export-string-as input 'latex))))
    (should (= 1 (csquotes "#+begin_quote zermelo1908 :pages 20-22\nQuoted.\n#+end_quote")))
    (should (= 1 (csquotes "As Zermelo has it, [[quote:zermelo1908][set theory]] is foundational.")))
    ;; The document's own, options and all, is left be; and a document sans cited quotes gets nothing.
    (should (= 1 (csquotes "#+latex_header: \\usepackage[autostyle]{csquotes}\n#+begin_quote zermelo1908\nQuoted.\n#+end_quote")))
    (should (= 0 (csquotes "#+begin_quote\nPlain.\n#+end_quote")))))

(provide 'org-defblock-misc-tests)
;;; org-defblock-misc-tests.el ends here
