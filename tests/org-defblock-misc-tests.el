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

(define-org-call-test "latex-definitions block passes content through in LaTeX."
  :call (org-block/latex-definitions 'latex "\\newcommand{\\foo}{bar}" nil)
  :returns "#+begin_export latex
\\newcommand{\\foo}{bar}
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
\\!\\!${}^{\\textnormal{{\\thefootnote}}}$
               \\newsavebox{\\OrgSpecialBlockExtrasMarginBox}
               \\begin{lrbox}{\\OrgSpecialBlockExtrasMarginBox}
               \\begin{minipage}{\\paperwidth - \\textwidth - \\oddsidemargin - 1in - 3ex}
               \\raggedright \\iffalse Otherwise default alignment is fully justified. \\fi
               \\footnotesize
               \\setminted{fontsize=\\footnotesize, breaklines} \\iffalse HACK! \\fi
               \\color{gray!80}
               {\\color{black}${}^{\\textnormal{{\\thefootnote}}}$}
\\normalfont
 #+end_export
note
#+begin_export latex
               \\end{minipage}
               \\end{lrbox}
               \\marginpar{\\usebox{\\OrgSpecialBlockExtrasMarginBox}\\stepcounter{footnote}}
               \\hspace{-1.9ex}
               \\global\\let\\OrgSpecialBlockExtrasMarginBox\\relax
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

(provide 'org-defblock-misc-tests)
;;; org-defblock-misc-tests.el ends here
