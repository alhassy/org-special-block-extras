;;; org-box-block-tests.el --- Tests for the box block and org-subtle-colors  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(define-org-call-test "box block wraps content in a styled <div> for HTML."
  :call (org-block/box 'html "inner text" nil)
  :returns "#+begin_export html
<div style=\"padding: 1em;background-color: #CCFFCC;border-radius: 15px;font-size: 0.9em;\"> <h3></h3> 
#+end_export
inner text
#+begin_export html
 </div>
#+end_export")

(define-org-call-test "box block renders the title in an <h3> for HTML."
  :call (org-block/box 'html "content" "My Box")
  :returns "#+begin_export html
<div style=\"padding: 1em;background-color: #CCFFCC;border-radius: 15px;font-size: 0.9em;\"> <h3>My Box</h3> 
#+end_export
content
#+begin_export html
 </div>
#+end_export")

(define-org-call-test "box block passes background-color to the HTML div style."
  :call (org-block/box 'html "text" nil :background-color "blue")
  :returns "#+begin_export html
<div style=\"padding: 1em;background-color: #CCFFFF;border-radius: 15px;font-size: 0.9em;\"> <h3></h3> 
#+end_export
text
#+begin_export html
 </div>
#+end_export")

(define-org-call-test "box block uses tcolorbox for LaTeX."
  :call (org-block/box 'latex "content" nil)
  :returns "#+begin_export latex
\\colorlet{osbe-box-back}{red!5!white}\\colorlet{osbe-box-frame}{red!75!black}
\\colorlet{osbe-box-title}{yellow!50!red}\\begin{tcolorbox}[title={},colback=osbe-box-back,
colframe=osbe-box-frame,colbacktitle=osbe-box-title,coltitle=red!25!black, fonttitle=\\bfseries,
subtitle style={boxrule=0.4pt, colback=yellow!50!red!25!white}]
#+end_export
content
#+begin_export latex
\\end{tcolorbox}
#+end_export")

(define-org-call-test "box block passes the title to tcolorbox in LaTeX."
  :call (org-block/box 'latex "content" "Title")
  :returns "#+begin_export latex
\\colorlet{osbe-box-back}{red!5!white}\\colorlet{osbe-box-frame}{red!75!black}
\\colorlet{osbe-box-title}{yellow!50!red}\\begin{tcolorbox}[title={Title},colback=osbe-box-back,
colframe=osbe-box-frame,colbacktitle=osbe-box-title,coltitle=red!25!black, fonttitle=\\bfseries,
subtitle style={boxrule=0.4pt, colback=yellow!50!red!25!white}]
#+end_export
content
#+begin_export latex
\\end{tcolorbox}
#+end_export")

(deftest "box block names hex colours via \\definecolor in LaTeX, since tcolorbox options reject #RRGGBB."
  (let ((latex (org-block/box 'latex "content" nil :background-color "#CCFFCC" :frame-color 'blue)))
    (should (s-contains? "\\definecolor{osbe-box-back}{HTML}{CCFFCC}" latex))
    (should (s-contains? "\\colorlet{osbe-box-frame}{blue}" latex))
    (should-not (s-contains? "#CCFFCC" latex))))

(deftest "org-subtle-colors maps known names to hex codes."
  (should (equal "#CCFFCC" (org-subtle-colors "green")))
  (should (equal "#CCFFFF" (org-subtle-colors "blue")))
  (should (equal "#FFFF99" (org-subtle-colors "yellow"))))

(deftest "org-subtle-colors passes unknown values through unchanged."
  (should (equal "#1a2b3c" (org-subtle-colors "#1a2b3c"))))

;;; Shadows

(define-org-call-test "a box's shadow may be a single colour, given as a string, without quoting it into the CSS"
  :call (org--box-shadow "pink")
  :returns "box-shadow: 10px 10px 20px 0px pink; width: 50%; margin: auto")

(define-org-call-test "a box's shadow may be a plist of placed colours"
  :call (org--box-shadow '(:left cyan :right pink))
  :returns "width: 50%; margin: auto; box-shadow: 8px 6px 13px 8px pink,-16px 12px 20px 16px cyan")

(define-org-call-test "a box's inset shadow prefixes each of its preset colours with inset"
  :call (org--box-shadow 'inset)
  :returns "width: 50%; margin: auto; box-shadow: 8px 6px 13px 8px inset hsl(1, 100%, 50%),-16px 12px 20px 16px
inset hsl(60, 100%, 50%),48px 36px 71px 28px inset hsl(180, 100%, 50%)")

(provide 'org-box-block-tests)
;;; org-box-block-tests.el ends here
