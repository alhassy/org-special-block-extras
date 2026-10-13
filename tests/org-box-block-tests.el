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
\\begin{tcolorbox}[title={},colback=red!5!white
,colframe=red!75!black
,colbacktitle=yellow!50!red
,coltitle=red!25!black, fonttitle=\\bfseries,subtitle style={boxrule=0.4pt, colback=yellow!50!red!25!white}]
#+end_export
content
#+begin_export latex
\\end{tcolorbox}
#+end_export")

(define-org-call-test "box block passes the title to tcolorbox in LaTeX."
  :call (org-block/box 'latex "content" "Title")
  :returns "#+begin_export latex
\\begin{tcolorbox}[title={Title},colback=red!5!white
,colframe=red!75!black
,colbacktitle=yellow!50!red
,coltitle=red!25!black, fonttitle=\\bfseries,subtitle style={boxrule=0.4pt, colback=yellow!50!red!25!white}]
#+end_export
content
#+begin_export latex
\\end{tcolorbox}
#+end_export")

(deftest "org-subtle-colors maps known names to hex codes."
  (should (equal "#CCFFCC" (org-subtle-colors "green")))
  (should (equal "#CCFFFF" (org-subtle-colors "blue")))
  (should (equal "#FFFF99" (org-subtle-colors "yellow"))))

(deftest "org-subtle-colors passes unknown values through unchanged."
  (should (equal "#1a2b3c" (org-subtle-colors "#1a2b3c"))))

(provide 'org-box-block-tests)
;;; org-box-block-tests.el ends here
