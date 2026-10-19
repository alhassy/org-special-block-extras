;;; calc.el --- Snapshot of the calculational-proof showcase example, a math block with hints  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

(define-org-example-test "calc"
  :input "
#+begin_math
  x
={ Explanation of why $x \\;=\\; y$.
   Actually, let me explain:
   \\[   x
      ={ hint 1 } x′
      ={ hint 2 } y \\]
   Words may follow a nested calculation.
 }
  y
≤{ Explanation of why $y \\;\\leq\\; z$
   # Explain it more; this comment is absent from the export ;-)
 }
  z
#+end_math"
  :html "\\begin{align*}
&amp; x
\\\\ = &amp; \\quad \\class{tooltip osbe-tip-hint
osbe-hint}{\\class{osbe-hint-body}{{\\color[RGB]{128,0,0} \\left[ \\begin{array}{@{}l@{}}
\\text{Explanation of why $x \\;=\\; y$.} \\\\ \\text{Actually, let me explain:} \\\\ \\begin{aligned}[t]
&amp; x \\\\ = &amp; \\quad \\class{tooltip osbe-tip-hint
osbe-hint}{\\class{osbe-hint-body}{{\\color[RGB]{128,0,0} \\left[ \\text{hint 1}
\\right.}}\\class{osbe-hint-fold}{{\\color[RGB]{128,0,0} \\left[ \\text{…} \\right.}}} \\\\ &amp; x′ \\\\ =
&amp; \\quad \\class{tooltip osbe-tip-hint osbe-hint}{\\class{osbe-hint-body}{{\\color[RGB]{128,0,0}
\\left[ \\text{hint 2} \\right.}}\\class{osbe-hint-fold}{{\\color[RGB]{128,0,0} \\left[ \\text{…}
\\right.}}} \\\\ &amp; y \\end{aligned} \\\\ \\text{Words may follow a nested calculation.} \\end{array}
\\right.}}\\class{osbe-hint-fold}{{\\color[RGB]{128,0,0} \\left[ \\text{…} \\right.}}} \\notag \\\\ &amp; y
\\\\ ≤ &amp; \\quad \\class{tooltip osbe-tip-hint
osbe-hint}{\\class{osbe-hint-body}{{\\color[RGB]{128,0,0} \\left[ \\text{Explanation of why $y \\;\\leq\\;
z$} \\right.}}\\class{osbe-hint-fold}{{\\color[RGB]{128,0,0} \\left[ \\text{…} \\right.}}} \\notag \\\\
&amp; z
\\end{align*}"
  :latex "\\begin{align*}
& x
\\\\ = & \\quad {\\color[RGB]{128,0,0} \\left[ \\begin{array}{@{}l@{}} \\text{Explanation of why $x \\;=\\;
y$.} \\\\ \\text{Actually, let me explain:} \\\\ \\begin{aligned}[t] & x \\\\ = & \\quad {\\color[RGB]{128,0,
0} \\left[ \\text{hint 1} \\right.} \\\\ & x′ \\\\ = & \\quad {\\color[RGB]{128,0,0} \\left[ \\text{hint 2}
\\right.} \\\\ & y \\end{aligned} \\\\ \\text{Words may follow a nested calculation.} \\end{array} \\right.}
\\notag \\\\ & y
\\\\ ≤ & \\quad {\\color[RGB]{128,0,0} \\left[ \\text{Explanation of why $y \\;\\leq\\; z$} \\right.} \\notag
\\\\ & z
\\end{align*}")

;;; calc.el ends here
