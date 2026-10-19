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
x
&amp;\\quad = \\quad
y &amp;&amp; \\class{tooltip osbe-tip-hint
osbe-hint}{\\class{osbe-hint-body}{\\begin{array}[t]{@{}l@{}} \\text{Explanation of why $x \\;=\\; y$.}
\\\\ \\text{Actually, let me explain:} \\\\ \\begin{aligned}[t] x &amp;\\quad = \\quad x′ &amp;&amp;
\\class{tooltip osbe-tip-hint osbe-hint}{\\class{osbe-hint-body}{\\text{hint
1}}\\class{osbe-hint-fold}{\\text{…}}} \\\\ &amp;\\quad = \\quad y &amp;&amp; \\class{tooltip
osbe-tip-hint osbe-hint}{\\class{osbe-hint-body}{\\text{hint 2}}\\class{osbe-hint-fold}{\\text{…}}}
\\end{aligned} \\\\ \\text{Words may follow a nested calculation.}
\\end{array}}\\class{osbe-hint-fold}{\\text{…}}} \\\\
&amp;\\quad ≤ \\quad
z &amp;&amp; \\class{tooltip osbe-tip-hint osbe-hint}{\\class{osbe-hint-body}{\\text{Explanation of
why $y \\;\\leq\\; z$}}\\class{osbe-hint-fold}{\\text{…}}}
\\end{align*}"
  :latex "\\begin{align*}
x
&\\quad = \\quad
y && \\begin{array}[t]{@{}l@{}} \\text{Explanation of why $x \\;=\\; y$.} \\\\ \\text{Actually, let me
explain:} \\\\ \\begin{aligned}[t] x &\\quad = \\quad x′ && \\text{hint 1} \\\\ &\\quad = \\quad y &&
\\text{hint 2} \\end{aligned} \\\\ \\text{Words may follow a nested calculation.} \\end{array} \\\\
&\\quad ≤ \\quad
z && \\text{Explanation of why $y \\;\\leq\\; z$}
\\end{align*}")

;;; calc.el ends here
