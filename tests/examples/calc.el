;;; calc.el --- Snapshot of the `calc' showcase example  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

(define-org-example-test "calc"
  :input "
#+begin_calc :hint-format \"\\\\left\\{ %s\\\\right.\"
+     x
+     y -- Explanation of why $x \\;=\\; y$
  Actually, let me explain:
  * x
  * x′ -- hint 1
  * y  -- hint 2

  No words can appear (in the export) *after* a nested calculation, for now.
+ [≤] z
  --
  Explanation of why $y \\;\\leq\\; z$

  -- explain it more, this is ignored from export ;-)
#+end_calc"
  :html "$$\\begin{align*} & \\begin{split}x\\end{split} 
\\\\
 = \\;\\; & \\qquad \\color{maroon}{\\left{ {\\large\\substack{\\text{ Explanation of why $x \\;=\\; y$ } \\hfill\\\\
\\text{ Actually, let me explain: } \\hfill\\\\ \\begin{split} & \\begin{split}x\\end{split} 
\\\\
 = \\;\\; & \\qquad \\color{maroon}{\\left{ {\\large\\substack{\\text{ hint 1 } \\hfill\\\\
}}\\right.} 
 \\\\ & \\begin{split}x′ \\end{split}\\\\
 = \\;\\; & \\qquad \\color{maroon}{\\left{ {\\large\\substack{\\text{ hint 2 } \\hfill\\\\
}}\\right.} 
 \\\\ & \\begin{split}y  \\end{split} 
\\end{split}\\hfill\\\\
}}\\right.} 
 \\\\ & \\begin{split}    y \\end{split}\\\\
 ≤ \\;\\; & \\qquad \\color{maroon}{\\left{ {\\large\\substack{\\text{ Explanation of why $y \\;\\leq\\; z$ } \\hfill\\\\
}}\\right.} 
 \\\\ & \\begin{split} z\\end{split} 
\\end{align*}$$"
  :latex "$$\\begin{align*} & \\begin{split}x\\end{split} 
\\\\
 = \\;\\; & \\qquad \\color{maroon}{\\left{ {\\large\\substack{\\text{ Explanation of why $x \\;=\\; y$ } \\hfill\\\\
\\text{ Actually, let me explain: } \\hfill\\\\ \\begin{split} & \\begin{split}x\\end{split} 
\\\\
 = \\;\\; & \\qquad \\color{maroon}{\\left{ {\\large\\substack{\\text{ hint 1 } \\hfill\\\\
}}\\right.} 
 \\\\ & \\begin{split}x′ \\end{split}\\\\
 = \\;\\; & \\qquad \\color{maroon}{\\left{ {\\large\\substack{\\text{ hint 2 } \\hfill\\\\
}}\\right.} 
 \\\\ & \\begin{split}y  \\end{split} 
\\end{split}\\hfill\\\\
}}\\right.} 
 \\\\ & \\begin{split}    y \\end{split}\\\\
 ≤ \\;\\; & \\qquad \\color{maroon}{\\left{ {\\large\\substack{\\text{ Explanation of why $y \\;\\leq\\; z$ } \\hfill\\\\
}}\\right.} 
 \\\\ & \\begin{split} z\\end{split} 
\\end{align*}$$")
