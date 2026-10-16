;;; calc.el --- Snapshot of the `calc' showcase example  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

(define-org-example-test "calc"
  :input "
#+begin_calc :hint-format \"\\\\left\\\\{ %s\\\\right.\"
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
  :html "$$\\begin{align*} & \\begin{aligned}x\\end{aligned} 
\\\\
 = \\;\\; & \\qquad \\color{#800000}{\\left\\{ {\\large\\substack{\\text{ Explanation of why $x \\;=\\; y$ }
\\hfill\\\\
\\text{ Actually, let me explain: } \\hfill\\\\ \\begin{aligned} & \\begin{aligned}x\\end{aligned} 
\\\\
 = \\;\\; & \\qquad \\color{#800000}{\\left\\{ {\\large\\substack{\\text{ hint 1 } \\hfill\\\\
}}\\right.} 
 \\\\ & \\begin{aligned}x′ \\end{aligned}\\\\
 = \\;\\; & \\qquad \\color{#800000}{\\left\\{ {\\large\\substack{\\text{ hint 2 } \\hfill\\\\
}}\\right.} 
 \\\\ & \\begin{aligned}y  \\end{aligned} 
\\end{aligned}\\hfill\\\\
}}\\right.} 
 \\\\ & \\begin{aligned}    y \\end{aligned}\\\\
 ≤ \\;\\; & \\qquad \\color{#800000}{\\left\\{ {\\large\\substack{\\text{ Explanation of why $y \\;\\leq\\; z$
} \\hfill\\\\
}}\\right.} 
 \\\\ & \\begin{aligned} z\\end{aligned} 
\\end{align*}$$"
  :latex "\\definecolor{osbe-calc}{HTML}{800000}\\begin{align*} & \\begin{aligned}x\\end{aligned} 
\\\\
 = \\;\\; & \\qquad \\color{osbe-calc}{\\left\\{ {\\large\\substack{\\text{ Explanation of why $x \\;=\\; y$ }
\\hfill\\\\
\\text{ Actually, let me explain: } \\hfill\\\\ \\begin{aligned} & \\begin{aligned}x\\end{aligned} 
\\\\
 = \\;\\; & \\qquad \\color{osbe-calc}{\\left\\{ {\\large\\substack{\\text{ hint 1 } \\hfill\\\\
}}\\right.} 
 \\\\ & \\begin{aligned}x′ \\end{aligned}\\\\
 = \\;\\; & \\qquad \\color{osbe-calc}{\\left\\{ {\\large\\substack{\\text{ hint 2 } \\hfill\\\\
}}\\right.} 
 \\\\ & \\begin{aligned}y  \\end{aligned} 
\\end{aligned}\\hfill\\\\
}}\\right.} 
 \\\\ & \\begin{aligned}    y \\end{aligned}\\\\
 ≤ \\;\\; & \\qquad \\color{osbe-calc}{\\left\\{ {\\large\\substack{\\text{ Explanation of why $y \\;\\leq\\;
z$ } \\hfill\\\\
}}\\right.} 
 \\\\ & \\begin{aligned} z\\end{aligned} 
\\end{align*}")
