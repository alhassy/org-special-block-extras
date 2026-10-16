;;; switches.el --- Snapshot of the block switches showcase example  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

(define-org-example-test "switches"
  :input "
#+begin_box Exercise :let (N 7)
What do the first N odd numbers sum to?
#+end_box

#+begin_box Answer :let (N 7 SUM (* N N)) :folded \"Peek at the answer\"
The first N odd numbers sum to N × N, which is SUM.
#+end_box

#+begin_remark :noexport t
Ask the class for a proof by induction next week.
#+end_remark

#+begin_parallel :when (eq backend 'html) :boxed \"On the web only\"
Columns suit wide screens.
#+columnbreak:
Print gets one column.
#+end_parallel"
  :html "<div style=\"padding: 1em;background-color: #CCFFCC;border-radius: 15px;font-size: 0.9em;\">
<h3>Exercise</h3> 
<p>
What do the first 7 odd numbers sum to?
</p>
</div>

<details class=\"code-details\" style=\"padding: 1em; background-color: #e5f5e5; border-radius: 15px;
color: hsl(157 75% 20%); font-size: 0.9em; box-shadow: 0.05em 0.1em 5px 0.01em #00000057;\">
<summary><strong><font face=\"Courier\" size=\"3\" color=\"green\">Peek at the
answer</font></strong></summary>
<div style=\"padding: 1em;background-color: #CCFFCC;border-radius: 15px;font-size: 0.9em;\">
<h3>Answer</h3> 
<p>
The first 7 odd numbers sum to 7 × 7, which is 49.
</p>
</div>

</details>



<div style=\"padding: 1em;background-color: #CCFFCC;border-radius: 15px;font-size: 0.9em;\"> <h3>On
the web only</h3> 
<div style=\"column-count: 2;\">
<p>
Columns suit wide screens.
</p>
<p>
Print gets one column.
</p>
</div>
</div>"
  :latex "\\colorlet{osbe-box-back}{red!5!white}\\colorlet{osbe-box-frame}{red!75!black}
\\colorlet{osbe-box-title}{yellow!50!red}\\begin{tcolorbox}[title={Exercise},colback=osbe-box-back,
colframe=osbe-box-frame,colbacktitle=osbe-box-title,coltitle=red!25!black, fonttitle=\\bfseries,
subtitle style={boxrule=0.4pt, colback=yellow!50!red!25!white}]
What do the first 7 odd numbers sum to?
\\end{tcolorbox}

\\definecolor{osbe-bg}{HTML}{e5f5e5}\\colorlet{osbe-fg}{green}\\begin{quote}
\\begin{tcolorbox}[colback=osbe-bg,colframe=osbe-fg,title={Peek at the answer},sharp corners,
boxrule=0.4pt]
\\colorlet{osbe-box-back}{red!5!white}\\colorlet{osbe-box-frame}{red!75!black}
\\colorlet{osbe-box-title}{yellow!50!red}\\begin{tcolorbox}[title={Answer},colback=osbe-box-back,
colframe=osbe-box-frame,colbacktitle=osbe-box-title,coltitle=red!25!black, fonttitle=\\bfseries,
subtitle style={boxrule=0.4pt, colback=yellow!50!red!25!white}]
The first 7 odd numbers sum to 7 × 7, which is 49.
\\end{tcolorbox}

\\end{tcolorbox}
\\end{quote}")

(provide 'switches)
;;; switches.el ends here
