;;; highlight.el --- Snapshot of the highlight block and link showcase example  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

(define-org-example-test "highlight"
  :input "
Skimming readers see [[highlight:yellow][the one sentence that matters]], and [[highlight:pink][the one to reread]].

#+begin_highlight #e5f5e5
A whole paragraph, gently set apart.
#+end_highlight"
  :html "<p>
Skimming readers see <span style=\"background-color:yellow;\">the one sentence that matters</span>, and <span style=\"background-color:pink;\">the one to reread</span>.
</p>

<div style=\"background-color:#e5f5e5;\">
<p>
A whole paragraph, gently set apart.
</p>
</div>"
  :latex "Skimming readers see \\colorlet{osbe-highlight}{yellow}\\colorbox{osbe-highlight}{the one sentence that matters}, and \\colorlet{osbe-highlight}{pink}\\colorbox{osbe-highlight}{the one to reread}.

\\definecolor{osbe-highlight}{HTML}{e5f5e5}\\colorbox{osbe-highlight}{\\parbox{\\dimexpr\\linewidth-2\\fboxsep}{
A whole paragraph, gently set apart.
}}")

(provide 'highlight)
;;; highlight.el ends here
