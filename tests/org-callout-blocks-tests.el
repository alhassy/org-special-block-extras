;;; org-callout-blocks-tests.el --- Tests for the callout blocks: note, tip, important, warning, caution  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-export-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))
(require 'ox-md)

(define-org-export-test "a warning callout, given a title, shows it beside its icon above a coloured rule, in HTML" [callout]
  :input "
          #+begin_warning Mind the gap
          Trains do *not* wait.
          #+end_warning"
  :equals "<div class=\"callout\" style=\"border-left: 0.25em solid #9a6700; padding: 0.5em 1em; margin: 1em 0;\">
           <p style=\"color: #9a6700; font-weight: bold; margin: 0;\">⚠️ Mind the gap</p>
           <p>
           Trains do <b>not</b> wait.
           </p>

           </div>
           ")

(define-org-export-test "a tip callout is titled Tip by default, and is a left-ruled tcolorbox in LaTeX" [callout]
  :input "
          #+begin_tip
          Use =C-h k= to learn what a key does.
          #+end_tip"
  :to latex
  :equals "\\definecolor{osbe-callout}{HTML}{1a7f37}\\begin{tcolorbox}[colframe=osbe-callout,colback=osbe-callout!5!white,colbacktitle=osbe-callout!5!white,coltitle=osbe-callout,fonttitle=\\bfseries,title={Tip},sharp corners,leftrule=3pt,toprule=0pt,rightrule=0pt,bottomrule=0pt]
           Use \\texttt{C-h k} to learn what a key does.

           \\end{tcolorbox}
           ")

(define-org-export-test "a tip callout is GitHub's untitled “> [!TIP]” alert in Markdown" [callout]
  :input "
          #+options: toc:nil
          #+begin_tip
          Use =C-h k= to learn what a key does.
          #+end_tip"
  :to md
  :equals "
           
           > [!TIP]
           >
           > Use `C-h k` to learn what a key does.
           
           ")

(define-org-export-test "a warning callout, given a title, keeps it as a bold first paragraph of its Markdown alert" [callout]
  :input "
          #+options: toc:nil
          #+begin_warning Mind the gap
          Trains do *not* wait.
          #+end_warning"
  :to md
  :equals "
           
           > [!WARNING]
           >
           > **Mind the gap**
           >
           > Trains do **not** wait.
           
           ")

(deftest "each callout is a block titled by its own name, without a link type or tag" [callout]
  (pcase-dolist (`(,name . ,_) org-callouts)
    (should (equal (org-block-signature name) (format "#+begin_%s TITLE=%S" name (capitalize (symbol-name name)))))
    (should-not (org-link-get-parameter (symbol-name name) :export))
    (should-not (fboundp (intern (format "org-deftag/%s" name))))))

(provide 'org-callout-blocks-tests)
;;; org-callout-blocks-tests.el ends here
