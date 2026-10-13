;;; org-details-block-tests.el --- Tests for the details block  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(define-org-call-test "details block uses \"Details\" as the default title in HTML."
  :call (org-block/details 'html "body text" nil)
  :returns "#+begin_export html
<details class=\"code-details\"
                 style =\"padding: 1em;
                          background-color: #e5f5e5;
                          border-radius: 15px;
                          color: hsl(157 75%);
                          font-size: 0.9em;
                          box-shadow: 0.05em 0.1em 5px 0.01em  #00000057;\">
                  <summary>
                    <strong>
                      <font face=\"Courier\" size=\"3\" color=\"green\">
                         Details
                      </font>
                    </strong>
                  </summary>
                  
#+end_export
body text
#+begin_export html

               </details>
#+end_export")

(define-org-call-test "details block renders a custom title in HTML."
  :call (org-block/details 'html "content" "Spoiler")
  :returns "#+begin_export html
<details class=\"code-details\"
                 style =\"padding: 1em;
                          background-color: #e5f5e5;
                          border-radius: 15px;
                          color: hsl(157 75%);
                          font-size: 0.9em;
                          box-shadow: 0.05em 0.1em 5px 0.01em  #00000057;\">
                  <summary>
                    <strong>
                      <font face=\"Courier\" size=\"3\" color=\"green\">
                         Spoiler
                      </font>
                    </strong>
                  </summary>
                  
#+end_export
content
#+begin_export html

               </details>
#+end_export")

(define-org-call-test "details block passes background-color through to HTML."
  :call (org-block/details 'html "content" nil :background-color "#ff0000")
  :returns "#+begin_export html
<details class=\"code-details\"
                 style =\"padding: 1em;
                          background-color: #ff0000;
                          border-radius: 15px;
                          color: hsl(157 75%);
                          font-size: 0.9em;
                          box-shadow: 0.05em 0.1em 5px 0.01em  #00000057;\">
                  <summary>
                    <strong>
                      <font face=\"Courier\" size=\"3\" color=\"green\">
                         Details
                      </font>
                    </strong>
                  </summary>
                  
#+end_export
content
#+begin_export html

               </details>
#+end_export")

(define-org-call-test "details block produces a tcolorbox for LaTeX."
  :call (org-block/details 'latex "content" nil)
  :returns "#+begin_export latex
\\definecolor{osbe-bg}{HTML}{e5f5e5}\\colorlet{osbe-fg}{green}\\begin{quote}
                              \\begin{tcolorbox}[colback=osbe-bg,colframe=osbe-fg,title={Details},sharp corners,boxrule=0.4pt]
                                   
#+end_export
content
#+begin_export latex

                               \\end{tcolorbox}
                \\end{quote}
#+end_export")

(define-org-call-test "details block renders a custom title in LaTeX."
  :call (org-block/details 'latex "content" "My Title")
  :returns "#+begin_export latex
\\definecolor{osbe-bg}{HTML}{e5f5e5}\\colorlet{osbe-fg}{green}\\begin{quote}
                              \\begin{tcolorbox}[colback=osbe-bg,colframe=osbe-fg,title={My Title},sharp corners,boxrule=0.4pt]
                                   
#+end_export
content
#+begin_export latex

                               \\end{tcolorbox}
                \\end{quote}
#+end_export")

(provide 'org-details-block-tests)
;;; org-details-block-tests.el ends here
