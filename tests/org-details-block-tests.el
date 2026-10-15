;;; org-details-block-tests.el --- Tests for the details block  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(define-org-call-test "details block uses \"Details\" as the default title in HTML."
  :call (org-block/details 'html "body text" nil)
  :returns "#+begin_export html
<details class=\"code-details\" style=\"padding: 1em; background-color: #e5f5e5; border-radius: 15px; color: hsl(157 75% 20%); font-size: 0.9em; box-shadow: 0.05em 0.1em 5px 0.01em #00000057;\">
<summary><strong><font face=\"Courier\" size=\"3\" color=\"green\">Details</font></strong></summary>

#+end_export
body text
#+begin_export html

</details>
#+end_export")

(define-org-call-test "details block renders a custom title in HTML."
  :call (org-block/details 'html "content" "Spoiler")
  :returns "#+begin_export html
<details class=\"code-details\" style=\"padding: 1em; background-color: #e5f5e5; border-radius: 15px; color: hsl(157 75% 20%); font-size: 0.9em; box-shadow: 0.05em 0.1em 5px 0.01em #00000057;\">
<summary><strong><font face=\"Courier\" size=\"3\" color=\"green\">Spoiler</font></strong></summary>

#+end_export
content
#+begin_export html

</details>
#+end_export")

(define-org-call-test "details block passes background-color through to HTML."
  :call (org-block/details 'html "content" nil :background-color "#ff0000")
  :returns "#+begin_export html
<details class=\"code-details\" style=\"padding: 1em; background-color: #ff0000; border-radius: 15px; color: hsl(157 75% 20%); font-size: 0.9em; box-shadow: 0.05em 0.1em 5px 0.01em #00000057;\">
<summary><strong><font face=\"Courier\" size=\"3\" color=\"green\">Details</font></strong></summary>

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


;;; src blocks with :folded

(deftest "a src block with :folded t exports inside a details block titled by its language"
  (let ((html (org-export-string "#+begin_src emacs-lisp :folded t\n(+ 1 2)\n#+end_src")))
    (should (string-match-p "<details.*\n<summary>.*>emacs-lisp</font>" html))
    ;; The code itself is still exported by Org, exactly as an unfolded src block.
    (should (string-match-p "<summary>\\(.\\|\n\\)*<pre class=\"src src-emacs-lisp\">\\(<code>\\)?(\\+ 1 2)\\(.\\|\n\\)*</details>" html))))

(deftest "a src block with :folded \"TITLE\" is titled TITLE; with :folded nil, or no :folded, it is left alone"
  (should (string-match-p ">See the code</font>"
                          (org-export-string "#+begin_src emacs-lisp :folded \"See the code\" :exports code\n(+ 1 2)\n#+end_src")))
  (should-not (string-match-p "<details" (org-export-string "#+begin_src emacs-lisp :folded nil\n(+ 1 2)\n#+end_src")))
  (should-not (string-match-p "<details" (org-export-string "#+begin_src emacs-lisp\n(+ 1 2)\n#+end_src"))))

(org-deftest "a folded src block within a list item stays in that item"
  :text "- item\n  #+begin_src emacs-lisp :folded t\n  (+ 1 2)\n  #+end_src\n- next"
  (org--fold-src-blocks 'html)
  (should (equal (buffer-string)
                 "- item\n  #+begin_details \"emacs-lisp\"\n  #+begin_src emacs-lisp :folded t\n  (+ 1 2)\n  #+end_src\n  #+end_details\n- next")))

;;; toggle-details links

(define-org-export-test "a toggle-details link is a button that unfolds every details block, or folds them all" [toggle-details]
  :input "
          [[toggle-details:][Show me everything]]"
  :equals "<p>
           <button onclick=\"let all = [...document.querySelectorAll('details')], open = all.some(d => !d.open); all.forEach(d => d.open = open);\">Show me everything</button></p>
           ")

(deftest "a toggle-details link is labelled “Unfold all details” by default, and exports to nothing in LaTeX" [toggle-details]
  (should (string-match-p ">Unfold all details</button>" (org-export-string "\n[[toggle-details:]]")))
  (should (equal "" (org-export-string "\n[[toggle-details:]]" 'latex))))

(provide 'org-details-block-tests)
;;; org-details-block-tests.el ends here
