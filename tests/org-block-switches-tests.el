;;; org-block-switches-tests.el --- Switches every block understands: :noexport, :when, :let, :ignore, :boxed, :folded, :C-c_C-c  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-export-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

;;; Export switches

(define-org-export-test "a block marked :noexport t exports to nothing" [switches]
  :input "
          #+begin_red :noexport t
          A draft thought.
          #+end_red
          Published prose."
  :equals "<p>
           Published prose.</p>
           ")

(define-org-export-test "a block whose :when form is nil exports to nothing" [switches]
  :input "
          #+begin_red :when (eq backend 'latex)
          Only in print.
          #+end_red"
  :equals "")

(define-org-export-test "a block whose :when form is non-nil exports as usual; the form sees `backend'" [switches]
  :input "
          #+begin_red :when (eq backend 'latex)
          Only in print.
          #+end_red"
  :to latex
  :equals "\\begingroup\\color{red}
           Only in print.
           \\endgroup\\,
           ")

(define-org-export-test "a block marked :ignore t exports its contents as plain Org" [switches]
  :input "
          #+begin_red :ignore t
          Read /me/ plainly.
          #+end_red"
  :equals "<p>
           Read <i>me</i> plainly.</p>
           ")

(define-org-export-test "a block's :let replaces each whole-word variable by its value; later values see earlier ones" [switches]
  :input "
          #+begin_box Exercise :let (N 6 ANSWER (* N 7))
          What is N × 7? It is ANSWER; whereas Nx, and the word answer, are untouched.
          #+end_box"
  :equals "<div style=\"padding: 1em;background-color: #CCFFCC;border-radius: 15px;font-size: 0.9em;\"> <h3>Exercise</h3>
           <p>
           What is 6 × 7? It is 42; whereas Nx, and the word answer, are untouched.
           </p>
           </div>
           ")

(define-org-export-test "a block marked :boxed TITLE and :folded t is a box, folded away under the block's name" [switches]
  :input "
          #+begin_red :boxed \"Heads up\" :folded t
          Mind the gap.
          #+end_red"
  :equals "<details class=\"code-details\" style=\"padding: 1em; background-color: #e5f5e5; border-radius: 15px; color: hsl(157 75% 20%); font-size: 0.9em; box-shadow: 0.05em 0.1em 5px 0.01em #00000057;\">
           <summary><strong><font face=\"Courier\" size=\"3\" color=\"green\">Red</font></strong></summary>
           <div style=\"padding: 1em;background-color: #CCFFCC;border-radius: 15px;font-size: 0.9em;\"> <h3>Heads up</h3>
           <span style=\"color:red;\">
           <p>
           Mind the gap.
           </p>
           </span>
           </div>

           </details>
           ")

(define-org-export-test "a block marked :boxed t is a tcolorbox in LaTeX" [switches]
  :input "
          #+begin_red :boxed t
          Mind the gap.
          #+end_red"
  :to latex
  :equals "\\colorlet{osbe-box-back}{red!5!white}\\colorlet{osbe-box-frame}{red!75!black}\\colorlet{osbe-box-title}{yellow!50!red}\\begin{tcolorbox}[title={},colback=osbe-box-back,colframe=osbe-box-frame,colbacktitle=osbe-box-title,coltitle=red!25!black, fonttitle=\\bfseries,subtitle style={boxrule=0.4pt, colback=yellow!50!red!25!white}]
           \\begingroup\\color{red}
           Mind the gap.
           \\endgroup\\,
           \\end{tcolorbox}
           ")

(define-org-export-test "a block that declares a keyword of a switch's name receives it, rather than the switch" [switches]
  :input "
          #+begin_drawer :folded closed
          Socks.
          #+end_drawer"
  :using (org-defblock drawer (&key folded) (format "Drawer is %s: %s" folded contents))
  :equals "Drawer is closed:
           <p>
           Socks.
           </p>
           ")

(define-org-export-test "switches are welcome even on a block that refuses unknown keywords" [switches]
  :input "
          #+begin_strict :boxed Strictly
          Rules.
          #+end_strict"
  :using (org-defblock strict (&key colour) (format "STRICT %s" contents))
  :equals "<div style=\"padding: 1em;background-color: #CCFFCC;border-radius: 15px;font-size: 0.9em;\"> <h3>Strictly</h3>
           STRICT
           <p>
           Rules.
           </p>

           </div>
           ")

;;; Evaluating a switch's Lisp

(deftest "with `org-defblock-evaluate-switches' nil, a block's :when is an error rather than evaluated" [switches]
  (let ((org-defblock-evaluate-switches nil))
    (should-error (org-export-string "
#+begin_red :when t
Anything.
#+end_red")
                  :type 'user-error)))

(deftest "with `org-defblock-evaluate-switches' as query, each distinct form is asked about once" [switches]
  (let ((org-defblock-evaluate-switches 'query)
        (org--block-approved-forms nil)
        (asked 0))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) (cl-incf asked) t)))
      (org-export-string "
#+begin_red :when (eq backend 'html)
One.
#+end_red
#+begin_blue :when (eq backend 'html)
Two.
#+end_blue")
      (should (equal asked 1)))))

;;; C-c C-c

(defvar org--block-cc-switch-test-result nil
  "Set by the :C-c_C-c form of a test's block.")

(org-deftest "C-c C-c on a block with a :C-c_C-c form evaluates it" [switches C-c_C-c]
  :text "
#+begin_quiz :C-c_C-c (setq org--block-cc-switch-test-result \"Score: 3/4\")
What is 6 × 7?
#+end_quiz"
  :at "What"
  (let ((org--block-cc-switch-test-result nil))
    (should (org--block-cc-dispatcher))
    (should (equal org--block-cc-switch-test-result "Score: 3/4"))))

(org-deftest "C-c C-c on a block whose :C-c_C-c names a function calls it" [switches C-c_C-c]
  :text "
#+begin_quiz :C-c_C-c org--block-cc-switch-test-check
What is 6 × 7?
#+end_quiz"
  :at "What"
  (let ((called nil))
    (cl-letf (((symbol-function 'org--block-cc-switch-test-check) (lambda () (setq called t))))
      (org--block-cc-dispatcher)
      (should called))))

(org-deftest "C-c C-c on a block with a registered handler runs the handler, not its :C-c_C-c switch" [switches C-c_C-c]
  :text "
#+begin_quiz :C-c_C-c (error \"The switch should not run\")
What is 6 × 7?
#+end_quiz"
  :at "What"
  (let ((org--block-cc-handlers (list (cons "quiz" (lambda () 'handled)))))
    (should (eq (org--block-cc-dispatcher) 'handled))))

(org-deftest "C-c C-c on a block without a :C-c_C-c switch leaves the key to Org" [switches C-c_C-c]
  :text "
#+begin_quiz
What is 6 × 7?
#+end_quiz"
  :at "What"
  (should-not (org--block-cc-dispatcher)))

(provide 'org-block-switches-tests)
;;; org-block-switches-tests.el ends here
