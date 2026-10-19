;;; org-block-header-args-tests.el --- Tests for default block arguments, as src blocks' header-args  -*- lexical-binding: t; -*-

(unless (fboundp 'using)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

;;; ⟨1⟩ org-babel-default-header-args:NAME, in Lisp

(deftest "org-babel-default-header-args:NAME overrides a block's default when no local value is passed" [header-args]
  (using (org-defblock test-header-args-block (main-arg "" color "black") "" color)
    (dlet ((org-babel-default-header-args:test-header-args-block '((:color . "purple"))))
      (should (string-match-p "purple" (org-block/test-header-args-block 'html "" nil))))))

(deftest "`org-defblock' takes its main argument's default, by its name, from org-babel-default-header-args:NAME" [header-args]
  (using (org-defblock greeting (name "user" punct "!") "Greeting block" (format "Hello, %s%s" name punct))
    (dlet ((org-babel-default-header-args:greeting '((:name . "dev") (:punct . "!×4"))))
      (should (string= (org-block/greeting 'test-backend "some content" nil :punct nil)
                       (lf-string "#+begin_export test-backend
                                   Hello, dev!×4
                                   #+end_export"))))))

(deftest "`org-defblock-only' takes its defaults from org-babel-default-header-args:NAME too" [header-args]
  (using (org-defblock-only greeting (name "user" punct "!") "Greeting block" (format "Hello, %s%s" name punct))
    (dlet ((org-babel-default-header-args:greeting '((:name . "dev") (:punct . "!×4"))))
      (should (string= (org-block/greeting 'test-backend "some content" nil :punct nil)
                       (lf-string "#+begin_export test-backend
                                   Hello, dev!×4
                                   #+end_export"))))))

(deftest "a cl-defun-style org-defblock honours org-babel-default-header-args:NAME" [header-args]
  (using (org-defblock folded (&optional (title "More") &key (color "green") &context contents)
           "Fold CONTENTS under TITLE."
           (format "%s/%s: %s" title color contents))
    (dlet ((org-babel-default-header-args:folded '((:color . "teal"))))
      (should (s-contains-p "More/teal" (org-export-string "
#+begin_folded
Hello
#+end_folded" 'latex))))))

;;; ⟨2⟩ #+PROPERTY: header-args:NAME, ⟨3⟩ :header-args:NAME: drawers, ⟨4⟩ #+header: lines

(deftest "a block inherits header-args:NAME as a src block does: file-wide, then by heading, then by #+header:" [header-args]
  (using (org-defblock tint (&optional (title "Untitled") &key (color "black") (size "1em"))
           "A tinted title." (format "[%s/%s/%s]" title color size))
    (should (string-match-p (rx "[Untitled/red/2em]" (* anything) "[Untitled/teal/1em]" (* anything) "[Untitled/teal/3em]"
                                (* anything) "[Note/blue/3em]")
                            (org-export-string "
#+property: header-args:tint :color red :size 2em

#+begin_tint
File-wide: red, at 2em.
#+end_tint

* Section
:PROPERTIES:
:header-args:tint: :color teal
:END:

#+begin_tint
A heading's wins over the file's, which it replaces: teal, at 1em.
#+end_tint

** Subsection
:PROPERTIES:
:header-args:tint+: :size 3em
:END:

#+begin_tint
A + adds to what is inherited: teal, at 3em.
#+end_tint

#+header: :title Note
#+begin_tint :color blue
The block's own line wins over all, and #+header: over the headings: a blue Note, at 3em still.
#+end_tint" 'latex)))))

(deftest "a block's switches, such as :let, are inherited by header-args:NAME too" [header-args switches]
  (should (string-match-p "<p>\np ⇒ q\n</p>" (org-export-string "
* Logic
:PROPERTIES:
:header-args:red: :let (-> \"⇒\")
:END:

#+begin_red
p -> q
#+end_red" 'html))))

(deftest "a src block inherits our switches from Babel's header-args:LANG, beside Babel's own header arguments" [header-args switches]
  (let ((html (org-export-string "
#+property: header-args:emacs-lisp :folded t :exports code

#+begin_src emacs-lisp
(+ 1 2)
#+end_src" 'html)))
    (should (string-match-p "<details" html))
    (should (string-match-p "emacs-lisp" html))))

(org-deftest "C-c C-c runs a block's inherited :C-c_C-c switch" [header-args]
  :text "
* Quiz
:PROPERTIES:
:header-args:quiz: :C-c_C-c (setq org-block-header-args-tests--graded t)
:END:
#+begin_quiz
What is 1 + 1?
#+end_quiz"
  :at "What is"
  (using (org-defblock quiz (&context contents) "A quiz." contents)
    (dlet ((org-block-header-args-tests--graded nil))
      (org-ctrl-c-ctrl-c)
      (should org-block-header-args-tests--graded))))

(deftest "a header word Lisp reads only by unescaping it, such as the line break \\\\, stays as written" [header-args]
  (should (equal '("One" "\\\\" "two" :n 2) (--map (if (symbolp it) (if (keywordp it) it (symbol-name it)) it)
                                                  (org--read-header "One \\\\ two :n 2")))))

(provide 'org-block-header-args-tests)
;;; org-block-header-args-tests.el ends here
