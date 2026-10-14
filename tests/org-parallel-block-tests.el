;;; org-parallel-block-tests.el --- Tests for the parallel block  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(define-org-call-test "parallel block produces a multi-column CSS div for HTML."
  :call (org-block/parallel 'html "col1\n#+columnbreak:\ncol2" "2")
  :returns "#+begin_export html
<div style=\"column-count: 2;\">
#+end_export
col1
#+columnbreak:
col2
#+begin_export html
</div>
#+end_export")

(define-org-call-test "parallel block includes a column rule when :bar is set."
  :call (org-block/parallel 'html "text" "2" :bar "solid")
  :returns "#+begin_export html
<div style=\"column-count: 2; column-rule: 1px solid solid;\">
#+end_export
text
#+begin_export html
</div>
#+end_export")

(define-org-call-test "parallel block omits the column rule when :bar is nil."
  :call (org-block/parallel 'html "text" "2" :bar nil)
  :returns "#+begin_export html
<div style=\"column-count: 2;\">
#+end_export
text
#+begin_export html
</div>
#+end_export")

(define-org-call-test "parallel block uses the multicols environment for LaTeX."
  :call (org-block/parallel 'latex "text" "2")
  :returns "#+begin_export latex
\\par \\setlength{\\columnseprule}{0 pt}
\\begin{minipage}[t]{\\linewidth}
\\begin{multicols}{2}

#+end_export
text
#+begin_export latex

\\end{multicols}\\end{minipage}
#+end_export")

(define-org-call-test "parallel block converts #+columnbreak: to \\columnbreak in LaTeX."
  :call (org-block/parallel 'latex "a\n#+columnbreak:\nb" "2")
  :returns "#+begin_export latex
\\par \\setlength{\\columnseprule}{0 pt}
\\begin{minipage}[t]{\\linewidth}
\\begin{multicols}{2}

#+end_export
a
\\columnbreak
b
#+begin_export latex

\\end{multicols}\\end{minipage}
#+end_export")

;;; Hard columns

(deftest "parallel block's hard columns break only at whole #+columnbreak: lines, not at mentions of them"
  (let ((html (org-block/parallel 'html "Use ~#+columnbreak:~ here.\n#+columnbreak:\nRight." "50% 50%")))
    (should (s-contains-p "~#+columnbreak:~" html))
    (should (= 1 (s-count-matches "@@html:</div>" html)))
    (should-not (s-contains-p "width: nil" html))))

(deftest "parallel block treats any non-numeric widths as hard columns, such as 5em 20px"
  (should (s-contains-p "width: 5em" (org-block/parallel 'html "a\n#+columnbreak:\nb" "5em 20px"))))

(deftest "parallel block closes the column of a single hard width"
  (let ((html (org-block/parallel 'html "a" "100%")))
    (should (= (s-count-matches "<div" html) (s-count-matches "</div>" html)))))

(deftest "parallel block gives LaTeX the number of hard columns, never their widths"
  (should (s-contains-p "\\begin{multicols}{3}" (org-block/parallel 'latex "a" "25% 50% 25%"))))

(provide 'org-parallel-block-tests)
;;; org-parallel-block-tests.el ends here
