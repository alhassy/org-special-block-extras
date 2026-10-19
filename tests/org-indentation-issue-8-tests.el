;;; org-indentation-issue-8-tests.el --- Indentation preservation ---Issue ♯8

(unless (fboundp 'using)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

;;; Indentation preservation -- Issue ♯8

(define-org-export-test "indented blocks preserve list structure in HTML output" [issue♯8]
  :input "- item one
          - item two
            #+begin_testblock foo
            inner
            #+end_testblock
          - item three"
  :using (org-defblock testblock (&context contents)  "docs"  (concat "HANDLED:" contents))
  :equals
  "<ul class=\"org-ul\">
   <li>item one</li>
   <li><p>
   item two
   </p>
   HANDLED:
   <p>
   inner
   </p></li>
   <li>item three</li>
   </ul>
   ")

(define-org-export-test "indented blocks preserve list structure in LaTeX export" [issue♯8]
  :input "1. First
          2. Second
             #+begin_testblock foo
             inner
             #+end_testblock
          3. Third"
  :to latex
  :using (org-defblock testblock (&context contents)  "docs"  (concat "HANDLED:" contents))
  :equals
  "\\begin{enumerate}
   \\item First
   \\item Second
   HANDLED:
   inner
   \\item Third
   \\end{enumerate}
   ")

(define-org-export-test "indented blocks, in enumerations, do not introduce extra blank lines" [issue♯8]
  :input "- A
          #+begin_testblock foo
          body
          #+end_testblock
          next line
          - B"
  :using (org-defblock testblock (&context contents)  "docs"  (concat "HANDLED:" contents))
  :equals
  "<ul class=\"org-ul\">
   <li>A</li>
   </ul>
   HANDLED:
   <p>
   body
   </p>

   <p>
   next line
   </p>
   <ul class=\"org-ul\">
   <li>B</li>
   </ul>
   ")

;; This is the reproducible test case of issue ♯8.
;; https://github.com/alhassy/org-special-block-extras/issues/8#issue-814851814
(define-org-export-test "enumerations are preserved when they contain special blocks, for LaTeX" [issue♯8]
  :input "
          1. builtin source block
             #+begin_src latex
             \\uline{content 1}
             #+end_src
          2. custom (unregistered) block
             #+begin_proposition
             content 2
             #+end_proposition
          3. custom (registered) block
             #+begin_testblock nil
             content 3
             #+end_testblock
          4. prose
             \\n content 4
          "
  :to latex
  :using (org-defblock testblock (&context contents)  "docs"  (concat "HANDLED:" contents))
  :equals
  "\\begin{enumerate}
   \\item builtin source block
   \\begin{verbatim}
      \\uline{content 1}
   \\end{verbatim}
   \\item custom (unregistered) block
   \\begin{proposition}
   content 2
   \\end{proposition}
   \\item custom (registered) block
   HANDLED:
   content 3
   \\item prose
   \\n content 4
   \\end{enumerate}
   ")

(define-org-export-test "enumerations are preserved when they contain special blocks, for HTML" [issue♯8]
  :input "
          1. builtin source block
             #+begin_src latex
             \\uline{content 1}
             #+end_src
          2. custom (unregistered) block
             #+begin_proposition
             content 2
             #+end_proposition
          3. custom (registered) block
             #+begin_testblock nil
             content 3
             #+end_testblock
          4. prose
             \\n content 4
          "
  :using (org-defblock testblock (&context contents)  "docs"  (concat "HANDLED:" contents))
  :equals
  "<ol class=\"org-ol\">
<li><p>
builtin source block
</p>
<div class=\"org-src-container\">
<pre class=\"src src-latex\"><code>   \\uline{content 1}
</code></pre>
</div></li>
<li><p>
custom (unregistered) block
</p>
<div class=\"proposition\" id=\"org-id-1\">
<p>
content 2
</p>

</div></li>
<li><p>
custom (registered) block
</p>
HANDLED:
<p>
content 3
</p></li>
<li>prose
\\n content 4</li>
</ol>
"
  :modulo ("style=\"color: #98971a; font-weight: bold;\"" ;; Style is theme-dependent
           ))


(define-org-export-test "enumerations are preserved for blocks inside lists inside blocks" [issue♯8]
  :input "
          #+begin_foo X
          1. Something\\
             Indented line no. 1
          2. Something else
             #+begin_bar Y
             Indented line no. 2
             #+end_bar
             Indented line no. 3
          3. Something else
          #+end_foo
          1. Something else
             #+begin_baz Z
             Indented line no. 4
             #+end_baz
          "
  :to latex
  :using ((org-defblock foo (&context contents) (format "FOO⟨%s⟩" contents))
          (org-defblock bar (&context contents) (format "BAR⟨%s⟩" contents))
          (org-defblock baz (&context contents) (format "BAZ⟨%s⟩" contents)))
  :equals
  "FOO⟨
   \\begin{enumerate}
   \\item Something$\\backslash$
   Indented line no. 1
   \\item Something else
   BAR⟨
   Indented line no. 2
   ⟩
   Indented line no. 3
   \\item Something else
   \\end{enumerate}
   ⟩
   \\begin{enumerate}
   \\item Something else
   BAZ⟨
   Indented line no. 4
   ⟩
   \\end{enumerate}
   ")

(org-deftest "a block's contents lose only their least indentation, so their lines keep their relative indentation" [issue♯8]
  :text "
- item
  #+begin_stutter 1
  line one
    indented two
  #+end_stutter"
  :at "#+begin"
  (beginning-of-line)
  (should (equal (org-special-block-contents (org-special-block-after-point "stutter"))
                 "line one\n  indented two")))

(deftest "a list within a block within a list keeps its items siblings, and the outer list carries on" [issue♯8]
  (let ((latex (org-export-string "
1. outer one
   #+begin_box Inner
   - inner a
   - inner b
     #+begin_remark
     deep remark
     #+end_remark
   #+end_box
   text continuing item one
2. outer two" 'latex)))
    ;; inner a and inner b in one itemize, rather than b nested within a; and outer two in the same enumerate.
    (should (string-match-p "\\\\item inner a\n\\\\item inner b" latex))
    (should (= 1 (s-count-matches "begin{itemize}" latex)))
    (should (string-match-p "text continuing item one\n\\\\item outer two\n\\\\end{enumerate}" latex))))

(deftest "a list of propositions, each item's proof a block of paragraphs, stays one list" [issue♯8]
  (using (org-defblock prf (&context contents) "A proof." (format "\\begin{proof}\n%s\n\\end{proof}" contents))
    (let ((latex (org-export-string "
- first
- second
  #+begin_prf
  Call f.

  Minimise f.
  #+end_prf
- third" 'latex)))
      (should (= 1 (s-count-matches "begin{itemize}" latex)))
      (should (string-match-p "\\\\item second\n\\\\begin{proof}\nCall f\\.\n\nMinimise f\\." latex))
      (should (string-match-p "end{proof}\n\\\\item third" latex)))))
