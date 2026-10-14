;;; details.el --- Snapshot of the `details' showcase example  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

;; Unmaintained:
;;   1. The LaTex implementation requires:
;;      #+latex_header: \usepackage{tcolorbox}
;;
;;   2. The use of quotes in the :title-color keyword is only required for the LaTeX backend.
;;
;;   3. In the LaTeX implementation, We could use src_latex[:exports
;;      code]{\begin{quote}\fbox{\parbox{\linewidth}{\textbf{Details:}
;;      ...}}\end{quote}}; however, this does not work well with
;;      [[https://github.com/alhassy/emacs.d#bibliography--coloured-latex-using-minted][minted]]
;;      for coloured source blocks. Instead, we use ~tcolorbox~.
(define-org-example-test "details"
  :input "
Sometimes there is a remark or a code snippet that is useful to
have, but not relevant to the discussion at hand and so we want to
/fold away such [[doc:org-block/details][details]]/.

#+begin_details Example use cases
+ ‘Conversation-style’ articles, where the author asks the reader questions
  whose answers are “folded away” so the reader can think about the exercise
  before seeing the answer.

+ Hiding boring but important code snippets, such as a list of import
  declarations or a tedious implementation.
#+end_details

Full syntax ~#+begin_details \"title\" :background-color \"cyan\" :title-color \"green\"~."
  :html "<p>
Sometimes there is a remark or a code snippet that is useful to
have, but not relevant to the discussion at hand and so we want to
<i>fold away such <abbr class=\"tooltip\" title=\"Enclose contents in a folded up box, for HTML.<br><br>For LaTeX, this is just a boring, but centered, box.<br><br>By default, the TITLE of such blocks is “Details”,<br>its TITLE-COLOR is green, and BACKGROUND-COLOR is “#e5f5e5”.<br><br>In HTML, we show folded, details, regions with a nice greenish colour.<br><br>Regarding the Lisp function:<br>+ BACKEND refers to the current export backend.<br>+ RAW-CONTENTS refers to the text as the user wrote it verbatim.<br>&emsp;⇒ You may mention CONTENTS to refer to the ‘org parsed’ version of user text.<br>&emsp;⇒ CONTENTS and RAW-CONTENTS are identical whenever key CONTENTS-OCCUR-AS-LINK-DESCRIPTION is non-nil.<br><br>For example, upon LaTeX export, the Org special block<br><br>&emsp;&emsp; #+begin_details<br>&emsp;&emsp; Hello, world<br>&emsp;&emsp; #+end_details<br><br> is rewritten to the result of the call<br><br>&emsp;&emsp; (org-block/details ‘latex ''Hello, world'')\">details</abbr></i>.
</p>

<details class=\"code-details\" style=\"padding: 1em; background-color: #e5f5e5; border-radius: 15px; color: hsl(157 75% 20%); font-size: 0.9em; box-shadow: 0.05em 0.1em 5px 0.01em #00000057;\">
<summary><strong><font face=\"Courier\" size=\"3\" color=\"green\">Example use cases</font></strong></summary>
<ul class=\"org-ul\">
<li>‘Conversation-style’ articles, where the author asks the reader questions
whose answers are “folded away” so the reader can think about the exercise
before seeing the answer.</li>

<li>Hiding boring but important code snippets, such as a list of import
declarations or a tedious implementation.</li>
</ul>

</details>

<p>
Full syntax <code>#+begin_details \"title\" :background-color \"cyan\" :title-color \"green\"</code>.
</p>"
  :latex "Sometimes there is a remark or a code snippet that is useful to
have, but not relevant to the discussion at hand and so we want to
\\emph{fold away such \\hyperref[o-glossary-org-block/details]{details}\\label{o-glossary-declaration-site-org-block/details}}.

\\definecolor{osbe-bg}{HTML}{e5f5e5}\\colorlet{osbe-fg}{green}\\begin{quote}
\\begin{tcolorbox}[colback=osbe-bg,colframe=osbe-fg,title={Example use cases},sharp corners,boxrule=0.4pt]
\\begin{itemize}
\\item ‘Conversation-style’ articles, where the author asks the reader questions
whose answers are “folded away” so the reader can think about the exercise
before seeing the answer.

\\item Hiding boring but important code snippets, such as a list of import
declarations or a tedious implementation.
\\end{itemize}

\\end{tcolorbox}
\\end{quote}

Full syntax \\texttt{\\#+begin\\_details \"title\" :background-color \"cyan\" :title-color \"green\"}.")
