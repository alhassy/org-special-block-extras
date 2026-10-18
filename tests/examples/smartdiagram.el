;;; smartdiagram.el --- Snapshot of the `graph' block's `:smartdiagram' showcase example  -*- lexical-binding: t; -*-

;; This is only the showcase used by `osbe-example:smartdiagram' in the docs; the thorough tests
;; ---types, settings, item rows, prose, completion--- live in tests/graph/org-graph-smartdiagram-tests.el.

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

(define-org-example-test "smartdiagram"
  :input "Some lists want a picture rather than a graph: a centre and what orbits it, or a flow of steps.
~:smartdiagram TYPE~ hands the very same list to LaTeX's smartdiagram package instead of Graphviz:

#+begin_graph One Org source, rich HTML and LaTeX :smartdiagram constellation
#+graph:
| planet color              | black!75        |
| planet font               | \\bfseries       |
| planet text width         | 3cm             |
| satellite size            | 2.8cm           |
| satellite text width      | 2.6cm           |
| distance planet-satellite | 4.6cm           |
| font                      | \\small\\sffamily |

+ org-special-block-extras \\\\ define blocks & links
  | fontcolor | white |
  + =org-defblock= \\\\ custom blocks
  + =org-deflink= \\\\ custom links
  + =details= =parallel= \\\\ =proof= =margin=
  + =doc:= =kbd:= \\\\ =badge:= =fortune:=
  + HTML & LaTeX \\\\ one Org source
#+end_graph

Press =C-c C-c= inside the block to render it with =pdflatex=; LaTeX export typesets it in place."
  :html :live
  :latex "Some lists want a picture rather than a graph: a centre and what orbits it, or a flow of steps.
\\texttt{:smartdiagram TYPE} hands the very same list to LaTeX's smartdiagram package instead of
Graphviz:

\\begin{center}
{\\smartdiagramset{planet color={black!75},
  planet font={\\bfseries},
  planet text width={3cm},
  satellite size={2.8cm},
  satellite text width={2.6cm},
  distance planet-satellite={4.6cm},
  font={\\small\\sffamily}}%
\\ifdefined\\osbesmartdiagram\\else\\newsavebox\\osbesmartdiagram\\fi
\\sbox\\osbesmartdiagram{\\smartdiagram[constellation diagram]{%
  {\\color{white}org-special-block-extras\\\\
define blocks \\& links},
  {\\mbox{\\texttt{org-defblock}}\\\\
custom blocks},
  {\\mbox{\\texttt{org-deflink}}\\\\
custom links},
  {\\mbox{\\texttt{details}} \\mbox{\\texttt{parallel}}\\\\
\\mbox{\\texttt{proof}} \\mbox{\\texttt{margin}}},
  {\\mbox{\\texttt{doc:}} \\mbox{\\texttt{kbd:}}\\\\
\\mbox{\\texttt{badge:}} \\mbox{\\texttt{fortune:}}},
  {HTML \\& \\LaTeX{}\\\\
one Org source}}}%
\\begin{minipage}{\\wd\\osbesmartdiagram}\\centering
{\\large One Org source, rich HTML and \\LaTeX{}}\\par\\medskip
\\usebox\\osbesmartdiagram\\par
\\end{minipage}}
\\end{center}

Press \\texttt{C-c C-c} inside the block to render it with \\texttt{pdflatex}; \\LaTeX{} export
typesets it in place.")

(provide 'smartdiagram)
