;;; org-graph-smartdiagram-tests.el --- #+begin_graph :smartdiagram TYPE: an Org list ↦ LaTeX's smartdiagram  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

(defun tests-graph--smartdiagram (type body &optional title)
  "The LaTeX a `#+begin_graph TITLE :smartdiagram TYPE' block with BODY typesets."
  (org--smartdiagram body title (list :smartdiagram type)))

;;; Types and shapes

(deftest "a constellation's one top item is its planet, and that item's children its satellites, in order"
  (let ((latex (tests-graph--smartdiagram 'constellation "
+ =easy-access.el=
  + =(:k plist)= \\\\ replaces =(plist-get plist :k)=
  + =(:k hash)= \\\\ replaces =(gethash :k hash)=")))
    (should (s-contains-p "\\smartdiagram[constellation diagram]{%
  {\\mbox{\\texttt{easy-access.el}}},
  {\\mbox{\\texttt{(:k plist)}}\\\\
replaces \\mbox{\\texttt{(plist-get plist :k)}}},
  {\\mbox{\\texttt{(:k hash)}}\\\\
replaces \\mbox{\\texttt{(gethash :k hash)}}}}" latex))))

(deftest "a type's option and a two-word type are spelled as smartdiagram spells them"
  (should (equal "flow diagram:horizontal" (org--smartdiagram-type 'flow:horizontal)))
  (should (equal "connected constellation diagram" (org--smartdiagram-type 'connected-constellation)))
  (should-error (org--smartdiagram-type 'pie) :type 'user-error))

(deftest "a descriptive diagram's items are `+ TERM :: DESCRIPTION', each part braced, so commas stay text"
  (should (s-contains-p "{{Read},{=org--graph-read= turns the body into a tree, of items}}"
                        (s-replace "\\mbox{\\texttt{org-{}-{}graph-read}}" "=org--graph-read="
                                   (tests-graph--smartdiagram 'descriptive "
+ Read :: =org--graph-read= turns the body into a tree, of items
+ Parse :: =org--graph-parse= resolves ids")))))

(deftest "a flow's items are its top items; nesting, or a constellation of two planets, is an error"
  (should (s-contains-p "{Read},\n  {Parse}" (tests-graph--smartdiagram 'flow "
1. Read
2. Parse")))
  (should-error (tests-graph--smartdiagram 'flow "
+ Read
  + Parse") :type 'user-error)
  (should-error (tests-graph--smartdiagram 'constellation "
+ Sun
+ Moon") :type 'user-error))

;;; Settings and item rows

(deftest "#+graph: rows are \\smartdiagramset's, each value braced; an unknown key, or #+node:, is an error"
  (should (s-contains-p "\\smartdiagramset{planet color={black!75},\n  font={\\small\\sffamily}}"
                        (tests-graph--smartdiagram 'constellation "
#+graph:
| planet color | black!75        |
| font         | \\small\\sffamily |
+ Sun
  + Earth")))
  (should-error (tests-graph--smartdiagram 'constellation "
#+graph:
| plant color | black!75 |
+ Sun") :type 'user-error)
  (should-error (tests-graph--smartdiagram 'flow "
#+node:
| shape | box |
+ A") :type 'user-error))

(deftest "an item's color takes its slot of the colour list, the planet's is the planet color; fontcolor colours its text"
  (let ((latex (tests-graph--smartdiagram 'constellation "
+ Sun
  | color     | black!75 |
  | fontcolor | white    |
  + Mercury
  + Venus
    | color | orange!40 |")))
    (should (s-contains-p "set color list={red!40, orange!40}" latex))
    (should (s-contains-p "planet color={black!75}" latex))
    (should (s-contains-p "{\\color{white}Sun}" latex))))

(deftest "an item's tags splice #+name: templates, as in any graph; a Graphviz attribute on an item is an error"
  (should (s-contains-p "{\\color{gray}Venus}" (tests-graph--smartdiagram 'flow "
#+name: faint
| fontcolor | gray |
+ Venus :faint:")))
  (should-error (tests-graph--smartdiagram 'flow "
+ Venus
  | shape | box |") :type 'user-error))

;;; Title and prose

(deftest "the title sits above the diagram, a \\\\ breaking it; prose after the list sits beneath, rules and src blocks too"
  (let ((latex (tests-graph--smartdiagram 'flow "
+ A
+ B

-----

#+begin_src emacs-lisp
(seq-elt xs
         2)
#+end_src" "One syntax \\\\ =(key map)=")))
    (should (s-contains-p "{\\large One syntax\\\\\n\\mbox{\\texttt{(key map)}}}" latex))
    (should (s-contains-p "\\rule{\\linewidth}" latex))
    (should (s-contains-p "\\begin{varwidth}{\\linewidth}\n\\begin{verbatim}\n(seq-elt xs\n         2)" latex))
    (should (< (s-index-of "\\usebox" latex) (s-index-of "\\rule" latex)))))

;;; Rendering and export

(deftest "C-c C-c's PNG comes from pdflatex, when it is installed"
  (skip-unless (and (executable-find "pdflatex") (executable-find "pdftoppm")
                    (zerop (call-process "kpsewhich" nil nil nil "smartdiagram.sty"))))
  (let ((png (make-temp-file "org-graph-test-" nil ".png")))
    (unwind-protect
        (progn (should (equal '(0 . "") (org-graph--run-latex (tests-graph--smartdiagram 'flow "
+ A
+ B") "png" png 72)))
               (should (< 0 (file-attribute-size (file-attributes png)))))
      (delete-file png))))

(deftest "LaTeX export typesets a smartdiagram in place, and asks for smartdiagram in the preamble"
  (with-temp-buffer
    (let ((org--latex-head-used nil))
      (should (s-contains-p "\\begin{center}\n{" (org-block/graph 'latex "\n+ A\n+ B" nil :smartdiagram 'flow)))
      (should (member "smartdiagram" org--latex-head-used)))))

;;; Completion and eldoc

(org-deftest "on a smartdiagram's #+begin_graph line, a :KEY completes to the keys smartdiagrams take, not Graphviz's"
  :text "
#+begin_graph :smartdiagram constellation :d
+ A
#+end_graph"
  :at ":d"
  (let ((candidates (nth 2 (org-graph--completion-at-point))))
    (should (member ":dpi" candidates))
    (should-not (member ":rankdir" candidates))))

(org-deftest "on any #+begin_graph line, :smartdiagram completes to the diagram types"
  :text "
#+begin_graph :smartdiagram con
+ A
#+end_graph"
  :at "con"
  (should (member "constellation" (nth 2 (org-graph--completion-at-point)))))

(org-deftest "in a smartdiagram, the #+graph: table completes smartdiagram's keys and an item's table its few"
  :text "
#+begin_graph :smartdiagram constellation
#+graph:
| planet |
+ Sun
  | col |
#+end_graph"
  :at "planet"
  (let ((candidates (nth 2 (org-graph--completion-at-point))))
    (should (member "planet color" candidates))
    (should-not (member "rankdir" candidates)))
  (search-forward "| col")
  (should (equal '("color" "fontcolor" "fontsize" ":inherit") (nth 2 (org-graph--completion-at-point)))))

(org-deftest "in a smartdiagram, eldoc explains a #+graph: row's key with smartdiagram's default"
  :text "
#+begin_graph :smartdiagram constellation
#+graph:
| planet color | black!75 |
+ Sun
#+end_graph"
  :at "planet color"
  (let (doc)
    (org-graph--eldoc (lambda (d &rest _) (setq doc d)))
    (should (equal "The colour of a constellation's planet.  Default: lightgray!60." doc))))

(org-deftest "C-c ' on a smartdiagram edits it in a buffer that completes smartdiagram's keys"
  :text "
#+begin_graph :smartdiagram flow
#+graph:
| font |
+ A
#+end_graph"
  :at "+ A"
  (org-edit-special)
  (unwind-protect
      (progn (goto-char (point-min)) (search-forward "| font") (should (org-graph--smartdiagram-p))
             (should (member "font" (nth 2 (org-graph--completion-at-point)))))
    (org-edit-src-abort)))

(provide 'org-graph-smartdiagram-tests)
;;; org-graph-smartdiagram-tests.el ends here
