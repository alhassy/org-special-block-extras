;;; org-smartdiagram-block-tests.el --- #+begin_smartdiagram TITLE :type TYPE: an Org list ↦ LaTeX's smartdiagram  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

(defun tests-smartdiagram--latex (type body &optional title)
  "Returns the LaTeX a `#+begin_smartdiagram TITLE :type TYPE' block with BODY typesets."
  (org--smartdiagram body title type))

;;; Types and shapes

(deftest "a constellation's one top item is its planet, and that item's children its satellites, in order"
  (let ((latex (tests-smartdiagram--latex 'constellation "
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
                                   (tests-smartdiagram--latex 'descriptive "
+ Read :: =org--graph-read= turns the body into a tree, of items
+ Parse :: =org--graph-parse= resolves ids")))))

(deftest "a flow's items are its top items; nesting, or a constellation of two planets, is an error"
  (should (s-contains-p "{Read},\n  {Parse}" (tests-smartdiagram--latex 'flow "
1. Read
2. Parse")))
  (should-error (tests-smartdiagram--latex 'flow "
+ Read
  + Parse") :type 'user-error)
  (should-error (tests-smartdiagram--latex 'constellation "
+ Sun
+ Moon") :type 'user-error))

;;; Settings and item rows

(deftest "#+graph: rows are \\smartdiagramset's, each value braced; an unknown key, or #+node:, is an error"
  (should (s-contains-p "\\smartdiagramset{planet color={black!75},\n  font={\\small\\sffamily}}"
                        (tests-smartdiagram--latex 'constellation "
#+graph:
| planet color | black!75        |
| font         | \\small\\sffamily |
+ Sun
  + Earth")))
  (should-error (tests-smartdiagram--latex 'constellation "
#+graph:
| plant color | black!75 |
+ Sun") :type 'user-error)
  (should-error (tests-smartdiagram--latex 'flow "
#+node:
| shape | box |
+ A") :type 'user-error))

(deftest "an item's color takes its slot of the color list, the planet's is the planet color; fontcolor colors its text"
  (let ((latex (tests-smartdiagram--latex 'constellation "
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
  (should (s-contains-p "{\\color{gray}Venus}" (tests-smartdiagram--latex 'flow "
#+name: faint
| fontcolor | gray |
+ Venus :faint:")))
  (should-error (tests-smartdiagram--latex 'flow "
+ Venus
  | shape | box |") :type 'user-error))

;;; Title and prose

(deftest "the title sits above the diagram, a \\\\ breaking it; prose after the list sits beneath, rules and src blocks too"
  (let ((latex (tests-smartdiagram--latex 'flow "
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

;;; Rendering and export: a thin facade over #+begin_image latex

(deftest "a smartdiagram sans :type is a user error naming the types"
  (should-error (tests-smartdiagram--latex nil "
+ A") :type 'user-error))

(deftest "C-c C-c's images come from #+begin_image latex, when pdflatex and smartdiagram are installed"
  (skip-unless (and (executable-find "pdflatex") (executable-find "pdftocairo")
                    (zerop (call-process "kpsewhich" nil nil nil "smartdiagram.sty"))))
  (let ((default-directory (make-temp-file "org-smartdiagram-test-" t)))
    (unwind-protect
        (progn (org--image-render 'latex (org--smartdiagram-image-source (tests-smartdiagram--latex 'flow "
+ A
+ B")) nil "flow")
               (should (< 0 (file-attribute-size (file-attributes "flow.png"))))
               (should (file-exists-p "flow.svg")))
      (delete-directory default-directory t))))

(deftest "LaTeX export typesets a smartdiagram in place, and asks for smartdiagram in the preamble"
  (with-temp-buffer
    (let ((org--latex-head-used nil))
      (should (s-contains-p "\\begin{center}\n{" (org--smartdiagram-block 'latex "\n+ A\n+ B" nil :type 'flow)))
      (should (member org-smartdiagram-latex-head org--latex-head-used)))))

(deftest "a smartdiagram's {{{macros}}} expand by the buffer's #+macro: lines, as a graph's do"
  (with-temp-buffer
    (org-mode)
    (insert "#+macro: hi Hello\n")
    (let ((org--latex-head-used nil))
      (should (s-contains-p "{Hello},\n  {B}" (org--smartdiagram-block 'latex "\n+ {{{hi}}}\n+ B" nil :type 'flow))))))

(deftest "HTML export is #+begin_image latex's: an inline SVG of the LaTeX, its packages above a line of dashes"
  (let ((svg (make-temp-file "org-smartdiagram-test-" nil ".svg" "<svg/>")) seen)
    (unwind-protect
        (cl-letf (((symbol-function 'org--image-file)
                   (lambda (language source _args _anchor extension) (setq seen (list language source extension)) svg)))
          (with-temp-buffer
            (should (s-contains-p "<img class=\"osbe-image\" alt=\"Steps\" src=\"data:image/svg+xml;base64,"
                                  (org--smartdiagram-block 'html "\n+ A\n+ B" "Steps" :type 'flow))))
          (should (eq 'latex (car seen)))
          (should (s-starts-with-p (concat org-smartdiagram-latex-head "\n-----\n{") (cadr seen)))
          (should (equal "svg" (caddr seen))))
      (delete-file svg))))

;;; Completion and eldoc

(org-deftest "on a #+begin_smartdiagram line, a :KEY completes to the block's own keys, not Graphviz's"
  :text "
#+begin_smartdiagram :type constellation :r
+ A
#+end_smartdiagram"
  :at ":r"
  (let ((candidates (nth 2 (org-graph--completion-at-point))))
    (should (equal '(":type" ":file" ":resolution" ":as-image") candidates))))

(org-deftest "on a #+begin_smartdiagram line, :type completes to the diagram types"
  :text "
#+begin_smartdiagram :type con
+ A
#+end_smartdiagram"
  :at "con"
  (should (member "constellation" (nth 2 (org-graph--completion-at-point)))))

(org-deftest "in a smartdiagram, the #+graph: table completes smartdiagram's keys and an item's table its few"
  :text "
#+begin_smartdiagram :type constellation
#+graph:
| planet |
+ Sun
  | col |
#+end_smartdiagram"
  :at "planet"
  (let ((candidates (nth 2 (org-graph--completion-at-point))))
    (should (member "planet color" candidates))
    (should-not (member "rankdir" candidates)))
  (search-forward "| col")
  (should (equal '("color" "fontcolor" "fontsize") (nth 2 (org-graph--completion-at-point)))))

(org-deftest "in a smartdiagram, eldoc explains a #+graph: row's key with smartdiagram's default"
  :text "
#+begin_smartdiagram :type constellation
#+graph:
| planet color | black!75 |
+ Sun
#+end_smartdiagram"
  :at "planet color"
  (let (doc)
    (org-graph--eldoc (lambda (d &rest _) (setq doc d)))
    (should (equal "The color of a constellation's planet.  Default: lightgray!60." doc))))

(org-deftest "C-c ' on a smartdiagram edits it in a buffer that completes smartdiagram's keys"
  :text "
#+begin_smartdiagram :type flow
#+graph:
| font |
+ A
#+end_smartdiagram"
  :at "+ A"
  (org-edit-special)
  (unwind-protect
      (progn (goto-char (point-min)) (search-forward "| font") (should (org-graph--smartdiagram-p))
             (should (member "font" (nth 2 (org-graph--completion-at-point)))))
    (org-edit-src-abort)))

(provide 'org-smartdiagram-block-tests)
;;; org-smartdiagram-block-tests.el ends here
