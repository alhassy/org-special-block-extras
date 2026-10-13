;;; box.el --- Snapshot of the `box' showcase example  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

;; Unmaintained:
;;   For the LaTeX backend, the following call is required.
(define-org-example-test "box"
  :input "It can be useful to draw attention to some important text by
enclosing it in a [[doc:org-block/box][box]].

#+begin_box Uses of callout boxes
Such boxes often callout tips, warnings, cautionary info or
emphasises core information.
#+end_box"
  :html "<p>
It can be useful to draw attention to some important text by
enclosing it in a <abbr class=\"tooltip\" title=\"Enclose text in a box, possibly with a title.<br><br>By default, the box’s COLOR is green for HTML and red for LaTeX,<br>and it has no TITLE.<br><br>SHADOW is an alternate style of boxing: It shows the contents in a centred<br>box with a shadow colour being the given non-nil value of SHADOW.<br>If SHADOW is a hexidecimal colour, it should be a string enclosed in double quotes.<br>More accurately, the following are all valid uses:<br><br>&emsp;&emsp;#+begin_box title :shadow t<br>&emsp;&emsp;#+begin_box title :shadow inset<br><br>&emsp;&emsp;#+begin_box title :shadow ''pink''<br>&emsp;&emsp;#+begin_box title :shadow pink<br><br>&emsp;&emsp;#+begin_box title :shadow (cyan pink orange yellow)<br>&emsp;&emsp;#+begin_box title :shadow (cyan ''inset pink'' orange)<br>&emsp;&emsp;#+begin_box title :shadow (:left cyan :right pink :deep-right orange :deep-left yellow)<br><br>Notice that prefixing colours with ‘inset’ causes the colour to be within the box,<br>rather than spread, with blur, around the box. The use of ‘inset’ swaps left and right.<br><br>The HTML export uses a padded div, whereas the LaTeX export<br>requires the tcolorbox package.<br><br>In the future, I will likely expose more arguments.<br><br>Regarding the Lisp function:<br>+ BACKEND refers to the current export backend.<br>+ RAW-CONTENTS refers to the text as the user wrote it verbatim.<br>&emsp;⇒ You may mention CONTENTS to refer to the ‘org parsed’ version of user text.<br>&emsp;⇒ CONTENTS and RAW-CONTENTS are identical whenever key CONTENTS-OCCUR-AS-LINK-DESCRIPTION is non-nil.<br><br>For example, upon LaTeX export, the Org special block<br><br>&emsp;&emsp; #+begin_box<br>&emsp;&emsp; Hello, world<br>&emsp;&emsp; #+end_box<br><br> is rewritten to the result of the call<br><br>&emsp;&emsp; (org-block/box ‘latex ''Hello, world'')\">box</abbr>.
</p>

<div style=\"padding: 1em;background-color: #CCFFCC;border-radius: 15px;font-size: 0.9em;\"> <h3>Uses of callout boxes</h3> 
<p>
Such boxes often callout tips, warnings, cautionary info or
emphasises core information.
</p>
</div>"
  :latex "It can be useful to draw attention to some important text by
enclosing it in a \\hyperref[o-glossary-org-block/box]{box}\\label{o-glossary-declaration-site-org-block/box}.

\\begin{tcolorbox}[title={Uses of callout boxes},colback=red!5!white
,colframe=red!75!black
,colbacktitle=yellow!50!red
,coltitle=red!25!black, fonttitle=\\bfseries,subtitle style={boxrule=0.4pt, colback=yellow!50!red!25!white}]
Such boxes often callout tips, warnings, cautionary info or
emphasises core information.
\\end{tcolorbox}")
