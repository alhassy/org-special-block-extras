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
enclosing it in a <abbr class=\"tooltip\"
tabindex=\"0\">box<template><pre>Enclose text in a box, possibly with a TITLE.

By default, the box’s BACKGROUND-COLOR is green for HTML and red for LaTeX;
for LaTeX, there are also FRAME-COLOR and TITLE-BACKGROUND-COLOR.

SHADOW is an alternate style of boxing: It shows the contents in a centred
box with a shadow colour being the given non-nil value of SHADOW.
If SHADOW is a hexadecimal colour, it should be a string enclosed in double quotes.
More accurately, the following are all valid uses:

    #+begin_box title :shadow t
    #+begin_box title :shadow inset

    #+begin_box title :shadow \"pink\"
    #+begin_box title :shadow pink

    #+begin_box title :shadow (cyan pink orange yellow)
    #+begin_box title :shadow (cyan \"inset pink\" orange)
    #+begin_box title :shadow (:left cyan :right pink :deep-right orange :deep-left yellow)

Notice that prefixing colours with ‘inset’ causes the colour to be within the box,
rather than spread, with blur, around the box. The use of ‘inset’ swaps left and right.

The HTML export uses a padded div, whereas the LaTeX export
requires the tcolorbox package; Markdown, sans colours, gets a quote opening with TITLE in bold.

Regarding the Lisp function:
+ BACKEND refers to the current export backend.
+ RAW-CONTENTS refers to the text as the user wrote it, sans its least indentation.
  ⇒ You may mention CONTENTS to refer to the ‘org parsed’ version of user text.
  ⇒ CONTENTS and RAW-CONTENTS are identical whenever key CONTENTS-OCCUR-AS-LINK-DESCRIPTION is non-nil.

For example, upon LaTeX export, the Org special block

     #+begin_box
     Hello, world
     #+end_box

 is rewritten to the result of the call

     (org-block/box ‘latex \"Hello, world\")</pre></template></abbr>.
</p>

<div style=\"padding: 1em;background-color: #CCFFCC;border-radius: 15px;font-size: 0.9em;\"> <h3>Uses
of callout boxes</h3> 
<p>
Such boxes often callout tips, warnings, cautionary info or
emphasises core information.
</p>
</div>"
  :latex "It can be useful to draw attention to some important text by
enclosing it in a \\hyperref[o-glossary-org-block/box]{box}
\\label{o-glossary-declaration-site-org-block/box}.

\\colorlet{osbe-box-back}{red!5!white}\\colorlet{osbe-box-frame}{red!75!black}
\\colorlet{osbe-box-title}{yellow!50!red}\\begin{tcolorbox}[title={Uses of callout boxes},
colback=osbe-box-back,colframe=osbe-box-frame,colbacktitle=osbe-box-title,coltitle=red!25!black,
fonttitle=\\bfseries,subtitle style={boxrule=0.4pt, colback=yellow!50!red!25!white}]
Such boxes often callout tips, warnings, cautionary info or
emphasises core information.
\\end{tcolorbox}")
