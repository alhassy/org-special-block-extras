;;; tooltip.el --- Snapshot of the `tooltip' showcase example  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

(define-org-example-test "tooltip"
  :input "It can be annoying to make readers jump to the bottom of the page to
read a footnote.  Instead, I prefer to write an /inline [[doc:org-block/tooltip][remark]]/
containing, say, references or additional explanation.

   /[[tooltip:Allah][The God of Abraham; known as Elohim in the Bible]] does not burden a
       soul beyond what it can bear./ --- Quran 2:286

Or, without specifying an explicit label:

   /Allah[[tooltip:][The God of Abraham; known as Elohim in the Bible]] does not
       burden a soul beyond what it can bear./ --- Quran 2:286"
  :html "<p>
It can be annoying to make readers jump to the bottom of the page to
read a footnote.  Instead, I prefer to write an <i>inline <abbr class=\"tooltip\" title=\"Produce an HTML tooltip.<br><br>Regarding the Lisp function:<br>+ BACKEND refers to the current export backend.<br>+ RAW-CONTENTS refers to the text as the user wrote it verbatim.<br>&emsp;⇒ You may mention CONTENTS to refer to the ‘org parsed’ version of user text.<br>&emsp;⇒ CONTENTS and RAW-CONTENTS are identical whenever key CONTENTS-OCCUR-AS-LINK-DESCRIPTION is non-nil.<br><br>For example, upon LaTeX export, the Org special block<br><br>&emsp;&emsp; #+begin_tooltip<br>&emsp;&emsp; Hello, world<br>&emsp;&emsp; #+end_tooltip<br><br> is rewritten to the result of the call<br><br>&emsp;&emsp; (org-block/tooltip ‘latex ''Hello, world'')\">remark</abbr></i>
containing, say, references or additional explanation.
</p>

<p>
<i><abbr class=\"tooltip\" title=\"The God of Abraham; known as Elohim in the Bible\">Allah</abbr>&emsp13; does not burden a
    soul beyond what it can bear.</i> &mdash; Quran 2:286
</p>

<p>
Or, without specifying an explicit label:
</p>

<p>
<i>Allah<abbr class=\"tooltip\" title=\"The God of Abraham; known as Elohim in the Bible\">°</abbr>&emsp13; does not
    burden a soul beyond what it can bear.</i> &mdash; Quran 2:286
</p>"
  :latex "It can be annoying to make readers jump to the bottom of the page to
read a footnote.  Instead, I prefer to write an \\emph{inline \\hyperref[o-glossary-org-block/tooltip]{remark}\\label{o-glossary-declaration-site-org-block/tooltip}}
containing, say, references or additional explanation.

\\emph{<abbr class=\"tooltip\" title=\"The God of Abraham; known as Elohim in the Bible\">Allah</abbr>&emsp13; does not burden a
    soul beyond what it can bear.} --- Quran 2:286

Or, without specifying an explicit label:

\\emph{Allah<abbr class=\"tooltip\" title=\"The God of Abraham; known as Elohim in the Bible\">°</abbr>&emsp13; does not
    burden a soul beyond what it can bear.} --- Quran 2:286")
