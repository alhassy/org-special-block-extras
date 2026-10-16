;;; org-markdown-export-tests.el --- Tests for the blocks' Markdown exports, and the bugs found en route  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-export-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))
(require 'ox-md)

(define-org-export-test "a highlight is <mark>ed in Markdown: a link's text, and a block paragraph by paragraph" [markdown]
  :input "
          #+options: toc:nil
          See [[highlight:pink][this]].

          #+begin_highlight
          One.

          Two.
          #+end_highlight"
  :to md
  :equals "See <mark>this</mark>.
           
           
           
           <mark>One.</mark>
           
           <mark>Two.</mark>
           
           ")

(define-org-export-test "a box is a quote in Markdown, opening with its title in bold" [markdown]
  :input "
          #+options: toc:nil
          #+begin_box Exercise
          What is 2 + 2?
          #+end_box"
  :to md
  :equals "

           > **Exercise**
           >
           > What is 2 + 2?

           ")

(define-org-export-test "a remark block is a note alert in Markdown, titled by its editor, its replacement spelled out" [markdown]
  :input "
          #+options: toc:nil
          #+begin_remark Musa
          Teh cat.
          #+replacewith:
          The cat.
          #+end_remark"
  :to md
  :equals "

           > [!NOTE]
           >
           > **Musa**
           >
           > **Replace:** Teh cat.
           >
           > **With:** The cat.

           ")

(define-org-export-test "a proof tree is a $$ display in Markdown, since GitHub's maths knows no \\[" [markdown]
  :input "
          #+options: toc:nil
          #+begin_tree
          + MP :: q
            - p
            - p ⇒ q
          #+end_tree"
  :to md
  :equals "$$\\frac{\\displaystyle p \\qquad p ⇒ q}{q}[\\text{MP}]$$

           ")

(define-org-export-test "an org-demo is its source as code, then its result, each under a bold label, in Markdown" [markdown]
  :input "
          #+options: toc:nil
          #+begin_org-demo
          *bold*
          #+end_org-demo"
  :to md
  :equals "

           **Source**

               *bold*

           **Result**

           **bold**

           ")

(define-org-export-test "a glossary tooltip's title is escaped plain text in Markdown, which has no tooltipster" [markdown]
  :input "
          #+options: toc:nil
          #+begin_documentation Monoid :label mon
          A set with an associative <op> & a \"unit\".
          Really.
          #+end_documentation

          doc:mon"
  :to md
  :equals "

<abbr title=\"A set with an associative &lt;op&gt; &amp; a
&quot;unit&quot;.&#10;Really.\">Monoid</abbr>

")

(define-org-export-test "a spoiler is numbered blanks in Markdown, with the answers folded away beneath" [markdown]
  :input "
          #+options: toc:nil
          #+begin_spoiler
          ((Yusuf)) met ((Yacoub)).
          #+end_spoiler"
  :to md
  :equals "

           <ins>&emsp;1&emsp;</ins> met <ins>&emsp;2&emsp;</ins>.

           <details><summary>Answers</summary>

           1.  Yusuf
           2.  Yacoub

           </details>

           ")

(define-org-export-test "latex-definitions export nothing in Markdown, whose renderers would show them" [markdown]
  :input "
          #+options: toc:nil
          Before.
          #+begin_latex-definitions
          \\def\\N{\\mathbb{N}}
          #+end_latex-definitions
          After."
  :to md
  :equals "Before.
           
           
           
           After.
           
           ")

;;; Bugs found en route

(define-org-call-test "a badge sans colour is green, and sans logo asks for none"
  :call (org-link/badge "key|value" nil 'html)
  :returns "<img src=\"https://img.shields.io/badge/key-value-green\">")

(deftest "a tooltip's Org is exported by Org, and a docstring is shown as written: (* 25) and /etc/ stay as is" [tooltip]
  (should (equal (org--tooltip-html "A /typed/ monoid; e.g., (* 25).") "<p>\nA <i>typed</i> monoid; e.g., (* 25).</p>"))
  (should (equal (org--tooltip-html "(* 25)\nthe '/etc/passwd' <file>" :plain) "<pre>(* 25)\nthe '/etc/passwd' &lt;file&gt;</pre>")))

(provide 'org-markdown-export-tests)
;;; org-markdown-export-tests.el ends here
