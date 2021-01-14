<h1> A unified interface for Emacs' Org-mode block & link types (•̀ᴗ•́)و </h1>

<h2> Which is used to obtain 30 new custom blocks and 34 link types ¯\_(ツ)_/¯ </h2>

<div align="center">

<div class="org-center">
<p>

</p>

<p>
<a href="https://www.gnu.org/software/emacs"><img src="https://img.shields.io/badge/Emacs-27-green?logo=gnu-emacs"></a>
<a href="https://orgmode.org"><img src="https://img.shields.io/badge/Org-9.4-blue?logo=gnu"></a>
</p>

<span>

<p>
<a href="https://github.com/alhassy/org-special-block-extras"><img src="https://img.shields.io/badge/org--special--block--extras-2.1-informational?logo=Gnu-Emacs"></a>
</p>

<a href="https://melpa.org/#/org-special-block-extras"><img alt="MELPA" src="https://melpa.org/packages/org-special-block-extras-badge.svg"/></a>

</span>

<p>
<a href="https://www.gnu.org/licenses/gpl-3.0.en.html"><img src="https://img.shields.io/badge/license-GNU_3-informational?logo=read-the-docs"></a>
<a href="https://github.com/alhassy/emacs.d#what-does-literate-programming-look-like"><img src="https://img.shields.io/badge/docs-literate-success?logo=read-the-docs"></a>
<a href="https://twitter.com/intent/tweet?text=This looks super neat (•̀ᴗ•́)و::&url=https://github.com/alhassy/org-special-block-extras"><img src="https://img.shields.io/twitter/url?url=https://github.com/alhassy/org-special-block-extras"></a>
<a href="https://github.com/alhassy/org-special-block-extras/issues"><img src="https://img.shields.io/badge/contributions-welcome-green?logo=nil"></a>
</p>

<p>
<a href="https://alhassy.github.io/"><img src="https://img.shields.io/badge/author-musa_al--hassy-purple?logo=nintendo-3ds"></a>
<a href="https://www.buymeacoffee.com/alhassy"><img src="https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee"></a>
</p>

<p>
<a href="https://alhassy.github.io/about"><img src="https://img.shields.io/badge/Hire-me-success?logo=nil"></a>
</p>

<p>
<a href="https://youtu.be/BQdNhtJSbqk"><img src="https://img.shields.io/badge/EmacsConf-2020-informational?logo=youtube"></a>
</p>
</div>

<div class="org-center">
<p>
<b>Abstract</b>
</p>
</div>

> The aim is to write something once using Org-mode markup
> then generate the markup for multiple backends.
> That is, ***write once, generate many!***
>
> In particular, we are concerned with *‘custom’, or ‘special’, blocks* which
> delimit how a particular region of text is supposed to be formatted according to
> the possible export backends.  In some sense, special blocks are meta-blocks.
> Rather than writing text in, say, LaTeX environments using LaTeX commands or in
> HTML `div`'s using HTML tags, we promote using Org-mode markup in special blocks
> &#x2014;Org markup cannot be used explicitly within HTML or LaTeX environments.
>
> *Special blocks*, like `centre` and `quote`, allow us to use Org-mode as the primary
> interface regardless of whether the final result is an HTML or PDF article;
> sometime we need to make our own special blocks to avoid a duplication of
> effort.  However, this can be difficult and may require familiarity with
> relatively advanced ELisp concepts, such as macros and hooks; as such, users may
> not be willing to put in the time and instead use ad-hoc solutions.
>
> We present a new macro, [defblock](org-special-block-extras-defblock), which is similar in-spirit to Lisp's standard
> <defun> except that where the latter defines functions, ours defines new
> special blocks for Emacs' Org-mode &#x2014;as well as, simultaneously, defining new
> Org link types. Besides the macro, the primary contribution of this effort is an
> interface for special blocks that *admits* arguments and is familar to Org users
> &#x2014;namely, we ‘try to reuse’ the familiar `src`-block interface, including
> header-args, but for special blocks.
>
> It is hoped that the ease of creating custom special blocks will be a gateway
> for many Emacs users to start using Lisp.
>
> **
>
> <span style="color:green;">
>
> A 5-page PDF covering ELisp fundamentals
>
> </span>
>
> ** can be found **[here](https://alhassy.github.io/ElispCheatSheet/CheatSheet.pdf)**.
>
> This article is featured in EmacsConf2020, with slides [here](https://alhassy.github.io/org-special-block-extras/emacs-conf-2020):
> No pictures, instead we use this system to make the  slides
> have a variety of styling information; i.e., we write Org
> and the result looks nice. “Look ma, no HTML required!”

![img](images/minimal-working-example-multiforms.png "Write in Emacs using Org-mode, export beautifully to HTML or LaTeX")

<details class="code-details"
                 style ="padding: 1em;
                          background-color: #e5f5e5;
                          /* background-color: pink; */
                          border-radius: 15px;
                          color: hsl(157 75%);
                          font-size: 0.9em;
                          box-shadow: 0.05em 0.1em 5px 0.01em  #00000057;">
                  <summary>
                    <strong>
                      <font face="Courier" size="3" color="green">
                         Super Simple Intro to Emacs’ Org-mode
                      </font>
                    </strong>
                  </summary>


Emacs’ Org-mode is an outliner, a rich markup language, spreadsheet tool,
literate programming system, and so much more. It is an impressive reason to
use Emacs (•̀ᴗ•́)و

Org-mode syntax is very *natural*; e.g., the following is Org-mode!

    + Numbered and bulleted lists are as expected.
      - Do the things:
        1.  This first
        2.  This second
        44. [@44] This forty-fourth
      - =[@𝓃]= at the beginning of an iterm forces
        list numbering to start at 𝓃
      - =[ ]= or =[X]= at the beginning for checkbox lists
      - Use =Alt ↑, ↓= to move items up and down lists;
        renumbering happens automatically.

    + Definitions lists:
       ~- term :: def~
    + Use a comment, such as =# separator=, between two lists
      to communicate that these are two lists that /happen/ to be
      one after the other. Or use any /non-indented/ text to split
      a list into two.

    * My top heading, section
      words
    ** Child heading, subsection
      more words
    *** Grandchild heading, subsubsection
        even more!

**Export** In Emacs, press <kbd> C-c C-e h o </kbd> to obtain an HTML webpage ---*like this
one!*&#x2014; of the Org-mode markup; use <kbd> C-c C-e l o </kbd> to obtain a PDF rendition.

You can try Org-mode notation and see how it renders live at:
<http://mooz.github.io/org-js/>

---

You make a heading by writing `* heading` at the start of a line, then you can
<kbd> TAB </kbd> to fold/unfold its contents. A table of contents, figures, tables can be
requested as follows:

    # figures not implemented in the HTML backend
    # The 𝓃 is optional and denotes headline depth
    #+toc: headlines 𝓃
    #+toc: figures
    #+toc: tables

---

-   **Markup** elements can be nested.

    <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


    <colgroup>
    <col  class="org-left" />

    <col  class="org-left" />
    </colgroup>
    <thead>
    <tr>
    <th scope="col" class="org-left">Syntax</th>
    <th scope="col" class="org-left">Result</th>
    </tr>
    </thead>

    <tbody>
    <tr>
    <td class="org-left"><code>/Emphasise/</code>, italics</td>
    <td class="org-left"><i>Emphasise</i></td>
    </tr>


    <tr>
    <td class="org-left"><code>*Strong*</code>, bold</td>
    <td class="org-left"><b>Strong</b></td>
    </tr>


    <tr>
    <td class="org-left"><code>*/very strongly/*</code>, bold italics</td>
    <td class="org-left"><b><i>very strongly</i></b></td>
    </tr>


    <tr>
    <td class="org-left"><code>=verbatim=</code>, monospaced typewriter</td>
    <td class="org-left"><code>verbatim</code></td>
    </tr>


    <tr>
    <td class="org-left"><code>+deleted+</code></td>
    <td class="org-left"><del>deleted</del></td>
    </tr>


    <tr>
    <td class="org-left"><code>_inserted_</code></td>
    <td class="org-left"><span class="underline">inserted</span></td>
    </tr>


    <tr>
    <td class="org-left"><code>super^{script}ed</code></td>
    <td class="org-left">super<sup>script</sup>ed</td>
    </tr>


    <tr>
    <td class="org-left"><code>sub_{scripted}ed</code></td>
    <td class="org-left">sub<sub>scripted</sub>ed</td>
    </tr>
    </tbody>
    </table>

    -   Markup can span across multiple lines, by default no more than 2.
    -   In general, markup cannot be ‘in the middle’ of a word.
-   New lines demarcate paragraphs
-   Use `\\` to force line breaks without starting a new paragraph
-   Use *at least* 5 dashes, `-----`, to form a horizontal rule

<a href="https://github.com/alhassy/org-special-block-extras"><img src="https://img.shields.io/badge/org--special--block--extras-2.0-informational?logo=Gnu-Emacs"></a>
provides support for numerous other kinds of markup elements, such as `red:hello`
which becomes “

<span style="color:red;">

hello

</span>

 ”.

---

**Working with tables**

    #+ATTR_HTML: :width 100%
    #+name: my-tbl
    #+caption: Example table
    | Who? | What? |
    |------+-------|
    | me   | Emacs |
    | you  | Org   |

Note the horizontal rule makes a header row and is formed by typing <kbd> | - </kbd> then
pressing <kbd> TAB </kbd>. You can <kbd> TAB </kbd> between cells.

---

**Working with links**

Link syntax is `[[source url][description]]`; e.g., we can refer to the above
table with `[[my-tbl][woah]]`.
Likewise for images: `file:path-to-image.`

---

**Mathematics**


<div style="column-rule-style: none;column-count: 2;">


<div style="padding: 1em; background-color: cyan;border-radius: 15px; font-size: 0.9em; box-shadow: 0.05em 0.1em 5px 0.01em #00000057;"><h3>Source</h3>


\begin{verbatim}
\[ \sin^2 x + \cos^2 x = \int_\pi^{\pi + 1} 1 dx = {3 \over 3} \]

\end{verbatim}

</div>


<div style="padding: 1em; background-color: cyan;border-radius: 15px; font-size: 0.9em; box-shadow: 0.05em 0.1em 5px 0.01em #00000057;"><h3>Result</h3>

\[ \sin^2 x + \cos^2 x = \int_\pi^{\pi + 1} 1 dx = {3 \over 3} \]

</div>

</div>

-   Instead of `\[...\]`, which displays a formula on its own line, centred, use
    `$...$` to show a formula inline.
-   Captioned equations are numbered and can be referenced via links,
    as shown below.


<div style="column-rule-style: none;column-count: 2;">


<div style="padding: 1em; background-color: #CCFFCC;border-radius: 15px; font-size: 0.9em; box-shadow: 0.05em 0.1em 5px 0.01em #00000057;"><h3>Source</h3>


\begin{verbatim}
#+name: euler
\begin{equation}
e ^ {i \pi} + 1 = 0
\end{equation}

See equation [[euler]].

\end{verbatim}

</div>


<div style="padding: 1em; background-color: #CCFFCC;border-radius: 15px; font-size: 0.9em; box-shadow: 0.05em 0.1em 5px 0.01em #00000057;"><h3>Result</h3>

\begin{equation}
\label{orge5bdafe}
e ^ {i \pi} + 1 = 0
\end{equation}

See equation [1](#orge5bdafe).

</div>

</div>

---

**Source code**


<div style="column-rule-style: none;column-count: 2;">


<div style="padding: 1em; background-color: #FFFFCC;border-radius: 15px; font-size: 0.9em; box-shadow: 0.05em 0.1em 5px 0.01em #00000057;"><h3>Source</h3>


\begin{verbatim}
#+begin_src C -n
int tot = 1;                    (ref:start)
for (int i = 0; i != 10; i++)   (ref:loop)
   tot *= i;                    (ref:next)
printf("The factorial of 10 is %d", tot);
#+end_src

\end{verbatim}

</div>


<div style="padding: 1em; background-color: #FFFFCC;border-radius: 15px; font-size: 0.9em; box-shadow: 0.05em 0.1em 5px 0.01em #00000057;"><h3>Result</h3>

    1  int tot = 1;                                   (start)
    2  for (int i = 0; i != 10; i++)                  (loop)
    3     tot *= i;                                   (next)
    4  printf("The factorial of 10 is %d", tot);

</div>

</div>

The labels `(ref:name)` refer to the lines in the source code and can be
referenced with link syntax: `[[(name)]]`. Hovering over the link, in the HTML
export, will dynamically highlight the corresponding line of code.  To strip-out
the labels from the displayed block, use `-r -n` in the header so it becomes
`#+begin_src C -r -n`, now the references become line numbers.

---

Another reason to use Org:
If you use `:results raw`, you obtain **dynamic templates** that may use Org-markup:


<div style="column-rule-style: none;column-count: 2;">


<div style="padding: 1em; background-color: #CCCC99;border-radius: 15px; font-size: 0.9em; box-shadow: 0.05em 0.1em 5px 0.01em #00000057;"><h3>Source</h3>


\begin{verbatim}
#+BEGIN_SRC C :results raw replace
printf("*bold* +%d+ (strikethrough) /slanted/", 12345);
#+END_SRC

♯+RESULTS:
*bold* +12345+ (strikethrough) /slanted/

\end{verbatim}

</div>


<div style="padding: 1em; background-color: #CCCC99;border-radius: 15px; font-size: 0.9em; box-shadow: 0.05em 0.1em 5px 0.01em #00000057;"><h3>Result</h3>

    printf("*bold* +%d+ (strikethrough) /slanted/", 12345);

♯+RESULTS:
**bold** <del>12345</del> (strikethrough) *slanted*

</div>

</div>

The `#+RESULTS:` is obtained by pressing <kbd> C-c C-c </kbd> on the `src` block, to execute
it and obtain its result.

Also: Notice that a C program can be run without a `main` ;-)

That is, we can write code *in between* prose that is intended to be read like an
essay:

![img](https://alhassy.github.io/emacs.d/images/literate-programming.png)

---

-   <a href="https://alhassy.github.io/emacs.d/CheatSheet.pdf"><img src="https://img.shields.io/badge/Lifemacs-CheatSheet-informational?logo=Gnu-Emacs"></a>
    ⇒ A brief reference of Emacs keybindings; 2 pages
-   <a href="https://alhassy.github.io/ElispCheatSheet/CheatSheet.pdf"><img src="https://img.shields.io/badge/Elisp-CheatSheet-success?logo=Gnu-Emacs"></a> ⇒ A *compact* Emacs Lisp Reference; 7 pages

---

**Single source of truth:** This mini-tutorial can be included into other Org files
by declaring

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left"><code>#+include: ~/.emacs.d/init.org::#Mini-tutorial-on-Org-mode</code></td>
</tr>
</tbody>
</table>

---

For more, see <https://orgmode.org/features.html>.


</details>

<!--

> The full article may be read as a [PDF](https://alhassy.github.io/org-special-block-extras/index.pdf) or as [HTML](https://alhassy.github.io/org-special-block-extras) &#x2014;or visit the [repo](https://github.com/alhassy/org-special-block-extras).
> Installation instructions are .

-->


# Table of Contents

1.  [Installation Instructions](#Installation-Instructions)
2.  [Minimal working example](#Minimal-working-example)
3.  [Bye!](#Bye)

> The full article may be read as a [PDF](https://alhassy.github.io/org-special-block-extras/index.pdf) or as [HTML](https://alhassy.github.io/org-special-block-extras) &#x2014;or visit the [repo](https://github.com/alhassy/org-special-block-extras).
> Installation instructions are .

</div>
