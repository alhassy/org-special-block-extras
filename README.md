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
<a href="https://github.com/alhassy/org-special-block-extras"><img src="https://img.shields.io/badge/org--special--block--extras-2.0-informational?logo=Gnu-Emacs"></a>
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
> We present a new macro, [defblock](org-special-block-extras--defblock), which is similar in-spirit to Lisp's standard
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


<a id="Installation-Instructions"></a>

# Installation Instructions

Manually or using [quelpa](https://github.com/alhassy/emacs.d#installing-emacs-packages-directly-from-source):

    ;; ⟨0⟩ Download the org-special-block-extras.el file manually or using quelpa
    (quelpa '(org-special-block-extras :fetcher github :repo
    "alhassy/org-special-block-extras"))

    ;; ⟨1⟩ Have this always active in Org buffers
    (add-hook #'org-mode-hook #'org-special-block-extras-mode)

    ;; ⟨1′⟩ Or use: “M-x org-special-block-extras-mode” to turn it on/off

**Or** with [use-package](https://github.com/alhassy/emacs.d#use-package-the-start-of-initel):

    (use-package org-special-block-extras
      :ensure t
      :hook (org-mode . org-special-block-extras-mode)
      :custom
        (org-special-block-extras--docs-libraries
         '("~/org-special-block-extras/documentation.org")
         "The places where I keep my ‘#+documentation’")
        ;; (org-special-block-extras-fancy-links
        ;; nil "Disable this feature.")
      :config
      ;; Use short names like ‘defblock’ instead of the fully qualified name
      ;; ‘org-special-block-extras--defblock’
        (org-special-block-extras-short-names))

Then, provide support for a new type of special block, say re-using the `src`
blocks that, say, folds up all such blocks in HTML export, by declaring the
following.

    (org-special-block-extras--defblock src (lang nil) (title nil exports nil file nil)
      "Fold-away all ‘src’ blocks as ‘<details>’ HTML export.
    If a block has a ‘:title’, use that to title the ‘<details>’."
      (format "<details> <summary> %s </summary> <pre> %s </pre></details>"
              (or title (concat "Details; " lang))
              raw-contents))


<a id="Minimal-working-example"></a>

# Minimal working example

The following example showcases the prominent features of this library.

    #+begin_parallel
    [[color:orange][Are you excited to learn some Lisp?]] [[blue:Yes!]]

    Pop-quiz: How does doc:apply work?
    #+end_parallel

    #+begin_details Answer
    link-here:solution
    Syntactically, ~(apply f '(x0 ... xN)) = (f x0 ... xN)~.

    [[remark:Musa][Ain't that cool?]]

    #+begin_spoiler aqua
    That is, [[color:magenta][we can ((apply)) a function to a list of arguments!]]
    #+end_spoiler

    #+end_details

    #+html: <br>
    #+begin_box
    octoicon:report Note that kbd:C-x_C-e evaluates a Lisp form!
    #+end_box

    #+LATEX_HEADER: \usepackage{multicol}
    #+LATEX_HEADER: \usepackage{tcolorbox}
    #+latex: In the LaTeX output, we have a glossary.

    show:GLOSSARY

    badge:Thanks|for_reading
    tweet:https://github.com/alhassy/org-special-block-extras
    badge:|buy_me_a coffee|gray|https://www.buymeacoffee.com/alhassy|buy-me-a-coffee

Here is what it looks like as HTML (left) and LaTeX (right):

![img](images/minimal-working-example.png)

The above section, , presents a few puzzles to get you
comfortable with `defblock` ;-)


<a id="Bye"></a>

# Bye!

<img src="https://img.shields.io/badge/thanks-for_reading-nil?logo=nil">
<a href="https://twitter.com/intent/tweet?text=This looks super neat (•̀ᴗ•́)و::&url=https://github.com/alhassy/org-special-block-extras"><img src="https://img.shields.io/twitter/url?url=https://github.com/alhassy/org-special-block-extras"></a>
<a href="https://www.buymeacoffee.com/alhassy"><img src="https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee"></a>
