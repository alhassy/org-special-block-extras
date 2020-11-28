<h1> 29 new custom blocks and 32 link types for Emacs' Org-mode (•̀ᴗ•́)و </h1>

<div class="org-center">


[![badge:Emacs](https://img.shields.io/badge/Emacs-23%2F26%2F28-green?logo=gnu-emacs)](https://www.gnu.org/software/emacs)
[![badge:Org](https://img.shields.io/badge/Org-9.3.6-blue?logo=gnu)](https://orgmode.org)

<span>

[![badge:org--special--block--extras](https://img.shields.io/badge/org--special--block--extras-1.2-informational?logo=Gnu-Emacs)](https://github.com/alhassy/org-special-block-extras)

<a href="https://melpa.org/#/org-special-block-extras"><img alt="MELPA" src="https://melpa.org/packages/org-special-block-extras-badge.svg"/></a>

</span>

[![badge:license](https://img.shields.io/badge/license-GNU_3-informational?logo=read-the-docs)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![badge:docs](https://img.shields.io/badge/docs-literate-success?logo=read-the-docs)](https://github.com/alhassy/emacs.d#what-does-literate-programming-look-like)
[![badge:https://github.com/alhassy/org-special-block-extras](https://img.shields.io/twitter/url?url=https://github.com/alhassy/org-special-block-extras)](https://twitter.com/intent/tweet?text=This%20looks%20super%20neat%20%28%E2%80%A2%CC%80%E1%B4%97%E2%80%A2%CC%81%29%D9%88%3A:&url=https://github.com/alhassy/org-special-block-extras)
[![badge:contributions](https://img.shields.io/badge/contributions-welcome-green)](https://github.com/alhassy/org-special-block-extras/issues)

[![badge:author](https://img.shields.io/badge/author-musa_al--hassy-purple?logo=nintendo-3ds)](https://alhassy.github.io/)
[![badge:](https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee)](https://www.buymeacoffee.com/alhassy)
</div>

<div class="org-center">
**Abstract**
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
> Consequently, we extend the number of block types available to the Emacs
> Org-mode user **without forcing the user** to learn HTML or LaTeX.
> Indeed, I am not a web developer and had to learn a number of HTML concepts
> in the process &#x2014;the average Org user should not have to do so.
>
> Similarly, we provide a number of ‘link types’ `[[linktype:label][description]]`
> for producing in-line coloured text and SVG “badges”.
>
> We begin with the first two sections serving as mini-tutorials on special blocks
> and on link types. The special block setup we use is *extensible* in that a new
> block named `𝒞` will automatically be supported if the user defines a function
> `org-special-block-extras--𝒞` that formats the text of a block.  \*The remaining
> sections are literate implementation matter, along with examples and
> screenshots.\*
>
> In summary, we provide 20 colour block types and 20 colour link types,
> an ‘editor comment’ block type as well as a link type,
> a ‘details’ block type, a ‘parallel’ multiple columns view block type,
> a ‘link here’ link type, 8 badge link types,
> and block and link types for making documentation-glossary entries.
> That is, **we provide 29 block types and 34 link types**.

> The full article may be read as a [PDF](https://alhassy.github.io/org-special-block-extras/index.pdf) or as [HTML](https://alhassy.github.io/org-special-block-extras) &#x2014;or visit the [repo](https://github.com/alhassy/org-special-block-extras).
> Installation instructions are [below](#Summary).



## Installation Instructions

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
      :hook (org-mode . org-special-block-extras-mode))

Then, provide support for a new type of special block named `foo` that, say
replaces all words *foo* in a block, by declaring the following.

    (defun org-special-block-extras--foo (backend contents)
      "The FOO block type replaces all occurances of ‘foo’ with ‘bar’,
    unless a ‘:replacement:’ is provided."
      (-let [(contents′ . (&alist 'replacement))
               (org-special-block-extras--extract-arguments contents 'replacement)]
        (s-replace "foo" (or replacement "bar") contents′)))


## Minimal working example

The following example showcases the prominent features of this library.

    #+begin_parallel
    [[color:orange][Are you excited to learn some Lisp?]] blue:yes!

    Pop-quiz: How does doc:apply work?
    #+end_parallel

    #+begin_details
    link-here:solution
    Syntactically, ~(apply f '(x0 ... xN)) = (f x0 ... xN)~.

    [[remark:Musa][Ain't that cool?]]

    [[color:purple][We can apply a function to a list of arguments!]]
    #+end_details

    octoicon:report Note that kbd:C-x_C-e evaluates a Lisp form!

    #+LATEX_HEADER: \usepackage{multicol}
    #+LATEX_HEADER: \usepackage{tcolorbox}
    #+latex: In the LaTeX output, we have a glossary.

    show:GLOSSARY

Way better instructions and images may be found at the HTML site.

## Bye!
