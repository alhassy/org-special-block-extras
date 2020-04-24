<h1> Twenty-nine new custom block and 32 link types for Emacs' Org-mode (•̀ᴗ•́)و </h1>

<div class="org-center">
***Warning: Incomplete!***
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
> That is, **we provide 29 block types and 32 link types**.

<div class="org-center">
> The full article may be read as a [PDF](https://alhassy.github.io/org-special-block-extras/README.pdf) or as [HTML](https://alhassy.github.io/org-special-block-extras/README.html) or as pure [Org](https:/raw.githubusercontent.com/alhassy/org-special-block-extras/master/org-special-block-extras.org)!
> &#x2014;or visit the [repo](https://github.com/alhassy/org-special-block-extras).
</div>

![img](images/foo_block.png "Extensibility! *Plug and play support for new block types!*")

*First, a gallery of what's possible!*

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">**Write Org-markup once, generate for many backends ^\_^**</td>
</tr>
</tbody>
</table>

![img](images/colours.jpg)

![img](images/colour_links.png)

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">**Displaying thoughts side-by-side ^\_^**</td>
</tr>
</tbody>
</table>

![img](images/parallel.png)

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">**“First-class editor comments” In order: Chrome, Emacs Web Wowser, Org source, PDF**</td>
</tr>
</tbody>
</table>

![img](images/edcomm.png)

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">**Visually hiding, folding away, details**</td>
</tr>
</tbody>
</table>

![img](images/details.png)

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">**An Emacs interface to <https://shields.io/>**</td>
</tr>
</tbody>
</table>

![img](images/badges.png)

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">**Tooltips for documentation and glossary items &#x2013;in the browser!**</td>
</tr>
</tbody>
</table>

![img](images/tooltips_browser.png)

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">**Tooltips for documentation and glossary items &#x2013;in Emacs!**</td>
</tr>
</tbody>
</table>

![img](images/tooltips_emacs.png)

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">**Tooltips for documentation and glossary items &#x2013;in the PDF!**</td>
</tr>
</tbody>
</table>

![img](images/tooltips_pdf.png)

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">**Declaring documentation-glossary items**</td>
</tr>
</tbody>
</table>

![img](images/tooltips_declaration.png)

> The full article may be read as a [PDF](https://alhassy.github.io/org-special-block-extras/README.pdf) or as [HTML](https://alhassy.github.io/org-special-block-extras/README.html) or as pure [Org](https:/raw.githubusercontent.com/alhassy/org-special-block-extras/master/org-special-block-extras.org)!
> &#x2014;or visit the [repo](https://github.com/alhassy/org-special-block-extras).


# Table of Contents
