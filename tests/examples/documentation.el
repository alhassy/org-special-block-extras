;;; documentation.el --- Snapshot of the glossary showcase: entries as blocks or headings, and show:GLOSSARY  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

(define-org-example-test "documentation"
  :input "Our glossary, listing every term this page cites ---even those cited further below:

                                     show:GLOSSARY

* Category Theory                                                   :documentation:
:PROPERTIES:
:DOC_KEYS: cat
:END:
A theory of typed composition; e.g., typed monoids.

* Using our terms

A glossary entry may instead be a block, which exports nothing:

#+begin_documentation Monoid :label mon
A set with an associative binary operation that has a unit.
#+end_documentation

Then, doc:cat studies [[doc:mon][monoids]] and their kin."
  :html "<p>
Our glossary, listing every term this page cites &mdash;even those cited further below:
</p>

<p>
<abbr class=\"tooltip\" title=\"A set with an associative binary operation that has a
unit.\">Monoid</abbr> <abbr class=\"tooltip\" title=\"A theory of typed composition; e.g., typed
monoids.\">Category Theory</abbr>
</p>
<div id=\"outline-container-org0000001\" class=\"outline-2\">
<h2 id=\"org0000001\">Category Theory&nbsp;&nbsp;&nbsp<span class=\"tag\"><span
class=\"documentation\">documentation</span></span></h2>
<div class=\"outline-text-2\" id=\"text-org0000001\">
<p>
A theory of typed composition; e.g., typed monoids.
</p>
</div>
</div>
<div id=\"outline-container-org0000004\" class=\"outline-2\">
<h2 id=\"org0000004\">Using our terms</h2>
<div class=\"outline-text-2\" id=\"text-org0000004\">
<p>
A glossary entry may instead be a block, which exports nothing:
</p>



<p>
Then, <abbr class=\"tooltip\" title=\"A theory of typed composition; e.g., typed monoids.\">Category
Theory</abbr> studies <abbr class=\"tooltip\" title=\"A set with an associative binary operation that
has a unit.\">monoids</abbr> and their kin.
</p>
</div>
</div>"
  :latex "Our glossary, listing every term this page cites ---even those cited further below:

\\vspace{1em}\\phantomsection\\textbf{Monoid}\\quad\\label{o-glossary-mon}A set with an associative
binary operation that has a unit. See page \\pageref{o-glossary-declaration-site-mon}
\\vspace{1em}\\phantomsection\\textbf{Category Theory}\\quad\\label{o-glossary-cat}A theory of typed
composition; e.g., typed monoids. See page \\pageref{o-glossary-declaration-site-cat}
\\section*{Category Theory\\hfill{}\\textsc{documentation}}
\\label{sec:org0000001}
A theory of typed composition; e.g., typed monoids.
\\section*{Using our terms}
\\label{sec:org0000004}

A glossary entry may instead be a block, which exports nothing:



Then, \\hyperref[o-glossary-cat]{Category Theory}\\label{o-glossary-declaration-site-cat} studies
\\hyperref[o-glossary-mon]{monoids}\\label{o-glossary-declaration-site-mon} and their kin.")
