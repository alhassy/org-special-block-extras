;;; parallel.el --- Snapshot of the `parallel' showcase example  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

;; LatexRequirement:
;;   #+latex_header: \usepackage{multicol}
(define-org-example-test "parallel"
  :input "
Reduce the amount of whitespace noise in your articles by using the
[[doc:org-block/parallel][parallel]] block to place ideas
side-by-side ---with up to 𝓃-many columns.

----------------------------------------------------------------------

#+begin_parallel 2
*“Soft Columns”:* Writing ~#+begin_parallel 𝓃 :bar BAR~ will produce
𝒏-many parallel columns, possibly separated by solid rules, or a
“bar”. /This style allows text to freely move between columns,
depending on the size of the browser, which may dynamically shrink
and grow./ The value ~BAR~ can either be ~t~, ~nil~, or any
(backend)-valid colour specification; such as ~red~ or ~green~.
#+end_parallel

----------------------------------------------------------------------

#+begin_parallel 25% 50% 25% :bar green

*“Hard Columns”:* Alternatively, /for non-uniform column widths, 𝓃 may
instead be a specification of the widths of the columns./

#+columnbreak:  
However, this extra flexibility comes at an additional cost: The
contents of the block must now contain /𝓃-1/ lines consisting of
~#+columnbreak:~, when the specification determines 𝓃-many columns.

#+columnbreak:
Goodbye (“God-be-with-ye”) to the right!

#+end_parallel

# In the Soft Columns style above, any ‘#+columnbreak:’ are merely ignored.

In summary, articles can get lengthy when vertical whitespace is
wasted on thin lines; instead, one could save space by using
/[[doc:org-block/parallel][parallel]] columns of text/."
  :html "<p>
Reduce the amount of whitespace noise in your articles by using the
<abbr class=\"tooltip\"
tabindex=\"0\">parallel<template><pre>Place ideas side-by-side, possibly with a separator.

There are COLS many columns, and they may be separated by solid
vertical rules if BAR is a non-nil (colour) value.

+ COLS is either a number or a sequence of the shape: 10% 20% 30%.
+ BAR is either ‘t’, ‘nil’, or a colour such as ‘red’ or ‘blue’.

----------------------------------------------------------------------

“Soft Columns”: Writing “#+begin_parallel 𝓃 :bar t” will produce
𝒏-many parallel columns, possibly separated by solid rules, or a
“bar”. This style allows text to freely move between columns,
depending on the size of the browser, which may dynamically
shrink and grow.  BAR can either be ‘t’, ‘nil’, or
any (backend)-valid colour specification; such as ‘red’ or
‘green’.

“Hard Columns”: Alternatively, for non-uniform column widths,
COLS may instead be a specification of the widths of the
columns. However, this extra flexibility comes at an additional
cost: The contents of the block must now contain 𝒏-1 lines
consisting of ‘#+columnbreak:’, when the specification determines
𝒏 columns, as shown in the following example.

   #+begin_parallel 20% 60% 20% :bar green
   Hello, to the left!

   #+columnbreak:
   A super duper wide middle margin!

   #+columnbreak:
   Goodbye (“God-be-with-ye”) to the right!
   #+end_parallel

The specification is 𝒏 measurements denoting widths; which may be
in any HTML recognisable units; e.g., “5em 20px 30%” is valid.
I personally advise only the use of percentage measurements.

In the Soft Columns style above, any ‘#+columnbreak:’ are merely ignored.
With LaTeX export, the use of ‘#+columnbreak:’ is used to request a column break.

Regarding the Lisp function:
+ BACKEND refers to the current export backend.
+ RAW-CONTENTS refers to the text as the user wrote it verbatim.
  ⇒ You may mention CONTENTS to refer to the ‘org parsed’ version of user text.
  ⇒ CONTENTS and RAW-CONTENTS are identical whenever key CONTENTS-OCCUR-AS-LINK-DESCRIPTION is non-nil.

For example, upon LaTeX export, the Org special block

     #+begin_parallel
     Hello, world
     #+end_parallel

 is rewritten to the result of the call

     (org-block/parallel ‘latex \"Hello, world\")</pre></template></abbr> block to place ideas
side-by-side &mdash;with up to 𝓃-many columns.
</p>

<hr />

<div style=\"column-count: 2;\">
<p>
<b>“Soft Columns”:</b> Writing <code>#+begin_parallel 𝓃 :bar BAR</code> will produce
𝒏-many parallel columns, possibly separated by solid rules, or a
“bar”. <i>This style allows text to freely move between columns,
depending on the size of the browser, which may dynamically shrink
and grow.</i> The value <code>BAR</code> can either be <code>t</code>, <code>nil</code>, or any
(backend)-valid colour specification; such as <code>red</code> or <code>green</code>.
</p>
</div>

<hr />

<div style=\"display: flex; justify-content: space-between; align-items: flex-start;\"><div
style=\"width: 25%; margin: 10px; border-right: 4px solid green;\">
<p>
<b>“Hard Columns”:</b> Alternatively, <i>for non-uniform column widths, 𝓃 may
instead be a specification of the widths of the columns.</i>
</p>

<p>
</div><div style=\"width: 50%; margin: 10px; border-right: 4px solid green;\">
However, this extra flexibility comes at an additional cost: The
contents of the block must now contain <i>𝓃-1</i> lines consisting of
<code>#+columnbreak:</code>, when the specification determines 𝓃-many columns.
</p>

<p>
</div><div style=\"width: 25%; margin: 10px; border-right: none;\">
Goodbye (“God-be-with-ye”) to the right!
</p>
</div></div>

<p>
In summary, articles can get lengthy when vertical whitespace is
wasted on thin lines; instead, one could save space by using
<i><abbr class=\"tooltip\"
tabindex=\"0\">parallel<template><pre>Place ideas side-by-side, possibly with a separator.

There are COLS many columns, and they may be separated by solid
vertical rules if BAR is a non-nil (colour) value.

+ COLS is either a number or a sequence of the shape: 10% 20% 30%.
+ BAR is either ‘t’, ‘nil’, or a colour such as ‘red’ or ‘blue’.

----------------------------------------------------------------------

“Soft Columns”: Writing “#+begin_parallel 𝓃 :bar t” will produce
𝒏-many parallel columns, possibly separated by solid rules, or a
“bar”. This style allows text to freely move between columns,
depending on the size of the browser, which may dynamically
shrink and grow.  BAR can either be ‘t’, ‘nil’, or
any (backend)-valid colour specification; such as ‘red’ or
‘green’.

“Hard Columns”: Alternatively, for non-uniform column widths,
COLS may instead be a specification of the widths of the
columns. However, this extra flexibility comes at an additional
cost: The contents of the block must now contain 𝒏-1 lines
consisting of ‘#+columnbreak:’, when the specification determines
𝒏 columns, as shown in the following example.

   #+begin_parallel 20% 60% 20% :bar green
   Hello, to the left!

   #+columnbreak:
   A super duper wide middle margin!

   #+columnbreak:
   Goodbye (“God-be-with-ye”) to the right!
   #+end_parallel

The specification is 𝒏 measurements denoting widths; which may be
in any HTML recognisable units; e.g., “5em 20px 30%” is valid.
I personally advise only the use of percentage measurements.

In the Soft Columns style above, any ‘#+columnbreak:’ are merely ignored.
With LaTeX export, the use of ‘#+columnbreak:’ is used to request a column break.

Regarding the Lisp function:
+ BACKEND refers to the current export backend.
+ RAW-CONTENTS refers to the text as the user wrote it verbatim.
  ⇒ You may mention CONTENTS to refer to the ‘org parsed’ version of user text.
  ⇒ CONTENTS and RAW-CONTENTS are identical whenever key CONTENTS-OCCUR-AS-LINK-DESCRIPTION is non-nil.

For example, upon LaTeX export, the Org special block

     #+begin_parallel
     Hello, world
     #+end_parallel

 is rewritten to the result of the call

     (org-block/parallel ‘latex \"Hello, world\")</pre></template></abbr> columns of text</i>.
</p>"
  :latex "Reduce the amount of whitespace noise in your articles by using the
\\hyperref[o-glossary-org-block/parallel]{parallel}
\\label{o-glossary-declaration-site-org-block/parallel} block to place ideas
side-by-side ---with up to 𝓃-many columns.

\\noindent\\rule{\\textwidth}{0.5pt}

\\par \\setlength{\\columnseprule}{0 pt}
\\begin{minipage}[t]{\\linewidth}
\\begin{multicols}{2}
\\textbf{“Soft Columns”:} Writing \\texttt{\\#+begin\\_parallel 𝓃 :bar BAR} will produce
𝒏-many parallel columns, possibly separated by solid rules, or a
“bar”. \\emph{This style allows text to freely move between columns,
depending on the size of the browser, which may dynamically shrink
and grow.} The value \\texttt{BAR} can either be \\texttt{t}, \\texttt{nil}, or any
(backend)-valid colour specification; such as \\texttt{red} or \\texttt{green}.

\\end{multicols}\\end{minipage}

\\noindent\\rule{\\textwidth}{0.5pt}

\\par \\setlength{\\columnseprule}{2 pt}
\\begin{minipage}[t]{\\linewidth}
\\begin{multicols}{3}
\\textbf{“Hard Columns”:} Alternatively, \\emph{for non-uniform column widths, 𝓃 may
instead be a specification of the widths of the columns.}

\\columnbreak
However, this extra flexibility comes at an additional cost: The
contents of the block must now contain \\emph{𝓃-1} lines consisting of
\\texttt{\\#+columnbreak:}, when the specification determines 𝓃-many columns.

\\columnbreak
Goodbye (“God-be-with-ye”) to the right!

\\end{multicols}\\end{minipage}

In summary, articles can get lengthy when vertical whitespace is
wasted on thin lines; instead, one could save space by using
\\emph{\\hyperref[o-glossary-org-block/parallel]{parallel}
\\label{o-glossary-declaration-site-org-block/parallel} columns of text}.")
