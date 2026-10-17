;;; align.el --- Snapshot of the `align' showcase example  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

(define-org-example-test "align"
  :input "An ~align~ block lines equations up at each =&=; a line may end in a coloured tag, to which an
=eqref:= link refers. Shorthands, such as =->= below, come from the =:let= switch every block has.

#+begin_align :let (-> \"\\\\Rightarrow\" == \"\\\\equiv\")
p -> q &== \\neg p \\lor q          :tag-blue: Implication
p \\land q &== p == q == p \\lor q  :tag-orange: Golden Rule
#+end_align

By [[eqref:Golden Rule]], conjunction is defined by equivalence and disjunction."
  :html "<p>
An <code>align</code> block lines equations up at each <code>&amp;</code>; a line may end in a
coloured tag, to which an
<code>eqref:</code> link refers. Shorthands, such as <code>-&gt;</code> below, come from the
<code>:let</code> switch every block has.
</p>

\\begin{align*}
p \\Rightarrow q &amp;\\equiv \\neg p \\lor q          \\tag{$\\color{blue}{\\text{Implication}}$}
\\label{Implication} \\\\
p \\land q &amp;\\equiv p \\equiv q \\equiv p \\lor q  \\tag{$\\color{orange}{\\text{Golden Rule}}$}
\\label{Golden Rule}
\\end{align*}

<p>
By \\eqref{Golden Rule}, conjunction is defined by equivalence and disjunction.
</p>"
  :latex "An \\texttt{align} block lines equations up at each \\texttt{\\&}; a line may end in a coloured tag,
to which an
\\texttt{eqref:} link refers. Shorthands, such as \\texttt{->} below, come from the \\texttt{:let}
switch every block has.

\\begin{align*}
p \\Rightarrow q &\\equiv \\neg p \\lor q          \\tag{$\\color{blue}{\\text{Implication}}$}
\\label{Implication} \\\\
p \\land q &\\equiv p \\equiv q \\equiv p \\lor q  \\tag{$\\color{orange}{\\text{Golden Rule}}$}
\\label{Golden Rule}
\\end{align*}

By \\eqref{Golden Rule}, conjunction is defined by equivalence and disjunction.")
