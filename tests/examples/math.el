;;; math.el --- Snapshot of the `math' showcase example  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

(define-org-example-test "math"
  :input "Hello, in this blog post I'd like to talk about quantifiers.

# First, I'll define some helpers that I use multiple times; they are declared, not shown.
#+begin_math
\\def\\Plus{teal:\\bigoplus}
\\def\\plus{\\;teal:\\oplus\\;}
\\def\\f#1{brown:f violet:{(#1)}}
\\def\\xstart{red:a}
\\def\\xend{cyan:b}
#+end_math

Anyhow, let's talk about quantifiers...
#+begin_math
\\Plus_{violet:x = \\xstart}^{\\xend} \\f{x} \\; = \\; \\f{\\xstart} \\plus \\f{a+1} \\plus \\f{a+2} \\plus \\cdots \\plus \\f{\\xend}
#+end_math

The above “quantification” means [[teal:Loop sequentially with]]
[[brown:loop-bodies f(x)]] [[teal:fused using ⊕]], using [[violet:x as the
name of the current element]], [[red:starting at a]] and [[cyan:ending at b]]."
  :html "<p>
Hello, in this blog post I'd like to talk about quantifiers.
</p>

<p style=\"display:none\">\\[\\def\\Plus{{\\color{teal}\\bigoplus}}
\\def\\plus{\\;{\\color{teal}\\oplus}\\;}
\\def\\f#1{{\\color{brown}f} {\\color{violet}{(#1)}}}
\\def\\xstart{{\\color{red}a}}
\\def\\xend{{\\color{cyan}b}}\\]</p>

<p>
Anyhow, let's talk about quantifiers&hellip;
</p>
\\[
\\Plus_{{\\color{violet}x} = \\xstart}^{\\xend} \\f{x} \\; = \\; \\f{\\xstart} \\plus \\f{a+1} \\plus \\f{a+2}
\\plus \\cdots \\plus \\f{\\xend}
\\]

<p>
The above “quantification” means <span style=\"color:teal;\">Loop sequentially with</span>
<span style=\"color:brown;\">loop-bodies f(x)</span> <span style=\"color:teal;\">fused using ⊕</span>,
using <span style=\"color:violet;\">x as the name of the current element</span>, <span
style=\"color:red;\">starting at a</span> and <span style=\"color:cyan;\">ending at b</span>.
</p>"
  :latex "Hello, in this blog post I'd like to talk about quantifiers.



Anyhow, let's talk about quantifiers\\ldots{}

\\[
\\Plus_{{\\color{violet}x} = \\xstart}^{\\xend} \\f{x} \\; = \\; \\f{\\xstart} \\plus \\f{a+1} \\plus \\f{a+2}
\\plus \\cdots \\plus \\f{\\xend}
\\]

The above “quantification” means \\begingroup\\color{teal}Loop sequentially with\\endgroup\\,
\\begingroup\\color{brown}loop-bodies f(x)\\endgroup\\, \\begingroup\\color{teal}fused using ⊕\\endgroup\\,,
 using \\begingroup\\color{violet}x as the name of the current element\\endgroup\\,,
\\begingroup\\color{red}starting at a\\endgroup\\, and \\begingroup\\color{cyan}ending at b\\endgroup\\,.")
