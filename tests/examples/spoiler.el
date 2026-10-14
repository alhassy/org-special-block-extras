;;; spoiler.el --- Snapshot of the `spoiler' showcase example  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

(define-org-example-test "spoiler"
  :input "The ~spoiler~ block is for “fill in the blanks” or “guess, and hover
to see the results” style of exposition.

#+begin_spoiler
((Yusuf)) said to his father ((Yacoub)), /“O my father, indeed I have seen
((eleven stars)) and ((the sun and the moon)); I saw them prostrating to me.”/
#+end_spoiler

There are also options for the left delimiter of a hidden item, the right delimiter,
and the color of the hidden item. These default to ~grey, ((, ))~ respectively.

#+begin_spoiler orange :left \"[\" :right \"]\"
[Yusuf] said to his father [Yacoub], /“O my father, indeed I have seen
[eleven stars] and [the sun and the moon]; I saw them prostrating to me.”/
#+end_spoiler"
  :html "<p>
The <code>spoiler</code> block is for “fill in the blanks” or “guess, and hover
to see the results” style of exposition.
</p>

<style> .spoiler-grey {color: grey; background-color:grey;} .spoiler-grey:hover {color: black; background-color:white;} </style>
<p>
<span class=\"spoiler-grey\"> Yusuf </span> said to his father <span class=\"spoiler-grey\"> Yacoub </span>, <i>“O my father, indeed I have seen
<span class=\"spoiler-grey\"> eleven stars </span> and <span class=\"spoiler-grey\"> the sun and the moon </span>; I saw them prostrating to me.”</i>
</p>


<p>
There are also options for the left delimiter of a hidden item, the right delimiter,
and the color of the hidden item. These default to <code>grey, ((, ))</code> respectively.
</p>

<style> .spoiler-orange {color: orange; background-color:orange;} .spoiler-orange:hover {color: black; background-color:white;} </style>
<p>
<span class=\"spoiler-orange\"> Yusuf </span> said to his father <span class=\"spoiler-orange\"> Yacoub </span>, <i>“O my father, indeed I have seen
<span class=\"spoiler-orange\"> eleven stars </span> and <span class=\"spoiler-orange\"> the sun and the moon </span>; I saw them prostrating to me.”</i>
</p>"
  :latex "The \\texttt{spoiler} block is for “fill in the blanks” or “guess, and hover
to see the results” style of exposition.


\\fbox{\\phantom{Yusuf}}\\footnote{Yusuf} said to his father \\fbox{\\phantom{Yacoub}}\\footnote{Yacoub}, \\emph{“O my father, indeed I have seen
\\fbox{\\phantom{eleven stars}}\\footnote{eleven stars} and \\fbox{\\phantom{the sun and the moon}}\\footnote{the sun and the moon}; I saw them prostrating to me.”}


There are also options for the left delimiter of a hidden item, the right delimiter,
and the color of the hidden item. These default to \\texttt{grey, ((, ))} respectively.


\\fbox{\\phantom{Yusuf}}\\footnote{Yusuf} said to his father \\fbox{\\phantom{Yacoub}}\\footnote{Yacoub}, \\emph{“O my father, indeed I have seen
\\fbox{\\phantom{eleven stars}}\\footnote{eleven stars} and \\fbox{\\phantom{the sun and the moon}}\\footnote{the sun and the moon}; I saw them prostrating to me.”}")
