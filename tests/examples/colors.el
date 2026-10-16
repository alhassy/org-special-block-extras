;;; colors.el --- Snapshot of the `colors' showcase example  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

(define-org-example-test "colors"
  :input "
#+begin_red
Look at my text, it's red!
#+end_red

#+begin_pink
Look at my text, it's pink!
#+end_pink

#+begin_color cyan
Look at my text, it's cyan!
#+end_color

green:Look
blue:Here
orange:Buddo!

[[color:#ff0066][Any hex colour]], [[color:navy][any colour's name]], and [[red:][a described red link]]."
  :html "<span style=\"color:red;\">
<p>
Look at my text, it's red!
</p>
</span>

<span style=\"color:pink;\">
<p>
Look at my text, it's pink!
</p>
</span>

<span style=\"color:cyan;\">
<p>
Look at my text, it's cyan!
</p>
</span>

<p>
<span style=\"color:green;\">Look</span>
<span style=\"color:blue;\">Here</span>
<span style=\"color:orange;\">Buddo</span>!
</p>

<p>
<span style=\"color:#ff0066;\">Any hex colour</span>, <span style=\"color:navy;\">any colour's
name</span>, and <span style=\"color:red;\">a described red link</span>.
</p>"
  :latex "\\begingroup\\color{red}
Look at my text, it's red!
\\endgroup\\,

\\begingroup\\color{pink}
Look at my text, it's pink!
\\endgroup\\,

\\begingroup\\color{cyan}
Look at my text, it's cyan!
\\endgroup\\,

\\begingroup\\color{green}Look\\endgroup\\,
\\begingroup\\color{blue}Here\\endgroup\\,
\\begingroup\\color{orange}Buddo\\endgroup\\,!

\\definecolor{osbe-color}{HTML}{ff0066}\\begingroup\\color{osbe-color}Any hex colour\\endgroup\\,,
\\definecolor{osbe-color}{rgb}{0.000,0.000,0.502}\\begingroup\\color{osbe-color}any colour's
name\\endgroup\\,, and \\begingroup\\color{red}a described red link\\endgroup\\,.")
