;;; org-defblock-docstring-tests.el --- `org-defblock' docstrings

(unless (fboundp 'using)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

;;; org-defblock docstrings

(deftest "`org--replace-examples-in-string' works as expected, ignoring casing -- executable documentation!"
  (should (equal
           (org--replace-examples-in-string
            (lf-string "The method `apply' can be used in multiple ways.

                        For example to sum a list:
                        #+begin_example
                        (apply #'+ nil)
                        #+end_example

                        Or, to sum a list with some starting numbers:
                        #+BEGIN_EXAMPLE
                        (apply #'+ 1 2 '(3 4))
                        #+END_EXAMPLE

                        Enjoy!")
            (lambda (example _backend) (format "\n %s ⇒ %s" example (eval (car (read-from-string example))))))
           (lf-string "The method `apply' can be used in multiple ways.

                       For example to sum a list:

                        (apply #'+ nil) ⇒ 0

                       Or, to sum a list with some starting numbers:

                        (apply #'+ 1 2 '(3 4)) ⇒ 10

                       Enjoy!"))))

(deftest "`org--replace-examples-in-string' can read an “:exporting-to” key"
  (using (org-defblock shout (&context contents) (upcase contents))
    (should (equal
             (org--replace-examples-in-string
              (lf-string "The `shout' block can be used as follows:

                        For LaTeX,
                        #+begin_example :exporting-to latex
                        #+begin_shout
                        Hello, world!
                        #+end_shout
                        #+end_example

                        Or, for HTML:
                        #+BEGIN_EXAMPLE :exporting-to html
                        #+begin_shout
                        Hello, world!
                        #+end_shout
                        #+END_EXAMPLE

                        Note: No other backends are supported.")
              (lambda (example backend) (format "%s\n ⭆%s" example (org-export-string example backend))))
             (lf-string "The `shout' block can be used as follows:

                       For LaTeX,
                       #+begin_shout
                       Hello, world!
                       #+end_shout
                        ⭆
                       HELLO, WORLD!


                       Or, for HTML:
                       #+begin_shout
                       Hello, world!
                       #+end_shout
                        ⭆
                       <p>
                       HELLO, WORLD!
                       </p>


                       Note: No other backends are supported.")))))

(deftest "`org-defblock' docstring is saved to the symbol variable, unchanged"
  (using (org-defblock emoji-greet (pleasantry "" to nil &context raw-contents)
           "Greet someone, with a flair of emojis.

#+begin_example :exporting-to latex
Sometimes we want to greet someone with many emojis, that's a job for the “emoji-greet” block.
#+begin_emoji-greet \"Why, hello there\" :to \"The love of my life\"
Have you noticed how blessed we are?
#+end_emoji-greet
#+end_example
"
           (format "🗣️ %s\n👀 %s\n🗯️ %s 👋" pleasantry to raw-contents))

    (should (equal (documentation-property 'org-block/emoji-greet 'variable-documentation)

                   "Greet someone, with a flair of emojis.

#+begin_example :exporting-to latex
Sometimes we want to greet someone with many emojis, that’s a job for the “emoji-greet” block.
#+begin_emoji-greet \"Why, hello there\" :to \"The love of my life\"
Have you noticed how blessed we are?
#+end_emoji-greet
#+end_example
"))))


(deftest "`org-defblock' docstrings actually Org-export any “#+example” blocks in the generated `defun'"
  (using (org-defblock emoji-greet (pleasantry "" to nil &context raw-contents)
           "Greet someone, with a flair of emojis.

#+begin_example :exporting-to latex
Sometimes we want to greet someone with many emojis, that's a job for the “emoji-greet” block.
#+begin_emoji-greet \"Why, hello there\" :to \"The love of my life\"
Have you noticed how blessed we are?
#+end_emoji-greet
#+end_example
"
           (format "🗣️ %s\n👀 %s\n🗯️ %s 👋" pleasantry to raw-contents))

    (should (equal (documentation #'org-block/emoji-greet)

                   "Greet someone, with a flair of emojis.

For example,

    Sometimes we want to greet someone with many emojis, that’s a job for the “emoji-greet” block.
    #+begin_emoji-greet \"Why, hello there\" :to \"The love of my life\"
    Have you noticed how blessed we are?
    #+end_emoji-greet

LaTeX-exports to

    Sometimes we want to greet someone with many emojis, that’s a job for the “emoji-greet” block.
    🗣️ Why, hello there
    👀 The love of my life
    🗯️ Have you noticed how blessed we are? 👋


Regarding the Lisp function:
+ BACKEND refers to the current export backend.
+ RAW-CONTENTS refers to the text as the user wrote it, sans its least indentation.
  ⇒ Its body asks, by &context, for CONTENTS: the ‘org parsed’ version of user text.
  ⇒ CONTENTS and RAW-CONTENTS are identical for the description of a link; whence the LINK? of &context is non-nil.

For example, upon LaTeX export, the Org special block

     #+begin_emoji-greet
     Hello, world
     #+end_emoji-greet

 is rewritten to the result of the call

     (org-block/emoji-greet ‘latex \"Hello, world\")"))))

(deftest "a block's doc: link shows its docstring, in Emacs and as an HTML tooltip; issue #54" [docs]
  (using (org-defblock admonition (type nil title nil &context contents)
           "Enclose text in a box denoting an admonition of TYPE, titled TITLE."
           (format "<section class=\"admonition-%s\"><h3>%s</h3>%s</section>" type (or title type) contents))
    (should (string-prefix-p "Enclose text in a box denoting an admonition"
                             (cadr (org-docs-get "org-block/admonition"))))
    (should (string-match-p "<template><pre>Enclose text in a box denoting an admonition"
                            (org-export-string "doc:org-block/admonition" 'html)))))

(deftest "a block docstring's closing (fn …) line stays last, so C-h f shows it as the signature; issue #54" [docs]
  (using (org-defblock admonition (type nil title nil &context contents)
           "Enclose text in a box denoting an admonition.

\(fn TYPE [:title TITLE])"
           contents)
    (should (equal (help-split-fundoc (documentation 'org-block/admonition t) 'org-block/admonition)
                   (cons "(org-block/admonition TYPE [:title TITLE])"
                         "Enclose text in a box denoting an admonition.

Regarding the Lisp function:
+ BACKEND refers to the current export backend.
+ RAW-CONTENTS refers to the text as the user wrote it, sans its least indentation.
  ⇒ Its body asks, by &context, for CONTENTS: the ‘org parsed’ version of user text.
  ⇒ CONTENTS and RAW-CONTENTS are identical for the description of a link; whence the LINK? of &context is non-nil.

For example, upon LaTeX export, the Org special block

     #+begin_admonition
     Hello, world
     #+end_admonition

 is rewritten to the result of the call

     (org-block/admonition `latex \"Hello, world\")")))))
