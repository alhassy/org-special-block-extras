(setq needed-libraries
      '(s cl-lib dash org seq quelpa lf))

(require 'package)
(push '("melpa" . "https://melpa.org/packages/") package-archives)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg needed-libraries)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(load-file "org-special-block-extras.el")
(org-special-block-extras-short-names)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personal Testing Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'lf)  ;; to make use of “lf-string”
(require 'ert) ;; to make use of “should”


(cl-defun ⟰  (input &optional (backend 'html))
  "Export Org INPUT along BACKEND.

In particular, both org special blocks & links are exported into BACKEND.

Pictogram explanation: ⟰ is read ‘export’; it “exports upward to the moon whatever we have”."
  (org-special-block-extras-mode) ;; Ensure new blocks are registered
  (org-export-string-as
     input
     backend
     :body-only))


(defmacro deftest (desc tags &rest body)
  "Declare tests with meaningful string names, that reflect the test's main goal.

DESC is a string, TAGS is a vector.

The first tag should be the name of the main function being tested;
this name is prepended to the name of underlying ert-deftest.
This way, tests are grouped/namespaced when running ert from the command line.

Example ERT call: (ert '(tag my-cool-tag))"
  `(ert-deftest ,(intern
                  (concat
                   (format "%s::" (seq-elt tags 0))
                   (seq-map (lambda (c) (if (<= 65 c 122) c ?_))
                                   desc))) ()
     :tags (quote ,(seq--into-list tags))
     ,@body))
  ;; Convert all non-letters to ‘_’; A = 65, z = 122.
  ;; Without the replace, “M-x ert” crashes when it comes to selecting the test
  ;; to run.

(defmacro ≋ (lhs rhs)
    "A shorthand for (should (equal LHS RHS))."
    `(should (equal ,lhs ,rhs)))

(defmacro ↯ (form &optional message)
    "The given FORM should error (with MESSAGE, ignoring whitespace).

Example: (↯ (error \"hola\") \"hola\")

Pictogram explanation: You've made such a big mistake, that the
heavens strike you down with a lightening bolt ↯↯"
  (if message
      `(should (equal (s-collapse-whitespace (cl-second (should-error ,form)))
                      (s-collapse-whitespace ,message)))
    `(should-error ,form)))


;; I like the shapes: (× some-crashing-form) and (↯ crashes with-this-message)
(defalias '✓ 'should)
(defalias '× '↯)


(defmacro ⇝ (expr &rest regexp)
    "The given EXPR should match the given REGEXP, which is wrapped by ‘rx’.

REGEXP could also be a string, in which case we are doing string equality.
Either way, whitespace is ignored in both arguments.

The symbol “⇝” should be read “rewrites to” or “elaborates to”.

I prefer this form since it has the main form we're asserting
against-at the forefront and makes it clear we're matching
against strings.

For example,

  (should (s-matches? \"An English greeting.\" (documentation 'speak)))

Becomes:

  (⇝ (documentation 'speak) \"An English greeting.\")

Pictogram explanation: A given expression “rewrites, reduces,” to
a given matching pattern. Such arrows are popular in Term Rewriting Systems."
  `(should (s-matches? (s-collapse-whitespace (rx ,(cons 'seq regexp)))
                       (s-collapse-whitespace ,expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq calc (⟰ "#+begin_calc :hint-format \"\\\\left\\{ %s\\\\right.\"
                  +     x
                  +     y -- Explanation of why $x \\;=\\; y$
                    Actually, let me explain:
                    * x
                    * x′ -- hint 1
                    * y  -- hint 2

                    No words can appear (in the export) *after* a nested calculation, for now.
                  + [≤] z
                    --
                    Explanation of why $y \\;\\leq\\; z$

                    -- explain it more, this is ignored from export ;-)
                  #+end_calc"))

(deftest "It's an align environment, in displayed math mode"
  [calc]
  (⇝ calc "$$\\begin{align*}" (* anything) "\\end{align*}$$"))

(deftest "The calculation has 4 proof steps"
  [calc]
  ;; The number of steps in a calculation is the number of items in each nesting, minus 1 (at each nesting).
  ;; Above we have depth 2 with 3 items in each depth, for a total of (3-1) + (3-1) = 2 + 2 = 4.
  (should (= 4 (s-count-matches (rx (seq "\\" (* whitespace) (any "=" "≤"))) calc))))


(deftest "Of our 4 steps, 3 of them are equalities and one is an inclusion."
  [calc]
  (should (= 3 (s-count-matches (rx "= \\;\\;") calc)))
  (should (= 1 (s-count-matches (rx "≤ \\;\\;") calc))))

(deftest "All of the hints actually appear in the calculational proof"
  [calc]
  (mapc (lambda (hint) (should (s-contains? hint calc)))
        '("Explanation of why $x \\;=\\; y$"
          "Actually, let me explain:"
          "hint 1"
          "hint 2"
          "Explanation of why $y \\;\\leq\\; z$")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run all MWE tests
;; (ert "mwe")

(setq mwe (⟰
 "
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

/Allah [[margin:][The God of Abraham; known as Elohim in the Bible]] does not burden a soul
beyond what it can bear./ --- Quran 2:286

#+LATEX_HEADER: \\usepackage{multicol}
#+LATEX_HEADER: \\usepackage{tcolorbox}
#+latex: In the LaTeX output, we have a glossary.

show:GLOSSARY

badge:Thanks|for_reading
tweet:https://github.com/alhassy/org-special-block-extras
badge:|buy_me_a coffee|gray|https://www.buymeacoffee.com/alhassy|buy-me-a-coffee
"))

(deftest "It starts with a 2-column div for ‘parallel’"
  [mwe parallel]
  (⇝ mwe "<div style=\"column-rule-style: none;column-count: 2;\">"
         (* anything)
         "</div>"))

(deftest "Its initial question is in ‘orange’, with answer in ‘blue’"
  [mwe color orange blue]
  (⇝ mwe "<span style=\"color:orange;\">Are you excited to learn some Lisp?</span>"
         (* anything)
         "<span style=\"color:blue;\">Yes!</span>"))

(deftest "Its second question, about ‘apply’, has a tooltip"
  [mwe doc]
  (⇝ mwe "Pop-quiz: How does "
         "<abbr class=\"tooltip\" title=\"Call FUNCTION with our remaining args, "
         "using our last arg as list of args.<br>Then return the value FUNCTION returns."
         "<br>Thus, (apply '+ 1 2 '(3 4)) returns 10.<br><br>(fn FUNCTION &rest ARGUMENTS)\""
         ">apply</abbr> work?"))

(deftest "Its ‘details’ block is titled “Answer”, in green"
  [mwe details]
  (⇝ mwe
     "<details"
     (* anything) ;; styling
     "<summary>"
     (* anything) ;; styling
     "<font face=\"Courier\" size=\"3\" color=\"green\">"
     (* anything)
     "Answer"))

(deftest "Its details block begins with an SVG anchor identified as ‘solution’"
  [mwe link-here]
  (⇝ mwe "<details"
         (* anything) ;; styling
         "<a class=\"anchor\""
         (* anything)
         "href=\"#solution\">" ;; link-here:solution
         "<svg"
         (* anything)
         "</svg></a>"
         (* anything)
         "Syntactically, <code>(apply f '(x0 ... xN)) = (f x0 ... xN)</code>."))

(deftest "Its top-level remark is my name in a box, then the text, then a closing box delimiter"
  [mwe remark]
  (⇝ mwe "<details"
         (* anything)
         "<p style=\"color: black;\">"
         "<span style=\"border-width:1px;border-style:solid;padding:5px\">"
         "<strong>[Musa:</strong>"
         "</span>"
         " Ain't that cool?  "
         "<span style=\"border-width:1px;border-style:solid;padding:5px\"><strong>]</strong></span>"
         ))

(deftest "The aqua-coloured ‘spoiler’ appears within a magenta coloured piece of text"
  [mwe spoiler color magenta gensym]
  (⇝ mwe "<details"
         (* anything)
         ;; The local spoiler style is declared
         "<style>"
         (* anything) ;; A random id; e.g., #g289
         "{color: aqua; background-color:aqua;}"
         (* anything)
         ":hover {color: black; background-color:white;} "
         "</style>"
         (* anything)
         ;; Then it is used
         "That is, <span style=\"color:magenta;\">"
         "we can <span id="
         (* anything) ;; our random id is used here
         "> apply </span>" ;; Here is the spoiler!
         " a function to a list of arguments!</span>"
         (* anything)
         "</details>"))


(deftest "It has a title-less green box starting with an octoicon"
  [mwe box octoicon kbd]
  (⇝ mwe
     "<div style=\"padding: 1em;"
     (* anything)
     "<h3></h3>"
     (* anything)
     "<svg" ;; octoicon:report
     (* anything)
     "Note that <kbd> C-x C-e </kbd> evaluates a Lisp form!"
     (* anything)
     "</div>"))

(deftest "Its Quranic quote has the user requested tooltip indicated by a small raised circle"
  [mwe margin]
  (⇝ mwe
     "Allah <abbr class=\"tooltip\""
     " title=\"The God of Abraham; known as Elohim in the Bible\">°</abbr>&emsp13;" ;; ∘ !
     " does not burden a soul beyond what it can bear."
     (* anything)
     "Quran 2:286"))

(deftest "It concludes with three beautiful badges"
  [mwe badge]
  ;; badge:Thanks|for_reading
  ;; tweet:https://github.com/alhassy/org-special-block-extras
  ;; badge:|buy_me_a coffee|gray|https://www.buymeacoffee.com/alhassy|buy-me-a-coffee
  (⇝ mwe
     "<img src=\"https://img.shields.io/badge/Thanks-for_reading-nil?logo=nil\">"
     (* anything)
     "<img src=\"https://img.shields.io/twitter/url?url=https://github.com/alhassy/org-special-block-extras\">"
     (* anything)
     "<img src=\"https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee\">"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
