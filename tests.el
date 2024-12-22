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

(defvar deftest-space "·"
  "The symbol used in-places of whitespace.

The default is interpunct style, or middle-dot, see 0 below.

Here are other symbols I've considered using:
0. Interpunct style is barely noticeable and non-intrusive.
   ⇒ OSBE·org·source·exports·to·HTML·without·any·problems
1. Hyphens is a common Lisp & English convention for forming compounds.
   ⇒ OSBE-org-source-exports-to-HTML-without-any-problems
2. Snakecase is also popular
   ⇒ OSBE_org_source_exports_to_HTML_without_any_problems
3. The underbracket is also neat
   ⇒ OSBE␣org␣source␣exports␣to␣HTML␣without␣any␣problems
4. There is no need to force convention onto ourselves; get funky:
   ⇒ OSBE◌org◌source◌exports◌to◌HTML◌without◌any◌problems")

(defmacro deftest (desc tags &rest body)
  "Declare tests with meaningful string names, that reflect the test's main goal.

DESC is a string, TAGS is a vector.

The first tag should be the name of the main function being tested;
this name is prepended to the name of underlying ert-deftest.
This way, tests are grouped/namespaced when running ert from the command line.

I use Org-blocks with ‘:comments link’, this then serves to delimit
my tests into “suites”.

Example ERT call: (ert '(tag my-cool-tag))
"
  `(ert-deftest ,(intern
                  (concat
                   ;; ⇨ ➝ ➡ ➩ ➾ ⚝
                   (format "%s··⇨··" (seq-elt tags 0))
                   (s-replace-all `((" " . ,deftest-space) ("'" . "’") ("," . "︐") ("`" . "‵") (";" . "︔") ("[" . "⁅") ("]" . "⁆"))
                                  (s-collapse-whitespace (s-trim desc)))))
     ()
     :tags (quote ,(seq--into-list tags))
     ,@body))
  ;;
  ;; DESC may contain spaces, commas, quotes, and other natural punctuation and ASCII.
  ;; For example, the description “Howdy, Musa's 3 `friends!`” is acceptable (and it
  ;; gets converted into the Lisp name “ Howdy︐·Musa’s·3·‵friends!‵ ”).
  ;; Without the s-replace-all, “M-x ert” crashes when it comes to selecting the test
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

;; Even though I intend to “C-c C-e h o” myself to update
;; the webpage for this package, I like having this test
;; since it's run in “emacs -Q” which ensures there's no
;; implicit state / packages being used when I run “C-c C-e h o”.
;;
(deftest "OSBE org source exports to HTML without any problems"
  [html-export]
  (find-file "org-special-block-extras.org")

  ;; The html-export-style:𝒳 link downloads styles, let's disable/mock it.
  (org-deflink html-export-style () "")

  ;; Some doc:𝒳 links refer to my personal docs, so let's get those.
  (org-docs-load-libraries (list "~/org-special-block-extras/documentation.org"))

  ;; FIXME: I should not have to set this up; this is an error with “show:𝒳”?
  (setq o-value "whoops")

  (should (org-html-export-to-html)))

;; [[file:org-special-block-extras.org::#NEW-org-deflink][Define links as you define functions: doc:org-deflink:4]]
(org-deflink shout
  "Capitalise the link description, if any, otherwise capitalise the label.

The link text appears as red bold in both Emacs and in HTML export."
  [:face '(:foreground "red" :weight bold)
   ;; :help-echo (org-link/shout o-label o-description 'html)
   :display 'full
   :keymap (C-m (message-box "hola"))
   :follow (message-box "%s and %s" pre current-prefix-arg)
   ]
  (format "<span style=\"color:red\"> %s </span>"
          (upcase (or o-description o-label))))

(deftest "org-deflink makes documented functions"
  [org-deflink]
  (⇝ (documentation #'org-link/shout)
     "Capitalise the link description, if any, otherwise capitalise the label.

     The link text appears as red bold in both Emacs and in HTML export."))

(deftest "org-deflink works as expected, plain links"
  [org-deflink]
  (should (not (null (symbol-function 'org-link/shout))))
  (⇝ (⟰ "shout:hello")
     "<p> <span style=\"color:red\"> HELLO </span></p>"))

(deftest "org-deflink works as expected, bracket links"
  [org-deflink]
  (⇝ (⟰ "[[shout:hello]]")
     "<p> <span style=\"color:red\"> HELLO </span></p>")
  (⇝ (⟰ "[[shout:hello][world!]]")
     "<p> <span style=\"color:red\"> WORLD! </span></p>"))

(deftest "org-deflink works as expected, angle links"
  [org-deflink]
  (⇝ (⟰ "<shout: hello world!>")
     "<p> <span style=\"color:red\"> HELLO WORLD! </span></p>"))
;; Define links as you define functions: doc:org-deflink:4 ends here

(org-defblock scream
  (speaker "Default_Speaker")
  [:face '(:foreground "green" :weight bold)]
  "Capitalise the contents! Seen in red bold in Emacs!"
  (format "%s: %s" speaker (upcase contents)))

(deftest "Upcase works as expected on links, with only labels"
         [basic-defblock org-link]
         (⇝ (⟰ "pre scream:hello post")
            "pre hello: HELLO post"))

(deftest "Upcase works as expected on links, with descriptions"
         [basic-defblock org-link]
         (⇝ (⟰ "pre [[scream:hello][my dear friends]] post")
            "hello: MY DEAR FRIENDS post"))

(deftest "Upcase works as expected on blocks"
         [basic-defblock]
         (⇝ (⟰ "pre
#+begin_scream hello
my amigos
#+end_scream
post")
"pre"
(* anything)
"hello: "
"\n<p>"
"\nMY AMIGOS"
"\n</p>"
(* anything)
"<p>\npost"))

(deftest "Upcase works as expected on blocks, with default main argument"
         [basic-defblock main-arg]
         (⇝ (⟰ "pre
#+begin_scream
my amigos
#+end_scream
post")
"pre"
(* anything)
"Default_Speaker: "
"\n<p>"
"\nMY AMIGOS"
"\n</p>"
(* anything)
"<p>\npost"))

;; [[file:org-special-block-extras.org::#Folded-Details][Folded Details ---As well as boxed text and subtle colours:3]]
(deftest "The result is a <details> tag containing the user's title & text."
  [details]
  (⇝ (⟰ "#+begin_details TITLE-RIGHT-HERE
          My aside...
          #+end_details")
     "<details"
     (* anything)
     "TITLE-RIGHT-HERE"
     (* anything)
     "My aside"
     (* anything)
     "</details>"))
;; Folded Details ---As well as boxed text and subtle colours:3 ends here

(deftest "We have an HTML box enclosing the user's title (in <h3) and text"
  [box]
  (⇝ (⟰ "#+begin_box Pay Attention!
                 This is the key insight...
                 #+end_box")
     "<div style=\"padding: 1em;background-color: #CCFFCC;border-radius: 15px;"
     (* anything)
     "<h3>Pay Attention!</h3>"
     (* anything)
     "This is the key insight"
     (* anything)))

;; [[file:org-special-block-extras.org::#Parallel][Parallel:2]]
(deftest "Parallel blocks work as expected - Defaults"
  [parallel block]
  (⇝ (⟰ "#+begin_parallel
         Hello, to the left!

         #+columnbreak:
         A super duper wide middle margin!


         #+columnbreak:
         Goodbye (“God-be-with-ye”) to the right!

         #+columnbreak:
         woah
         #+end_parallel")

     ;; The result is 2 columns with no rule between them and it contains the
     ;; user's text, but ignores the “#+columnbreak”.
     "<div style=\"column-rule-style: none nil;column-count: 2;\">"
     (* anything)
     "Hello, to the left!"
     (* anything)
     "A super duper wide middle margin!"
     (* anything)
     "Goodbye (“God-be-with-ye”) to the right!"
     (* anything)
     "woah"))


(deftest "Parallel blocks work as expected - Soft Columns"
  [parallel block]
  (⇝ (⟰ "#+begin_parallel 2 :bar t
                  X

                  #+columnbreak:

                  Y

                  Z
                  #+end_parallel")

     ;; The result is 2 columns with a solid BLACK rule between them and it
     ;; contains the user's text, but ignores the “#+columnbreak”.
     "<div style=\"column-rule-style: solid black;column-count: 2;\">"
     (* anything)
     "X"
     (* anything)
     "Y"
     (* anything)
     "Z"
     (* anything)))


(deftest "Parallel blocks work as expected - Hard Columns"
  [parallel block]
  (⇝ (⟰ "#+begin_parallel 20% 60% 20% :bar green
                  X

                  #+columnbreak:

                  Y

                  #+columnbreak:

                  Z
                  #+end_parallel")

     ;; The result is 3 columns with a solid GREEN rule between them and it
     ;; contains the user's text along with the “#+columnbreak” evaluated at the
     ;; expected positions.
     "<div><div style=\"width: 20%; margin: 10px; border-right:4px solid green; float:  left;\">"
     (* anything)
     "X"
     (* anything)
     "</div><div style=\"width: 60%; margin: 10px; border-right:4px solid green; float:  left;\">"
     (* anything)
     "Y"
     (* anything)
     "</div><div style=\"width: 20%; margin: 10px; border-right:4px none; float:  left;\">"
     (* anything)
     "Z"))
;; Parallel:2 ends here

;; [[file:org-special-block-extras.org::#editor-comments][Editor Comments:4]]
(deftest "The user's remark is enclosed in the default delimiters"
  [remark]
  (⇝ (⟰ "#+begin_remark
                   Here is some meta-commentary...
                  #+end_remark")
     (* anything) "[Editor Remark:"
     (* anything) "Here is some meta-commentary"
     (* anything)  "]"))

;; The other features of remark blocks should be tested;
;; but this is not a pressing, nor interesting, concern.
;; Editor Comments:4 ends here

;; [[file:org-special-block-extras.org::#Colours][Colours:4]]
(deftest "It is an HTML span styled red that contains the user's text"
  [color red block]
  (⇝ (⟰ "#+begin_red
          My cool thoughts...
          #+end_red")
     "<span style=\"color:red;\">"
     (* anything)
     "My cool thoughts"
     (* anything)
     "</span>"))

;; We have an HTML span styled with the user's color and it contains the user's text
(deftest "It works as expected"
  [color pink block]
  (⇝ (⟰ "#+begin_color pink
          My cool thoughts...
          #+end_color")
     "<span style=\"color:pink;\">"
     (* anything)
     "My cool thoughts"
     (* anything)
     "</span>"))
;; Colours:4 ends here

;; [[file:org-special-block-extras.org::#kbd:nice-keystroke-renditions][Nice Keystroke Renditions: kbd:C-h_h:3]]
(deftest "It becomes <kbd> tags, but final symbol non-ascii *may* be ignored"
  [kbd direct-org-links]
  (⇝ (⟰ "kbd:C-u_80_-∀") "<p>\n<kbd style=\"\">C-u 80</kbd>_-∀</p>"))

(deftest "[[It]] becomes <kbd> tags"
  [kbd square-org-links]
  (⇝ (⟰ "[[kbd:C-u_80_-]]") "<p>\n<kbd style=\"\">C-u 80 -</kbd></p>"))

(deftest "<It> becomes <kbd> tags, and surrounding space is trimmed"
  [kbd angle-org-links]
  (⇝ (⟰ "<kbd: C-u 80 - >")  "<p>\n<kbd style=\"\">C-u 80 -</kbd></p>"))

;; FIXME: uh-oh!
(when nil
(deftest "It has a tooltip documenting the underlying Lisp function, when possible"
  [kbd tooltip]
  (⇝ (⟰ "<kbd: M-s h .>")

     "<abbr class=\"tooltip\""
     (* anything)
     "Highlight each instance of the symbol at point.<br>Uses the
     next face from ‘hi-lock-face-defaults’ without
     prompting,<br>unless you use a prefix argument.<br>Uses
     ‘find-tag-default-as-symbol-regexp’ to retrieve the symbol
     at point.<br><br>This uses Font lock mode if it is enabled;
     otherwise it uses overlays,<br>in which case the
     highlighting will not update as you type.&emsp;The
     Font<br>Lock mode is considered ''enabled'' in a buffer if
     its ‘major-mode’<br>causes ‘font-lock-specified-p’ to return
     non-nil, which means<br>the major mode specifies support for
     Font Lock."
     (* anything)
     "<kbd style=\"border-color: red\">M-s h .</kbd></abbr>")))
;; Nice Keystroke Renditions: kbd:C-h_h:3 ends here

;; [[file:org-special-block-extras.org::#Link-Here-OctoIcons][  /“Link Here!”/ & OctoIcons:3]]
(deftest "It works as expected: We have an anchor with the given ID, and the default SVG chain icon."
  [link:here]
  (⇝ (⟰ "link-here:example-location (Click the icon and see the URL has changed!)")
     "<a class=\"anchor\" aria-hidden=\"true\" id=\"example-location\" href=\"#example-location\"><svg"
     (* anything)
     "</svg></a> (Click the icon and see the URL has changed!)"
     (* anything)))
;;   /“Link Here!”/ & OctoIcons:3 ends here

;; [[file:org-special-block-extras.org::#Badge-Links][Badge Links:2]]
(deftest "It works when all 5 arguments are provided"
  [badge]
  (⇝ (⟰ "badge:Let_me_google_that|for_you!|orange|https://lmgtfy.app/?q=badge+shields.io&iie=1|Elixir")
     "<a href=\"https://lmgtfy.app/?q=badge+shields.io&iie=1\">"
     "<img src=\"https://img.shields.io/badge/Let_me_google_that-for_you%21-orange?logo=Elixir\"></a>"))

(deftest "It works when we use [[link]] syntax with generous spaces and newlines"
  [badge]
  (⇝ (⟰ "[[badge: Let me google that | for you! | orange |
         https://lmgtfy.app/?q=badge+shields.io&iie=1|Elixir]]")

     "<a href=\"https://lmgtfy.app/?q=badge+shields.io&iie=1\">"
     (* anything)
     "<img src=\"https://img.shields.io/badge/Let%20me%20google%20that-for%20you%21-orange?logo=Elixir\">"))

(deftest "It works when only the first 2 arguments are provided; asterisks are passed unaltered into the first argument"
  [badge]
  (⇝ (⟰ "badge:Let_me_*not*_google_that|for_you")
     "<img src=\"https://img.shields.io/badge/Let_me_%2Anot%2A_google_that-for_you-nil?logo=nil\">"))

(deftest "It works when all 5 arguments are provided - URL ‘here’ makes it a local link"
  [badge]
  (⇝ (⟰ "badge:key|value|informational|here|Elixir")
     "<a id=\"key\" href=\"#key\">"
     "<img src=\"https://img.shields.io/badge/key-value-informational?logo=Elixir\"></a>"))

(deftest "We can use spaces, commas, dashes, and percentage symbols in the first argument"
  [badge]
  (⇝ (⟰ "badge:example_with_spaces,_-,_and_%|points_right_here|orange|here")
     "<a id=\"example_with_spaces,_-,_and_%\" href=\"#example_with_spaces,_-,_and_%\">"
     "<img src=\"https://img.shields.io/badge/example_with_spaces%2C_--%2C_and_%25-points_right_here-orange?logo=nil\"></a>"))

(deftest "It works when only first 2 arguments are given: Default colour & logo are green & no logo shown"
  [badge]
  (⇝ (⟰ "badge:key|value")
     "<img src=\"https://img.shields.io/badge/key-value-nil?logo=nil\">"))

(deftest "When only a key is provided, the value slot is shown as an empty green stub"
  [badge]
  (⇝ (⟰ "badge:key")
     "<img src=\"https://img.shields.io/badge/key--nil?logo=nil\">"))

(deftest "When only a value is provided, only the value is shown in a default green ---no stub for the missing key, yay"
  [badge]
  (⇝ (⟰ "badge:|value")
        "<img src=\"https://img.shields.io/badge/-value-nil?logo=nil\">"))


(deftest "It's only a green stub when provided with an empty key and empty value"
  [badge]
  (⇝ (⟰ "badge:||green")
     "<img src=\"https://img.shields.io/badge/--green?logo=nil\">"))

(deftest "It's only a green stub when we use a totally [[badge:]]"
  [badge]
  (⇝ (⟰ "[[badge:]]")
     "<img src=\"https://img.shields.io/badge/--nil?logo=nil\">"))
;; Badge Links:2 ends here

;; [[file:org-special-block-extras.org::#COMMENT-Intro][Intro, motivating examples:4]]
(deftest "It gives a tooltip whose title is the Lisp docs of APPLY"
  [doc]
  (⇝ (⟰ "doc:apply")
     "<abbr class=\"tooltip\" title=\""
     (literal (s-replace "\n" "<br>" (documentation #'apply)))
     "\">apply</abbr>"))

(setq angst
      (⟰ "#+begin_documentation Existential Angst :label \"ex-angst\"
                       A negative feeling arising from freedom and responsibility.

                       Also known as
                       1. /Existential Dread/, and
                       2. *Existential Anxiety*.

                       Perhaps a distraction, such as [[https://www.w3schools.com][visiting W3Schools]], may help ;-)

                       Then again, ~coding~ can be frustrating at times, maybe have
                       a slice of pie with maths by reading “$e^{i×π} + 1 = 0$” as a poem ;-)
                       #+end_documentation

                       We now use this as doc:ex-angst."))

(deftest "Documentation blocks are not exported; they produce a new osbe--docs entry"
  [doc documentation o--name&doc]
    (should (org-docs-get "ex-angst")))

;; Upon export, the #+begin_documentation is /not/ present.
;; We have the text outside that block only.
;; Along, with a htmlified tooltip of the new entry.
(deftest "The osbe--docs entry of the documentation block appears within a tooltip"
    [doc documentation o--name&doc]
    (⇝ angst " <p> We now use this as <abbr class=\"tooltip\" title=\""
             (literal (org-ospe-html-export-preserving-whitespace
                       (cl-second (org-docs-get "ex-angst"))))
                    "\">Existential Angst</abbr>.</p> "))
;; Intro, motivating examples:4 ends here

;; [[file:org-special-block-extras.org::#Marginal-one-off-remarks][Marginal, “one-off”, remarks:2]]
(setq margin (⟰ "/Allah[[margin:][The God of Abraham; known as Elohim
               in the Bible]] does not burden a soul beyond what it can bear./
               --- Quran 2:286"))

(deftest "It exports into an <abbr> tooltip"
  [margin]
  (⇝ margin "<abbr class=\"tooltip\""))

(deftest "It mentions the margin link's key before the tooltip"
  [margin]
  (⇝ margin "Allah<abbr class=\"tooltip\""))

(deftest "Our personalised marginal remark appears in an <abbr> title"
  [margin]
  (⇝ margin "title=\"The God of Abraham; known as Elohim"
            "<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; in the Bible\">"))

(deftest "The marginal remark appears in a tiny circle"
  [margin]
  (⇝ margin "<abbr" (* anything) "°</abbr>"))
;; Marginal, “one-off”, remarks:2 ends here

;; [[file:org-special-block-extras.org::#Equational-Proofs][Equational Proofs:4]]
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
;; Equational Proofs:4 ends here
