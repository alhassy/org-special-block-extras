(require 'ert)
(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'dash)

;;; Bespoke test macro “deftest”
;; I dislike writing “(ert-deftest my-method-test () "my-method does the thing" :tags '(my-method) body)”
;; and instead prefer “(deftest "my-method does the thing" body)”.
;; My approach also simplifies things by surfacing test :tags into the “M-x ert” view.
;; Let's set that up!

(defmacro deftest (description &rest body)
  "Declare tests with meaningful string names, that reflect the test's main goal.

DESCRIPTION is a string.

Benefits of this macro:
+ Converts the description string into a readable and valid ERT symbol
+ Derives the tag from the first word in the test description (usually the method name)
+ Allows natural punctuation like quotes, commas, etc.

Each test now starts with a human-readable string and gets converted
into a test function.

The first tag should be the name of the main function being tested;
this name is prepended to the name of underlying ert-deftest.
This way, tests are grouped/namespaced when running ert from the command line.

I use Org-blocks with ‘:comments link’, this then serves to delimit
my tests into “suites”.

Example ERT call: (ert '(tag my-cool-method))

DESCRIPTION may contain spaces, commas, quotes, and other natural punctuation and ASCII.
For example, the description “Howdy, Musa's 3 `friends!`” is acceptable (and it
gets converted into the Lisp name “ Howdy︐·Musa’s·3·‵friends!‵ ”).
Without the s-replace-all, “M-x ert” crashes when it comes to selecting the test
to run.

Example use:

(deftest \"`length' of nil is 0\"
  (should (equal (length nil) 0)))

This generates an ERT test named `length·of·nil·is·0'.
"
  (declare (indent defun))  
  (cl-assert (stringp description) nil "“deftest” expects its first argument to be a string literal")
  (let* ((test-name
          (thread-last description
                       s-trim
                       s-collapse-whitespace
                       (s-replace-all `((" " . ,deftest-space)
                                         ("`" . "")
                                         ("'" . "")
                                         ("," . "︐")
                                         ("`" . "‵")
                                         (";" . "︔")
                                         ("[" . "⁅")
                                         ("]" . "⁆")))))
         (method-being-tested (car (s-split " " (s-trim test-name)))))
    `(ert-deftest ,(intern test-name) ()
     :tags (quote ,method-being-tested)
     ,@body)))

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

;;; Tests for org-special-block struct

(ert-deftest test-org-special-block-after-point ()
  "Test that `org-special-block-after-point' correctly parses a special block."
  (with-temp-buffer
    ;; Insert a sample special block
    (insert (lf-string "#+begin_foo mainarg :x 1 :y 2
                        block content
                        #+end_foo"))
    (goto-char (point-min))
    ;; Call the parser
    (-let [(&org-special-block 'name 'main-arg 'kwdargs 'contents)
           (org-special-block-after-point "foo")]
      (should (equal name "foo"))
      (should (equal main-arg "mainarg"))
      (should (equal kwdargs  '(:x 1 :y 2)))
      (should (equal contents "block content")))))


(ert-deftest test-org-eval-replace-block ()
  "Ensure `org-eval-replace-block' replaces a block correctly."

  ;; Dynamically define a mock handler function
  (cl-letf (((symbol-function 'org-block/foo)
             (lambda (backend contents arg &rest args)
               (format "REPLACED: %s | %s | %s | %s"
                       backend contents arg args))))

    (with-temp-buffer
      (insert (lf-string "Start
               #+begin_foo mainarg :x 1
               Body text
               #+end_foo
               End"))

      ;; Identify the block boundaries, manually
      (goto-char (point-min))
      (search-forward "#+begin_foo")
      (let* ((start (line-beginning-position))
             (end (progn (search-forward "#+end_foo") (line-end-position)))
             (blk (make-org-special-block
                   :name "foo"
                   :start-point start
                   :end-point end
                   :main-arg "\"mainarg\""
                   :kwdargs '(:x 1)
                   :contents "Body text")))

        ;; Evaluate the method
        (org-eval-replace-block blk 'test-backend)

        ;; Assert buffer was transformed as expected
        (should (equal (buffer-string)
                       (lf-string "Start
                                   REPLACED: test-backend | Body text | \"mainarg\" | (:x 1)
                                   End")))))))


(ert-deftest test-org-eval-replace-block-within-enumeration ()
  "Ensure `org-eval-replace-block' replaces a block correctly, in an enumeration."
  ;; Dynamically define a mock handler function
  (cl-letf (((symbol-function 'org-block/foo)
             (lambda (backend contents arg &rest args)
               (format "+ Args: %s %s \n+ Contents: %s" arg args (s-trim contents)))))
    (with-temp-buffer
      (insert (lf-string "
               Welcome class, today we will discuss the following:
               0. Why are we here?
               1. Why we do what we do?
                  #+begin_foo mainarg :x 1
                  Body text
                  #+end_foo
                  Or not do.
               2. Why we ask ‘why’?

               Take care!"))
      ;; Identify the block and evaluate it
      (goto-char (point-min))
      (-let [block (org-special-block-after-point)]
        (-let [(&org-special-block 'name 'start-point 'end-point) block]
          (should (equal name "foo"))
          (should (equal start-point 102))
          (should (equal end-point 152))
          (org-eval-replace-block block 'test-backend)
          ;; Assert buffer was transformed as expected
          (should (equal (buffer-string)
                         (lf-string "
                Welcome class, today we will discuss the following:
                0. Why are we here?
                1. Why we do what we do?
                   + Args: mainarg (:x 1) 
                   + Contents: Body text
                   Or not do.
                2. Why we ask ‘why’?
                
                Take care!"))))))))

;;; org--rewrite-special-blocks-by-handlers

(ert-deftest org--rewrite-special-blocks-by-handlers/foo-block-test ()
  (with-temp-buffer
    ;; (Note that OSBE would not pick-up the following if they were declared in a `cl-flet'.)
    (setq org--supported-blocks '("foo") ;; Sample supported blocks
          org--current-backend nil) ;; Mocked global var  
    ;; A dummy handler that transforms “FOO” blocks
    (defun org-block/foo (backend contents arg &rest args)  
      (format "FOO block (%s): %s [arg: %s] [args: %s]" backend contents arg args))      
    ;; All supported blocks ℬ have a handler function “org-block/ℬ”.
    (should (--all-p (functionp (intern (format "org-block/%s" it))) org--supported-blocks))
    (insert
     (lf-string "\t#+begin_foo mainarg :x 1 :y 2
                   This is foo block content.
                   #+end_foo

                  However, the next is left alone:
                  #+begin_foobar mainarg :x 1 :y 2
                  This is foobar block content.
                  #+end_foobar
                  "))
    (goto-char (point-min))
    (org--rewrite-special-blocks-by-handlers 'test-backend)
    (should (equal (buffer-string)
                   "	FOO block (test-backend): This is foo block content. [arg: mainarg] [args: (:x 1 :y 2)]

                  However, the next is left alone:
                  #+begin_foobar mainarg :x 1 :y 2
                  This is foobar block content.
                  #+end_foobar
                  "))))


;;; org-defblock-only
 
(deftest "`org-defblock-only' returns the name of the defined function"
  (should (equal
           (org-defblock-only speak (who "dev" signoff "!")
             "Speaking block"
             (format "%s says hi%s%s" who signoff (if (equal backend 'latex) "~LaTeX~" "")))
           'org-block/speak)))

(deftest "`org-defblock-only' defines a function with the correct symbol name"
  (should (fboundp 'org-block/speak)))

(deftest "`org-defblock-only' attaches a docstring to the generated function"
  (should (equal (documentation 'org-block/speak)
                 "Speaking block

(fn BACKEND RAW-CONTENTS &optional WHO &rest ## &key O-LINK? (SIGNOFF \"!\") &allow-other-keys)")))

(deftest "`org-defblock-only' generates expected output with explicit arguments"
  (should (string= (org-block/speak 'html "ignored contents" "Ada" :signoff ", cheerio!")
                   (lf-string "#+begin_export html 
                               Ada says hi, cheerio!
                               #+end_export"))))

(deftest "`org-defblock-only' honours default values for missing main and keyword args"
  (should (string= (org-block/speak 'html "ignored contents" "")
                   (lf-string "#+begin_export html 
                               dev says hi!
                               #+end_export"))))

(deftest "`org-defblock-only' ignores extra unexpected arguments safely"
  (should (string= (org-block/speak 'html "" "" "" 'extra 'args :are 'ignored)
                   (lf-string "#+begin_export html 
                               dev says hi!
                               #+end_export"))))

(deftest "`org-defblock-only' uses header-arg defaults when block args are blank"
  (let ((org--header-args '((speak . (:main-arg "Mickey" :signoff ", buddo!")))))
    (should (string= (org-block/speak 'html "contents" "" :signoff "")
                     (lf-string "#+begin_export html 
                                 Mickey says hi, buddo!
                                 #+end_export")))))

(deftest "`org-defblock-only' alters output depending on export backend"
  (should (string-match "dev says hi!" (org-block/speak 'html "" "")))
  (should (string-match "dev says hi!~LaTeX~" (org-block/speak 'latex "" ""))))


;;; org-defblock

(ert-deftest org-defblock/handler-definition ()
  "Test that a block handler is defined via `org-defblock' and evaluates correctly."
  ;; Define a simple block
  (org-defblock hello2 (who "world" punct "!") "Greeter block"
                (format "Hello, %s%s" who punct))
  (should (fboundp 'org-block/hello2))
  (should (string= (org-block/hello2 'test-backend "ignored" "Emacs" :punct "!!")
                   (lf-string "#+begin_export test-backend 
                               Hello, Emacs!!
                               #+end_export"))))


(ert-deftest org-defblock/default-argument-values ()
  "Test that default argument values work with `defblock-header-args'."
  (org-defblock greeting2 (name "user" punct "!") "Greeting block"
                (format "Hello, %s%s" name punct))
  ;; Set defaults
  (org-set-block-header-args greeting2 :main-arg "dev" :punct "!×4")
  ;; Simulate calling with nil main arg and nil keyword arg
  (should (string= (org-block/greeting2 'test-backend "some content" nil :punct nil)
                   (lf-string "#+begin_export test-backend 
                               Hello, dev!×4
                               #+end_export"))))


(ert-deftest org-defblock/link-handling ()
  "Test that a link associated with an `org-defblock' is defined and formats correctly."
  (org-defblock notice2 () [:face 'italic] "Example."
                (format "NOTICE: %s" contents))
  (should (fboundp 'org-block/notice2))
  (should (fboundp 'org-link/notice2))
  ;; Simulate the link function evaluation
  ;; (org-link/notice O-LABEL O-DESCRIPTION O-BACKEND)
  (should (equal (org-link/notice2  "Some note here" nil 'test-backend)
                 "NOTICE: Some note here")))


(cl-defun export (string &optional (backend 'html))
  "Export Org STRING along BACKEND, with `org-special-block-extras' enabled."
  (with-temp-buffer
    (insert string)
    (let ((org-inhibit-startup t))
      (org-mode)
      (org-special-block-extras-mode)
      (org-export-as backend nil nil :body-only nil))))


(ert-deftest org-defblock/export-html ()
  "Test that `org-defblock' handlers export to HTML correctly."

  ;; Define the block 
  (org-defblock highlight2 (label "Note" style "color:red") "Highlight block"
                (format "<div style='%s'><strong>%s:</strong> %s</div>" style label contents))

  ;; Setup test buffer with an org-mode block
  (should (thread-last
            (export "Look: \n #+begin_highlight2 Warning :style color:orange\nSomething **important** here.\n#+end_highlight2")
            ;; TODO: FIXME: Where's the initial text “Look:” ?            
            (string-match
             "^<div class=\"highlight2\" id=\".*\">
<p>
Something <b><b>important</b></b> here.
</p>

</div>
$"))))


;;; Old tests

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
          )

(deftest "org-deflink makes documented functions"
  [org-deflink]
  (⇝ (documentation #'org-link/shout)
     "Capitalise the link description, if any, otherwise capitalise the label.

     The link text appears as red bold in both Emacs and in HTML export."))

(deftest "org-deflink works as expected, plain links"
         [org-deflink]
         (should (not (null (symbol-function 'org-link/shout))))
         (⇝ (export "shout:hello")
     "<p> <span style=\"color:red\"> HELLO </span></p>"))

(deftest "org-deflink works as expected, bracket links"
         [org-deflink]
         (⇝ (export "[[shout:hello]]")
     "<p> <span style=\"color:red\"> HELLO </span></p>")
         (⇝ (export "[[shout:hello][world!]]")
     "<p> <span style=\"color:red\"> WORLD! </span></p>"))

(deftest "org-deflink works as expected, angle links"
         [org-deflink]
         (⇝ (export "<shout: hello world!>")
     "<p> <span style=\"color:red\"> HELLO WORLD! </span></p>"))
;; Define links as you define functions: doc:org-deflink:4 ends here

(org-defblock scream
  (speaker "Default_Speaker")
  [:face '(:foreground "green" :weight bold)]
  "Capitalise the contents! Seen in red bold in Emacs!"
  (format "%s: %s" speaker (upcase contents)))

(deftest "Upcase works as expected on links, with only labels"
    [basic-defblock org-link]
         (⇝ (export "pre scream:hello post")
            "pre hello: HELLO post"))

(deftest "Upcase works as expected on links, with descriptions"
         [basic-defblock org-link]
         (⇝ (export "pre [[scream:hello][my dear friends]] post")
            "hello: MY DEAR FRIENDS post"))

(deftest "Upcase works as expected on blocks"
         [basic-defblock]
         (⇝ (export "pre
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
         (⇝ (export "pre
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

;; [[file:org-special-block-extras.org::#kbd:nice-keystroke-renditions][Nice Keystroke Renditions: kbd:C-h_h:3]]
(deftest "It becomes <kbd> tags, but final symbol non-ascii *may* be ignored"
  [kbd direct-org-links]
  (⇝ (export "kbd:C-u_80_-∀") "<p>\n<kbd style=\"\">C-u 80</kbd>_-∀</p>"))

(deftest "[[It]] becomes <kbd> tags"
  [kbd square-org-links]
  (⇝ (export "[[kbd:C-u_80_-]]") "<p>\n<kbd style=\"\">C-u 80 -</kbd></p>"))

(deftest "<It> becomes <kbd> tags, and surrounding space is trimmed"
  [kbd angle-org-links]
  (⇝ (export "<kbd: C-u 80 - >")  "<p>\n<kbd style=\"\">C-u 80 -</kbd></p>"))

;; FIXME: uh-oh!
(when nil
(deftest "It has a tooltip documenting the underlying Lisp function, when possible"
  [kbd tooltip]
  (⇝ (export "<kbd: M-s h .>")

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
