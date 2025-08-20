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

(cl-defmacro deftest (description &optional tags &rest body)
  "Declare tests with meaningful string names, that reflect the test's main goal.

DESCRIPTION is a string.

Benefits of this macro:
+ Converts the description string into a readable and valid ERT symbol
+ Derives the tag from the first word in the test description (usually the method name)
+ Allows natural punctuation like quotes, commas, etc.

Each test now starts with a readable natural-language string-based
description and gets automatically tagged based on the function under
test.

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

(deftest \"`length' of nil is 0\" [zero]
  (should (equal (length nil) 0)))

(ert '(tag zero))
(ert '(tag length))

This generates an ERT test named `length·of·nil·is·0'.
"
  (declare (indent defun))  
  (cl-assert (stringp description) nil "“deftest” expects its first argument to be a string literal")
  (let*
      ((replacements `((" " . ,deftest-space)
                       ("`" . "")
                       ("'" . "")
                       ("," . "︐")
                       ("`" . "‵")
                       (";" . "︔")
                       ("[" . "⁅")
                       ("]" . "⁆")))
       (provided-tags (seq--into-list (and (vectorp tags) tags)))
       (test-name
        (concat ;; ⇨ ➝ ➡ ➩ ➾ ⚝
         (if provided-tags
             (format "%s··⇨··" (s-join "︐" (mapcar #'prin1-to-string provided-tags)))
           "")
        (thread-last description s-trim s-collapse-whitespace (s-replace-all replacements))))
       (method-being-tested
        (thread-last description s-trim (s-split " ") car (s-replace-all replacements))))
    `(ert-deftest ,(intern test-name) ()
       :tags ',(cons method-being-tested provided-tags)
       ;; Actually treat ‘tags’ as optional.
       ,@(if (vectorp tags) body (cons tags body)))))

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

;;; Parsing & evaluating org-special-block structures

(deftest "`org-special-block-after-point' correctly parses block structure, when given its name"
  (with-temp-buffer
    (insert (lf-string "#+begin_foo mainarg :x 1 :y 2
                        block content
                        #+end_foo"))
    (goto-char (point-min))
    (-let [(&org-special-block 'name 'main-arg 'kwdargs 'contents)
           (org-special-block-after-point "foo")]
      (should (equal name "foo"))
      (should (equal main-arg "mainarg"))
      (should (equal kwdargs '(:x 1 :y 2)))
      (should (equal contents "block content")))))


(deftest "`org-special-block-after-point' correctly parses any block structure after point"
  (with-temp-buffer
    (insert (lf-string "#+begin_bar \"The main arg\" :key₁ value₁ :key₂ value₂
                        some content here,
                        
                        possibly empty
                        #+end_bar"))
    (goto-char (point-min))
    (-let [(&org-special-block 'name 'main-arg 'kwdargs 'contents)
           (org-special-block-after-point)]
      (should (equal name "bar"))
      (should (equal main-arg "The main arg"))
      (should (equal kwdargs '(:key₁ value₁ :key₂ value₂)))
      (should (equal contents
"some content here,

possibly empty")))))

  
(deftest "`org-special-block-after-point' correctly parses block structure with a main arg but no keyword args"
  (with-temp-buffer
    (insert (lf-string "#+begin_bar \"The main arg\"
#+end_bar"))
    (goto-char (point-min))
    (-let [(&org-special-block 'name 'main-arg 'kwdargs 'contents)
           (org-special-block-after-point)]
      (should (equal name "bar"))
      (should (equal main-arg "The main arg"))
      (should (equal kwdargs nil))
      (should (equal contents "")))))


(deftest "`org-special-block-after-point' correctly parses block structure with no main arg but given keyword args"
  (with-temp-buffer
    (insert (lf-string "#+begin_bar :key₁ value₁ :key₂ value₂
#+end_bar"))
    (goto-char (point-min))
    (-let [(&org-special-block 'name 'main-arg 'kwdargs 'contents)
           (org-special-block-after-point)]
      (should (equal name "bar"))
      (should (equal main-arg ""))
      (should (equal kwdargs '(:key₁ value₁ :key₂ value₂)))
      (should (equal contents "")))))


(deftest "`org-special-block-after-point' correctly parses block structure having no contents or args"
  (with-temp-buffer
    (insert (lf-string "#+begin_baz
                        #+end_baz"))
    (goto-char (point-min))
    (-let [(&org-special-block 'name 'main-arg 'kwdargs 'contents)
           (org-special-block-after-point)]
      (should (equal name "baz"))
      (should (equal main-arg ""))
      (should (equal kwdargs nil))
      (should (equal contents "")))))


(deftest "`org-special-block-after-point' correctly parses block structure with main-arg & keyword args omitted"
    (with-temp-buffer
      (insert (lf-string 
       "#+begin_shout
        world
        #+end_shout"))
      (goto-char (point-min))
      (-let [(&org-special-block 'name 'main-arg 'kwdargs 'contents)
             (org-special-block-after-point)]
        (should (equal name "shout"))
        (should (equal main-arg ""))
        (should (equal kwdargs nil))
        (should (equal contents "world")))))


(deftest "`org-special-block-after-point' correctly parses block structure with no main-arg but keyword args provided"
    (with-temp-buffer
      (insert (lf-string 
       "#+begin_shout :to me :and you
        world
        #+end_shout"))
      (goto-char (point-min))
      (-let [(&org-special-block 'name 'main-arg 'kwdargs 'contents)
             (org-special-block-after-point)]
        (should (equal name "shout"))
        (should (equal main-arg ""))
        (should (equal kwdargs '(:to me :and you)))
        (should (equal contents "world")))))


(deftest "`org-special-block-after-point' correctly parses block structure with no main-arg nor contents but keyword args provided"
    (with-temp-buffer
      (insert (lf-string 
       "#+begin_shout :to me :and you
        #+end_shout"))
      (goto-char (point-min))
      (-let [(&org-special-block 'name 'main-arg 'kwdargs 'contents)
             (org-special-block-after-point)]
        (should (equal name "shout"))
        (should (equal main-arg ""))
        (should (equal kwdargs '(:to me :and you)))
        (should (equal contents "")))))


(deftest "`org-eval-replace-block' replaces a block with handler output"
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


(deftest "`org-eval-replace-block' handles special block within numbered list"
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

(deftest "`org--rewrite-special-blocks-by-handlers' transforms supported blocks but leaves others unchanged"
  (with-temp-buffer
    ;; Setup supported blocks and mock global backend
    (setq org--supported-blocks '("foo")
          org--current-backend nil)

    ;; Dummy handler for "foo" blocks
    (defun org-block/foo (backend contents arg &rest args)
      (format "FOO block (%s): %s [arg: %s] [args: %s]" backend contents arg args))

    ;; Ensure all supported blocks have handlers
    (should (--all-p (functionp (intern (format "org-block/%s" it))) org--supported-blocks))

    ;; Insert Org content with both supported and unsupported blocks
    (insert
     (lf-string "\t#+begin_foo mainarg :x 1 :y 2
                   This is foo block content.
                   #+end_foo

                  However, the next is left alone:
                  #+begin_foobar mainarg :x 1 :y 2
                  This is foobar block content.
                  #+end_foobar
                  "))

    ;; Run the transformer
    (goto-char (point-min))
    (org--rewrite-special-blocks-by-handlers 'test-backend)

    ;; Assert transformation
    (should (equal (buffer-string)
                   "	FOO block (test-backend): This is foo block content. [arg: mainarg] [args: (:x 1 :y 2)]

                  However, the next is left alone:
                  #+begin_foobar mainarg :x 1 :y 2
                  This is foobar block content.
                  #+end_foobar
                  "))))

;;; org-defblock-only

(org-defblock-only speak (who "dev" signoff "!")
  "Speaking block"
  (format "%s says hi%s%s" who signoff (if (equal backend 'latex) "~LaTeX~" "")))


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

BACKEND refers to the current export backend.
RAW-CONTENTS refers to the text as the user wrote it verbatim.
⇒ You may mention CONTENTS to refer to the ‘org parsed’ version of user text.
⇒ CONTENTS and RAW-CONTENTS are identical whenever CONTENTS-OCCUR-AS-LINK-DESCRIPTION is non-nil.

(fn BACKEND RAW-CONTENTS &optional WHO &rest ## &key (SIGNOFF \"!\") (CONTENTS-OCCUR-AS-LINK-DESCRIPTION nil) &allow-other-keys)")))

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

;;; Test utility “exporting”

(cl-defmacro exporting (string &key (to 'html) using equals modulo)
  "Assert that exporting STRING to backend TO equals EQUALS (optionally modulo MODULO).

If EQUALS is omitted, this generates the expectations only.
This is useful in combination with `C-u C-x C-e'.

Tldr: Run an export assertion, optionally installing temporary defblocks via :USING and cleaning them up.
Args:
+ TO is the name of a backend, such as `html' (default) or `latex'.
+ STRING is the input Org string (raw; `lf-string' is applied to it).
+ EQUALS is the expected output as a string; if omitted, return the actual export.
+ MODULO is a string or list of strings replaced by \".*\" before comparing via regex.
+ USING  is either a single (org-defblock …) form OR a list of such forms.
  Each form is evaluated; any functions it introduces are unbound
  afterwards to avoid polluting the global namespace.
  (If we use top-level org-defblock forms instead, we can have unexpected
  calls when the same name is used for different blocks in different tests! 🤮)
  This is a poor-man's `cl-letf'.

API Notes:
+ (exporting A :equals B)            ≋  (should (equal (export A) B))
+ (exporting A :equals B :modulo C)  ≋  “A equals B with all instances of C replaced by .*”

Example use:
  (exporting \"A B C D\" :to ascii :equals \"A P C T\" :modulo (\"P\" \"T\"))

See the associated deftest for more example uses.
"
  (declare (indent defun))
  ;; Normalize :using into a list of (org-defblock …) forms but DO NOT evaluate here (at macro-expansion time)
  ;; to avoid polluting the namespace even though the generated code has not yet actually been executed (at runtime).
  (let ((using-forms
         (pcase using
           ((pred null) nil)
           (`(org-defblock . ,_) (list using))
           ((and (pred listp) forms)
            (progn
              (dolist (f forms)
                (cl-assert (and (consp f) (eq (car f) 'org-defblock))
                           nil ":using list must contain only (org-defblock …) forms"))
              forms))
           (_ (error ":using must be a single (org-defblock …) or a list of them")))))
    ;; Build the runtime assertion form
    (let* ((actual   `(export (lf-string ,string) ',to))
           (expected (if (not equals)
                         nil
                       (if (not modulo)
                           `(lf-string ,equals)
                         `(thread-last ,equals
                            lf-string
                            regexp-quote
                            (s-replace-all
                             ',(--map (cons it ".*")
                                      (if (listp modulo) modulo (list modulo))))
                            (format "^%s$")))))
           (assertion
            (cond
             ((not equals) actual) ; return actual when no equals provided
             (modulo `(should (equal 0 (string-match-p ,expected ,actual))))
             (t      `(should (equal ,actual ,expected))))))
      ;; Generate code that evaluates :using at runtime and cleans up
      (if (null using-forms)
          ;; No temp blocks: just run the assertion/return form
          assertion
        ;; With temp blocks: eval them now, remember symbols, cleanup with unwind-protect
        `(let* ((__new_syms
                 (cl-mapcan
                  (lambda (form)
                    (let ((res (eval form)))      ; eval NOW, at runtime
                      (cond ((null res) nil)
                            ((listp res) (cl-copy-list res))
                            (t          (list res)))))
                  ',using-forms)))
           (unwind-protect
               ,assertion
             ;; Cleanup both generics and link fns; ignore if absent
             (mapc (lambda (sym) (ignore-errors (fmakunbound sym))) __new_syms)))))))


(deftest "`exporting' works as intended"
  ;; Basic usage
  (exporting "A B C D" :to ascii :equals "A P C T" :modulo ("P" "T"))

  ;; No pollution of global namespace
  (should-not (fboundp 'org-block/shout))
  (should-not (fboundp 'org-link/shout))
  (exporting "shout:hello" :to ascii :using (org-defblock shout (wat) "docs" (upcase wat)) :equals "HELLO\n")
  (should-not (fboundp 'org-block/shout))
  (should-not (fboundp 'org-link/shout))

  ;; :using may be omitted
  (exporting "shout:hello" :to ascii :equals "<shout:hello>\n")

  ;; :using may be a (singleton) list
  (exporting "shout:hello"
             :to ascii
             :using ((org-defblock shout (wat) "docs" (upcase wat)))
             :equals "HELLO\n")
  
  ;; :using may be a multi-element list
  (exporting "shout:hello quiet:WORLD"
             :to ascii
             :using ((org-defblock shout (wat) "docs" (upcase wat))
                     (org-defblock quiet (wat) "docs" (downcase wat)))
             :equals "HELLO world\n"))


(cl-defun export (string &optional (backend 'html))
  "Export Org STRING along BACKEND, with `org-special-block-extras' enabled."
  (with-temp-buffer
    (insert "\n") ;; Without the newline, we lose any initial string.
    (insert string)
    (let ((org-inhibit-startup t))
      (org-mode)
      (org-special-block-extras-mode)
      (org-export-as backend nil nil :body-only nil))))

;;; Tests about header-args and delimiters, via `org-defblock'

(deftest "mismatched begin/end is OK for unsupported blocks" [delimiters]
  (let (org--supported-blocks) ;; Nothing is supported
    (should
     (exporting 
       "#+begin_shout
        content 3
        #+end_stutter"))))


(deftest "mismatched begin/end for supported blocks shows a helpful message" [delimiters]
  (-let [org--supported-blocks '(shout)] ;; “shout” is supported
    (thread-last
      (exporting "#+begin_shout 0
              content 3
              #+end_stutter")
      should-error
      cl-second
      (equal   "‘org-special-block-after-point’: I had trouble parsing “#+begin_shout ⟨args⟩?\n⟨content⟩?\n#+end_shout”. Are the required pieces there? 🤔")
      should)))


(deftest "main-arg may be a quoted string" [header-args]
  (-let [org--supported-blocks '(shout)] ;; “shout” is supported
    (exporting
      "#+begin_shout \"Hello, to the \"
        world
        #+end_shout"
      :using (org-defblock shout (greeting) "docs" (concat greeting (upcase contents)))
      :equals "Hello, to the 
                <p>
                WORLD
                </p>
                ")))

(deftest "main-arg may be omitted" [header-args]
  (exporting
    "#+begin_shout
        world
        #+end_shout"
    :using (org-defblock shout (greeting) "docs" (concat greeting (upcase contents)))
    :equals "
               <p>
               WORLD
               </p>
               "))


(deftest "main-arg may be omitted but keywords provided" [header-args]
  (exporting
    "
#+begin_shout :to me
        world
#+end_shout
"
    :using (org-defblock shout (greeting nil to nil) "docs" (format "%s ⟶%s⟶ %s" greeting to (upcase contents)))
    :equals "nil ⟶me⟶ 
                <p>
                WORLD
                </p>
                "))

;;; org-defblock and org-undefblock

(deftest "`org-defblock' defines a handler function and evaluates it correctly"
  (org-defblock hello (who "world" punct "!") "Greeter block"
                (format "Hello, %s%s" who punct))
  (should (fboundp 'org-block/hello))
  (should (string= (org-block/hello 'test-backend "ignored" "Emacs" :punct "!!")
                   (lf-string "#+begin_export test-backend 
                               Hello, Emacs!!
                               #+end_export"))))

(deftest "`org-defblock' uses default values set by `org-set-block-header-args'"
  (org-defblock greeting (name "user" punct "!") "Greeting block"
                (format "Hello, %s%s" name punct))
  (org-set-block-header-args greeting :main-arg "dev" :punct "!×4")
  (should (string= (org-block/greeting 'test-backend "some content" nil :punct nil)
                   (lf-string "#+begin_export test-backend 
                               Hello, dev!×4
                               #+end_export"))))

(deftest "`org-defblock' defines associated link functions that evaluate correctly"
  (org-defblock notice () [:face 'italic] "Example."
                (format "NOTICE: %s" contents))
  (should (fboundp 'org-block/notice))
  (should (fboundp 'org-link/notice))
  (should (equal (org-link/notice "Some note here" nil 'test-backend)
                 "NOTICE: Some note here")))

(deftest "`org-defblock' exports a custom block to HTML with content formatting"
  (org-defblock highlight (label "Note" style "color:red") "Highlight block"
                (format "<div style='%s'><strong>%s:</strong> %s</div>" style label contents))
  (exporting "Look: 
              #+begin_highlight Warning :style color:orange
              Something **important** here.
              #+end_highlight"             
             :equals
             "<p>
              Look: 
              </p>
              <div class=\"highlight\" id=\"org0b5090e\">
              <p>
              Something <b><b>important</b></b> here.
              </p>
              
              </div>
              "
             :modulo "org0b5090e"))

(deftest "`org-undefblock' removes org-special-block-support for a block type: Both block & link support"
  (-let (org--supported-blocks)
    (org-defblock shout () (upcase contents))
    (should (-contains? org--supported-blocks 'shout))
    (should (fboundp 'org-block/shout))
    (should (fboundp 'org-link/shout))
    (exporting "#+begin_shout
  hello, world
  #+end_shout"
      :to latex
      :equals "
  HELLO, WORLD
  ")
    (exporting "[[shout: hello, world ]]"
      :to latex
      :equals " HELLO, WORLD \n")
    
    ;; Stick “un” after the “-” in “org-defblock” to remove support for it.
    (org-undefblock shout () (upcase contents))
    (should-not (-contains? org--supported-blocks 'shout))
    (should-not (fboundp 'org-block/shout))
    (should-not (fboundp 'org-link/shout))
    (--all-p (should-not (plist-get (org-link-set-parameters "shout") it))
             '(:export :face :follow :display :keymap :help-echo))
    (exporting "#+begin_shout
  hello, world
  #+end_shout"
      :to latex
      :equals "\\begin{shout}
  hello, world
  \\end{shout}
  ")
    (exporting "[[shout: hello, world ]]"
      :to latex
      :equals "\\url{shout: hello, world }\n")))


;;; Indentation preservation -- Issue ♯8

(deftest "indented blocks preserve list structure in HTML output" [issue♯8]
  (exporting "- item one
              - item two
                #+begin_testblock foo
                inner
                #+end_testblock
              - item three"
             :using (org-defblock testblock ()  "docs"  (concat "HANDLED:" contents))
             :equals
             "<ul class=\"org-ul\">
              <li>item one</li>
              <li><p>
              item two
              </p>
              HANDLED:
              <p>
              inner
              </p></li>
              <li>item three</li>
              </ul>
              "))
              
(deftest "indented blocks preserve list structure in LaTeX export" [issue♯8]
  (exporting  "1. First
               2. Second
                  #+begin_testblock foo
                  inner
                  #+end_testblock
               3. Third"
              :to latex
             :using (org-defblock testblock ()  "docs"  (concat "HANDLED:" contents))              
              :equals
              "\\begin{enumerate}
               \\item First
               \\item Second
               HANDLED:
               inner
               \\item Third
               \\end{enumerate}
               "))
                 
(deftest "indented blocks, in enumerations, do not introduce extra blank lines" [issue♯8]
  (exporting "- A
                #+begin_testblock foo
                body
                #+end_testblock
                next line
              - B"
             :using (org-defblock testblock ()  "docs"  (concat "HANDLED:" contents))             
             :equals
             "<ul class=\"org-ul\">
              <li>A</li>
              </ul>
              HANDLED:
              <p>
              body
              </p>
              
              <p>
              next line
              </p>
              <ul class=\"org-ul\">
              <li>B</li>
              </ul>
              "))

;; This is the reproducible test case of issue ♯8.
;; https://github.com/alhassy/org-special-block-extras/issues/8#issue-814851814
(deftest "enumerations are preserved when they contain special blocks, for LaTeX" [issue♯8]
  (exporting "
              1. builtin source block
                 #+begin_src latex
                 \\uline{content 1}
                 #+end_src
              2. custom (unregistered) block
                 #+begin_proposition
                 content 2
                 #+end_proposition
              3. custom (registered) block
                 #+begin_testblock nil
                 content 3
                 #+end_testblock
              4. prose
                 \\n content 4
              "
             :to latex
             :using (org-defblock testblock ()  "docs"  (concat "HANDLED:" contents))             
             :equals
             "\\begin{enumerate}
              \\item builtin source block
              \\begin{verbatim}
                 \\uline{content 1}
              \\end{verbatim}
              \\item custom (unregistered) block
              \\begin{proposition}
              content 2
              \\end{proposition}
              \\item custom (registered) block
              HANDLED:
              content 3
              \\item prose
              \\n content 4
              \\end{enumerate}
              "))

(deftest "enumerations are preserved when they contain special blocks, for HTML" [issue♯8]
  (exporting "
              1. builtin source block
                 #+begin_src latex
                 \\uline{content 1}
                 #+end_src
              2. custom (unregistered) block
                 #+begin_proposition
                 content 2
                 #+end_proposition
              3. custom (registered) block
                 #+begin_testblock nil
                 content 3
                 #+end_testblock
              4. prose
                 \\n content 4
              "
             :using (org-defblock testblock ()  "docs"  (concat "HANDLED:" contents))             
             :equals
             "<ol class=\"org-ol\">
              <li><p>
              builtin source block
              </p>
              <div class=\"org-src-container\">
              <pre class=\"src src-latex\">   <span style=\"color: #98971a; font-weight: bold;\">\\uline</span>{content 1}
              </pre>
              </div></li>
              <li><p>
              custom (unregistered) block
              </p>
              <div class=\"proposition\" id=\"org0087298\">
              <p>
              content 2
              </p>
              
              </div></li>
              <li><p>
              custom (registered) block
              </p>
              HANDLED:
              <p>
              content 3
              </p></li>
              <li>prose
              \\n content 4</li>
              </ol>
              "
             :modulo ("org0087298" ;; Randomly generated Org ID
                      "style=\"color: #98971a; font-weight: bold;\"" ;; Style is theme-dependent
                      )))
  
;; TODO: Prettify resulting HTML so the expectations are easier to read. See `e2e.el'.

(deftest "enumerations are preserved for blocks inside lists inside blocks" [issue♯8] 
  (exporting "
              #+begin_foo X
              1. Something\\
                 Indented line no. 1
              2. Something else
                 #+begin_bar Y   
                 Indented line no. 2
                 #+end_bar
                 Indented line no. 3
              3. Something else
              #+end_foo
              1. Something else
                 #+begin_baz Z
                 Indented line no. 4
                 #+end_baz
              "
             :to latex
             :using ((org-defblock foo () (format "FOO⟨%s⟩" contents))
                     (org-defblock bar () (format "BAR⟨%s⟩" contents))
                     (org-defblock baz () (format "BAZ⟨%s⟩" contents)))
             :equals
             "FOO⟨
              \\begin{enumerate}
              \\item Something$\\backslash$
              Indented line no. 1
              \\item Something else
              BAR⟨
              Indented line no. 2
              ⟩
              Indented line no. 3
              \\item Something else
              \\end{enumerate}
              ⟩
              \\begin{enumerate}
              \\item Something else
              BAZ⟨
              Indented line no. 4
              ⟩
              \\end{enumerate}
              "))

;;; Old tests
(when nil
  
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
          ))

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

) ;; End ignoring old tests
