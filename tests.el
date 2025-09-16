;;; “html”: HTML Templating Facade: Use Lisp notation as an easy-to-type and easy-to-manipulate realization of HTML notation

(defun html--stringify (x)
  "Return X as a string for attribute values and text nodes."
  (cond
   ((stringp x) x)
   ((symbolp x) (symbol-name x))
   ((numberp x) (number-to-string x))
   ((null x) "")
   (t (format "%s" x))))

(defun html--to-shr (node)
  "Convert NODE written as (tag :k v ... children...) into SHR DOM:
 (tag ((k . v) ...) child ...). Children/text are converted recursively.
 Keywords become symbols by stripping the leading :."
  (cond
   ;; Base Case: Literal Node
   ((or (stringp node) (numberp node) (symbolp node) (null node))
    (html--stringify node))

   ;; Recursive Case: Element node
   ((consp node)
    (let* ((tag  (car node))
           (rest (cdr node))
           (attrs nil))
      ;; collect leading :key value pairs
      (while (and rest (keywordp (car rest)))
        (let* ((kw  (pop rest))                 ; e.g., :class
               (val (pop rest))                 ; e.g., org-ul
               (sym (intern (substring (symbol-name kw) 1)))) ; class
          (push (cons sym (html--stringify val)) attrs)))
      ;; build SHR DOM: (tag ((k . v) ...) child ...)
      (cons tag
            (cons (nreverse attrs)
                  (mapcar #'html--to-shr rest)))))
   ;; Fallback: stringify
   (t (html--stringify node))))

(defun html (form)
  "Render FORM (in keyword-attr style) to HTML via `shr-dom-to-xml'.
Example:
  (html `(div :class \"c\" (a :href ,url \"link\")))

This is a super simple aesthetic facade over the built-in `shr.el', the Simple HTML Renderer
where we extend the literals to include numbers and attributes & child HTML elements are optional:

     (equal (shr-dom-to-xml '(div ((class . header)) (p nil \"123\")))
            (html '(div :class header (p 123))))

Besides the extended literal support and using a keywords for attributes, this method relies
on the existing ecosystem of Elisp evaluation, such as quasi-quoting. It is only a facade:
Where `shr-dom-to-xml' requires an alist of attributes and string literals, this method
uses a spliced plist of attributes and accepts string, numeric, and symbolic literals.


In-particular, whereas many JavaScript tools use “handlebars” or “mustaches” for interpolation,
Lisp uses quasi-quoting:

   AngularJS:         <div> Hello, my name is {{userFullName}} </div>
   Emacs Lisp:        (html `(div ,(concat \"Hello, my name is \" user-full-name)))

While slightly more verbose, the Lisp approach does not require an external tool (viz AngularJS)
& so no new syntax to learn, and the HTML templates can be composed from smaller modular pieces.
The humble quasi-quote has existed long before JS and HTML; it makes all kinds of templating easy.

NOTE: Syntax of HTML elements is defined recursively as follows.

HTML ::= String | Number | Symbol
      |  (element-name :attribute₁ value₁ … :attributeₙ valueₙ content*)
         ;; where ‘content*’ is any number of HTML values and ‘valueᵢ’ are string|number values.
         ;; Only ‘element-name’ is mandatory

  This keeps the call-site simple and lets you use symbols/numbers naturally in attributes and text.

NOTE: If you want an HTML string literal to be read verbatim, wrap it in `org-html-encode-plain-text'
to escape HTML entities, such as: \"<This> is </verbatim> &amp;\".

NOTE: To parse an HTML string in a format that `shr-dom-to-xml' can print back, use:

(let* ((s \"<span class=\\\"note\\\">Hello world</span>\") 
       (dom (with-temp-buffer
              (insert s)
              (libxml-parse-html-region))))
  dom)

With this DOM, one can also use `dom-by-tag', `dom-search', `dom-attr', `dom-children', etc."
  (if (null form)
      (error ("The ‘html’ method expects a non-empty list."))
    (shr-dom-to-xml (html--to-shr form))))


(deftest "`html' example use involving quasi-quoting"
  (should (equal
           "<div class=\"container\" id=\"top\"> <a href=\"https://google.com\">Google</a></div>"
           (let ((my-url "https://google.com"))
             (html
              `(div :class "container" :id "top"
                    (a :href ,my-url
                       "Google")))))))


(deftest "`html' attributes and body are optional"
  (should (equal (html '(br)) "<br></br>")))


(deftest "`html' attributes & body can be symbols or numbers; content is list"
  (should (equal "<ul class=\"org-ul\"> <li>1</li> <li>2</li> <li>3</li></ul>"
                 (html '(ul :class org-ul
                            (li 1)
                            (li 2)
                            (li 3))))))

(deftest "`html' content can be a symbol"
  (should (equal "<p>hello-world</p>"
                 (html '(p hello-world)))))

(deftest "`html' content can be a number"
  (should (equal "<p>12.3</p>"
                 (html '(p 12.3)))))

(deftest "`html' nested works as expected"
  (should (equal "<div class=\"container\"> <div class=\"row\"> <div class=\"col-8\"> <p>paragraph 1</p></div> <div class=\"col-4\"> <p>paragraph 2</p></div></div></div>"
                 (html
                  '(div :class container
                        (div :class row
                             (div :class col-8
                                  (p "paragraph 1"))
                             (div :class col-4
                                  (p "paragraph 2"))))))))

(deftest "`html' forms can be generated programmatically"
  (should (equal "<div> <p>1</p> <p>2</p> <p>3</p></div>"
                 (html (cons 'div (mapcar (lambda (n) `(p ,n)) '(1 2 3)))))))

(deftest "`html' content can be spliced forms"
  (should (equal "<div> <p>1</p> <p>2</p> <p>3</p></div>"
                 (html `(div ,@(mapcar (lambda (n) `(p ,n)) '(1 2 3)))))))


(deftest "`html' semi-realistic example"
  (should (equal "<html lang=\"en\"> <head> <meta charset=\"UTF-8\"></meta> <title>Hello, World!</title></head> <body> <div class=\"my-class\"> <h1>HTML generated by shr!</h1> <ol> <li> times 5 is 5</li> <li> times 5 is 10</li> <li> times 5 is 15</li> <li> times 5 is 20</li> <li> times 5 is 25</li></ol></div></body></html>"
                 (html
                  `(html :lang en
                         (head 
                          (meta :charset UTF-8)
                          (title "Hello, World!"))
                         (body 
                          (div :class my-class
                               (h1  "HTML generated by shr!")
                               (ol ,@(cl-loop for i from 1 to 5 collect `(li  ,(format " times %s is %s" 5 (* i 5))))))))))))


;; NOTE: Consider making a LaTeX Templating Facade:
;; Use Lisp notation as an easy-to-type and easy-to-manipulate realization of mathematical notation.
;; E.g., (latex '(lambda (x) (f x))) = $(λ x. f x)$

;;; Requires
(require 'ert)
(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'dash)

(load-file "./org-special-block-extras.el")

;; 
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

(deftest "`org-defblock-only' returns the name of the defined function"
  (should (equal
           (org-defblock-only speak (who "dev" signoff "!")
             "Speaking block"
             (format "%s says hi%s%s" who signoff (if (equal backend 'latex) "~LaTeX~" "")))
           'org-block/speak))
  ;; clean-up
  (org-undefblock speak))

(deftest "`org-defblock-only' defines a function with the correct symbol name"
  (using (org-defblock-only speak (who "dev" signoff "!")
           "Speaking block"
           (format "%s says hi%s%s" who signoff (if (equal backend 'latex) "~LaTeX~" "")))
    (should (fboundp 'org-block/speak))))

(deftest "`org-defblock-only' attaches a docstring to the generated function"
  (using (org-defblock-only speak nil "Speaking block!" nil)
    (should (equal (documentation 'org-block/speak)
                   "Speaking block!

Regarding the Lisp function:
+ BACKEND refers to the current export backend.
+ RAW-CONTENTS refers to the text as the user wrote it verbatim.
  ⇒ You may mention CONTENTS to refer to the ‘org parsed’ version of user text.
  ⇒ CONTENTS and RAW-CONTENTS are identical whenever key CONTENTS-OCCUR-AS-LINK-DESCRIPTION is non-nil.

For example, upon LaTeX export, the Org special block

     #+begin_speak
     Hello, world
     #+end_speak

 is rewritten to the result of the call

     (org-block/speak ‘latex \"Hello, world\")"))))


(deftest "`org-defblock-only' defines a handler function and evaluates it correctly"
  (using (org-defblock-only hello (who "world" punct "!") "Greeter block"
           (format "Hello, %s%s" who punct))
    (should (fboundp 'org-block/hello))
    (should (string= (org-block/hello 'test-backend "ignored" "Emacs" :punct "!!")
                     (lf-string "#+begin_export test-backend 
                               Hello, Emacs!!
                               #+end_export")))))

(deftest "`org-defblock-only' uses default values set by `org-set-block-header-args'"
  (using (org-defblock-only greeting (name "user" punct "!") "Greeting block"
           (format "Hello, %s%s" name punct))
    (org-set-block-header-args greeting :main-arg "dev" :punct "!×4")
    (should (string= (org-block/greeting 'test-backend "some content" nil :punct nil)
                     (lf-string "#+begin_export test-backend 
                               Hello, dev!×4
                               #+end_export")))))

(deftest "TODO `org-defblock-only' inconsistent with `org-defblock'" [BUG FIXME]
  ;; ⟨1⟩ `org-defblock' works as expected
  (exporting "Look: 
              #+begin_highlight Warning :style color:orange
              Something **important** here.
              #+end_highlight"
    :using (org-defblock highlight (label "Note" style "color:red") "Highlight block"
             (format "<div style='%s'><strong>%s:</strong> %s</div>" style label contents))
    :equals  "<p>
            Look: 
            </p>
            <div style='color:orange'><strong>Warning:</strong> 
            <p>
            Something <b><b>important</b></b> here.
            </p>
            </div>
            ")
  ;; 🤮 `org-defblock-only' deviates 😲
  (exporting "Look: 
              #+begin_highlight Warning :style color:orange
              Something **important** here.
              #+end_highlight"
    :using (org-defblock-only highlight (label "Note" style "color:red") "Highlight block"
             (format "<div style='%s'><strong>%s:</strong> %s</div>" style label contents))
    :equals ;; NOTE: The <div>s are different!
    "<p>
Look: 
</p>
<div class=\"highlight\" id=\"orgf9d49c1\">
<p>
Something <b><b>important</b></b> here.
</p>

</div>
"
    :modulo ("orgf9d49c1") ;; Randomly generated Org ID
    ))


;; TODO: `org-undefblock-only' does not yet exist
(deftest "`org-undefblock-only' removes org-special-block-support for a block type: Both block & link support" [TODO]
  (-let (org--supported-blocks)
    (org-defblock-only shout () (upcase contents))
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
    
    ;; Stick “un” after the “-” in “org-defblock-only” to remove support for it.
    (org-undefblock shout () (upcase contents)) ;; TODO: Should be `org-undefblock-only'
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


(deftest "`org-defblock-only' docstring is optional"
  (should (org-defblock-only shout nil (upcase contents))))

             "Speaking block"
             (format "%s says hi%s%s" who signoff (if (equal backend 'latex) "~LaTeX~" "")))
  (should (fboundp 'org-block/speak))))

(deftest "`org-defblock-only' attaches a docstring to the generated function"
  (using (org-defblock-only speak nil "Speaking block!" nil)
    (should (equal (documentation 'org-block/speak)
                   "Speaking block!

Regarding the Lisp function:
+ BACKEND refers to the current export backend.
+ RAW-CONTENTS refers to the text as the user wrote it verbatim.
  ⇒ You may mention CONTENTS to refer to the ‘org parsed’ version of user text.
  ⇒ CONTENTS and RAW-CONTENTS are identical whenever key CONTENTS-OCCUR-AS-LINK-DESCRIPTION is non-nil.

For example, upon LaTeX export, the Org special block

     #+begin_speak
     Hello, world
     #+end_speak

 is rewritten to the result of the call

     (org-block/speak ‘latex \"Hello, world\")"))))

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
            (lambda (example _backend) (format "\n\t%s ⇒ %s" example (eval (car (read-from-string example))))))
           (lf-string "The method `apply' can be used in multiple ways.
                       
                       For example to sum a list:
                       
                       	(apply #'+ nil) ⇒ 0
                       
                       Or, to sum a list with some starting numbers:
                       
                       	(apply #'+ 1 2 '(3 4)) ⇒ 10
                       
                       Enjoy!"))))

(deftest "`org--replace-examples-in-string' can read an “:exporting-to” key"
  (using (org-defblock shout () (upcase contents))
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
              (lambda (example backend) (format "%s\n ⭆ %s" example (org-export-string example backend))))
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
  (using (org-defblock emoji-greet (pleasantry "" to nil)
           "Greet someone, with a flair of emojis.

#+begin_example :exporting-to latex
Sometimes we want to greet someone with many emojis, that's a job for the “emoji-greet” block.
#+begin_emoji-greet \"Why, hello there\" :to \"The love of my life\"
Have you noticed how blessed we are?
#+end_emoji-greet
#+end_example
"
           (format "🗣️ %s \n👀 %s \n🗯️ %s 👋" pleasantry to raw-contents))
    
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
  (using (org-defblock emoji-greet (pleasantry "" to nil)
           "Greet someone, with a flair of emojis.

#+begin_example :exporting-to latex
Sometimes we want to greet someone with many emojis, that's a job for the “emoji-greet” block.
#+begin_emoji-greet \"Why, hello there\" :to \"The love of my life\"
Have you noticed how blessed we are?
#+end_emoji-greet
#+end_example
"
           (format "🗣️ %s \n👀 %s \n🗯️ %s 👋" pleasantry to raw-contents))
    
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
+ RAW-CONTENTS refers to the text as the user wrote it verbatim.
  ⇒ You may mention CONTENTS to refer to the ‘org parsed’ version of user text.
  ⇒ CONTENTS and RAW-CONTENTS are identical whenever key CONTENTS-OCCUR-AS-LINK-DESCRIPTION is non-nil.

For example, upon LaTeX export, the Org special block

     #+begin_emoji-greet
     Hello, world
     #+end_emoji-greet

 is rewritten to the result of the call

     (org-block/emoji-greet ‘latex \"Hello, world\")"))))

;;; Test utilities “using” and “exporting”

(defmacro using (block-defs &rest body)
  "Evaluate BLOCK-DEFS (an org-defblock form or list of them), run BODY, then undo via org-undefblock.

BLOCK-DEFS may be either:
  - a single form:   (org-defblock name kwds &optional link-display docstring &rest body)
  - a list of forms: ((org-defblock …) (org-defblock …) …)

For each (org-defblock NAME …), we generate the matching (org-undefblock NAME …)
and call those in cleanup so both block and link support are removed.

Each form is evaluated; any functions it introduces are unbound
afterwards to avoid polluting the global namespace.
(If we use top-level org-defblock forms instead, we can have unexpected
calls when the same name is used for different blocks in different tests! 🤮)

This is a poor-man's `cl-letf'."
  (declare (indent 1))
  (let* ((defs (pcase block-defs
                 (`(org-defblock . ,_) (list block-defs))
                 (`(org-defblock-only . ,_) (list block-defs))
                 ((and (pred listp) fs)
                  (progn
                    (dolist (f fs)
                      (cl-assert (and (consp f) (or (eq (car f) 'org-defblock) (eq (car f) 'org-defblock-only)))
                                 nil "using: each element must be an (org-defblock …) form"))
                    fs))
                 (_ (error "using: must be an (org-defblock[-only] …) form or a list of them"))))
         ;; Build matching (org-undefblock NAME …) forms by swapping the head symbol.
         (undefs (mapcar (lambda (f) (cons 'org-undefblock (cdr f))) defs)))
    `(let ((__defs ',defs)
           (__undefs ',undefs))
       (unwind-protect
           (progn
             ;; Evaluate all definitions at runtime (not at macro-expansion).
             (dolist (f __defs) (eval f))
             ,@body)
         ;; Best-effort cleanup; don't error if an undef fails.
         (dolist (u __undefs)
           (ignore-errors (eval u)))))))


(cl-defmacro exporting (string &key (to 'html) using equals modulo)
  "Assert that exporting STRING to backend TO equals EQUALS (optionally modulo MODULO).

If EQUALS is omitted, this generates the expectations only.
This is useful in combination with `C-u C-x C-e'.

Tldr: Run an org-export-string assertion, optionally installing temporary defblocks via :USING and cleaning them up.
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
+ (exporting A :equals B)            ≋  (should (equal (org-export-string A) B))
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
           (`(org-defblock-only . ,_) (list using))
           ((and (pred listp) forms)
            (progn
              (dolist (f forms)
                (cl-assert (and (consp f) (eq (car f) 'org-defblock))
                           nil ":using list must contain only (org-defblock[-only] …) forms"))
              forms))
           (_ (error ":using must be a single (org-defblock[-only] …) or a list of them")))))
    ;; Build the runtime assertion form
    (let* ((actual   `(org-export-string (lf-string ,string) ',to))
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
      ;; wrap assertion in “using” if needed
      (if using-forms
          `(using ,using-forms
             ,assertion)
        assertion))))


(deftest "`exporting' works as intended"
  ;; Basic usage
  (exporting "A B C D" :to ascii :equals "A P C T" :modulo ("P" "T"))

  ;; No pollution of global namespace
  ;; (should-not (fboundp 'org-block/shouting))
  ;; (should-not (fboundp 'org-link/shouting))
  (exporting "shouting:hello" :to ascii :using (org-defblock shouting (wat) "docs" (upcase wat)) :equals "HELLO\n")
  (should-not (fboundp 'org-block/shouting))
  (should-not (fboundp 'org-link/shouting))

  ;; :using may be omitted
  (exporting "shouting:hello" :to ascii :equals "<shouting:hello>\n")

  ;; :using may be a (singleton) list
  (exporting "shouting:hello"
    :to ascii
    :using ((org-defblock shouting (wat) "docs" (upcase wat)))
    :equals "HELLO\n")
  
  ;; :using may be a multi-element list
  (exporting "shout:hello quiet:WORLD"
    :to ascii
    :using ((org-defblock shout (wat) "docs" (upcase wat))
            (org-defblock quiet (wat) "docs" (downcase wat)))
    :equals "HELLO world\n"))

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
  (using (org-defblock hello (who "world" punct "!") "Greeter block"
                (format "Hello, %s%s" who punct))
  (should (fboundp 'org-block/hello))
  (should (string= (org-block/hello 'test-backend "ignored" "Emacs" :punct "!!")
                   (lf-string "#+begin_export test-backend 
                               Hello, Emacs!!
                               #+end_export")))))

(deftest "`org-defblock' uses default values set by `org-set-block-header-args'"
  (using (org-defblock greeting (name "user" punct "!") "Greeting block"
                (format "Hello, %s%s" name punct))
  (org-set-block-header-args greeting :main-arg "dev" :punct "!×4")
  (should (string= (org-block/greeting 'test-backend "some content" nil :punct nil)
                   (lf-string "#+begin_export test-backend 
                               Hello, dev!×4
                               #+end_export")))))

(deftest "`org-defblock' defines associated link functions that evaluate correctly"
  (using (org-defblock notice () [:face 'italic] "Example."
                (format "NOTICE: %s" contents))
  (should (fboundp 'org-block/notice))
  (should (fboundp 'org-link/notice))
  (should (equal (org-link/notice "Some note here" nil 'test-backend)
                 "NOTICE: Some note here"))))

(deftest "`org-defblock' exports a custom block to HTML with content formatting"
  (exporting "Look: 
              #+begin_highlight Warning :style color:orange
              Something **important** here.
              #+end_highlight"
             :using (org-defblock highlight (label "Note" style "color:red") "Highlight block"
                      (format "<div style='%s'><strong>%s:</strong> %s</div>" style label contents))
             :equals  "<p>
                      Look: 
                      </p>
                      <div style='color:orange'><strong>Warning:</strong> 
                      <p>
                      Something <b><b>important</b></b> here.
                      </p>
                      </div>
                      "))
                        

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
  (⇝ (org-export-string "shout:hello")
     "<p> <span style=\"color:red\"> HELLO </span></p>"))

(deftest "org-deflink works as expected, bracket links"
  [org-deflink]
  (⇝ (org-export-string "[[shout:hello]]")
     "<p> <span style=\"color:red\"> HELLO </span></p>")
  (⇝ (org-export-string "[[shout:hello][world!]]")
     "<p> <span style=\"color:red\"> WORLD! </span></p>"))

(deftest "org-deflink works as expected, angle links"
         [org-deflink]
         (⇝ (org-export-string "<shout: hello world!>")
     "<p> <span style=\"color:red\"> HELLO WORLD! </span></p>"))
;; Define links as you define functions: doc:org-deflink:4 ends here

(org-defblock scream
  (speaker "Default_Speaker")
  [:face '(:foreground "green" :weight bold)]
  "Capitalise the contents! Seen in red bold in Emacs!"
  (format "%s: %s" speaker (upcase contents)))

(deftest "Upcase works as expected on links, with only labels"
    [basic-defblock org-link]
         (⇝ (org-export-string "pre scream:hello post")
            "pre hello: HELLO post"))

(deftest "Upcase works as expected on links, with descriptions"
         [basic-defblock org-link]
         (⇝ (org-export-string "pre [[scream:hello][my dear friends]] post")
            "hello: MY DEAR FRIENDS post"))

(deftest "Upcase works as expected on blocks"
         [basic-defblock]
         (⇝ (org-export-string "pre
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
         (⇝ (org-export-string "pre
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
  (⇝ (org-export-string "kbd:C-u_80_-∀") "<p>\n<kbd style=\"\">C-u 80</kbd>_-∀</p>"))

(deftest "[[It]] becomes <kbd> tags"
  [kbd square-org-links]
  (⇝ (org-export-string "[[kbd:C-u_80_-]]") "<p>\n<kbd style=\"\">C-u 80 -</kbd></p>"))

(deftest "<It> becomes <kbd> tags, and surrounding space is trimmed"
  [kbd angle-org-links]
  (⇝ (org-export-string "<kbd: C-u 80 - >")  "<p>\n<kbd style=\"\">C-u 80 -</kbd></p>"))

;; FIXME: uh-oh!
(when nil
(deftest "It has a tooltip documenting the underlying Lisp function, when possible"
  [kbd tooltip]
  (⇝ (org-export-string "<kbd: M-s h .>")

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
