(require 'ert)
(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'dash)

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


;;; defblock

(ert-deftest org-defblock/handler-definition ()
  "Test that a block handler is defined via `org-defblock' and evaluates correctly."
  ;; Define a simple block
  (org-defblock hello (who "world" punct "!") "Greeter block"
                (format "Hello, %s%s" who punct))
  (should (fboundp 'org-block/hello))
  (should (string= (org-block/hello 'test-backend "ignored" "Emacs" :punct "!!")
                   (lf-string "#+begin_export test-backend 
                               Hello, Emacs!!
                               #+end_export"))))


(ert-deftest org-defblock/default-argument-values ()
  "Test that default argument values work with `defblock-header-args'."
  (org-defblock greeting1 (name "user" punct "!") "Greeting block"
                (format "Hello, %s%s" name punct))
  ;; Set defaults
  (org-set-block-header-args greeting1 :main-arg "dev" :punct "!×4")
  ;; Simulate calling with nil main arg and nil keyword arg
  (should (string= (org-block/greeting1 'test-backend "some content" nil :punct nil)
                   (lf-string "#+begin_export test-backend 
                               Hello, dev!×4
                               #+end_export"))))


(ert-deftest org-defblock/link-handling ()
  "Test that a link associated with an `org-defblock' is defined and formats correctly."
  (org-defblock notice () [:face 'italic] "Example."
                (format "NOTICE: %s" contents))
  (should (fboundp 'org-block/notice))
  (should (fboundp 'org-link/notice))
  ;; Simulate the link function evaluation
  ;; (org-link/notice O-LABEL O-DESCRIPTION O-BACKEND)
  (should (equal (org-link/notice  "Some note here" nil 'test-backend)
                 "NOTICE: Some note here")))


(cl-defun export (input &optional (backend 'html))
  "Export Org INPUT along BACKEND."
  (org-export-string-as input backend :body-only))


(ert-deftest org-defblock/export-html ()
  "Test that `org-defblock' handlers export to HTML correctly."
  
  ;; Define the block
  (org-defblock highlight (label "Note" style "color:red") "Highlight block"
                (format "<div style='%s'><strong>%s:</strong> %s</div>" style label contents))

  ;; Setup test buffer with an org-mode block
  (should (equal (export "Look: \n #+begin_highlight Warning :style color:orange\nSomething **important** here.\n#+end_highlight")
                 ;; FIXME: Where's the initial text “Look:” ?
                 "<div style='color:orange'><strong>Warning:</strong> 
<p>
Something <b><b>important</b></b> here.
</p>
</div>
")))



(ert-deftest org--create-defmethod/handler-creation ()
  "Test that a method is created from `org--create-defmethod-of-defblock'."
  ;; Because this function returns code, we eval the result in tests to observe behaviour.
  (should (equal (eval (org--create-defmethod-of-defblock
                        'greet0                                   ;; name
                        "Greet0ing block"                         ;; docstring
                        'html                                    ;; backend 
                        '(who "dev" signoff "!")                 ;; args list
                        '((format "%s says hi%s" who signoff)))) ;; body
                 'org-block/greet0))
  (should (fboundp 'org-block/greet0))  
  ;; Docs exist
  (should (equal (plist-get  (symbol-plist 'org-block/greet0) 'function-documentation)
                 "Greet0ing block"))
  ;; Basic usage
  (should (string= (org-block/greet0 'html "ignored contents" "Ada" :signoff ", cheerio!")
                   (lf-string "#+begin_export html 
                               Ada says hi, cheerio!
                               #+end_export")))
  ;; Default values are honoured
  (should (string= (org-block/greet0 'html "ignored contents" "")
                   (lf-string "#+begin_export html 
                               dev says hi!
                               #+end_export")))
  ;; Extra args are ignored
  (should (string= (org-block/greet0 'html "" "" "" 'extra 'args :are 'ignored)
                   (lf-string "#+begin_export html 
                               dev says hi!
                               #+end_export")))
  ;; Test that header arg defaults override blank block arguments.
  (let ((org--header-args '((greet0 . (:main-arg "Mickey" :signoff ", buddo!")))))
    (should (string= (org-block/greet0 'html "contents" "" :signoff "")
                     (lf-string "#+begin_export html 
                               Mickey says hi, buddo!
                               #+end_export"))))
  ;; Ensure method only works for specified backend via `(eql BACKEND)`.
  ;; We already tested it works for 'html, but it should error for 'latex since it's not defined
  ;; for it.
  (should (org-block/greet0 'html "" ""))
  (should-error (org-block/greet0 'latex "" "" ))
  ;; Let's define it for another backend; the definitions are independent.
  (eval (org--create-defmethod-of-defblock
         'greet0
         "A tooltip doc"
         'test-backend
         '(who "me" signoff "!")
         '((format "%s says hi%s ⟨TEST⟩" who signoff))))
  (should (string-match "Ada says hi, cheerio!" (org-block/greet0 'html "ignored contents" "Ada" :signoff ", cheerio!")))
  (should (string-match "Ada says hi, cheerio! ⟨TEST⟩" (org-block/greet0 'test-backend "ignored contents" "Ada" :signoff ", cheerio!")))
  (should (string-match "me says hi ⟨TEST⟩" (org-block/greet0 'test-backend "ignored contents" "" :signoff ""))))

;;;; Old tests

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
