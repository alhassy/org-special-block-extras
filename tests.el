
(require 'ert)
(require 'org)

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
      (should (equal contents "block content\n")))))


(ert-deftest org--support-special-blocks-with-args/foo-block-test ()
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
    (org--support-special-blocks-with-args 'test-backend)
    (should (equal (s-trim (buffer-string))
                   "FOO block (test-backend): This is foo block content.
 [arg: mainarg] [args: (:x 1 :y 2)]	

                  However, the next is left alone:
                  #+begin_foobar mainarg :x 1 :y 2
                  This is foobar block content.
                  #+end_foobar"))))

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
