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
