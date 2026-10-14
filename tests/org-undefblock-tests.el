;;; org-undefblock-tests.el --- `org-defblock' and `org-undefblock'

(unless (fboundp 'using)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

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
  (using (org-defblock notice (:link [:face 'italic]) "Example."
                (format "NOTICE: %s" contents))
  (should (fboundp 'org-block/notice))
  (should (fboundp 'org-link/notice))
  (should (equal (org-link/notice "Some note here" nil 'test-backend)
                 "NOTICE: Some note here"))))

(define-org-export-test "`org-defblock' exports a custom block to HTML with content formatting"
  :input "Look:
          #+begin_highlight Warning :style color:orange
          Something **important** here.
          #+end_highlight"
  :using (org-defblock highlight (label "Note" style "color:red") "Highlight block"
           (format "<div style='%s'><strong>%s:</strong> %s</div>" style label contents))
  :equals "<p>
           Look:
           </p>
           <div style='color:orange'><strong>Warning:</strong>
           <p>
           Something <b><b>important</b></b> here.
           </p>
           </div>
           ")


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
