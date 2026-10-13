;;; org-defblock-only-tests.el --- `org-defblock-only' ---terse syntax

(unless (fboundp 'using)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

;;; org-defblock-only -- terse syntax
;; (ert '(tag "org-defblock-only"))

(deftest "`org-defblock-only' docstring is optional"
  (should (org-defblock-only shout nil (upcase contents))))

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
             (format "<div style='%s'><strong>%s:</strong>%s</div>" style label contents))
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
             (format "<div style='%s'><strong>%s:</strong>%s</div>" style label contents))
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
  :expected-result :failed
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
