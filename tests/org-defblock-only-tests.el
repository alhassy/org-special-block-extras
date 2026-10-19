;;; org-defblock-only-tests.el --- `org-defblock-only' ---terse syntax

(unless (fboundp 'using)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

;;; org-defblock-only -- terse syntax
;; (ert '(tag "org-defblock-only"))

(deftest "`org-defblock-only' docstring is optional"
  (should (org-defblock-only shout (&context contents) (upcase contents))))

(deftest "`org-defblock-only' returns the name of the defined function"
  (should (equal
           (org-defblock-only speak (who "dev" signoff "!" &context backend)
             "Speaking block"
             (format "%s says hi%s%s" who signoff (if (equal backend 'latex) "~LaTeX~" "")))
           'org-block/speak))
  ;; clean-up
  (org-undefblock speak))

(deftest "`org-defblock-only' defines a function with the correct symbol name"
  (using (org-defblock-only speak (who "dev" signoff "!" &context backend)
           "Speaking block"
           (format "%s says hi%s%s" who signoff (if (equal backend 'latex) "~LaTeX~" "")))
    (should (fboundp 'org-block/speak))))

(deftest "`org-defblock-only' attaches a docstring to the generated function"
  (using (org-defblock-only speak nil "Speaking block!" nil)
    (should (equal (documentation 'org-block/speak)
                   "Speaking block!

Regarding the Lisp function:
+ BACKEND refers to the current export backend.
+ RAW-CONTENTS refers to the text as the user wrote it, sans its least indentation.
  ⇒ Its body asks, by &context, for CONTENTS: the ‘org parsed’ version of user text.
  ⇒ CONTENTS and RAW-CONTENTS are identical for the description of a link; whence the LINK? of &context is non-nil.

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

(deftest "TODO `org-defblock-only' inconsistent with `org-defblock'" [BUG FIXME]
  ;; ⟨1⟩ `org-defblock' works as expected
  (exporting "Look:
              #+begin_aside Warning :style color:orange
              Something **important** here.
              #+end_aside"
    :using (org-defblock aside (label "Note" style "color:red" &context contents) "Aside block"
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
              #+begin_aside Warning :style color:orange
              Something **important** here.
              #+end_aside"
    :using (org-defblock-only aside (label "Note" style "color:red" &context contents) "Aside block"
             (format "<div style='%s'><strong>%s:</strong>%s</div>" style label contents))
    :equals ;; NOTE: The <div>s are different!
    "<p>
Look:
</p>
<div class=\"aside\" id=\"org-id-1\">
<p>
Something <b><b>important</b></b> here.
</p>

</div>
"
    ))


