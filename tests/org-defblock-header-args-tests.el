;;; org-defblock-header-args-tests.el --- Header-args and delimiters, via `org-defblock'

(unless (fboundp 'using)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

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


(define-org-export-test "main-arg may be a quoted string" [header-args]
  :input "#+begin_shout \"Hello, to the \"
          world
          #+end_shout"
  :using (org-defblock shout (greeting &context contents) "docs" (concat greeting (upcase contents)))
  :equals "Hello, to the
           <p>
           WORLD
           </p>
           ")

(define-org-export-test "main-arg may be omitted" [header-args]
  :input "#+begin_shout
          world
          #+end_shout"
  :using (org-defblock shout (greeting &context contents) "docs" (concat greeting (upcase contents)))
  :equals "
           <p>
           WORLD
           </p>
           ")


(define-org-export-test "main-arg may be omitted but keywords provided" [header-args]
  :input "
          #+begin_shout :to me
          world
          #+end_shout
          "
  :using (org-defblock shout (greeting nil to nil &context contents) "docs" (format "%s ⟶%s⟶ %s" greeting to (upcase contents)))
  :equals "nil ⟶me⟶
           <p>
           WORLD
           </p>
           ")
