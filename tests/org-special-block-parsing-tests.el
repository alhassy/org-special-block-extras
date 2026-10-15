;;; org-special-block-parsing-tests.el --- Parsing & evaluating `org-special-block' structures

(unless (fboundp 'using)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

;;; Parsing & evaluating org-special-block structures

(define-org-block-parse-test "`org-special-block-after-point' parses a block's name, main argument, keyword arguments, and contents"
  :input "#+begin_foo mainarg :x 1 :y 2
          block content
          #+end_foo"
  :block "foo"
  :parses-to (:name "foo" :main-arg "mainarg" :kwdargs (:x 1 :y 2) :contents "block content"))

(define-org-block-parse-test "`org-special-block-after-point' parses the first block after point, whatever its name"
  :input "#+begin_bar \"The main arg\" :key₁ value₁ :key₂ value₂
          some content here,

          possibly empty
          #+end_bar"
  :parses-to (:name "bar" :main-arg "The main arg" :kwdargs (:key₁ value₁ :key₂ value₂)
              :contents "some content here,\n\npossibly empty"))

(define-org-block-parse-test "`org-special-block-after-point' parses a block with a main argument but no keyword arguments, nor contents"
  :input "#+begin_bar \"The main arg\"
          #+end_bar"
  :parses-to (:name "bar" :main-arg "The main arg" :kwdargs nil :contents ""))

(define-org-block-parse-test "`org-special-block-after-point' parses a block with keyword arguments but no main argument, nor contents"
  :input "#+begin_shout :to me :and you
          #+end_shout"
  :parses-to (:name "shout" :main-arg "" :kwdargs (:to me :and you) :contents ""))

(define-org-block-parse-test "`org-special-block-after-point' parses a block with keyword arguments and contents, but no main argument"
  :input "#+begin_shout :to me :and you
          world
          #+end_shout"
  :parses-to (:name "shout" :main-arg "" :kwdargs (:to me :and you) :contents "world"))

(define-org-block-parse-test "`org-special-block-after-point' parses a block with contents, but no arguments"
  :input "#+begin_shout
          world
          #+end_shout"
  :parses-to (:name "shout" :main-arg "" :kwdargs nil :contents "world"))

(define-org-block-parse-test "`org-special-block-after-point' parses a block having no contents nor arguments"
  :input "#+begin_baz
          #+end_baz"
  :parses-to (:name "baz" :main-arg "" :kwdargs nil :contents ""))

(define-org-block-parse-test "`org-special-block-after-point' skips a #+begin_NAME that Org reads as text, such as inside a src block"
  :input "#+begin_src org
          #+begin_foo shown literally
          #+end_foo
          #+end_src
          #+begin_foo real
          #+end_foo"
  :block "foo"
  :parses-to (:name "foo" :main-arg "real" :kwdargs nil :contents ""))


(org-deftest "`org-eval-replace-block' replaces a block with handler output"
  :text (lf-string "Start
                    #+begin_foo mainarg :x 1
                    Body text
                    #+end_foo
                    End")
  :at "#+begin_foo"
  (cl-letf (((symbol-function 'org-block/foo)
             (lambda (backend contents arg &rest args)
               (format "REPLACED: %s | %s | %s | %s" backend contents arg args))))
    ;; Identify the block boundaries, manually
    (let* ((start (line-beginning-position))
           (end (progn (search-forward "#+end_foo") (line-end-position)))
           (blk (make-org-special-block
                 :name "foo"
                 :start-point start
                 :end-point end
                 :main-arg "\"mainarg\""
                 :kwdargs '(:x 1)
                 :contents "Body text")))
      (org-eval-replace-block blk 'test-backend)
      (should (equal (buffer-string)
                     (lf-string "Start
                                 REPLACED: test-backend | Body text | \"mainarg\" | (:x 1)
                                 End"))))))


(org-deftest "`org-eval-replace-block' handles special block within numbered list"
  :text (lf-string "
                    Welcome class, today we will discuss the following:
                    0. Why are we here?
                    1. Why we do what we do?
                       #+begin_foo mainarg :x 1
                       Body text
                       #+end_foo
                       Or not do.
                    2. Why we ask ‘why’?

                    Take care!")
  (cl-letf (((symbol-function 'org-block/foo)
             (lambda (backend contents arg &rest args)
               (format "+ Args: %s %s\n+ Contents: %s" arg args (s-trim contents)))))
    (-let* ((block (org-special-block-after-point))
            ((&org-special-block 'name 'start-point 'end-point) block))
      (should (equal name "foo"))
      (should (equal start-point 102))
      (should (equal end-point 152))
      (org-eval-replace-block block 'test-backend)
      (should (equal (buffer-string)
                     (lf-string "
                      Welcome class, today we will discuss the following:
                      0. Why are we here?
                      1. Why we do what we do?
                         + Args: mainarg (:x 1)
                         + Contents: Body text
                         Or not do.
                      2. Why we ask ‘why’?

                      Take care!"))))))
