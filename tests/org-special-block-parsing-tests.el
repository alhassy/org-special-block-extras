;;; org-special-block-parsing-tests.el --- Parsing & evaluating `org-special-block' structures

(unless (fboundp 'using)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

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
               (format "+ Args: %s %s\n+ Contents: %s" arg args (s-trim contents)))))
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
