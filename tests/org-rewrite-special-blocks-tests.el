;;; org-rewrite-special-blocks-tests.el --- `org--rewrite-special-blocks-by-handlers'

(unless (fboundp 'using)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

;;; org--rewrite-special-blocks-by-handlers

(org-deftest "`org--rewrite-special-blocks-by-handlers' transforms supported blocks but leaves others unchanged"
  :text (lf-string "\t#+begin_foo mainarg :x 1 :y 2
                    This is foo block content.
                    #+end_foo

                   However, the next is left alone:
                   #+begin_foobar mainarg :x 1 :y 2
                   This is foobar block content.
                   #+end_foobar
                   ")
  ;; Only “foo” is supported, and its handler is a stub; the real blocks and backend stay untouched.
  (let ((org--supported-blocks '("foo"))
        (org--current-backend nil))
    (cl-letf (((symbol-function 'org-block/foo)
               (lambda (backend contents arg &rest args)
                 (format "FOO block (%s): %s [arg: %s] [args: %s]" backend contents arg args))))
      (org--rewrite-special-blocks-by-handlers 'test-backend)
      (should (equal (buffer-string)
                     (lf-string "\tFOO block (test-backend): This is foo block content. [arg: mainarg] [args: (:x 1 :y 2)]

                   However, the next is left alone:
                   #+begin_foobar mainarg :x 1 :y 2
                   This is foobar block content.
                   #+end_foobar
                   "))))))
