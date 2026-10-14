;;; org-rewrite-special-blocks-tests.el --- `org--rewrite-special-blocks-by-handlers'

(unless (fboundp 'using)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

;;; org--rewrite-special-blocks-by-handlers

(deftest "`org--rewrite-special-blocks-by-handlers' transforms supported blocks but leaves others unchanged"
  ;; Setup supported blocks and mock global backend, without clobbering the real ones
  (let ((org--supported-blocks '("foo"))
        (org--current-backend nil))
    (with-temp-buffer
      (org-mode)

      ;; Dummy handler for "foo" blocks
      (defun org-block/foo (backend contents arg &rest args)
        (format "FOO block (%s): %s [arg: %s] [args: %s]" backend contents arg args))

      ;; Ensure all supported blocks have handlers
      (should (--all-p (functionp (intern (format "org-block/%s" it))) org--supported-blocks))

      ;; Insert Org content with both supported and unsupported blocks
      (insert
       (lf-string "\t#+begin_foo mainarg :x 1 :y 2
                   This is foo block content.
                   #+end_foo

                  However, the next is left alone:
                  #+begin_foobar mainarg :x 1 :y 2
                  This is foobar block content.
                  #+end_foobar
                  "))

      ;; Run the transformer
      (goto-char (point-min))
      (org--rewrite-special-blocks-by-handlers 'test-backend)

      ;; Assert transformation
      (should (equal (buffer-string)
                     (lf-string "\tFOO block (test-backend): This is foo block content. [arg: mainarg] [args: (:x 1 :y 2)]

                  However, the next is left alone:
                  #+begin_foobar mainarg :x 1 :y 2
                  This is foobar block content.
                  #+end_foobar
                  "))))))
