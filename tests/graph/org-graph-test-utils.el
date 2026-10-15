;;; org-graph-test-utils.el --- Shared machinery for tests/graph/*-tests.el: `deftest' and the `graph' relation  -*- lexical-binding: t; -*-

;;; Requires

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 's)
(require 'dash)

(load-file (expand-file-name "~/snap/snap.el"))
(load-file (expand-file-name "~/org-special-block-extras/org-special-block-extras.el"))
(load-file (expand-file-name "~/org-special-block-extras/org-graph-block.el"))

;; Tests stub primitives such as `call-process' via `cl-letf'; under native-comp, that compiles a
;; trampoline per primitive, which fails in batch.  Our code loads as source, so stubs work without them.
(setq native-comp-enable-subr-trampolines nil)

;;; Test fixtures

;; Identity fixture for readable ERT test names
(snap-define-fixture deftest)

;; Relation: Org graph syntax → dot output
;; :input  is the raw block body (string)
;; :output is the expected dot source (string)
;; C-u C-x C-e on any `define-graph-test' call auto-fills :output from actual.
(snap-define-relation graph (input output)
  "Verify that INPUT (a #+begin_graph block body) produces OUTPUT (dot source)."
  (let ((actual (org--graph-raw->dot input nil)))
    (should (equal output actual))
    (list :output actual)))

(defun tests-graph--make-graph-buffer (content)
  "Return a temp buffer containing CONTENT wrapped in a #+begin_graph block.
Leaves point at the end of CONTENT so callers can position it as needed."
  (let ((buf (generate-new-buffer " *test-graph-context*")))
    (with-current-buffer buf
      (org-mode)
      (insert "#+begin_graph\n")
      (insert content)
      (unless (bolp) (insert "\n"))
      (insert "#+end_graph\n"))
    buf))

(provide 'org-graph-test-utils)
