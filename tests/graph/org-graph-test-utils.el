;;; org-graph-test-utils.el --- Shared machinery for tests/graph/*-tests.el: `deftest' and the `graph' relation  -*- lexical-binding: t; -*-

;;; Requires

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 's)
(require 'dash)

;; For `deftest', `org-deftest', and snap; it loads org-special-block-extras too.
(unless (fboundp 'org-deftest)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))
(load-file (expand-file-name "~/org-special-block-extras/lisp/org-graph-block.el"))

;; Tests stub primitives such as `call-process' via `cl-letf'; under native-comp, that compiles a
;; trampoline per primitive, which fails in batch.  Our code loads as source, so stubs work without them.
(setq native-comp-enable-subr-trampolines nil)

;;; Test fixtures

;; Relation: Org graph syntax → dot output
;; :input  is the raw block body (string)
;; :output is the expected dot source (string)
;; C-u C-x C-e on any `define-graph-test' call auto-fills :output from actual.
(snap-define-relation graph (input output)
  "Verify that INPUT (a #+begin_graph block body) produces OUTPUT (dot source), both filled by `org-test--fill'."
  (let ((actual (org-test--fill (org--graph-raw->dot input nil))))
    (should (equal (org-test--fill output) actual))
    (list :output actual)))

(provide 'org-graph-test-utils)
