;;; org-graph-table-context-tests.el --- org-graph--table-context  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; org-graph--table-context

(deftest "`org-graph--table-context' returns `nodes' for a list-item table"
  (let ((buf (tests-graph--make-graph-buffer
              "
+ mynode
  | shape | ellipse |")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "| shape |")
      (should (eq 'nodes (org-graph--table-context))))
    (kill-buffer buf)))

(deftest "`org-graph--table-context' returns `graphs' for a #+graph: table"
  (let ((buf (tests-graph--make-graph-buffer
              "
#+graph:
| rankdir | TB |")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "| rankdir |")
      (should (eq 'graphs (org-graph--table-context))))
    (kill-buffer buf)))

(deftest "`org-graph--table-context' returns `nodes' for a #+node: table"
  (let ((buf (tests-graph--make-graph-buffer
              "
#+node:
| shape | box |")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "| shape |")
      (should (eq 'nodes (org-graph--table-context))))
    (kill-buffer buf)))

(deftest "`org-graph--table-context' returns `edges' for a #+edge: table"
  (let ((buf (tests-graph--make-graph-buffer
              "
#+edge:
| color | red |")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "| color |")
      (should (eq 'edges (org-graph--table-context))))
    (kill-buffer buf)))

(deftest "`org-graph--table-context' skips separator rows correctly"
  (let ((buf (tests-graph--make-graph-buffer
              "
#+graph:
|------+----|
| rankdir | LR |")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "| rankdir |")
      (should (eq 'graphs (org-graph--table-context))))
    (kill-buffer buf)))

(deftest "`org-graph--table-context' reaches past earlier tables to the directive owning them all; a #+name: table is a node template"
  (let ((buf (tests-graph--make-graph-buffer
              "
#+edge:
| color | red |

| style | dotted |
#+name: wife
| shape | box |")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "| style |")
      (should (eq 'edges (org-graph--table-context)))
      (search-forward "| shape |")
      (should (eq 'nodes (org-graph--table-context))))
    (kill-buffer buf)))
