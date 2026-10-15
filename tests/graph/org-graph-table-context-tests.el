;;; org-graph-table-context-tests.el --- org-graph--table-context  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; org-graph--table-context

(org-deftest "`org-graph--table-context' returns `nodes' for a list-item table"
  :text "
#+begin_graph
+ mynode
  | shape | ellipse |
#+end_graph"
  :at "| shape |"
  (should (eq 'nodes (org-graph--table-context))))

(org-deftest "`org-graph--table-context' returns `graphs' for a #+graph: table"
  :text "
#+begin_graph
#+graph:
| rankdir | TB |
#+end_graph"
  :at "| rankdir |"
  (should (eq 'graphs (org-graph--table-context))))

(org-deftest "`org-graph--table-context' returns `nodes' for a #+node: table"
  :text "
#+begin_graph
#+node:
| shape | box |
#+end_graph"
  :at "| shape |"
  (should (eq 'nodes (org-graph--table-context))))

(org-deftest "`org-graph--table-context' returns `edges' for a #+edge: table"
  :text "
#+begin_graph
#+edge:
| color | red |
#+end_graph"
  :at "| color |"
  (should (eq 'edges (org-graph--table-context))))

(org-deftest "`org-graph--table-context' skips separator rows correctly"
  :text "
#+begin_graph
#+graph:
|------+----|
| rankdir | LR |
#+end_graph"
  :at "| rankdir |"
  (should (eq 'graphs (org-graph--table-context))))

(org-deftest "`org-graph--table-context' reaches past earlier tables to the directive owning them all; a #+name: table is a node template"
  :text "
#+begin_graph
#+edge:
| color | red |

| style | dotted |
#+name: wife
| shape | box |
#+end_graph"
  :at "| style |"
  (should (eq 'edges (org-graph--table-context)))
  (search-forward "| shape |")
  (should (eq 'nodes (org-graph--table-context))))
