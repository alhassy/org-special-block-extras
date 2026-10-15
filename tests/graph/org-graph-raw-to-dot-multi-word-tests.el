;;; org-graph-raw-to-dot-multi-word-tests.el --- org--graph-raw->dot — multi-word nodes and Org comments  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; org--graph-raw->dot — multi-word nodes and Org comments

(deftest "`org--graph-raw->dot' auto-quotes multi-word node names"
  (let ((dot (org--graph-raw->dot "
+ Org mode" nil)))
    (should (s-contains-p "\"Org mode\"" dot))))

(deftest "`org--graph-raw->dot' auto-quotes on both ends of an edge"
  (let ((dot (org--graph-raw->dot "
+ Org mode
  + Org agenda" nil)))
    (should (s-contains-p "\"Org mode\" -> \"Org agenda\"" dot))))

(deftest "`org--graph-raw->dot' ignores bare Org comment lines"
  (let ((dot (org--graph-raw->dot
              "
+ A
  # this is a comment
  + B"
              nil)))
    (should (s-contains-p "A -> B" dot))))

(deftest "`org--graph-raw->dot' comment before table row does not drop the table"
  ;; A # comment between a list item and its inline table must not lose the table
  (let ((dot (org--graph-raw->dot
              "
+ A
  + B
    # a comment
    | :edge.label | calls |"
              nil)))
    (should (s-contains-p "label=calls" dot))))

(deftest "`org--graph-raw->dot' mixed comments and nodes produce correct graph"
  (let ((dot (org--graph-raw->dot
              "
+ Emacs
  + \"Org mode\"
    # comment
    + \"Org agenda\"
      # another comment
      | :edge.label | \"hola\" |"
              nil)))
    (should (s-contains-p "\"Org mode\" -> \"Org agenda\"" dot))
    (should (s-contains-p "label=\"hola\"" dot))))
