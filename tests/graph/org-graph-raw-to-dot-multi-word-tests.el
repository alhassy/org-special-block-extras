;;; org-graph-raw-to-dot-multi-word-tests.el --- org--graph-raw->dot — multi-word nodes and Org comments  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; org--graph-raw->dot — multi-word nodes and Org comments

(deftest "`org--graph-raw->dot' auto-quotes multi-word node names"
  (let ((dot (org--graph-raw->dot "+ Org mode\n" "directed")))
    (should (s-contains-p "\"Org mode\"" dot))))

(deftest "`org--graph-raw->dot' auto-quotes on both ends of an edge"
  (let ((dot (org--graph-raw->dot "+ Org mode\n  + Org agenda\n" "directed")))
    (should (s-contains-p "\"Org mode\" -> \"Org agenda\"" dot))))

(deftest "`org--graph-raw->dot' ignores bare Org comment lines"
  (let ((dot (org--graph-raw->dot
              "+ A\n  # this is a comment\n  + B\n"
              "directed")))
    (should (s-contains-p "A -> B" dot))))

(deftest "`org--graph-raw->dot' comment before table row does not drop the table"
  ;; A # comment between a list item and its inline table must not lose the table
  (let ((dot (org--graph-raw->dot
              "+ A\n  + B\n    # a comment\n    | :edge.label | calls |\n"
              "directed")))
    (should (s-contains-p "label=calls" dot))))

(deftest "`org--graph-raw->dot' mixed comments and nodes produce correct graph"
  (let ((dot (org--graph-raw->dot
              (concat "+ Emacs\n"
                      "  + \"Org mode\"\n"
                      "    # comment\n"
                      "    + \"Org agenda\"\n"
                      "      # another comment\n"
                      "      | :edge.label | \"hola\" |\n")
              "directed")))
    (should (s-contains-p "\"Org mode\" -> \"Org agenda\"" dot))
    (should (s-contains-p "label=\"hola\"" dot))))
