;;; org-graph-parse-table-tests.el --- org--graph-parse-table  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; org--graph-parse-table

(deftest "`org--graph-parse-table' returns nil on nil input"
  (should (null (org--graph-parse-table nil))))

(deftest "`org--graph-parse-table' returns nil on blank input"
  (should (null (org--graph-parse-table "   "))))

(deftest "`org--graph-parse-table' parses a basic 2-col table"
  (should (equal '(("shape" "ellipse") ("color" "red"))
                 (org--graph-parse-table "| shape | ellipse |\n| color | red |"))))

(deftest "`org--graph-parse-table' skips separator rows"
  (should (equal '(("key" "value") ("foo" "bar"))
                 (org--graph-parse-table "| key | value |\n|-----+-------|\n| foo | bar |"))))

(deftest "`org--graph-parse-table' parses a 3-col row"
  (should (equal '(("to" "mynode" "label=hello style=dashed"))
                 (org--graph-parse-table "| to | mynode | label=hello style=dashed |"))))
