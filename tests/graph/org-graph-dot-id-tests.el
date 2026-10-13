;;; org-graph-dot-id-tests.el --- org--graph-dot-id  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; org--graph-dot-id

(deftest "`org--graph-dot-id' leaves plain identifiers unchanged"
  (should (equal "lambda"   (org--graph-dot-id "lambda")))
  (should (equal "node_1"   (org--graph-dot-id "node_1")))
  (should (equal "A"        (org--graph-dot-id "A"))))

(deftest "`org--graph-dot-id' leaves already-quoted strings unchanged"
  (should (equal "\"Org mode\""   (org--graph-dot-id "\"Org mode\"")))
  (should (equal "\"hello world\"" (org--graph-dot-id "\"hello world\""))))

(deftest "`org--graph-dot-id' leaves bare numbers unchanged"
  (should (equal "42"   (org--graph-dot-id "42")))
  (should (equal "-3.14" (org--graph-dot-id "-3.14"))))

(deftest "`org--graph-dot-id' wraps multi-word names in double quotes"
  (should (equal "\"Org mode\""   (org--graph-dot-id "Org mode")))
  (should (equal "\"Org agenda\"" (org--graph-dot-id "Org agenda"))))

(deftest "`org--graph-dot-id' wraps names with hyphens in double quotes"
  (should (equal "\"my-node\"" (org--graph-dot-id "my-node"))))

(deftest "`org--graph-dot-id' escapes interior quotes"
  (should (equal "\"say \\\"hi\\\"\""
                 (org--graph-dot-id "say \"hi\""))))
