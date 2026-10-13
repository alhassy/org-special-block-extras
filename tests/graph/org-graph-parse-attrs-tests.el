;;; org-graph-parse-attrs-tests.el --- org--graph-parse-attrs  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; org--graph-parse-attrs

(deftest "`org--graph-parse-attrs' returns empty string on nil"
  (should (equal "" (org--graph-parse-attrs nil))))

(deftest "`org--graph-parse-attrs' parses unquoted key=value pairs"
  (should (equal "style=dashed color=red"
                 (org--graph-parse-attrs "style=dashed color=red"))))

(deftest "`org--graph-parse-attrs' parses quoted values containing spaces"
  (should (equal "label=\"hello world\""
                 (org--graph-parse-attrs "label=\"hello world\""))))

(deftest "`org--graph-parse-attrs' handles mixed quoted and unquoted pairs"
  (should (equal "label=\"foo bar\" style=dashed penwidth=2"
                 (org--graph-parse-attrs "label=\"foo bar\" style=dashed penwidth=2"))))
