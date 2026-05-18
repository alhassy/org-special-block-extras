;;; tests-graph.el --- Tests for org-special-block-extras-graph  -*- lexical-binding: t; -*-

;;; Requires

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 's)
(require 'dash)

(load-file (expand-file-name "~/snap/snap.el"))
(load-file "./org-special-block-extras.el")
(load-file "./org-special-block-extras-graph.el")

;;; Test fixtures

;; Identity fixture for readable ERT test names
(snap-define-fixture deftest)

;; Relation: Org graph syntax → dot output
;; :input  is the raw block body (string)
;; :output is the expected dot source (string)
;; C-u C-x C-e on any `define-graph-test' call auto-fills :output from actual.
(snap-define-relation graph (input output)
  "Verify that INPUT (a #+begin_graph block body) produces OUTPUT (dot source)."
  (let ((actual (org--graph-raw->dot input "directed")))
    (should (equal output actual))
    (list :output actual)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org--graph-extract-preamble

(deftest "`org--graph-extract-preamble' buckets #+graph: table rows"
  (let* ((p (org--graph-extract-preamble
             "#+graph:\n| rankdir | TB |\n| bgcolor | #000 |\n\n+ A\n  + B\n")))
    (should (equal '(("rankdir" "TB") ("bgcolor" "#000"))
                   (plist-get p :graph-rows)))
    (should (null (plist-get p :node-rows)))
    (should (s-contains-p "+ A" (plist-get p :list-body)))))

(deftest "`org--graph-extract-preamble' buckets #+node: and #+edge: rows"
  (let* ((p (org--graph-extract-preamble
             "#+node:\n| shape | box |\n#+edge:\n| color | red |\n+ X\n")))
    (should (equal '(("shape" "box")) (plist-get p :node-rows)))
    (should (equal '(("color" "red")) (plist-get p :edge-rows)))))

(deftest "`org--graph-extract-preamble' collects #+rank: lines"
  (let* ((p (org--graph-extract-preamble "#+rank: A B\n#+rank: C D\n+ A\n")))
    (should (equal '("A B" "C D") (plist-get p :ranks)))))

(deftest "`org--graph-extract-preamble' collects #+cluster: with label and attrs"
  (let* ((p (org--graph-extract-preamble
             "#+cluster: foo \"My Cluster\"\n| color | blue |\n+ A\n  | cluster | foo |\n"))
         (foo (cdr (assoc "foo" (plist-get p :clusters)))))
    (should foo)
    (should (equal "\"My Cluster\"" (plist-get foo :label)))
    (should (equal '(("color" "blue")) (plist-get foo :rows)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org--graph-raw->dot — snapshot relation tests

(define-graph-test "single node"
  :input "+ A\n"
  :output "digraph {
  A;
}")

(deftest "`org--graph-raw->dot' undirected graph uses -- arrows and graph keyword"
  (should (equal "graph {\n  A;\n  A -- B;\n  B;\n}"
                 (org--graph-raw->dot "+ A\n  + B\n" "undirected"))))

(define-graph-test "simple nesting A→B"
  :input "+ A\n  + B\n"
  :output "digraph {
  A;
  A -> B;
  B;
}")

(define-graph-test "A→B→C chain"
  :input "+ A\n  + B\n    + C\n"
  :output "digraph {
  A;
  A -> B;
  B;
  B -> C;
  C;
}")

(define-graph-test "EDGE row attaches label"
  :input "+ A\n  + B\n    | EDGE | label=calls |\n"
  :output "digraph {
  A;
  A -> B [ label=calls ];
  B;
}")

(define-graph-test "two EDGE rows produce two parallel arcs"
  :input "+ A\n  + B\n    | EDGE | label=first |\n    | EDGE | label=second |\n"
  :output "digraph {
  A;
  A -> B [ label=first ];
  A -> B [ label=second ];
  B;
}")

(define-graph-test "id row makes cross-reference alias"
  :input "+ lambda\n  | id | lam |\n  + church\n    | EDGE | label=encodes |\n"
  :output "digraph {
  lam;
  lam -> church [ label=encodes ];
  church;
}")

(define-graph-test "second occurrence emits edge only, not a second node decl"
  :input "+ A\n  + B\n+ C\n  + B\n    | EDGE | label=again |\n"
  :output "digraph {
  A;
  A -> B;
  B;
  C;
  C -> B [ label=again ];
}")

(deftest "`org--graph-raw->dot' explicit to row produces cross-edge"
  (should (equal "digraph {\n  a;\n  B -> a [ label=back ];\n  B;\n}"
                 (org--graph-raw->dot
                  (concat "+ A\n  | id | a |\n"
                          "+ B\n  | to | a | label=back |\n")
                  "directed"))))

(define-graph-test "#+rank: emits rank=same block"
  :input "#+rank: A B\n+ A\n+ B\n"
  :output "digraph {
  { rank=same; A; B }
  A;
  B;
}")

(define-graph-test "#+graph: table sets graph-level attrs"
  :input "#+graph:\n| rankdir | TB |\n+ A\n"
  :output "digraph {
  graph [ rankdir=TB ]
  A;
}")

(define-graph-test "#+node: table sets default node attrs"
  :input "#+node:\n| shape | box |\n+ A\n"
  :output "digraph {
  node [ shape=box ]
  A;
}")

(define-graph-test "node attrs styled inline"
  :input "+ A\n  | shape | ellipse |\n  | color | red |\n"
  :output "digraph {
  A [ shape=ellipse color=red ];
}")

(deftest "`org--graph-raw->dot' #+cluster: emits subgraph cluster block"
  (should (equal "digraph {\n  subgraph cluster_grp {\n    label=\"My Group\"\n    style=dashed\n    A\n  }\n  A;\n}"
                 (org--graph-raw->dot
                  (concat "#+cluster: grp \"My Group\"\n| style | dashed |\n"
                          "+ A\n  | cluster | grp |\n")
                  "directed"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTML export

(deftest "`#+begin_graph' HTML export produces an <img> tag"
  ;; org-export-string is provided by org-special-block-extras
  (let ((html (org-export-string
               "#+begin_graph directed\n+ A\n  + B\n    | EDGE | label=calls |\n#+end_graph\n"
               'html)))
    (should (s-contains-p "<img" html))
    (should (s-contains-p "base64" html))))

(deftest "`#+begin_graph' HTML export: node and edge appear in underlying dot"
  ;; Verify the dot source that gets embedded is structurally correct
  ;; by checking the raw->dot output directly.
  (let ((dot (org--graph-raw->dot
              "+ A\n  + B\n    | EDGE | label=calls |\n"
              "directed")))
    (should (s-contains-p "A -> B [ label=calls ];" dot))
    (should (s-contains-p "digraph" dot))))
