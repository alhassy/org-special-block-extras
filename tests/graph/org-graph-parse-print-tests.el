;;; org-graph-parse-print-tests.el --- Parsing to graph data, then printing it as dot  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; Parsing yields plain data

(deftest "`org--graph-parse' yields statements as data: arcs, then the declaration, then the children"
  (should (equal '((:node "A")
                   (:edge "A" "B" (("label" "calls")))
                   (:edge "B" "A" (("style" "dotted")))
                   (:node "B"))
                 (cadr (org--graph-parse "+ A\n  + B\n    | :edge.label | calls |\n    | :to | A |\n    | :to.style | dotted |\n")))))

(deftest "`org--graph-item-rows' routes reserved keys away from node attributes"
  (should (equal '(:id "x" :attrs (("shape" "box")) :children-rank "same"
                   :edges ((("label" "a") ("color" "red")) (("label" "b")))
                   :tos (("y" ("style" "dotted")) ("z")))
                 (org--graph-item-rows '((":id" "x") ("shape" "box") (":children-rank" "same")
                                         (":edge.label" "a") (":edge.color" "red") (":edge.label" "b")
                                         (":to" "y") (":to.style" "dotted") (":to" "z"))
                                       (make-hash-table :test #'equal)))))

;;; Reserved keys

(deftest "an unknown reserved key, or a row in the old spelling, is a user error rather than a silent attribute"
  (should-error (org--graph-raw->dot "+ A\n  | :colour | red |\n" "directed") :type 'user-error)
  (should-error (org--graph-raw->dot "+ A\n  + B\n    | EDGE | label=x |\n" "directed") :type 'user-error)
  (should-error (org--graph-raw->dot "+ A\n  | to | B |\n" "directed") :type 'user-error))

(deftest "a `:to.KEY' row with no `:to' row above it is a user error"
  (should-error (org--graph-raw->dot "+ A\n  | :to.label | x |\n" "directed") :type 'user-error))

;;; Printing quotes values, and picks the edge operator

(define-graph-test "values with spaces are quoted for us; already-quoted ones are left alone"
  :input "
+ A
  | label     | Two words   |
  | color     | \"#ff0000\" |
  | fillcolor | #00ff00     |"
  :output "digraph {
  A [ label=\"Two words\" color=\"#ff0000\" fillcolor=\"#00ff00\" ];
}")

(deftest "an undirected graph uses --, without touching any -> inside a label"
  (should (equal "graph {\n  a [ label=\"x -> y\" ];\n  a -- b;\n  b [ label=b ];\n}"
                 (org--graph-raw->dot "+ a\n  | label | x -> y |\n  + b\n" "undirected"))))

(define-graph-test "\"Org mode\" and Org mode name the same node"
  :input "
+ \"Org mode\"
+ A
  | :to | Org mode |"
  :output "digraph {
  \"Org mode\" [ label=\"Org mode\" ];
  A -> \"Org mode\";
  A [ label=A ];
}")

;;; Repeat occurrences merge

(deftest "a node's repeat occurrence adds new attributes, and warns about, while ignoring, contradicting ones"
  (let (warnings)
    (cl-letf (((symbol-function 'display-warning) (lambda (_type msg &rest _) (push msg warnings))))
      (should (equal "digraph {\n  a [ color=red shape=box label=a ];\n}"
                     (org--graph-raw->dot "+ a\n  | color | red |\n+ a\n  | color | blue |\n  | shape | box |\n" "directed"))))
    (should (equal '("Node `a' is given color=blue again, keeping its first color=red") warnings))))

;;; Clusters as arc endpoints

(define-graph-test "the arc from a parent to a cluster lands on the cluster's first node, clipped at its border"
  :input "
+ top
  + my-box
    | shape | cluster |
    + inner"
  :output "digraph {
  graph [ compound=true ]
  top [ label=top ];
  subgraph \"cluster_my-box\" {
    label=\"my-box\"
    inner [ label=inner ];
  }
  top -> inner [ lhead=\"cluster_my-box\" ];
}")

(define-graph-test "a `:to' arc may start at a cluster, and end at another"
  :input "
+ G
  | shape | cluster |
  | :to   | H       |
  + a
+ H
  | shape | cluster |
  + b"
  :output "digraph {
  graph [ compound=true ]
  subgraph cluster_G {
    label=G
    a [ label=a ];
  }
  a -> b [ ltail=cluster_G lhead=cluster_H ];
  subgraph cluster_H {
    label=H
    b [ label=b ];
  }
}")

;;; Export file names

(deftest "exporting to a non-HTML backend names the image after the dot source, so re-exports reuse one file"
  (let ((dot (org--graph-raw->dot "+ A\n" "directed"))
        (export (lambda () (org-block/graph 'latex "+ A\n" "directed" nil))))
    (should (equal (funcall export) (funcall export)))
    (should (s-contains-p (format ":file graph-%s.png" (substring (md5 dot) 0 12)) (funcall export)))))
