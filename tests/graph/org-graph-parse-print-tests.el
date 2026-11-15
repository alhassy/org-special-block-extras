;;; org-graph-parse-print-tests.el --- Parsing to graph data, then printing it as dot  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; Parsing yields plain data

(deftest "`org--graph-parse' yields statements as data: arcs, then the declaration, then the children"
  (should (equal '((:node "A")
                   (:edge "A" "B" (("label" "calls")))
                   (:edge "B" "A" (("style" "dotted")))
                   (:node "B"))
                 (cadr (org--graph-parse "
+ A
  + B [[A]]
    | :edge.label    | calls  |
    | :edge[A].style | dotted |")))))

(deftest "`org--graph-item-rows' routes :edge. keys away from node attributes"
  (should (equal '(:attrs (("shape" "box"))
                   :edge (("label" "a") ("color" "red"))
                   :targeted (("z" ("color" "red") ("label" "zed"))))
                 (org--graph-item-rows '(("shape" "box")
                                         (":edge.label" "a") (":edge.color" "red")
                                         (":edge[z].color" "red") (":edge[z].label" "zed"))
                                       (make-hash-table :test #'equal)))))

;;; Unknown keys

(deftest "an unknown :key is no attribute, so its table is a record, and the typo shows rather than vanishing"
  (should (s-contains-p "<TD>:colour</TD><TD>red</TD>" (org--graph-raw->dot "
+ A
  | :colour | red |" nil))))

(deftest "a lone-link item is a reference: node attributes on it are a user error"
  (should-error (org--graph-raw->dot "
+ <<b>> B
+ A
  + [[b]]
    | color | red |" nil) :type 'user-error))

(define-graph-test "a lone-link child may refer forward; the target's own item still names and labels it"
  :input "
+ Lisp
  + inspired by :: [[lam][:faint:]]
+ <<lam>> λ-calculus
#+name: faint
| style | dotted |"
  :output "digraph {
  Lisp [ label=Lisp ];
  Lisp -> lam [ label=\"inspired by\" style=dotted ];
  lam [ label=\"λ-calculus\" ];
}")

;;; Printing quotes values, and picks the edge operator

(define-graph-test "values with spaces are quoted for us; already-quoted ones are left alone"
  :input "
+ A
  | label     | Two words   |
  | color     | \"#ff0000\" |
  | fillcolor | #00ff00     |"
  :output "digraph {
  A [ label=\"Two words\" color=\"#ff0000\" fillcolor=\"#00ff00\" style=filled ];
}")

(deftest "an undirected graph is #+edge: :undirected:, arrowless arcs; a -> inside a label stays as written"
  (should (equal "digraph {
  edge [ dir=none ]
  a [ label=\"x -> y\" ];
  a -> b;
  b [ label=b ];
}"
                 (org--graph-raw->dot "
#+edge: :undirected:
+ a
  | label | x -> y |
  + b" nil))))

(define-graph-test "\"Org mode\" and Org mode name the same node"
  :input "
+ \"Org mode\"
+ A [[Org mode]]"
  :output "digraph {
  \"Org mode\" [ label=\"Org mode\" ];
  A -> \"Org mode\";
  A [ label=A ];
}")

;;; Ids and labels

(define-graph-test "`+ L', `+ <<L>> L', and `+ L' with a `label L' row all declare one and the same node"
  :input "
+ Lisp

+ <<Lisp>> Lisp

+ Lisp
  | label | Lisp |"
  :output "digraph {
  Lisp [ label=Lisp ];
}")

;;; Repeat occurrences merge

(deftest "a node's repeat occurrence adds new attributes, and warns about, while ignoring, contradicting ones"
  (let (warnings)
    (cl-letf (((symbol-function 'display-warning) (lambda (_type msg &rest _) (push msg warnings))))
      (should (equal "digraph {
  a [ color=red shape=box label=a ];
}"
                     (org--graph-raw->dot "
+ a
  | color | red |
+ a
  | color | blue |
  | shape | box |" nil))))
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

(define-graph-test "a [[link]] arc may start at a cluster, and end at another"
  :input "
+ G [[H]]
  | shape | cluster |
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
  (let* ((default-directory (make-temp-file "org-graph-" t "/"))
         (hash (substring (md5 (org--graph-raw->dot "
+ A" nil)) 0 12))
         (export (lambda (backend) (org--graph-block backend "
+ A" nil nil))))
    (should (equal (funcall export 'ascii) (funcall export 'ascii)))
    (should (s-contains-p (format ":file graph-%s.png" hash) (funcall export 'ascii)))
    (should (s-contains-p (format "\\includegraphics{graph-%s.pdf}" hash) (funcall export 'latex)))))

(deftest "exporting to LaTeX includes the graph as a PDF, by dot, shrunk only when wider than the line"
  (let* ((default-directory (make-temp-file "org-graph-" t "/"))
         (latex (org--graph-block 'latex "
+ A
  + B" nil nil)))
    (should (string-match "\\\\includegraphics{\\(graph-[0-9a-f]+\\.pdf\\)}" latex))
    (should (file-exists-p (match-string 1 latex)))
    (should (s-contains-p "\\ifdim\\wd0>\\linewidth\\resizebox{\\linewidth}{!}{\\box0}" latex))
    (should-not (s-contains-p "digraph" latex))))
