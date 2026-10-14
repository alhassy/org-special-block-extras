;;; org-graph-record-nodes-tests.el --- Record nodes  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; Record nodes

(define-graph-test "record node: three fields with port ids"
  :input "+ struct1\n  | shape | record |\n  + left\n    | id | f0 |\n  + mid dle\n    | id | f1 |\n  + right\n    | id | f2 |\n"
  :output "digraph {
  struct1 [ shape=record label=\"<f0> left|<f1> mid\\ dle|<f2> right\" ];
}")

(define-graph-test "record node: two records with cross-port edge"
  :input "+ struct1\n  | shape | record |\n  + left\n    | id | f0 |\n  + mid dle\n    | id | f1 |\n    | to | g0 |\n  + right\n    | id | f2 |\n+ struct2\n  | shape | record |\n  + one\n    | id | g0 |\n  + two\n    | id | g1 |\n"
  :output "digraph {
  struct1 [ shape=record label=\"<f0> left|<f1> mid\\ dle|<f2> right\" ];
  struct1:f1 -> struct2:g0;
  struct2 [ shape=record label=\"<g0> one|<g1> two\" ];
}")

(define-graph-test "record node: no id row — field name used as display label only, no port prefix"
  :input "+ box\n  | shape | record |\n  + alpha\n  + beta\n"
  :output "digraph {
  box [ shape=record label=\"alpha|beta\" ];
}")

(define-graph-test "Mrecord shape works identically to record"
  :input "+ rounded\n  | shape | Mrecord |\n  + x\n    | id | p0 |\n  + y\n    | id | p1 |\n"
  :output "digraph {
  rounded [ shape=Mrecord label=\"<p0> x|<p1> y\" ];
}")

(define-graph-test "record node preserves other node attrs beside shape"
  :input "+ s\n  | shape | record |\n  | color | red |\n  + a\n  + b\n"
  :output "digraph {
  s [ shape=record label=\"a|b\" color=red ];
}")

(define-graph-test "regression: field name with space normalises port to underscore form"
  ;; Without normalisation, `mid dle` as a port produces `struct1:mid dle` in the
  ;; edge statement, which Graphviz parses as node `struct1:mid` + stray node `dle`.
  ;; The `to` target `mid dle` has no explicit id row on the target field, so the
  ;; implicit port (spaces→underscores) is used for the edge but not the label.
  :input "+ s\n  | shape | record |\n  + mid dle\n    | to | mid dle |\n"
  :output "digraph {
  s [ shape=record label=\"mid\\ dle\" ];
  s:mid_dle -> s:mid_dle;
}")

(define-graph-test "{} child wraps fields in a layout-flip group"
  :input "+ s\n  | shape | record |\n  + a\n  + {}\n    + b\n    + c\n  + d\n"
  :output "digraph {
  s [ shape=record label=\"a|{b|c}|d\" ];
}")

(define-graph-test "nested {} groups flip orientation twice"
  :input "+ s\n  | shape | record |\n  + a\n  + {}\n    + b\n    + {}\n      + c\n      + d\n    + e\n  + f\n"
  :output "digraph {
  s [ shape=record label=\"a|{b|{c|d}|e}|f\" ];
}")

(define-graph-test "struct3: full Graphviz example with {} and port"
  :input "+ struct3\n  | shape | record |\n  + hello\\nworld\n  + {}\n    + b\n    + {}\n      + c\n      + d\n        | id | here |\n      + e\n    + f\n  + g\n  + h\n"
  :output "digraph {
  struct3 [ shape=record label=\"hello\\nworld|{b|{c|<here> d|e}|f}|g|h\" ];
}")

(define-graph-test "regression: ordinary node `to` row resolves port nested inside {} groups"
  ;; Port `x` is buried inside two {} groups; an ordinary node's `to` row must
  ;; still resolve it to struct3:x, not emit it as a bare (phantom) node.
  :input "+ struct3\n  | shape | record |\n  + {}\n    + {}\n      + d\n        | id | x |\n+ look\n  | to | x |\n"
  :output "digraph {
  struct3 [ shape=record label=\"{{<x> d}}\" ];
  look -> struct3:x;
  look [ label=look ];
}")

(define-graph-test "regression: bare port id in `to` row resolves to owner:port not bare id"
  ;; Field f1 of rec1 targets port g0; g0 belongs to rec2.
  ;; The emitted edge must be rec1:f1 -> rec2:g0, not rec1:f1 -> g0.
  :input "+ rec1\n  | shape | record |\n  + a\n    | id | f1 |\n    | to | g0 |\n+ rec2\n  | shape | record |\n  + b\n    | id | g0 |\n"
  :output "digraph {
  rec1 [ shape=record label=\"<f1> a\" ];
  rec1:f1 -> rec2:g0;
  rec2 [ shape=record label=\"<g0> b\" ];
}")

(define-graph-test "record node connected from a plain parent"
  :input "+ root\n  + rec\n    | shape | record |\n    + p\n    + q\n"
  :output "digraph {
  root [ label=root ];
  root -> rec;
  rec [ shape=record label=\"p|q\" ];
}")

(define-graph-test "record node: a field's nested item is an ordinary node of its own"
  :input "+ R\n  | shape | record |\n  + f\n    + g\n"
  :output "digraph {
  R [ shape=record label=\"f\" ];
  g [ label=g ];
}")
