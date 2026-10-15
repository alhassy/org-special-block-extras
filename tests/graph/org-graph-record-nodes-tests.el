;;; org-graph-record-nodes-tests.el --- Record nodes  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; Record nodes

(define-graph-test "record node: three fields with port ids"
  :input "
+ struct1
  | shape | record |
  + left
    | :id | f0 |
  + mid dle
    | :id | f1 |
  + right
    | :id | f2 |"
  :output "digraph {
  struct1 [ shape=record label=\"<f0> left|<f1> mid\\ dle|<f2> right\" ];
}")

(define-graph-test "record node: two records with cross-port edge"
  :input "
+ struct1
  | shape | record |
  + left
    | :id | f0 |
  + mid dle
    | :id | f1         |
    | :to | struct2.g0 |
  + right
    | :id | f2 |
+ struct2
  | shape | record |
  + one
    | :id | g0 |
  + two
    | :id | g1 |"
  :output "digraph {
  struct1 [ shape=record label=\"<f0> left|<f1> mid\\ dle|<f2> right\" ];
  struct1:f1 -> struct2:g0;
  struct2 [ shape=record label=\"<g0> one|<g1> two\" ];
}")

(define-graph-test "record node: no id row — field name used as display label only, no port prefix"
  :input "
+ box
  | shape | record |
  + alpha
  + beta"
  :output "digraph {
  box [ shape=record label=\"alpha|beta\" ];
}")

(define-graph-test "Mrecord shape works identically to record"
  :input "
+ rounded
  | shape | Mrecord |
  + x
    | :id | p0 |
  + y
    | :id | p1 |"
  :output "digraph {
  rounded [ shape=Mrecord label=\"<p0> x|<p1> y\" ];
}")

(define-graph-test "record node preserves other node attrs beside shape"
  :input "
+ s
  | shape | record |
  | color | red    |
  + a
  + b"
  :output "digraph {
  s [ shape=record label=\"a|b\" color=red ];
}")

(define-graph-test "regression: field name with space normalises port to underscore form"
  ;; Without normalisation, `mid dle` as a port produces `struct1:mid dle` in the
  ;; edge statement, which Graphviz parses as node `struct1:mid` + stray node `dle`.
  ;; The field has no :id row, yet an arc ends there, so its label declares the port.
  :input "
+ s
  | shape | record |
  + mid dle
    | :to | s.mid dle |"
  :output "digraph {
  s [ shape=record label=\"<mid_dle> mid\\ dle\" ];
  s:mid_dle -> s:mid_dle;
}")

(define-graph-test "{} child wraps fields in a layout-flip group"
  :input "
+ s
  | shape | record |
  + a
  + {}
    + b
    + c
  + d"
  :output "digraph {
  s [ shape=record label=\"a|{b|c}|d\" ];
}")

(define-graph-test "nested {} groups flip orientation twice"
  :input "
+ s
  | shape | record |
  + a
  + {}
    + b
    + {}
      + c
      + d
    + e
  + f"
  :output "digraph {
  s [ shape=record label=\"a|{b|{c|d}|e}|f\" ];
}")

(define-graph-test "struct3: full Graphviz example with {} and port"
  :input "
+ struct3
  | shape | record |
  + hello\\nworld
  + {}
    + b
    + {}
      + c
      + d
        | :id | here |
      + e
    + f
  + g
  + h"
  :output "digraph {
  struct3 [ shape=record label=\"hello\\nworld|{b|{c|<here> d|e}|f}|g|h\" ];
}")

(define-graph-test "an ordinary node's `:to' row reaches a port nested inside {} groups"
  :input "
+ struct3
  | shape | record |
  + {}
    + {}
      + d
        | :id | x |
+ look
  | :to | struct3.x |"
  :output "digraph {
  struct3 [ shape=record label=\"{{<x> d}}\" ];
  look -> struct3:x;
  look [ label=look ];
}")

(define-graph-test "a field's `:to' row reaches a port of another record"
  :input "
+ rec1
  | shape | record |
  + a
    | :id | f1      |
    | :to | rec2.g0 |
+ rec2
  | shape | record |
  + b
    | :id | g0 |"
  :output "digraph {
  rec1 [ shape=record label=\"<f1> a\" ];
  rec1:f1 -> rec2:g0;
  rec2 [ shape=record label=\"<g0> b\" ];
}")

(define-graph-test "record node connected from a plain parent"
  :input "
+ root
  + rec
    | shape | record |
    + p
    + q"
  :output "digraph {
  root [ label=root ];
  root -> rec;
  rec [ shape=record label=\"p|q\" ];
}")

(define-graph-test "record node: a field's nested item is an ordinary node of its own"
  :input "
+ R
  | shape | record |
  + f
    + g"
  :output "digraph {
  R [ shape=record label=\"f\" ];
  g [ label=g ];
}")

(define-graph-test "ports are scoped per record, so two records may share a field name"
  :input "
+ r1
  | shape | record |
  + head
+ r2
  | shape | record |
  + head
+ z
  | :to | r1.head |"
  :output "digraph {
  r1 [ shape=record label=\"<head> head\" ];
  r2 [ shape=record label=\"head\" ];
  z -> r1:head;
  z [ label=z ];
}")

(define-graph-test "a record whose shape comes from a template still has fields"
  :input "
#+name: rec
| shape | record |

+ r
  | :eval | rec |
  + f1
+ z
  | :to | r.f1 |"
  :output "digraph {
  r [ shape=record label=\"<f1> f1\" ];
  z -> r:f1;
  z [ label=z ];
}")

(define-graph-test "a `:to' target names a plain node, even when some record has a field of that name"
  :input "
+ r
  | shape | record |
  + church
+ church
+ z
  | :to | church |"
  :output "digraph {
  r [ shape=record label=\"church\" ];
  church [ label=church ];
  z -> church;
  z [ label=z ];
}")

(define-graph-test "a dotted `:to' target is a plain node unless the part before a dot names a record"
  :input "
+ v1.2
+ z
  | :to | v1.2 |"
  :output "digraph {
  \"v1.2\" [ label=\"v1.2\" ];
  z -> \"v1.2\";
  z [ label=z ];
}")

(define-graph-test "record and field names may themselves contain dots"
  :input "
+ org.el
  | shape | record |
  + v1.2
+ z
  | :to | org.el.v1.2 |"
  :output "digraph {
  \"org.el\" [ shape=record label=\"<v1.2> v1.2\" ];
  z -> \"org.el\":\"v1.2\";
  z [ label=z ];
}")
