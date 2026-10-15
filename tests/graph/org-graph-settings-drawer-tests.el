;;; org-graph-settings-drawer-tests.el --- :SETTINGS: drawer folding  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; :SETTINGS: drawer folding

(define-graph-test ":SETTINGS: drawer wraps preamble directives"
  :input "
:SETTINGS:
#+graph:
| rankdir | LR |
#+node:
| shape | box |
:END:
+ A"
  :output "digraph {
  graph [ rankdir=LR ]
  node [ shape=box ]
  A [ label=A ];
}")

(define-graph-test ":SETTINGS: drawer with #+rank: and #+edge:"
  :input "
:SETTINGS:
#+edge:
| color | red |
#+rank: A B
:END:
+ A
+ B"
  :output "digraph {
  edge [ color=red ]
  { rank=same; A; B }
  A [ label=A ];
  B [ label=B ];
}")

(define-graph-test ":SETTINGS: drawer with blank lines between directives"
  :input "
:SETTINGS:
#+graph:
| rankdir | TB |

#+node:
| shape | ellipse |
:END:
+ X"
  :output "digraph {
  graph [ rankdir=TB ]
  node [ shape=ellipse ]
  X [ label=X ];
}")

(define-graph-test "any drawer wraps preamble directives, whatever its name"
  :input "
:PALETTE:
#+node:
| shape | box |
:END:
+ A"
  :output "digraph {
  node [ shape=box ]
  A [ label=A ];
}")

(deftest "bare preamble and :SETTINGS: drawer produce identical output"
  (let ((bare   (org--graph-raw->dot "
#+graph:
| rankdir | LR |
#+node:
| color | red |
+ A" nil))
        (drawer (org--graph-raw->dot "
:SETTINGS:
#+graph:
| rankdir | LR |
#+node:
| color | red |
:END:
+ A" nil)))
    (should (equal bare drawer))))
