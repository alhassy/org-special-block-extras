;;; org-graph-settings-drawer-tests.el --- :SETTINGS: drawer folding  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; :SETTINGS: drawer folding

(define-graph-test ":SETTINGS: drawer wraps preamble directives"
  :input ":SETTINGS:\n#+graph:\n| rankdir | LR |\n#+node:\n| shape | box |\n:END:\n+ A\n"
  :output "digraph {
  graph [ rankdir=LR ]
  node [ shape=box ]
  A [ label=A ];
}")

(define-graph-test ":SETTINGS: drawer with #+rank: and #+edge:"
  :input ":SETTINGS:\n#+edge:\n| color | red |\n#+rank: A B\n:END:\n+ A\n+ B\n"
  :output "digraph {
  edge [ color=red ]
  { rank=same; A; B }
  A [ label=A ];
  B [ label=B ];
}")

(define-graph-test ":SETTINGS: drawer with blank lines between directives"
  :input ":SETTINGS:\n#+graph:\n| rankdir | TB |\n\n#+node:\n| shape | ellipse |\n:END:\n+ X\n"
  :output "digraph {
  graph [ rankdir=TB ]
  node [ shape=ellipse ]
  X [ label=X ];
}")

(deftest "bare preamble and :SETTINGS: drawer produce identical output"
  (let ((bare   (org--graph-raw->dot "#+graph:\n| rankdir | LR |\n#+node:\n| color | red |\n+ A\n" "directed"))
        (drawer (org--graph-raw->dot ":SETTINGS:\n#+graph:\n| rankdir | LR |\n#+node:\n| color | red |\n:END:\n+ A\n" "directed")))
    (should (equal bare drawer))))
