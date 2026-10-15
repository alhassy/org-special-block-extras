;;; graph.el --- Snapshot of the `graph' showcase example  -*- lexical-binding: t; -*-

;; This is only the showcase used by `osbe-example:graph' in the docs; the `graph' block's
;; thorough tests ---parsing, dot generation, clusters, records, C-c C-c--- live in tests/graph/.

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

(define-org-example-test "graph"
  :input "Org lists are already trees, so the ~graph~ block draws them with Graphviz:
nesting makes an edge, and a two-column table styles the item above it.

#+begin_graph directed :file lambda-calculus.png
#+graph:
| rankdir | LR |

#+node:
| shape | box |

+ λ-calculus
  | shape | ellipse |
  + Church encodings
    | :edge.label | encodes |
  + Combinators
    | :id | combinators |
    + S K I
+ Lisp
  | :to       | combinators |
  | :to.label | borrows     |
  | :to.style | dotted      |
#+end_graph

Press =C-c C-c= inside the block to render it to a PNG; =C-u C-c C-c= shows the dot source instead."
  :html :live
  :latex "Org lists are already trees, so the \\texttt{graph} block draws them with Graphviz:
nesting makes an edge, and a two-column table styles the item above it.


\\begin{verbatim}
digraph {
  graph [ rankdir=LR ]
  node [ shape=box ]
  \"λ-calculus\" [ shape=ellipse label=\"λ-calculus\" ];
  \"λ-calculus\" -> \"Church encodings\" [ label=encodes ];
  \"Church encodings\" [ label=\"Church encodings\" ];
  \"λ-calculus\" -> combinators;
  combinators [ label=Combinators ];
  combinators -> \"S K I\";
  \"S K I\" [ label=\"S K I\" ];
  Lisp -> combinators [ label=borrows style=dotted ];
  Lisp [ label=Lisp ];
}
\\end{verbatim}


Press \\texttt{C-c C-c} inside the block to render it to a PNG; \\texttt{C-u C-c C-c} shows the dot source instead.")
