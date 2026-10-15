;;; org-graph-buffer-tests.el --- A graph and the buffer around it: templates, #+name:, and :graph: sections  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; Buffer-wide #+name: templates

(defun tests-graph--dot-in-buffer (buffer-text body)
  "The dot of graph BODY, rendered in an Org buffer holding BUFFER-TEXT."
  (with-temp-buffer
    (org-mode)
    (insert buffer-text)
    (org--graph-raw->dot body nil)))

(deftest "a #+name: table outside any graph is a template every graph in the buffer may use"
  (should (equal "digraph {
  Khadija [ shape=box color=pink label=Khadija ];
}"
                 (tests-graph--dot-in-buffer "
#+NAME: wife
| shape | box  |
| color | pink |
" "
+ Khadija :wife:"))))

(deftest "a graph's own template wins over the buffer's; a buffer template may inherit others"
  (should (equal "digraph {
  Khadija [ style=dotted shape=box label=Khadija ];
  Fatima [ shape=ellipse label=Fatima ];
}"
                 (tests-graph--dot-in-buffer "
#+name: faint
| style | dotted |

#+name: wife :faint:
| shape | box |

#+name: daughter
| shape | box |
" "
#+name: daughter
| shape | ellipse |
+ Khadija :wife:
+ Fatima :daughter:"))))

(deftest "a template inside another graph stays that graph's own"
  (should-error (tests-graph--dot-in-buffer "
#+begin_graph
#+name: wife
| shape | box |
+ Khadija :wife:
#+end_graph
" "
+ Sawda :wife:")
                :type 'user-error))

;;; A #+name: on the block: a figure that [[links]] reach

(deftest "a named graph renders to NAME.png and exports as an image link, which its #+name: and #+caption: label"
  (let (rendered)
    (cl-letf (((symbol-function 'org-graph--run-dot) (lambda (_dot kind file) (setq rendered (list kind file)) '(0 . ""))))
      ;; It opens with the link, so the #+name: above the block labels the link rather than an empty export block.
      (should (s-starts-with-p "[[file:lineage.png]]\n"
                               (let ((org--handler-anchor "lineage")) (org-block/graph 'latex "+ λ" nil))))
      (should (equal '("png" "lineage.png") rendered))
      (let ((org--handler-anchor "lineage")) (org-block/graph 'html "+ λ" nil :svg t))
      (should (equal '("svg" "lineage.svg") rendered))
      (let ((org--handler-anchor "lineage")) (org-block/graph 'html "+ λ" nil :file "elsewhere.png"))
      (should (equal '("png" "elsewhere.png") rendered)))))

(deftest "C-c C-c on a named graph renders to NAME.png, unless :file says otherwise"
  (let (outfile)
    (cl-letf (((symbol-function 'org-graph--run-dot) (lambda (_dot _kind file) (setq outfile file) '(0 . "")))
              ((symbol-function 'org-graph--insert-results) #'ignore)
              ((symbol-function 'org-display-inline-images) #'ignore))
      (with-temp-buffer
        (org-mode)
        (insert "#+name: lineage\n#+begin_graph\n+ λ\n#+end_graph\n#+name: other\n#+begin_graph :file mine.png\n+ λ\n#+end_graph\n")
        (goto-char (point-min))
        (search-forward "+ λ")
        (org-graph--cc-handler)
        (should (equal "lineage.png" outfile))
        (search-forward "+ λ")
        (org-graph--cc-handler)
        (should (equal "mine.png" outfile))))))

(provide 'org-graph-buffer-tests)
;;; org-graph-buffer-tests.el ends here

;;; #+RESULTS: below a graph

(deftest "re-rendering replaces the whole previous #+RESULTS:, even a multi-line dot source block, keeping what follows"
  (with-temp-buffer
    (org-mode)
    (insert "
#+begin_graph
+ a
#+end_graph
#+RESULTS:
#+begin_src dot
digraph {
  a;
}
#+end_src

Prose after.")
    (goto-char (point-min))
    (search-forward "#+end_graph")
    (org-graph--insert-results (line-beginning-position) "[[file:graph.png]]")
    (should (equal (buffer-string) "
#+begin_graph
+ a
#+end_graph
#+RESULTS:
[[file:graph.png]]

Prose after."))))

(deftest "a buffer template may carry a #+caption: beside its #+name:, as any Org table may"
  (should (equal "digraph {
  Khadija [ shape=box label=Khadija ];
}"
                 (tests-graph--dot-in-buffer "
#+name: wife
#+caption: How a wife is drawn
| shape | box |
" "
+ Khadija :wife:"))))
