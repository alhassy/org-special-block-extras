;;; org-graph-directive-face-tests.el --- Directives and their known keys stand out; unknown keys do not  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-export-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))
(require 'org-graph-block)

(defun org-graph-face-test--has (needle face)
  "Returns whether the first NEEDLE in the buffer, fontified, has FACE."
  (save-excursion
    (goto-char (point-min))
    (search-forward needle)
    (memq face (ensure-list (get-text-property (match-beginning 0) 'face)))))

(org-deftest "in a graph, #+graph:, #+node:, #+edge:, and #+name: stand out, and so does each key known on its line" [graph faces]
  :text "
#+begin_graph
#+graph: rankdir=LR
#+node: shape=box colour=red
#+edge: color=gray shape=box
#+name: red fillcolor=pink
+ a
#+end_graph

#+node: shape=box"
  :at "#+begin_graph"
  (org-graph--setup-org-buffer)
  (font-lock-ensure)
  (should (org-graph-face-test--has "#+graph:" 'org-graph-directive))
  (should (org-graph-face-test--has "#+name:" 'org-graph-directive))
  (should (org-graph-face-test--has "rankdir" 'org-graph-key))
  (should (org-graph-face-test--has "fillcolor" 'org-graph-key))
  ;; A typo, and a node's attribute on an arc's line, stay plain.
  (should-not (org-graph-face-test--has "colour" 'org-graph-key))
  (should-not (save-excursion (goto-char (point-min)) (search-forward "#+edge:")
                              (search-forward "shape")
                              (memq 'org-graph-key (ensure-list (get-text-property (match-beginning 0) 'face)))))
  ;; Outside a graph, nothing is ours.
  (should-not (save-excursion (goto-char (point-max)) (search-backward "#+node:")
                              (memq 'org-graph-directive (ensure-list (get-text-property (point) 'face))))))
