;;; org-graph-edit-mode-tests.el --- C-c ' on a graph: completion and eldoc for table keys  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

(defun tests-graph--candidates ()
  "The completion candidates `org-graph--completion-at-point' offers at point."
  (nth 2 (org-graph--completion-at-point)))

;;; C-c ' opens the graph's own mode

(org-deftest "C-c ' on a graph block edits its body in `org-graph-edit-mode', which completes and explains table keys"
  :text "
#+begin_graph
+ A
#+end_graph"
  :at "+ A"
  (org-edit-special)
  (unwind-protect
      (progn (should (eq major-mode 'org-graph-edit-mode))
             (should (memq #'org-graph--completion-at-point completion-at-point-functions))
             (should (memq #'org-graph--eldoc eldoc-documentation-functions)))
    (org-edit-src-abort)))

;;; Completion

(deftest "after `:edge.' or `:edge[T].', completion offers only edge attributes, keeping the prefix"
  (dolist (prefix '(":edge." ":edge[church]."))
    (with-org-test-buffer (:mode #'org-graph-edit-mode
                           :text (format "
+ A
  | %s |" prefix)
                           :at prefix)
      (let ((candidates (tests-graph--candidates)))
        (should (member (concat prefix "arrowhead") candidates))
        (should (member (concat prefix "label") candidates))
        (should-not (member (concat prefix "shape") candidates))))))

(org-deftest "an item's key cell offers node attributes and the reserved keys, but not edge-only attributes"
  :mode #'org-graph-edit-mode
  :text "
+ A
  | sha |"
  :at "sha"
  (let ((candidates (tests-graph--candidates)))
    (should (member "shape" candidates))
    (should (member ":children-rank" candidates))
    (should (member ":edge[TARGET]." candidates))
    (should-not (member "arrowhead" candidates))))

(org-deftest "a value cell offers the values of its row's attribute, prefix and all"
  :mode #'org-graph-edit-mode
  :text "
+ A
  | :edge.dir | b |"
  :at "| b"
  (should (equal '("forward" "back" "both" "none") (tests-graph--candidates))))

(org-deftest "completion annotates each candidate with its description"
  :mode #'org-graph-edit-mode
  :text "
+ A
  | :edge. |"
  :at ":edge."
  (let ((annotate (plist-get (nthcdr 3 (org-graph--completion-at-point)) :annotation-function)))
    (should (equal "  Edge type for drawing arrowheads.  Default: forward (directed) none (undirected)."
                   (funcall annotate ":edge.dir")))))

(org-deftest "outside a graph, completion stays out of the way"
  :text "
| shape | box |"
  :at "sha"
  (should-not (org-graph--completion-at-point)))

;;; Eldoc

(deftest "eldoc describes the key on the row at point: attributes sans their edge prefix, with their default, and reserved keys"
  (dolist (case '(("| :edge[x].arrowhead | vee |" . "Style of arrowhead on the head node of an edge.  Default: normal.")
                  ("| :children-rank | same |"    . "Put this item's immediate children on one rank: same, min, max, source, or sink.")
                  ("| shape | box |"              . "Sets the shape of a node.  Default: ellipse.")))
    (with-org-test-buffer (:mode #'org-graph-edit-mode
                           :text (format "
+ A
  %s" (car case))
                           :at "| ")
      (let (doc)
        (org-graph--eldoc (lambda (d &rest _) (setq doc d)))
        (should (equal (cdr case) doc))))))

;;; The main Org buffer, and its #+begin_graph line

(org-deftest "every Org buffer completes and explains graph keys, but only inside a graph block"
  :text "
| sha | x |
#+begin_graph
+ A
  | :edge.arr | x |
#+end_graph"
  :at "sha"
  (should (memq #'org-graph--completion-at-point completion-at-point-functions))
  (should (memq #'org-graph--eldoc eldoc-documentation-functions))
  (should-not (org-graph--completion-at-point))
  (search-forward ":edge.arr")
  (should (member ":edge.arrowhead" (tests-graph--candidates))))

(org-deftest "on the #+begin_graph line, a :KEY completes to the block's keys, graph attributes, :node.KEY, and :edge.KEY"
  :text "
#+begin_graph Title :r
+ A
#+end_graph"
  :at ":r"
  (let ((candidates (tests-graph--candidates)))
    (should (member ":rankdir" candidates))
    (should (member ":node.shape" candidates))
    (should (member ":edge.arrowhead" candidates))
    (should (member ":directed" candidates))
    (should-not (member ":shape" candidates))
    (should-not (member ":node." candidates))))

(org-deftest "on the #+begin_graph line, a key's value completes to that attribute's values"
  :text "
#+begin_graph :rankdir L :node.shape b
+ A
#+end_graph"
  :at ":rankdir L"
  (should (equal '("TB" "LR" "BT" "RL") (tests-graph--candidates)))
  (search-forward ":node.shape b")
  (should (member "box" (tests-graph--candidates))))

(org-deftest "on the #+begin_graph line, the block's own keys complete to their values too"
  :text "
#+begin_graph :notes i :unknown-tags r
+ A
#+end_graph"
  :at ":notes i"
  (should (equal '("tooltip" "inline") (tests-graph--candidates)))
  (search-forward ":unknown-tags r")
  (should (equal '("error" "random-templates") (tests-graph--candidates))))

(deftest "on the #+begin_graph line, eldoc explains the key at point"
  (dolist (case '((":rankdir"    . "Sets direction of graph layout. dot only.  Default: TB.")
                  (":node.shape" . "Sets the shape of a node.  Default: ellipse.")
                  (":svg"        . "HTML export embeds an inline <svg>, its tooltips as prose's; with nil, a base64 PNG.")))
    (with-org-test-buffer (:text (format "
#+begin_graph Title %s x
+ A
#+end_graph" (car case))
                           :at (car case))
      (let (doc)
        (org-graph--eldoc (lambda (d &rest _) (setq doc d)))
        (should (equal (cdr case) doc))))))

(org-deftest "on the #+begin_graph line, but not on a key, eldoc defers to the block's signature"
  :text "
#+begin_graph Title :rankdir LR
+ A
#+end_graph"
  :at "Tit"
  (should-not (org-graph--eldoc (lambda (&rest _) (error "No doc expected here")))))

;;; Links show in full

(org-deftest "a [[TARGET][LABEL]] link in a graph shows both parts; elsewhere Org hides the target"
  :text "
[[outside][x]]
#+begin_graph
+ lisp [[church][cross]]
#+end_graph"
  (font-lock-ensure)
  (search-forward "[[outside")
  (should (eq 'org-link (get-text-property (match-beginning 0) 'invisible)))
  (search-forward "[[church")
  (should-not (text-property-not-all (match-beginning 0) (line-end-position) 'invisible nil)))

(org-deftest "a [[TARGET][LABEL]] link shows both parts in a graph's C-c ' buffer too"
  :mode #'org-graph-edit-mode
  :text "
+ lisp [[church][cross]]"
  :at "lisp "
  (font-lock-ensure)
  (should-not (text-property-not-all (point) (line-end-position) 'invisible nil)))

(provide 'org-graph-edit-mode-tests)
