;;; org-graph-edit-mode-tests.el --- C-c ' on a graph: completion and eldoc for table keys  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

(defmacro tests-graph--in-edit-buffer (content marker &rest body)
  "Run BODY in an `org-graph-edit-mode' buffer holding CONTENT, with point just after MARKER's first occurrence."
  (declare (indent 2))
  `(with-temp-buffer
     (org-graph-edit-mode)
     (insert ,content)
     (goto-char (point-min))
     (search-forward ,marker)
     ,@body))

(defun tests-graph--candidates ()
  "The completion candidates `org-graph--completion-at-point' offers at point."
  (nth 2 (org-graph--completion-at-point)))

;;; C-c ' opens the graph's own mode

(deftest "C-c ' on a graph block edits its body in `org-graph-edit-mode', which completes and explains table keys"
  (let ((buf (tests-graph--make-graph-buffer "
+ A")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "+ A")
      (org-edit-special)
      (unwind-protect
          (progn (should (eq major-mode 'org-graph-edit-mode))
                 (should (memq #'org-graph--completion-at-point completion-at-point-functions))
                 (should (memq #'org-graph--eldoc eldoc-documentation-functions)))
        (org-edit-src-abort)))
    (kill-buffer buf)))

;;; Completion

(deftest "after `:edge.' or `:edge[T].', completion offers only edge attributes, keeping the prefix"
  (dolist (prefix '(":edge." ":edge[church]."))
    (tests-graph--in-edit-buffer (format "
+ A
  | %s |" prefix) prefix
      (let ((candidates (tests-graph--candidates)))
        (should (member (concat prefix "arrowhead") candidates))
        (should (member (concat prefix "label") candidates))
        (should-not (member (concat prefix "shape") candidates))))))

(deftest "an item's key cell offers node attributes and the reserved keys, but not edge-only attributes"
  (tests-graph--in-edit-buffer "
+ A
  | sha |" "sha"
    (let ((candidates (tests-graph--candidates)))
      (should (member "shape" candidates))
      (should (member ":children-rank" candidates))
      (should (member ":edge[TARGET]." candidates))
      (should-not (member "arrowhead" candidates)))))

(deftest "a value cell offers the values of its row's attribute, prefix and all"
  (tests-graph--in-edit-buffer "
+ A
  | :edge.dir | b |" "| b"
    (should (equal '("forward" "back" "both" "none") (tests-graph--candidates)))))

(deftest "completion annotates each candidate with its description"
  (tests-graph--in-edit-buffer "
+ A
  | :edge. |" ":edge."
    (let ((annotate (plist-get (nthcdr 3 (org-graph--completion-at-point)) :annotation-function)))
      (should (equal "  Edge type for drawing arrowheads.  Default: forward (directed) none (undirected)."
                     (funcall annotate ":edge.dir"))))))

(deftest "outside a graph, completion stays out of the way"
  (with-temp-buffer
    (org-mode)
    (insert "
| shape | box |")
    (search-backward "sha")
    (should-not (org-graph--completion-at-point))))

;;; Eldoc

(deftest "eldoc describes the key on the row at point: attributes sans their edge prefix, with their default, and reserved keys"
  (dolist (case '(("| :edge[x].arrowhead | vee |" . "Style of arrowhead on the head node of an edge.  Default: normal.")
                  ("| :children-rank | same |"    . "Put this item's immediate children on one rank: same, min, max, source, or sink.")
                  ("| shape | box |"              . "Sets the shape of a node.  Default: ellipse.")))
    (tests-graph--in-edit-buffer (format "
+ A
  %s" (car case)) "| "
      (let (doc)
        (org-graph--eldoc (lambda (d &rest _) (setq doc d)))
        (should (equal (cdr case) doc))))))

;;; The main Org buffer, and its #+begin_graph line

(defmacro tests-graph--in-org-buffer (content marker &rest body)
  "Run BODY in an Org buffer holding CONTENT, with point just after MARKER's first occurrence."
  (declare (indent 2))
  `(with-temp-buffer
     (org-mode)
     (insert ,content)
     (goto-char (point-min))
     (search-forward ,marker)
     ,@body))

(deftest "every Org buffer completes and explains graph keys, but only inside a graph block"
  (tests-graph--in-org-buffer "
| sha | x |
#+begin_graph
+ A
  | :edge.arr | x |
#+end_graph" "sha"
    (should (memq #'org-graph--completion-at-point completion-at-point-functions))
    (should (memq #'org-graph--eldoc eldoc-documentation-functions))
    (should-not (org-graph--completion-at-point))
    (search-forward ":edge.arr")
    (should (member ":edge.arrowhead" (tests-graph--candidates)))))

(deftest "on the #+begin_graph line, a :KEY completes to the block's keys, graph attributes, :node.KEY, and :edge.KEY"
  (tests-graph--in-org-buffer "
#+begin_graph Title :r
+ A
#+end_graph" ":r"
    (let ((candidates (tests-graph--candidates)))
      (should (member ":rankdir" candidates))
      (should (member ":node.shape" candidates))
      (should (member ":edge.arrowhead" candidates))
      (should (member ":directed" candidates))
      (should-not (member ":shape" candidates))
      (should-not (member ":node." candidates)))))

(deftest "on the #+begin_graph line, a key's value completes to that attribute's values"
  (tests-graph--in-org-buffer "
#+begin_graph :rankdir L :node.shape b
+ A
#+end_graph" ":rankdir L"
    (should (equal '("TB" "LR" "BT" "RL") (tests-graph--candidates)))
    (search-forward ":node.shape b")
    (should (member "box" (tests-graph--candidates)))))

(deftest "on the #+begin_graph line, eldoc explains the key at point; elsewhere on it, it defers to the block's signature"
  (dolist (case '((":rankdir"    . "Sets direction of graph layout. dot only.  Default: TB.")
                  (":node.shape" . "Sets the shape of a node.  Default: ellipse.")
                  (":svg"        . "With t, HTML export embeds an inline <svg>, keeping tooltips; else a base64 PNG.")))
    (tests-graph--in-org-buffer (format "
#+begin_graph Title %s x
+ A
#+end_graph" (car case)) (car case)
      (let (doc)
        (org-graph--eldoc (lambda (d &rest _) (setq doc d)))
        (should (equal (cdr case) doc)))))
  (tests-graph--in-org-buffer "
#+begin_graph Title :rankdir LR
+ A
#+end_graph" "Tit"
    (should-not (org-graph--eldoc (lambda (&rest _) (error "No doc expected here"))))))

;;; Links show in full

(deftest "a [[TARGET][LABEL]] link in a graph shows both parts, in its Org buffer and its C-c ' buffer; elsewhere Org hides the target"
  (with-temp-buffer
    (org-mode)
    (insert "
[[outside][x]]
#+begin_graph
+ lisp [[church][cross]]
#+end_graph")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "[[outside")
    (should (eq 'org-link (get-text-property (match-beginning 0) 'invisible)))
    (search-forward "[[church")
    (should-not (text-property-not-all (match-beginning 0) (line-end-position) 'invisible nil)))
  (tests-graph--in-edit-buffer "
+ lisp [[church][cross]]" "lisp "
    (font-lock-ensure)
    (should-not (text-property-not-all (point) (line-end-position) 'invisible nil))))

(provide 'org-graph-edit-mode-tests)
