;;; org-graph-insert-attribute-row-tests.el --- org-graph--insert-attribute-row  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; org-graph--insert-attribute-row

(org-deftest "`org-graph--insert-attribute-row' inserts a new row and aligns"
  :text "
#+begin_graph
#+node:
| shape | box |
#+end_graph"
  :at "| shape |"
  ;; Simulate C-u RET: intercept completing-read and read-string.
  ;; Stub pick-color to nil so the color attr falls through to read-string.
  (cl-letf (((symbol-function 'completing-read)
             (lambda (prompt choices &rest _)
               (if (string-match-p "Attribute" prompt)
                   ;; Pick "color — Border/line color…"
                   (car (--filter (string-prefix-p "color" it) choices))
                 ;; No fixed values expected here, but handle if needed
                 (car choices))))
            ((symbol-function 'read-string)
             (lambda (_prompt &rest _) "#ff0000"))
            ((symbol-function 'org-graph--pick-color)
             (lambda () nil)))
    (org-graph--insert-attribute-row '(4)))
  (should (s-contains-p "| color" (buffer-string)))
  (should (s-contains-p "#ff0000" (buffer-string))))

(org-deftest "`org-graph--insert-attribute-row' is a no-op without prefix arg"
  :text "
#+begin_graph
#+node:
| shape | box |
#+end_graph"
  :at "| shape |"
  (let ((before (buffer-string)))
    (org-graph--insert-attribute-row nil)
    (should (equal before (buffer-string)))))

(org-deftest "`org-graph--insert-attribute-row' filters by context — no shape in graphs"
  :text "
#+begin_graph
#+graph:
| rankdir | TB |
#+end_graph"
  :at "| rankdir |"
  (let ((offered nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _)
                 (setq offered choices)
                 ;; Abort after capturing choices
                 (keyboard-quit))))
      (condition-case nil
          (org-graph--insert-attribute-row '(4))
        (quit nil)))
    ;; shape is nodes-only — must not appear in graphs context
    (should (not (--any? (string-prefix-p "shape" it) offered)))
    ;; rankdir is graphs-only — must appear
    (should (--any? (string-prefix-p "rankdir" it) offered))))

(org-deftest "`org-graph--insert-attribute-row' falls back to read-string when org-graph--pick-color returns nil"
  :text "
#+begin_graph
#+node:
| shape | box |
#+end_graph"
  :at "| shape |"
  ;; Simulates helm being absent or the user aborting without selecting a color.
  (cl-letf (((symbol-function 'completing-read)
             (lambda (prompt choices &rest _)
               (if (string-match-p "Attribute" prompt)
                   (car (--filter (string-prefix-p "color" it) choices))
                 (car choices))))
            ((symbol-function 'read-string)
             (lambda (_prompt &rest _) "#aabbcc"))
            ;; Stub pick-color to return nil (helm absent / aborted)
            ((symbol-function 'org-graph--pick-color)
             (lambda () nil)))
    (org-graph--insert-attribute-row '(4)))
  (should (s-contains-p "| color" (buffer-string)))
  (should (s-contains-p "#aabbcc" (buffer-string))))

(org-deftest "plain RET inside a graph block table runs org-return, not attribute insertion"
  :text "
#+begin_graph
#+node:
| shape | box |
#+end_graph"
  :at "| shape |"
  (end-of-line)
  (let ((before (buffer-string)))
    ;; Call the advice with no prefix arg (plain RET scenario)
    (let ((result (org-graph--maybe-insert-attribute-row nil nil t)))
      ;; Advice must return nil so :before-until lets org-return through
      (should (null result))
      ;; Buffer must be unchanged — no | key | value | row was inserted
      (should (equal before (buffer-string))))))

(org-deftest "C-u RET on a graph table triggers attribute insertion and suppresses org-return"
  :text "
#+begin_graph
#+node:
| shape | box |
#+end_graph"
  :at "| shape |"
  (cl-letf (((symbol-function 'completing-read)
             (lambda (prompt choices &rest _)
               (if (string-match-p "Attribute" prompt)
                   ;; Pick "style" — has fixed :values, avoids the helm branch
                   (car (--filter (string-prefix-p "style" it) choices))
                 ;; Value completion: pick first fixed option ("filled")
                 (car choices)))))
    ;; Call the advice with a C-u prefix arg
    (let ((result (org-graph--maybe-insert-attribute-row nil '(4) t)))
      ;; Advice must return t so :before-until suppresses org-return
      (should result)
      ;; The new attribute row must have been inserted
      (should (s-contains-p "| style" (buffer-string)))
      (should (s-contains-p "filled" (buffer-string))))))

(org-deftest "C-u RET works when RET runs org-return-indent, as a config's (local-set-key [return] 'org-return-indent) has it"
  :text "
#+begin_graph
#+node:
| shape | box |
#+end_graph"
  :at "| shape |"
  (cl-letf (((symbol-function 'completing-read)
             (lambda (prompt choices &rest _)
               (if (string-match-p "Attribute" prompt) (car (--filter (string-prefix-p "style" it) choices)) (car choices)))))
    ;; org-return-indent calls org-return sans the prefix argument; the keypress's C-u is current-prefix-arg.
    (let ((this-command 'org-return-indent) (current-prefix-arg '(4)))
      (call-interactively #'org-return-indent))
    (should (s-contains-p "| style" (buffer-string))))
  ;; A plain RET, sans C-u, is left to Org.
  (let ((this-command 'org-return-indent) (current-prefix-arg nil))
    (should-not (org-graph--maybe-insert-attribute-row t nil nil))))

;;; C-u RET on #+graph:, #+node:, #+edge:, and #+name: lines

(defmacro org-graph-test--answering (key value &rest body)
  "Runs BODY with the attribute prompt answering KEY, and the value prompt VALUE; returns the attributes offered."
  (declare (indent 2))
  `(let (offered)
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt choices &rest _)
                  (if (string-match-p "Attribute" prompt)
                      (progn (setq offered (--map (car (s-split " — " it)) choices))
                             (--first (equal (car (s-split " — " it)) ,key) choices))
                    ,value)))
               ((symbol-function 'read-string) (lambda (&rest _) ,value))
               ((symbol-function 'org-graph--pick-color) (lambda () nil)))
       ,@body)
     offered))

(org-deftest "C-u RET on a lone #+edge: line offers edge attributes alone, and appends the pair to the line"
  :text "
#+begin_graph
#+edge: fontsize=10
+ a
  + b
#+end_graph"
  :at "#+edge:"
  (let ((offered (org-graph-test--answering "color" "red" (org-graph--insert-attribute-row '(4)))))
    (should (member "arrowhead" offered))
    (should-not (member "shape" offered)))
  (should (s-contains-p "#+edge: fontsize=10 color=red\n" (buffer-string))))

(org-deftest "C-u RET on a #+node: line with a table beneath adds the row to that table"
  :text "
#+begin_graph
#+node:
| shape | box |
+ a
#+end_graph"
  :at "#+node:"
  (org-graph-test--answering "style" "filled" (org-graph--insert-attribute-row '(4)))
  (should (s-contains-p "#+node:\n| shape | box    |\n| style | filled |\n+ a" (buffer-string))))

(org-deftest "C-u RET on a #+graph: line quotes a value of several words"
  :text "
#+begin_graph
#+graph:
+ a
#+end_graph"
  :at "#+graph:"
  (org-graph-test--answering "label" "Our \"best\" graph" (org-graph--maybe-insert-attribute-row nil '(4) t))
  (should (s-contains-p "#+graph: label=\"Our \\\"best\\\" graph\"\n" (buffer-string))))

(org-deftest "C-u RET on a #+name: template, in a graph or out, offers node and edge attributes"
  :text "
#+name: hot color=red

#+begin_graph
#+name: faint
+ a :hot:faint:
#+end_graph"
  :at "#+name: hot"
  (let ((offered (org-graph-test--answering "penwidth" "2" (org-graph--insert-attribute-row '(4)))))
    (should (member "arrowhead" offered))
    (should (member "shape" offered)))
  (should (s-contains-p "#+name: hot color=red penwidth=2\n" (buffer-string)))
  (search-forward "#+name: faint")
  (should (eq (org-graph--directive-at-point) 'templates)))

(org-deftest "outside a graph, a #+name: of a data table is no template: C-u RET stays Org's"
  :text "
#+name: data
| x | 1 | 2 |"
  :at "#+name: data"
  (should-not (org-graph--directive-at-point))
  (should-not (org-graph--maybe-insert-attribute-row nil '(4) t)))

(org-deftest "eldoc on a directive line hints that C-u RET adds an attribute; on a KEY=VALUE pair, it describes the key"
  :text "
#+begin_graph
#+edge: color=red
+ a
#+end_graph"
  :at "#+edge:"
  (let (doc)
    (org-graph--eldoc (lambda (d &rest _) (setq doc d)))
    (should (s-contains-p "C-u RET" doc))
    (should (s-contains-p "edges" doc))
    (search-forward "colo")
    (org-graph--eldoc (lambda (d &rest _) (setq doc d)))
    (should (equal doc (org-graph--key-doc "color")))))

(org-deftest "a container's table styles its box: C-u RET offers container attributes, such as pencolor, not shape"
  :text "
#+begin_graph
+ Front end :container:
  | bgcolor | grey |
  + lexer
#+end_graph"
  :at "| bgcolor"
  (should (eq (org-graph--table-context) 'containers))
  (let ((offered (org-graph-test--answering "pencolor" "red" (org-graph--insert-attribute-row '(4)))))
    (should (member "pencolor" offered))
    (should-not (member "shape" offered)))
  (should (s-contains-p "| pencolor | red  |" (buffer-string))))

(org-deftest "strictly, a | shape | cluster | row makes an item's table a container's, too"
  :text "
#+begin_graph
+ Front end
  | shape | cluster |
  + lexer
#+end_graph"
  :at "| shape"
  (should (eq (org-graph--table-context) 'containers)))
