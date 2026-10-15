;;; org-graph-cc-handler-tests.el --- org-graph--cc-handler — C-u / :C-c_C-c dot XOR logic  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; org-graph--cc-handler — C-u / :C-c_C-c dot XOR logic

;; Shared macro: stub out the org-special-block-* functions and call-process
;; so tests run without graphviz or a real org export pipeline.
(defmacro tests-graph--with-cc-stubs (dot-source kwdargs &rest body)
  "Evaluate BODY with org-special-block-* and call-process stubbed.
DOT-SOURCE is the string returned by `org--graph-raw->dot'.
KWDARGS is the plist for the block's keyword arguments."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'org-special-block-after-point)
              (lambda (_)
                (make-org-special-block
                 :kwdargs  ,kwdargs
                 :contents "
+ A"
                 :main-arg ""
                 :end-point (point-max))))
             ((symbol-function 'org--graph-raw->dot)             (lambda (&rest _) ,dot-source))
             ((symbol-function 'call-process)                    (lambda (&rest _) 0))
             ((symbol-function 'write-region)                    (lambda (&rest _) nil))
             ((symbol-function 'make-temp-file)                  (lambda (&rest _) "/tmp/fake.dot"))
             ((symbol-function 'insert-file-contents)            (lambda (&rest _) nil))
             ((symbol-function 'delete-file)                     (lambda (&rest _) nil))
             ((symbol-function 'generate-new-buffer)             (lambda (&rest _) (get-buffer-create " *org-graph-err-stub*")))
             ((symbol-function 'org-display-inline-images)       (lambda (&rest _) nil))
             ((symbol-function 'org-graph--insert-results)       (lambda (&rest _) nil)))
     ,@body))

(org-deftest "`org-graph--cc-handler' plain C-c C-c renders PNG (no dot buffer)"
  :text "
#+begin_graph
+ A
#+end_graph"
  :at "+ A"
  (tests-graph--with-cc-stubs "digraph { A; }" nil
    (let ((current-prefix-arg nil)
          (dot-shown nil))
      (cl-letf (((symbol-function 'org-graph--show-dot) (lambda (_) (setq dot-shown t))))
        (org-graph--cc-handler)
        (should-not dot-shown)))))

(org-deftest "`org-graph--cc-handler' C-u C-c C-c shows dot source buffer"
  :text "
#+begin_graph
+ A
#+end_graph"
  :at "+ A"
  (tests-graph--with-cc-stubs "digraph { A; }" nil
    (let ((current-prefix-arg '(4))
          (dot-received nil))
      (cl-letf (((symbol-function 'org-graph--show-dot) (lambda (d) (setq dot-received d))))
        (org-graph--cc-handler)
        (should (equal "digraph { A; }" dot-received))))))

(org-deftest "`org-graph--cc-handler' :C-c_C-c dot without prefix shows dot"
  :text "
#+begin_graph
+ A
#+end_graph"
  :at "+ A"
  (tests-graph--with-cc-stubs "digraph { A; }" '(:C-c_C-c "dot")
    (let ((current-prefix-arg nil)
          (dot-received nil))
      (cl-letf (((symbol-function 'org-graph--show-dot) (lambda (d) (setq dot-received d))))
        (org-graph--cc-handler)
        (should (equal "digraph { A; }" dot-received))))))

(org-deftest "`org-graph--cc-handler' reads :C-c_C-c dot from a real header line, where it is the symbol `dot', not a string"
  :text "
#+begin_graph :C-c_C-c dot
+ A
#+end_graph"
  :at "+ A"
  (let ((current-prefix-arg nil) (dot-received nil))
    (cl-letf (((symbol-function 'org-graph--show-dot) (lambda (d) (setq dot-received d)))
              ((symbol-function 'org-graph--run-dot) (lambda (&rest _) (error "Rendered a PNG, rather than showing dot"))))
      (org-graph--cc-handler)
      (should (s-starts-with-p "digraph" dot-received)))))

(org-deftest "`org-graph--cc-handler' reads :file from a real header line, where it is a symbol, as the PNG's file name"
  :text "
#+begin_graph :file my-diagram.png
+ A
#+end_graph"
  :at "+ A"
  (let ((current-prefix-arg nil) (outfile nil))
    (cl-letf (((symbol-function 'org-graph--run-dot) (lambda (_dot _format file) (setq outfile file) '(0 . "")))
              ((symbol-function 'org-display-inline-images) #'ignore))
      (org-graph--cc-handler)
      (should (equal "my-diagram.png" outfile)))))

(org-deftest "`org-graph--cc-handler' :C-c_C-c dot WITH prefix toggles back to PNG"
  :text "
#+begin_graph
+ A
#+end_graph"
  :at "+ A"
  (tests-graph--with-cc-stubs "digraph { A; }" '(:C-c_C-c "dot")
    (let ((current-prefix-arg '(4))
          (dot-shown nil))
      (cl-letf (((symbol-function 'org-graph--show-dot) (lambda (_) (setq dot-shown t))))
        (org-graph--cc-handler)
        (should-not dot-shown)))))

(org-deftest "`org-graph--cc-handler' C-u inserts #+begin_src dot results block"
  :text "
#+begin_graph
+ A
#+end_graph"
  :at "+ A"
  ;; Override end-point to insert after current content (not point-max stub)
  (cl-letf (((symbol-function 'org-special-block-after-point)
             (lambda (_)
               (make-org-special-block
                :kwdargs  nil
                :contents "
+ A"
                :main-arg ""
                :end-point (save-excursion
                             (goto-char (point-max))
                             (search-backward "#+end_graph")
                             (1- (point))))))
            ((symbol-function 'org--graph-raw->dot)             (lambda (&rest _) "digraph { A; }"))
            ((symbol-function 'org-graph--show-dot)             (lambda (_) nil)))
    (let ((current-prefix-arg '(4)))
      (org-graph--cc-handler)))
  (let ((contents (buffer-string)))
    (should (s-contains-p "#+RESULTS:" contents))
    (should (s-contains-p "#+begin_src dot" contents))
    (should (s-contains-p "digraph { A; }" contents))
    (should (s-contains-p "#+end_src" contents))))
