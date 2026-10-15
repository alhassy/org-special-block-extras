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
                 :main-arg "directed"
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

(deftest "`org-graph--cc-handler' plain C-c C-c renders PNG (no dot buffer)"
  (let ((buf (tests-graph--make-graph-buffer "
+ A")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "+ A")
      (tests-graph--with-cc-stubs "digraph { A; }" nil
        (let ((current-prefix-arg nil)
              (dot-shown nil))
          (cl-letf (((symbol-function 'org-graph--show-dot) (lambda (_) (setq dot-shown t))))
            (org-graph--cc-handler)
            (should-not dot-shown)))))
    (kill-buffer buf)))

(deftest "`org-graph--cc-handler' C-u C-c C-c shows dot source buffer"
  (let ((buf (tests-graph--make-graph-buffer "
+ A")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "+ A")
      (tests-graph--with-cc-stubs "digraph { A; }" nil
        (let ((current-prefix-arg '(4))
              (dot-received nil))
          (cl-letf (((symbol-function 'org-graph--show-dot) (lambda (d) (setq dot-received d))))
            (org-graph--cc-handler)
            (should (equal "digraph { A; }" dot-received))))))
    (kill-buffer buf)))

(deftest "`org-graph--cc-handler' :C-c_C-c dot without prefix shows dot"
  (let ((buf (tests-graph--make-graph-buffer "
+ A")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "+ A")
      (tests-graph--with-cc-stubs "digraph { A; }" '(:C-c_C-c "dot")
        (let ((current-prefix-arg nil)
              (dot-received nil))
          (cl-letf (((symbol-function 'org-graph--show-dot) (lambda (d) (setq dot-received d))))
            (org-graph--cc-handler)
            (should (equal "digraph { A; }" dot-received))))))
    (kill-buffer buf)))

(deftest "`org-graph--cc-handler' :C-c_C-c dot WITH prefix toggles back to PNG"
  (let ((buf (tests-graph--make-graph-buffer "
+ A")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "+ A")
      (tests-graph--with-cc-stubs "digraph { A; }" '(:C-c_C-c "dot")
        (let ((current-prefix-arg '(4))
              (dot-shown nil))
          (cl-letf (((symbol-function 'org-graph--show-dot) (lambda (_) (setq dot-shown t))))
            (org-graph--cc-handler)
            (should-not dot-shown)))))
    (kill-buffer buf)))

(deftest "`org-graph--cc-handler' C-u inserts #+begin_src dot results block"
  (let ((buf (tests-graph--make-graph-buffer "
+ A")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "+ A")
      ;; Override end-point to insert after current content (not point-max stub)
      (cl-letf (((symbol-function 'org-special-block-after-point)
                 (lambda (_)
                   (make-org-special-block
                    :kwdargs  nil
                    :contents "
+ A"
                    :main-arg "directed"
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
    (kill-buffer buf)))
