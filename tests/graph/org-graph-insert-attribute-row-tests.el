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
