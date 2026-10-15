;;; org-graph-dispatcher-tests.el --- Dispatcher integration — C-c C-c and C-c '  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; Dispatcher integration — C-c C-c and C-c '

(deftest "org--block-cc-dispatcher routes C-c C-c into a graph block to org-graph--cc-handler"
  ;; This tests the full dispatch chain: org--block-cc-dispatcher →
  ;; org-special-block-at-point-p → org-graph--cc-handler.
  ;; We stub org-graph--cc-handler to record that it was called.
  (let ((buf (tests-graph--make-graph-buffer "
+ A")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "+ A")
      (let ((handler-called nil))
        (cl-letf (((symbol-function 'org-graph--cc-handler)
                   (lambda () (setq handler-called t) t)))
          (org--block-cc-dispatcher)
          (should handler-called))))
    (kill-buffer buf)))

(deftest "org--block-edit-special-dispatcher routes C-c ' into a graph block to org-graph--do-edit-special"
  ;; This tests the full dispatch chain: org--block-edit-special-dispatcher →
  ;; org-special-block-at-point-p → org-graph--do-edit-special.
  ;; We stub org-graph--do-edit-special to record that it was called.
  (let ((buf (tests-graph--make-graph-buffer "
+ A")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "+ A")
      (let ((handler-called nil))
        (cl-letf (((symbol-function 'org-graph--do-edit-special)
                   (lambda () (setq handler-called t) t)))
          (org--block-edit-special-dispatcher)
          (should handler-called))))
    (kill-buffer buf)))
