;;; org-remark-block-tests.el --- Tests for the remark block  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(deftest "remark block renders content when org-hide-editor-comments is nil."
  (let ((org-hide-editor-comments nil)
        (result (org-block/remark 'html "This is a remark." nil)))
    (should (string-match-p "remark" (downcase result)))))

(deftest "remark block produces no visible content when org-hide-editor-comments is t."
  (let ((org-hide-editor-comments nil))
    (setq org-hide-editor-comments t)
    (let ((result (org-block/remark 'html "This is a remark." nil)))
      (setq org-hide-editor-comments nil)
      (should-not (string-match-p "This is a remark" result)))))

(provide 'org-remark-block-tests)
;;; org-remark-block-tests.el ends here
