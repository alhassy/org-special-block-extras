;;; org-defblock-only-cl-defun-style-tests.el --- `org-defblock-only' ---cl-defun-style syntax, and its `parse-args' prototype

(unless (fboundp 'using)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

;;; org-defblock-only -- cl-defun-style
;; (ert '(tag "cl-defun-style"))

(cl-defun signature (f)
  "Get signature of method F, a symbol."
  (car (last (s-split "\n" (cl-fifth (seq--into-list (symbol-function f)))))))


(deftest "cl-defun-style invocation of `org-defblock-only' may have `nil' args"
  (using (org-defblock-only folded () (format "<details>%s</details>" contents))
    (should (equal (signature 'org-block/folded)
                   "(fn BACKEND RAW-CONTENTS &optional MAIN-ARG &rest ## &key (CONTENTS-OCCUR-AS-LINK-DESCRIPTION nil) &allow-other-keys)"))))


(deftest "cl-defun-style invocation of `org-defblock-only' may have `&optional' main arg"
  :expected-result :failed
  (using (org-defblock-only folded (&optional title) (format "<details>%s</details>" contents))
    (should (equal (signature 'org-block/folded)
                   "(fn BACKEND RAW-CONTENTS &optional title &rest ## &key (CONTENTS-OCCUR-AS-LINK-DESCRIPTION nil) &allow-other-keys)"))))


(deftest "cl-defun-style invocation of `org-defblock-only' may have `&optional' main arg with default value"
  :expected-result :failed
  (using (org-defblock-only folded (&optional (title "Click to see more!"))
           (format "<details><summary>%s</summary>%s</details>" title contents))
    (should (equal (signature 'org-block/folded)
                   "(fn BACKEND RAW-CONTENTS &optional (title \"Click to see more!\") &rest ## &key (CONTENTS-OCCUR-AS-LINK-DESCRIPTION nil) &allow-other-keys)"))))


(deftest "cl-defun-style invocation of `org-defblock-only' may have `&key' keyword args"
  :expected-result :failed
  (eval '(using (org-defblock-only folded (&key style class)
           (format "<details style=\"%s\" class=\"%s\">%s</details>" style class contents))
    (should (equal (signature 'org-block/folded)
                   "(fn BACKEND RAW-CONTENTS &rest ## &key STYLE CLASS (CONTENTS-OCCUR-AS-LINK-DESCRIPTION nil) &allow-other-keys)")))))


(deftest "cl-defun-style invocation of `org-defblock-only' may have `&key' keyword args with default values₁"
  :expected-result :failed
  (eval '(using (org-defblock-only folded (&key (style "color:blue") class)
           (format "<details style=\"%s\" class=\"%s\">%s</details>" style class contents))
    (should (equal (signature 'org-block/folded)
                   "(fn BACKEND RAW-CONTENTS &rest ## &key (STYLE \"color:blue\") CLASS (CONTENTS-OCCUR-AS-LINK-DESCRIPTION nil) &allow-other-keys)")))))


(deftest "cl-defun-style invocation of `org-defblock-only' may have `&key' keyword args with default values₂"
  :expected-result :failed
  (eval '(using (org-defblock-only folded (&key style (class "note"))
           (format "<details style=\"%s\" class=\"%s\">%s</details>" style class contents))
    (should (equal (signature 'org-block/folded)
                   "(fn BACKEND RAW-CONTENTS &rest ## &key STYLE (CLASS \"note\") (CONTENTS-OCCUR-AS-LINK-DESCRIPTION nil) &allow-other-keys)")))))


(deftest "cl-defun-style invocation of `org-defblock-only' may have `&key' keyword args with default values₃"
  :expected-result :failed
  (eval '(using (org-defblock-only folded (&key (style "color:blue") (class "note"))
           (format "<details style=\"%s\" class=\"%s\">%s</details>" style class contents))
    (should (equal (signature 'org-block/folded)
                   "(fn BACKEND RAW-CONTENTS &rest ## &key (STYLE \"color:blue\") (CLASS \"note\") (CONTENTS-OCCUR-AS-LINK-DESCRIPTION nil) &allow-other-keys)")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq args-list '(&optional c))
(--partition-by (member it '(&optional &key)) args-list)


(cl-defun parse-args (args-list)
  (cond
   ((-contains? args-list '&optional)
    (-let [((backend-name contents-name) (main-arg . more-args)) (-split-on '&optional args-list)]
      (list :backend-name  backend-name
            :contents-name contents-name
            :raw-contents-name (intern (format "raw-%s" contents-name))
            :main-arg-name (if (listp main-arg) (car main-arg) main-arg)
            :main-arg-value (if (listp main-arg) (cadr main-arg) nil)
            :keywords (when more-args
                        (if (equal '&key (car more-args))
                            (cdr more-args)
                          (error "“org-defblock[-only]” expected to see “&key” but saw “%s”" (car more-args)))))))
   (:otherwise
    (list :backend-name  'backend
          :contents-name 'contents
          :raw-contents-name 'raw-contents
          :main-arg-name (caar args-list)
          :main-arg-value (cadar args-list)
          :keywords (cdr args-list)))))

;; (ert '(tag "parse-args"))


(deftest "`parse-args' works as intended in terse style"
  (should (equal (parse-args '((title "Details") (style nil) (class nil)))
                 '( :backend-name backend
                    :contents-name contents
                    :raw-contents-name raw-contents
                    :main-arg-name title
                    :main-arg-value "Details"
                    :keywords ((style nil) (class nil))))))

(deftest "`parse-args' works as intended when there is an `&optional' clause"
  (should (equal (parse-args '(a b &optional c &key d))
                 '( :backend-name a
                    :contents-name b
                    :raw-contents-name raw-b
                    :main-arg-name c
                    :main-arg-value nil
                    :keywords (d)))))

(deftest "`parse-args' works as intended when there is an `&optional' clause and no `&key' clause"
  (should (equal (parse-args '(a b &optional c))
                 '( :backend-name a
                    :contents-name b
                    :raw-contents-name raw-b
                    :main-arg-name c
                    :main-arg-value nil
                    :keywords nil))))

(deftest "`parse-args' works as intended when there is an `&optional' clause with main-arg default value"
  (should (equal (parse-args '(a b &optional (c c-default) &key d))
                 '( :backend-name a
                    :contents-name b
                    :raw-contents-name raw-b
                    :main-arg-name c
                    :main-arg-value c-default
                    :keywords (d)))))

(deftest "`parse-args' works as intended when there is an `&optional' clause with main-arg default value and no `&key' clause"
  (should (equal (parse-args '(a b &optional (c c-default)))
                 '( :backend-name a
                    :contents-name b
                    :raw-contents-name raw-b
                    :main-arg-name c
                    :main-arg-value c-default
                    :keywords nil))))


;; TODO: WORKING HERE: Try to make this pass 😄
(deftest "`parse-args' works as intended when there is only an `&optional' clause"
  :expected-result :failed
  (should (equal (parse-args '(&optional (c c-default)))
                 '( :backend-name backend
                    :contents-name contents
                    :raw-contents-name raw-contents
                    :main-arg-name c
                    :main-arg-value c-default
                    :keywords nil))))
