;;; org-demo-tests.el --- Tests for the :demo switch: a demonstration, a test in the small, a suite in the large  -*- lexical-binding: t; -*-

(unless (fboundp 'using)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

;;; C-c C-c: taking a snapshot

(org-deftest "C-c C-c on a :demo block snapshots what it renders to, along HTML and LaTeX, stamped by its source" [demo]
  :text "
#+begin_box Title :demo t
Hi *there*
#+end_box"
  :at "Hi"
  (org-ctrl-c-ctrl-c)
  (should (string-match-p (format "#\\+end_box\n\n#\\+RESULTS\\[%s\\]:\n:results:\n#\\+begin_export html\n"
                                  (org-demo--hash "#+begin_box Title\nHi *there*\n#+end_box"))
                          (buffer-string)))
  (should (s-contains? "Hi <b>there</b>" (buffer-string)))
  (should (s-contains? "#+begin_export latex\n\\colorlet" (buffer-string)))
  (should (string-suffix-p "#+end_export\n:end:" (buffer-string))))

(org-deftest "C-c C-c on a :demo block replaces its snapshot, not adds another" [demo]
  :text "
#+begin_box :demo t
Hi
#+end_box"
  :at "Hi"
  (org-ctrl-c-ctrl-c)
  (search-forward "<p>\nHi") (replace-match "<p>\nHo")
  (goto-char (point-min)) (search-forward "Hi")
  (org-ctrl-c-ctrl-c)
  (should (= 1 (s-count-matches "#\\+RESULTS" (buffer-string))))
  (should-not (s-contains? "Ho" (buffer-string))))

(org-deftest "C-c C-c on a live demo takes no snapshot, as it renders afresh each export" [demo]
  :text "
#+begin_box :demo live
Hi
#+end_box"
  :at "Hi"
  (should (org-ctrl-c-ctrl-c))
  (should-not (s-contains? "#+RESULTS" (buffer-string))))

;;; Upon export: a demonstration, and a test in the small

(defun org-demo-tests--snapshotted (text)
  "TEXT ↦ TEXT with each of its demos snapshotted, as C-c C-c on each would."
  (with-org-test-buffer (:text text)
    (org-demo-refresh-buffer)
    (buffer-string)))

(deftest "a demo exports as What You Write, its source sans switches, then What You Get, its snapshot" [demo]
  (let ((html (org-export-string (org-demo-tests--snapshotted "
#+begin_box Title :demo t
Hi *there*
#+end_box"))))
    (should (string-match-p (concat "﴾What You Write﴿ </h6> <pre class=\"src src-org\">#\\+begin_box Title\nHi \\*there\\*\n#\\+end_box</pre>"
                                    ".*﴾What You Get﴿ </h6> <div style=[^>]*> <h3>Title</h3>\\s-*<p>\\s-*Hi <b>there</b>")
                            html))
    (should-not (s-contains? "RESULTS" html))))

(deftest "a demo shows its snapshot, not its rendering now, when its source is as it was" [demo]
  ;; As when the blog renders by a stale MELPA build of ours, which renders otherwise than the snapshot.
  (let* ((org (s-replace "<p>\nHi" "<p>\nPinned" (org-demo-tests--snapshotted "
#+begin_box :demo t
Hi
#+end_box")))
         (org-demo-check nil))
    (should (s-contains? "Pinned" (org-export-string org)))))

(deftest "a demo whose source changed since its snapshot shows its rendering now" [demo]
  (let* ((org (s-replace "\nHi\n" "\nAnew\n" (org-demo-tests--snapshotted "
#+begin_box :demo t
Hi
#+end_box")))
         (org-demo-check nil))
    (should (string-match-p "﴾What You Get﴿ </h6> <div[^>]*> <h3></h3>\\s-*<p>\\s-*Anew" (org-export-string org)))))

(deftest "an export reports a demo that renders otherwise than its snapshot, as org-demo-check says" [demo]
  (let ((org (s-replace "<p>\nHi" "<p>\nHo" (org-demo-tests--snapshotted "
#+begin_box :demo t
Hi
#+end_box"))))
    (let ((org-demo-check 'error))
      (should (equal (cadr (should-error (org-export-string org) :type 'user-error))
                     "The box :demo on line 3 renders to html otherwise than its snapshot; C-c C-c on it takes a new one, if that is intended")))
    ;; Only the export's own backend is checked: the LaTeX snapshot is as rendered.
    (let ((org-demo-check 'error))
      (should (org-export-string org 'latex)))))

(deftest "an export reports a demo whose source changed since its snapshot" [demo]
  (let ((org (s-replace "\nHi\n" "\nAnew\n" (org-demo-tests--snapshotted "
#+begin_box :demo t
Hi
#+end_box")))
        (org-demo-check 'error))
    (should (equal (cadr (should-error (org-export-string org) :type 'user-error))
                   "The box :demo on line 3 changed since its snapshot; C-c C-c on it takes a new one"))))

(deftest "with :ignore t, a demo's source is its contents alone; a comma escapes a heading, as in an example block" [demo]
  (let ((html (org-export-string "
#+begin_org :ignore t :demo t
,* A heading
/italics/
#+end_org")))
    (should (s-contains? "<pre class=\"src src-org\">* A heading\n/italics/</pre>" html))
    (should (string-match-p "﴾What You Get﴿ </h6> <div id=\"outline-container-org0000001\" class=\"outline-2\">\n<h2 id=\"org0000001\">A heading</h2>" html))))

(deftest "a demo's other switches apply to it; :let to its source, :folded to the whole showing" [demo]
  (let ((html (org-export-string "
#+begin_box :demo t :let (WHO \"Jasim\") :folded \"Peek\"
Hi WHO
#+end_box")))
    (should (string-match-p "<summary>.*Peek.*</summary>\n<div><div[^>]*> <h6[^>]*> ﴾What You Write﴿ </h6> <pre class=\"src src-org\">#\\+begin_box\nHi Jasim\n" html))))

(deftest "an inner demo's snapshot is not part of its outer's source, nor shown in it" [demo]
  (let ((html (org-export-string (org-demo-tests--snapshotted "
#+begin_org :ignore t :demo t
#+begin_tip :demo t
Inner.
#+end_tip
#+end_org"))))
    (should (s-contains? "<pre class=\"src src-org\">#+begin_tip :demo t\nInner.\n#+end_tip</pre>" html))
    (should-not (s-contains? "RESULTS" html))))

(deftest "a demo naming a symbol folds away its definition, wherever it is defined" [demo]
  (let ((html (org-export-string "
#+begin_box :demo org-block/box
Hi
#+end_box")))
    (should (string-match-p "﴾How It’s Implemented﴿</summary> <div class=\"org-src-container\">\n<pre class=\"src src-emacs-lisp\"><code>(.*org-defblock.* box (.*&amp;optional.* (title" html))))

(org-deftest "a symbol's definition is found in its file, else in the current buffer; a primitive's, nowhere" [demo]
  :text "
#+begin_src emacs-lisp
(defun org-demo-tests--nowhere-but-here (x) x)
#+end_src"
  (should (string-prefix-p "(defun s-trim (s)" (org-demo--definition 's-trim)))
  (should (string-prefix-p "(defcustom org-demo-check 'warn" (org-demo--definition 'org-demo-check)))
  (should (equal "(defun org-demo-tests--nowhere-but-here (x) x)" (org-demo--definition 'org-demo-tests--nowhere-but-here)))
  (should-not (org-demo--definition 'mapcar)))

;;; In the large: every demo of a file

(org-deftest "org-demo-failures names each demo that renders otherwise than its snapshot, else is nil" [demo]
  :text "
#+begin_box :demo t
Hi
#+end_box

#+begin_box :demo t
Bye
#+end_box"
  (org-demo-refresh-buffer)
  (should-not (org-demo-failures))
  (search-forward "<p>\nBye") (replace-match "<p>\nHo")
  (should (equal (org-demo-failures)
                 (list (format "The box :demo on line %s renders to html otherwise than its snapshot; C-c C-c on it takes a new one, if that is intended"
                               (progn (goto-char (point-min)) (search-forward "Bye") (1- (line-number-at-pos))))))))

(deftest "every demo of the manual renders as its snapshot" [demo manual]
  (should-not (org-demo-failures (expand-file-name "~/org-special-block-extras/org-special-block-extras.org"))))

(deftest "every demo of tests/demos.org, those that are tests rather than docs, renders as its snapshot" [demo]
  (should-not (org-demo-failures (expand-file-name "~/org-special-block-extras/tests/demos.org"))))

(provide 'org-demo-tests)
;;; org-demo-tests.el ends here
