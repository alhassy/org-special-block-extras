;;; org-demo.el --- The :demo switch: a block beside what it exports to, and a test that it still does  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.18
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; By way of example, C-c C-c within
;;
;;     #+begin_box Title :demo t
;;     Hi *there*
;;     #+end_box
;;
;; snapshots what the box exports to just after it, as Babel does a src block's results:
;;
;;     #+RESULTS[3f1c0a92be]:
;;     :results:
;;     #+begin_export html
;;     <div style="padding: 1em; …"> <h3>Title</h3> <p> Hi <b>there</b> </p> </div>
;;     #+end_export
;;     #+begin_export latex
;;     …
;;     #+end_export
;;     :end:
;;
;; Upon export, the block shows as ﴾What You Write﴿, its source, then ﴾What You Get﴿, its snapshot.  So a demo is
;; three things at once:
;;
;; + In the docs, a demonstration: a block's source, then what it renders to.
;; + In the small, a test: each export checks the block still renders as its snapshot; see `org-demo-check'.
;; + In the large, a test suite: `org-demo-failures' checks every demo of a file, along every snapshotted backend:
;;       (ert-deftest manual () (should-not (org-demo-failures "manual.org")))
;;
;; Its value says what else to show: `:demo t' nothing; `:demo org-block/box' the definition of `org-block/box',
;; folded away; `:demo (f v)' those of the function f and variable v.  A `live' demo, `:demo live', keeps no
;; snapshot: its rendering varies by machine, say, so it is rendered afresh each export and checks nothing.
;; With `:ignore t', the source is just the contents, sans the #+begin_ and #+end_ lines; hence
;;
;;     #+begin_org :ignore t :demo t
;;     /italics/ and _underline_
;;     #+end_org
;;
;; demonstrates plain Org.  As in an example block, a contents line starting with “*” or “#+” may be escaped by
;; a comma, so a demo may hold headings.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'find-func)
(require 'org)
(require 'org-element)
(require 'org-deflink)
(require 'org-block-switches)
(require 'org-block-export)
(require 'org-box-block)

(defcustom org-demo-backends '(html latex)
  "The backends along which C-c C-c snapshots what a :demo block renders to; see `org-demo-refresh'."
  :type '(repeat symbol)
  :group 'org-special-block-extras)

(defcustom org-demo-check 'warn
  "What an export does when a :demo block no longer renders as its snapshot.

+ `error': stop the export, naming the block.
+ `warn': warn, naming the block, and show the snapshot all the same.
+ nil: neither.

The snapshot is shown, unless the block's source changed since it was taken; then, its rendering now is."
  :type '(choice (const :tag "Stop the export" error) (const :tag "Warn" warn) (const :tag "Neither" nil))
  :group 'org-special-block-extras)

;;;; A demo's source, and what it renders to

(defun org-demo--site-p (site)
  "SITE, from `org--block-switch-site', ↦ whether its block is a :demo one."
  (and site (plist-get (nth 5 site) :demo)))

(defun org-demo--live-p (site)
  "SITE ↦ whether its demo is a `live' one, keeping no snapshot; e.g., `:demo live' or `:demo (live f)'."
  (memq 'live (ensure-list (plist-get (nth 5 site) :demo))))

(defun org-demo--source (site backend)
  "(SITE, BACKEND) ↦ its block as written sans switches; for :ignore, just its contents.
Any :let applies to the contents, for BACKEND; commas escaping their lines are dropped, as in an example block;
and so are the snapshots of inner demos, which are our bookkeeping, not what the writer wrote."
  (pcase-let* ((`(,_ ,_ ,_ ,name ,header ,switches) site)
               (contents (let ((case-fold-search t))
                           (replace-regexp-in-string org-demo--snapshot-regexp "" (org--block-switch-site-contents site))))
               (contents (-if-let (bindings (plist-get switches :let)) (org--block-let bindings contents backend) contents)))
    (org-unescape-code-in-string
     (if (plist-get switches :ignore) contents (org--block-source name (string-trim-right contents) header)))))

(defvar org-demo--heads nil
  "What the blocks of the last `org-demo--export' need atop the document: (LATEX-HEADS HTML-HEADS).")

(defun org-demo--export (source backend)
  "(SOURCE, BACKEND) ↦ Org SOURCE's body-only BACKEND export, as a document of its own, the same on every machine:
headings sans table of contents or numbers, src blocks sans htmlize's colours, `gensym' and Org's ids numbered.
What its blocks need atop the document is left in `org-demo--heads'."
  (let ((seed 0)
        (org-html-htmlize-output-type nil)
        (org-html-prefer-user-labels t)
        (org-export-with-toc nil)
        (org-export-with-section-numbers nil)
        ;; Last of all, as the blocks have rendered by then.
        (org-export-before-parsing-functions
         (append org-export-before-parsing-functions
                 (list (lambda (_) (setq org-demo--heads (list org--latex-head-used org--html-head-used)))))))
    (setq org-demo--heads nil)
    (cl-letf (((symbol-function 'gensym) (lambda (&rest _) (format "g%s" (cl-incf seed))))
              ((symbol-function 'org-export-new-reference) (lambda (references) (1+ (length references)))))
      (string-trim (org-export-string-as (format "\n%s\n" source) backend t)))))

(defun org-demo--hash (source)
  "SOURCE ↦ the hash its snapshot is stamped with, as #+RESULTS[HASH]:, so that a changed source is told apart."
  (substring (secure-hash 'sha1 source) 0 10))

(defun org-demo--normalise (s)
  "S ↦ S without whitespace, downcased; renderings are compared so."
  (downcase (replace-regexp-in-string "[ \t\n\r]+" "" s)))

;;;; The snapshot: a #+RESULTS: drawer of export blocks, one per backend

(defconst org-demo--snapshot-regexp
  "\n+[ \t]*#\\+results\\[[[:xdigit:]]\\{10\\}\\]:[ \t]*\n[ \t]*:results:[ \t]*\n\\(?:.*\n\\)*?[ \t]*:end:[ \t]*$"
  "A snapshot, as `org-demo--refresh-site' writes it; its hash of 10 digits tells it apart from Babel's of 40.")

(defun org-demo--snapshot (site)
  "SITE ↦ (END HASH RENDERINGS) of the #+RESULTS: drawer just after its block, if any; else nil.
END is the end of its :end: line; RENDERINGS is an alist (BACKEND . TEXT), from its export blocks."
  (save-excursion
    (goto-char (nth 1 site))
    (skip-chars-forward " \t\n")
    (beginning-of-line)
    (let ((case-fold-search t))
      (when (looking-at "[ \t]*#\\+results\\(?:\\[\\([[:xdigit:]]*\\)\\]\\)?:[ \t]*\n[ \t]*:results:[ \t]*$")
        (let ((hash (match-string 1)) (begin (match-end 0)))
          (when (re-search-forward "^[ \t]*:end:[ \t]*$" nil t)
            (let ((end (match-end 0)) renderings)
              (goto-char begin)
              (while (re-search-forward "^[ \t]*#\\+begin_export[ \t]+\\(\\S-+\\)[ \t]*\n\\(\\(?:.*\n\\)*?\\)[ \t]*#\\+end_export" end t)
                (push (cons (intern (downcase (match-string 1))) (string-trim (org-unescape-code-in-string (match-string 2))))
                      renderings))
              (list end hash (nreverse renderings)))))))))

(defun org-demo--where (site)
  "SITE ↦ how a message names its demo; e.g., “The box :demo on line 7”."
  (format "The %s :demo on line %s" (nth 3 site) (line-number-at-pos (nth 0 site))))

(defun org-demo--problem (site source backend snapshot rendering)
  "↦ how SITE's demo of SOURCE fails its SNAPSHOT along BACKEND, where it now renders to RENDERING; else nil."
  (pcase-let ((`(,_ ,hash ,renderings) snapshot)
              (where (org-demo--where site)))
    (cond ((not (equal hash (org-demo--hash source)))
           (format "%s changed since its snapshot; C-c C-c on it takes a new one" where))
          ((not (equal (org-demo--normalise (alist-get backend renderings)) (org-demo--normalise rendering)))
           (format "%s renders to %s otherwise than its snapshot; C-c C-c on it takes a new one, if that is intended"
                   where backend)))))

(defun org-demo--sites (&optional outermost)
  "↦ the `org--block-switch-site' of each :demo block of the buffer, the last first; if OUTERMOST, sans those within others."
  (let (sites)
    (org-element-map (org-element-parse-buffer 'element) org--switch-block-types
      (lambda (block)
        (let ((site (org--block-switch-site block)))
          (when (org-demo--site-p site) (push site sites)))))
    (if (not outermost) sites
      (--remove (let ((inner it)) (--some (and (< (car it) (car inner)) (<= (nth 1 inner) (nth 1 it))) sites)) sites))))

;;;; In the large: every demo of a file

(defun org-demo-failures (&optional file)
  "FILE, else the current buffer, ↦ a list of each problem of a :demo block that no longer renders as its snapshot.
Every snapshotted backend is checked; a demo sans snapshot, or a `live' one, checks nothing.  By way of example,
  (ert-deftest manual () (should-not (org-demo-failures \"manual.org\")))"
  (if file
      (with-temp-buffer
        (insert-file-contents file)
        (let ((default-directory (file-name-directory (expand-file-name file))) (org-inhibit-startup t))
          (org-mode)
          (org-demo-failures)))
    (cl-loop for site in (reverse (org-demo--sites))
             for snapshot = (and (not (org-demo--live-p site)) (org-demo--snapshot site))
             when snapshot
             append (cl-loop for (backend) in (nth 2 snapshot)
                             for source = (org-demo--source site backend)
                             for problem = (org-demo--problem site source backend snapshot (org-demo--export source backend))
                             when problem collect problem
                             ;; A stale snapshot is stale along every backend; once is enough.
                             until (and problem (not (equal (nth 1 snapshot) (org-demo--hash source))))))))

;;;; C-c C-c: take a demo's snapshot

(defun org-demo--site-at-point ()
  "↦ the `org--block-switch-site' of the innermost :demo block around point; else nil."
  (cl-loop for block = (org-element-lineage (org-element-at-point) org--switch-block-types t)
           then (org-element-lineage block org--switch-block-types)
           while block
           for site = (org--block-switch-site block)
           when (org-demo--site-p site) return site))

(defun org-demo--refresh-site (site)
  "Snapshot what SITE's demo renders to, along each of `org-demo-backends', in a #+RESULTS: drawer after it."
  (pcase-let* ((`(,_ ,end ,indent) site)
               (snapshot (org-demo--snapshot site))
               (blocks (mapconcat (lambda (backend)
                                    (format "#+begin_export %s\n%s\n#+end_export\n" backend
                                            (org-escape-code-in-string
                                             (condition-case err (org-demo--export (org-demo--source site backend) backend)
                                               (error (user-error "%s fails to render: %s" (org-demo--where site)
                                                                  (error-message-string err)))))))
                                  org-demo-backends ""))
               ;; The hash is of the source along the first backend, as `org-demo--problem' checks it so.
               (drawer (format "#+RESULTS[%s]:\n:results:\n%s:end:"
                               (org-demo--hash (org-demo--source site (car org-demo-backends))) blocks)))
    (save-excursion
      (goto-char end)
      (delete-region end (if snapshot (car snapshot) end))
      (insert "\n\n" (replace-regexp-in-string "^" (make-string indent ?\s) drawer)))))

(defun org-demo-refresh ()
  "Snapshot what the :demo block around point renders to, along each of `org-demo-backends', just after it.
A `live' demo keeps none.  ↦ nil when point is in no :demo block, so that C-c C-c is left to others; with a prefix
argument, C-c C-c is too, say for the block's own action."
  (interactive)
  (-when-let (site (and (not current-prefix-arg) (derived-mode-p 'org-mode) (org-demo--site-at-point)))
    (if (org-demo--live-p site)
        (message "A live demo keeps no snapshot; it renders afresh each export")
      (org-demo--refresh-site site)
      (message "Snapshot taken, along %s" (mapconcat #'symbol-name org-demo-backends " and ")))
    t))

(defun org-demo-refresh-buffer ()
  "Snapshot every :demo block of the buffer, as C-c C-c on each would; inner ones first, as they are their outer's source."
  (interactive)
  (let ((sites (--remove (org-demo--live-p it) (org-demo--sites))))
    (dolist (site sites)
      (org-demo--refresh-site site))
    (message "%s snapshots taken" (length sites))))

;; Ahead of other C-c C-c actions: the writer of :demo asked for this one.
(add-hook 'org-ctrl-c-ctrl-c-hook #'org-demo-refresh -10)

;;;; In the small: each export shows, and checks, each demo

(defun org-demo--definition (symbol)
  "SYMBOL ↦ the source of its definition, as a function, variable, or face: from its file, else the current buffer.
A primitive, written in C, has none for us; nor does a symbol that is defined nowhere we can find."
  (unless (subr-primitive-p (symbol-function symbol))
    (or (cl-loop with buffers = (buffer-list)
                 for type in '(nil defvar defface)
                 thereis (pcase (ignore-errors (find-definition-noselect symbol type))
                           (`(,buffer . ,(and pos (pred integerp)))
                            (prog1 (with-current-buffer buffer (save-excursion (goto-char pos) (thing-at-point 'sexp t)))
                              (unless (memq buffer buffers) (kill-buffer buffer))))))
        ;; Say, a src block in the document itself, evaluated by C-c C-c; so it has no file.
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward (format "^[ \t]*([^ \t\n]+[ \t]+'?%s\\_>" (regexp-quote (symbol-name symbol))) nil t)
            (goto-char (match-beginning 0))
            (skip-chars-forward " \t")
            (thing-at-point 'sexp t))))))

(defun org-demo--show (backend source rendering symbols)
  "↦ Org showing SOURCE, then its BACKEND RENDERING, then the definition of each of SYMBOLS, folded away.
In HTML, ﴾What You Write﴿ and ﴾What You Get﴿ are coloured panels; in Markdown, bold labels; else, boxes."
  (let ((definitions (-non-nil (mapcar #'org-demo--definition symbols))))
    (cond
     ((org-export-markdown-p)
      (format "*What You Write*\n#+begin_src org\n%s\n#+end_src\n\n*What You Get*\n\n#+begin_export md\n%s\n#+end_export"
              (org-escape-code-in-string source) (org-escape-code-in-string rendering)))
     ((eq backend 'html)
      (let ((title (lambda (it) (format "<h6 style=\"text-align:center; font-family: Lorna; padding: 0; margin: 0;\"> ﴾%s﴿ </h6>" it)))
            (panel (lambda (colour body)
                     (format "<div style=\"padding: 1em;background-color: %s;border-radius: 15px;font-size: 0.9em;\"> %s </div>"
                             (org-subtle-colors colour) body))))
        (format "#+begin_export html\n%s\n#+end_export"
                (org-escape-code-in-string
                 (concat
                  "<div>"
                  (funcall panel "lime" (format "%s <pre class=\"src src-org\">%s</pre>" (funcall title "What You Write")
                                                (org-html-encode-plain-text source)))
                  (funcall panel "peach" (format "%s %s" (funcall title "What You Get") rendering))
                  "</div>"
                  (when definitions
                    (format "<br> <details style=\"background-color: %s\"><summary style=\"text-align:center; font-family: Lorna; padding: 0; margin: 0; cursor: pointer;\">﴾How It’s Implemented﴿</summary> %s </details>"
                            (org-subtle-colors "custard")
                            (org-export-string-as (format "\n#+begin_src emacs-lisp\n%s\n#+end_src\n" (string-join definitions "\n\n"))
                                                  'html t))))))))
     (t
      (concat (format "#+begin_box What You Write :background-color %s\n#+begin_example\n%s\n#+end_example\n#+end_box\n"
                      (org-subtle-colors "lime") (org-escape-code-in-string source))
              (format "#+begin_box What You Get :background-color %s\n#+begin_export %s\n%s\n#+end_export\n#+end_box"
                      (org-subtle-colors "peach") backend (org-escape-code-in-string rendering))
              (when definitions
                (format "\n#+begin_details How It’s Implemented\n#+begin_src emacs-lisp\n%s\n#+end_src\n#+end_details"
                        (org-escape-code-in-string (string-join definitions "\n\n")))))))))

(defun org-demo--rewrite (site backend)
  "Replace SITE's demo, and its snapshot, by its showing along BACKEND, checked as `org-demo-check' says.
Its other switches then apply to that showing; e.g., :folded t folds the demo away."
  (pcase-let* ((`(,begin ,end ,_ ,name ,header ,switches) site)
               (snapshot (and (not (org-demo--live-p site)) (org-demo--snapshot site)))
               (symbols (--filter (and (symbolp it) (not (memq it '(t live)))) (ensure-list (plist-get switches :demo))))
               ;; The source shows :ignore's effect, so the showing must not have it twice.
               (switches (org-plist-delete (copy-sequence switches) :ignore))
               (markdown? (org-export-markdown-p)))
    (org-replace-text-while-preserving-indentation
     (marker-position begin) (if snapshot (car snapshot) end)
     (org--block-with-switches
      name switches backend (org--block-switch-site-contents site)
      ;; Our own source, not the contents handed us: those have lost the #+begin_ line, and :ignore's effect.
      (lambda (_)
        (let* ((source (org-demo--source site backend))
               (rendering (org-demo--export source (if markdown? 'md backend)))
               (problem (and snapshot (not markdown?) (alist-get backend (nth 2 snapshot)) org-demo-check
                             (org-demo--problem site source backend snapshot rendering))))
          ;; The host document needs what the demo's blocks need atop it.
          (pcase-let ((`(,latex ,html) org-demo--heads))
            (dolist (it latex) (cl-pushnew it org--latex-head-used :test #'equal))
            (dolist (it html) (org-html-head-require it)))
          (when problem
            (if (eq org-demo-check 'error) (user-error "%s" problem) (display-warning 'org-demo problem)))
          (org-demo--show backend source
                          (if (and snapshot (not markdown?) (equal (nth 1 snapshot) (org-demo--hash source)))
                              (or (alist-get backend (nth 2 snapshot)) rendering)
                            rendering)
                          symbols)))
      header))))

(defun org--rewrite-demos (backend)
  "Replace each outermost :demo block, and its snapshot, by its showing along BACKEND; see `org-demo--rewrite'.
An inner :demo block is part of its outer's source, so it is shown within the outer's rendering.
We run first of all the rewrites, on the block as written; and again later, for the blocks of #+INCLUDE: files."
  (setq backend (org--base-backend backend))
  (when (save-excursion (goto-char (point-min)) (re-search-forward ":demo\\_>" nil t))
    (dolist (site (org-demo--sites t))
      ;; Org prunes a commented subtree only later, so we spare rendering its demos, which would show nowhere.
      (unless (save-excursion (goto-char (car site)) (org-in-commented-heading-p))
        (org-demo--rewrite site backend)))))

(provide 'org-demo)
;;; org-demo.el ends here
