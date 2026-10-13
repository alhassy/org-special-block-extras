;;; org-test-utils.el --- Shared test machinery: `deftest', `using', `exporting', `define-org-export-test'

;; Every tests/org-*-tests.el that uses these loads this file first, guarded by (fboundp 'using).
;; Load order matters: were `using' unknown when a test is read, Emacs would eagerly expand the
;; (org-defblock …) forms inside it as plain arguments, and `using' would then receive a `progn'.

;;; TODO
;; 2015-04-08 Emacs Lisp Development Tips with John Wiegley
;; https://sachachua.com/blog/2015/04/2015-04-08-emacs-lisp-development-tips-with-john-wiegley/

;; TODO: Follow “Emacs Lisp Coding Conventions”, e.g., use “org-block--𝒮” not
;; “org-block/𝒮”! Why? Because the defun is an internal implementation detail
;; and so its name should reflect that intent.
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html


;; TODO: See ~/html.el/html.el


;; NOTE: Consider making a LaTeX Templating Facade:
;; Use Lisp notation as an easy-to-type and easy-to-manipulate realization of mathematical notation.
;; E.g., (latex '(lambda (x) (f x))) = $(λ x. f x)$

;;; Requires
(require 'ert)
(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'dash)

(load-file (expand-file-name "~/org-special-block-extras/org-special-block-extras.el"))
(load-file (expand-file-name "~/snap/snap.el")) ;; For `snap-define-fixture', `snap-define-relation', etc.

;;
;;; Bespoke test macro “deftest”
;; I dislike writing “(ert-deftest my-method-test () “my-method does the thing” :tags ‘(my-method) body)”
;; and instead prefer “(deftest “my-method does the thing” body)”.
;; My approach also simplifies things by surfacing test :tags into the “M-x ert” view.

;; `deftest’ is the identity fixture — no wrapping, readable ERT names.
;; snap.el no longer ships it directly; we define it here via snap-define-fixture.
(snap-define-fixture deftest)


;;; Test utilities “using” and “exporting”

(defmacro using (block-defs &rest body)
  "Evaluate BLOCK-DEFS (an org-defblock form or list of them), run BODY, then undo via org-undefblock.

BLOCK-DEFS may be either:
  - a single form:   (org-defblock name kwds &optional link-display docstring &rest body)
  - a list of forms: ((org-defblock …) (org-defblock …) …)

For each (org-defblock NAME …), we generate the matching (org-undefblock NAME …)
and call those in cleanup so both block and link support are removed.

Each form is evaluated; any functions it introduces are unbound
afterwards to avoid polluting the global namespace.
(If we use top-level org-defblock forms instead, we can have unexpected
calls when the same name is used for different blocks in different tests! 🤮)

This is a poor-man's `cl-letf'."
  (declare (indent 1))
  (let* ((defs (pcase block-defs
                 (`(org-defblock . ,_) (list block-defs))
                 (`(org-defblock-only . ,_) (list block-defs))
                 ((and (pred listp) fs)
                  (progn
                    (dolist (f fs)
                      (cl-assert (and (consp f) (or (eq (car f) 'org-defblock) (eq (car f) 'org-defblock-only)))
                                 nil "using: each element must be an (org-defblock …) form"))
                    fs))
                 (_ (error "using: must be an (org-defblock[-only] …) form or a list of them"))))
         ;; Build matching (org-undefblock NAME …) forms by swapping the head symbol.
         (undefs (mapcar (lambda (f) (cons 'org-undefblock (cdr f))) defs)))
    `(let ((__defs ',defs)
           (__undefs ',undefs))
       (unwind-protect
           (progn
             ;; Evaluate all definitions at runtime (not at macro-expansion).
             (dolist (f __defs) (eval f))
             ,@body)
         ;; Best-effort cleanup; don't error if an undef fails.
         (dolist (u __undefs)
           (ignore-errors (eval u)))))))


(cl-defmacro exporting (string &key (to 'html) using equals modulo)
  "Assert that exporting STRING to backend TO equals EQUALS (optionally modulo MODULO).

If EQUALS is omitted, this generates the expectations only.
This is useful in combination with `C-u C-x C-e'.

Tldr: Run an org-export-string assertion, optionally installing temporary defblocks via :USING and cleaning them up.
Args:
+ TO is the name of a backend, such as `html' (default) or `latex'.
+ STRING is the input Org string (raw; `lf-string' is applied to it).
+ EQUALS is the expected output as a string; if omitted, return the actual export.
+ MODULO is a string or list of strings replaced by \".*\" before comparing via regex.
+ USING  is either a single (org-defblock …) form OR a list of such forms.
  Each form is evaluated; any functions it introduces are unbound
  afterwards to avoid polluting the global namespace.
  (If we use top-level org-defblock forms instead, we can have unexpected
  calls when the same name is used for different blocks in different tests! 🤮)
  This is a poor-man's `cl-letf'.

API Notes:
+ (exporting A :equals B)            ≋  (should (equal (org-export-string A) B))
+ (exporting A :equals B :modulo C)  ≋  “A equals B with all instances of C replaced by .*”

Example use:
  (exporting \"A B C D\" :to ascii :equals \"A P C T\" :modulo (\"P\" \"T\"))

See the associated deftest for more example uses.
"
  (declare (indent defun))
  ;; Normalize :using into a list of (org-defblock …) forms but DO NOT evaluate here (at macro-expansion time)
  ;; to avoid polluting the namespace even though the generated code has not yet actually been executed (at runtime).
  (let ((using-forms
         (pcase using
           ((pred null) nil)
           (`(org-defblock . ,_) (list using))
           (`(org-defblock-only . ,_) (list using))
           ((and (pred listp) forms)
            (progn
              (dolist (f forms)
                (cl-assert (and (consp f) (eq (car f) 'org-defblock))
                           nil ":using list must contain only (org-defblock[-only] …) forms"))
              forms))
           (_ (error ":using must be a single (org-defblock[-only] …) or a list of them")))))
    ;; Build the runtime assertion form
    (let* ((actual   `(org-export-string (lf-string ,string) ',to))
           (expected (if (not equals)
                         nil
                       (if (not modulo)
                           `(lf-string ,equals)
                         `(thread-last ,equals
                                       lf-string
                                       regexp-quote
                                       (s-replace-all
                                        ',(--map (cons it ".*")
                                                 (if (listp modulo) modulo (list modulo))))
                                       (format "^%s$")))))
           (assertion
            (cond
             ((not equals) actual) ; return actual when no equals provided
             (modulo `(should (equal 0 (string-match-p ,expected ,actual))))
             (t      `(should (equal ,actual ,expected))))))
      ;; wrap assertion in “using” if needed
      (if using-forms
          `(using ,using-forms
             ,assertion)
        assertion))))


;;; define-org-export-test — relational test fixture for org-export assertions
;;
;; Generated by `define-relation'.  Each test is a single export
;; assertion with `C-u C-x C-e' snapshot updating, [Show Diff] and
;; [Update Snapshot] buttons on failure.
;;
;; Tests with multiple `exporting' calls or complex control flow
;; remain as plain `deftest' forms using the `exporting' macro above.

(snap-define-relation org-export (input to using equals modulo)
  "Export INPUT via backend TO, assert result EQUALS.
USING is a list of (org-defblock ...) forms installed temporarily.
MODULO enables regex-based approximate matching.
Comparison ignores trailing whitespace on each line, since editors strip it from EQUALS in the source."
  (let* ((trim (lambda (s) (replace-regexp-in-string "[ \t]+$" "" s)))
         ;; The expectations were recorded with this setting; batch Emacs defaults it to nil.
         (org-src-preserve-indentation t)
         (defs (cond
                ((null using) nil)
                ;; Single (org-defblock ...) form → wrap in list.
                ((and (consp using) (memq (car using) '(org-defblock org-defblock-only)))
                 (list using))
                (t using)))
         (undefs (mapcar (lambda (f) (cons 'org-undefblock (cdr f))) defs))
         (actual (unwind-protect
                     (progn
                       (dolist (f defs) (eval f))
                       (org-export-string (lf-string input) (or to 'html)))
                   (dolist (u undefs)
                     (ignore-errors (eval u))))))
    (if modulo
        (let* ((modulos (if (listp modulo) modulo (list modulo)))
               (pattern (thread-last equals
                          lf-string
                          (funcall trim)
                          regexp-quote
                          (s-replace-all (--map (cons it ".*") modulos))
                          (format "^%s$"))))
          (should (equal 0 (string-match-p pattern (funcall trim actual)))))
      (should (equal (funcall trim actual) (funcall trim (lf-string equals)))))
    (list :equals actual)))

;;; define-org-call-test — relational test fixture for handler calls

(snap-define-relation org-call (call returns)
  "Evaluating CALL ---e.g., (org-block/box 'html \"hi\" nil)--- yields RETURNS, modulo trailing whitespace per line."
  (let ((actual (eval call t))
        (trim (lambda (s) (if (stringp s) (replace-regexp-in-string "[ \t]+$" "" s) s))))
    (should (equal (funcall trim returns) (funcall trim actual)))
    (list :returns actual)))

(provide 'org-test-utils)
