;;; org-special-block-parse.el --- Read an Org block into an `org-special-block' struct  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.18
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8") (lf "26.10.1"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; The block a handler sees, as read from the buffer:
;;
;;     #+begin_details Hint :color red     ↦   #s(org-special-block :name "details" :main-arg "Hint"
;;     Try induction.                                                :kwdargs (:color red)
;;     #+end_details                                                 :contents "Try induction." …)
;;
;; `org-special-block-after-point' reads it via `org-element', so a “#+begin_NAME” that Org reads
;; as mere text ---say, within a src block--- is no block at all.  One of Org's own blocks, such as
;; quote, reads alike, since `org-defblock' may override it.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 's)
(require 'dash)
(require 'subr-x)
(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'lf)
(require 'org-block-header-args)

(defgroup org-special-block-extras nil
  "Special blocks, links, and tags for Org, defined as one defines functions."
  :group 'org
  :prefix "org-")

;;;; defstruct org-special-block

(cl-defstruct org-special-block
  "A representation of an Org special block."
  ;; Specification of slots:
  ;;
  ;;    ⟨start-point⟩#+begin_blk main-arg :key₀ val₀ … :keyₙ valₙ  ;; ⟵ ⟨parameters⟩, read as ⟨main-arg⟩ ⟨kwdargs⟩
  ;;    contents
  ;;    #+end_blk⟨end-point⟩
  (name             nil :type string  :documentation "The name of the special block.")
  (start-point      nil :type integer :documentation "Point at start of “#+begin_” line.")
  (end-point        nil :type integer :documentation "Point at end of clause “#+end_⟨name⟩”.")
  (main-arg         nil :type string  :documentation "First non-keyword argument")
  (kwdargs          nil :type plist   :documentation "The key-value arguments for the header.")
  (parameters       nil :type string  :documentation "The “#+begin_” line's text after the name, as written; else nil.")
  (contents         nil :type string  :documentation "Body text contents")
  (anchor           nil :type string  :documentation "The block's #+name:, which [[links]] reach; else nil."))

(defvar org--handler-anchor nil
  "The #+name: of the block whose handler is running; else nil.
E.g., a graph named `lineage' is a figure that [[lineage]] reaches.")

;; At compile time too, since `-let' patterns are expanded then.
(eval-and-compile
(defun dash-expand:&org-special-block (key source)
  "Destructuring which works with `org-special-block', for use with `-let'.

For example,

       (let ((blk (make-org-special-block :name \"foo\" :main-arg \"hola\")))
         (-let (((&org-special-block 'name 'main-arg) blk))
           (message \"Block %s with arg %s\" name main-arg)))

"
  `(progn (lf-assert ,source (org-special-block-p it))
          (,(intern (s-replace "'" "" (format "org-special-block-%s" key))) ,source))))

(defvar org--supported-blocks nil
  "Which special blocks, defined with `org-defblock', are supported.

Such blocks are read using `org-special-block-after-point'.

This is a list of strings.")

;;;; Org's own blocks

(defconst org--builtin-block-types '(quote-block verse-block center-block example-block comment-block export-block src-block)
  "Org's own block types, which an `org-defblock' of their name ---say, quote--- overrides; see `org-builtin-block'.")

(defun org--block-name (elem)
  "A special block, or one of Org's own, ELEM ↦ its name; e.g., \"tip\" for #+begin_tip, \"quote\" for #+begin_quote."
  (if (eq (org-element-type elem) 'special-block)
      (org-element-property :type elem)
    (save-excursion
      (goto-char (org-element-property :post-affiliated elem))
      (and (looking-at "[ \t]*#\\+begin_\\([^ \t\n]+\\)") (match-string-no-properties 1)))))

(defun org--block-parameters (elem)
  "A special block, or one of Org's own, ELEM ↦ its #+begin_ line's text after the name; e.g., \"Mind :color red\".
Org records it for a special block, yet not as such for its own, whose arguments mean things of their own."
  (if (eq (org-element-type elem) 'special-block)
      (org-element-property :parameters elem)
    (save-excursion
      (goto-char (org-element-property :post-affiliated elem))
      (and (looking-at "[ \t]*#\\+begin_[^ \t\n]+[ \t]+\\(.*[^ \t\n]\\)") (match-string-no-properties 1)))))

(defun org--block-source (name contents &optional header)
  "(NAME, CONTENTS, HEADER) ↦ \"#+begin_NAME HEADER\\nCONTENTS\\n#+end_NAME\", sans the space when HEADER is blank."
  (format "#+begin_%s%s\n%s\n#+end_%s" name (if (org-string-nw-p header) (concat " " header) "") contents name))

(defun org-builtin-block (name contents &optional header)
  "(NAME, CONTENTS, HEADER) ↦ Org's own #+begin_NAME HEADER block of CONTENTS, which `org-defblock' leaves to Org.
So a block overriding one of Org's own may hand some of them back; e.g., our quote, sans a citation:
  (org-parse (org-builtin-block \"quote\" raw-contents))
Its #+begin_ line carries a text property that `org--rewrite-special-blocks-by-handlers' steps past."
  (let ((source (org--block-source name contents header)))
    (put-text-property 0 (string-search "\n" source) 'org-builtin-block t source)
    source))

;;;; org-special-block-after-point

(defun org--special-block-element-at-line (&optional name)
  "The `org-element' special block whose #+begin_ line point is on, named NAME if given; else nil.
Hence nil for a “#+begin_NAME” that Org reads as mere text; e.g., inside a src or example block.
One of Org's own blocks counts too, as `org-defblock' may override it; see `org--builtin-block-types'."
  (let ((elem (org-element-at-point)))
    (and (memq (org-element-type elem) (cons 'special-block org--builtin-block-types))
         (= (org-element-property :post-affiliated elem) (line-beginning-position))
         (or (null name) (equal (downcase (format "%s" name)) (downcase (org--block-name elem))))
         elem)))

(defun org--next-block-element (&optional name)
  "NAME ↦ the `org-element' of the first block at or after point's line, named NAME if given; else an error.
Point moves past its #+begin_ line.  A “#+begin_NAME” that Org reads as literal text ---say, inside a src
block--- is skipped; one it reads as a paragraph, since its “#+end_NAME” is missing, is an error."
  (let ((begin-rx (format "^[ \t]*#\\+begin_%s\\(?:[ \t]\\|$\\)"
                          (if name (regexp-quote (format "%s" name)) "[A-Za-z0-9_-]+")))
        elem)
    (beginning-of-line)
    (while (and (not elem) (re-search-forward begin-rx nil t))
      (beginning-of-line)
      (setq elem (if (eq (org-element-type (org-element-at-point)) 'paragraph)
                     'malformed
                   (org--special-block-element-at-line name)))
      (forward-line 1))
    (when (memq elem '(nil malformed))
      (error "‘org-special-block-after-point’: I had trouble parsing “#+begin_%s ⟨args⟩?\n⟨content⟩?\n#+end_%s”. Are the required pieces there? 🤔" name name))
    elem))

(defun org--block-end-point (elem)
  "Block ELEM ↦ the end of its “#+end_” line, sans the blank lines `org-element' counts as part of the block."
  (save-excursion (goto-char (org-element-property :end elem)) (skip-chars-backward " \t\n\r") (line-end-position)))

(defun org--block-contents (elem)
  "Block ELEM ↦ the text between its delimiters, sans the least indentation of its lines and the blank lines around them.
So lines keep their indentation relative to one another, as an item nested in a list must (issue #8)."
  (let ((begin (or (org-element-property :contents-begin elem)
                   ;; Org's own blocks such as src record no contents region; theirs are the lines between the delimiters.
                   (and (not (eq (org-element-type elem) 'special-block))
                        (save-excursion (goto-char (org-element-property :post-affiliated elem))
                                        (forward-line 1) (point)))))
        (end (or (org-element-property :contents-end elem)
                 (save-excursion (goto-char (org--block-end-point elem)) (line-beginning-position)))))
    (if (and begin (< begin end))
        (string-trim-right
         (replace-regexp-in-string "\\`\\(?:[ \t]*\n\\)+" ""
                                   (org-remove-indentation (buffer-substring-no-properties begin end))))
      "")))

(defun org--block-arguments (elem)
  "Block ELEM ↦ (POSITIONAL . KWDARGS): its header line, after the name, as Lisp; KWDARGS atop those it inherits.
  #+begin_details Hint :color red, under :header-args:details: :title-color red  ↦  ((Hint) :title-color red :color red)
Inherited as a src block does its header-args; see `org--block-header-args'."
  (-let (((positional kwdargs) (--split-with (not (keywordp it)) (org--read-header (org--block-parameters elem)))))
    (cons positional (org--plist-merge (org--block-header-args elem (downcase (org--block-name elem))) kwdargs))))

(defun org-special-block-after-point (&optional name)
  "The first `org-special-block' at or after point, named NAME if given; read via `org-element'.

Point should be at, or before, the start of the “#+begin_” clause.
A “#+begin_NAME” that Org reads as literal text ---say, inside a src block--- is skipped;
one it reads as a paragraph, since its “#+end_NAME” is missing, is an error."
  (save-excursion
    (-let* ((elem (org--next-block-element name))
            ((positional . kwdargs) (org--block-arguments elem)))
      (make-org-special-block
       ;; Block keywords ignore case, as Org's do; e.g., #+BEGIN_SHOUT is a shout block.
       :name (downcase (org--block-name elem))
       :main-arg (mapconcat (lambda (it) (format "%s" it)) positional " ")
       :kwdargs kwdargs
       :parameters (org--block-parameters elem)
       :anchor (org-element-property :name elem)
       :contents (org--block-contents elem)
       ;; From the “#” of “#+begin_”, so the block's indentation stays put when it is replaced …
       :start-point (progn (goto-char (org-element-property :post-affiliated elem)) (skip-chars-forward " \t") (point))
       ;; … to the end of the “#+end_” line.
       :end-point (org--block-end-point elem)))))

(defun org-replace-text-while-preserving-indentation (start-point end-point multi-line-text)
  "Replace the region delimited by the given points with the given text, while preserving indentation."
  (save-excursion
    (goto-char start-point)
    (-let ((indent (current-indentation))
           ((head . tail) (split-string multi-line-text "\n")))
      (delete-region start-point end-point)
      (insert head)
      (when tail (insert "\n"))
      (insert
       (mapconcat
        (lambda (line) (concat (make-string indent ?\s) line))
        tail
        "\n")))))

(provide 'org-special-block-parse)
;;; org-special-block-parse.el ends here
