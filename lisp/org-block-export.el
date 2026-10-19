;;; org-block-export.el --- Replace each block by its handler's result, and load what it needs upon export  -*- lexical-binding: t; -*-

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

;; Before Org parses a buffer for export, each block that `org-defblock' declares is replaced by
;; its handler's result:
;;
;;     #+begin_foo mainarg :x 1        ↦   (org-block/foo backend "block content" "mainarg" :x 1)
;;     block content
;;     #+end_foo
;;
;; Whatever the handlers need atop the document ---LaTeX packages, HTML <head> lines--- is noted
;; as they run, and added once the export is done.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 's)
(require 'dash)
(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'ox-latex)
(require 'ox-html)
(require 'xml)
(require 'org-deflink)
(require 'org-special-block-parse)
(require 'org-block-switches)

(defvar org-special-block-add-html-extra)

;;;; org-eval-replace-block

(defun org--block-declared-keywords (args)
  "The flat (MAIN DEFAULT KEY₁ DEFAULT₁ …) arglist ARGS ↦ (KEY₁ …); see `org--defblock-desugar-args'.
  (title \"Details\" color \"green\" style nil)  ↦  (color style)"
  (mapcar #'car (-partition 2 (cddr args))))

(cl-defmethod org-eval-replace-block ((block org-special-block) backend)
  "Replace a special Org block with the result of evaluating its handler.

This function replaces the region from START-POINT to END-POINT of BLOCK
with the result of evaluating the corresponding handler function:
  `org-block/NAME'

Here, NAME is the `name' slot of the BLOCK (i.e., the name used in the
#+begin_NAME / #+end_NAME delimiters).

Each special block can include:
- A *main argument*: The first (optional) positional argument after the block name.
- *Keyword arguments*: Zero or more “:key value” pairs.
- A *body*: The content between the begin and end block markers.

The corresponding handler function must be named `org-block/NAME' with signature:

  (org-block/NAME BACKEND CONTENTS MAIN-ARG &rest KWDARGS)

Example:

  #+begin_foo mainarg :x 1 :y 2
  block content
  #+end_foo

Will be replaced with the result of evaluating:

  (org-block/foo BACKEND \"block content\" \"mainarg\" '(:x . 1) '(:y . 2))

The handler’s return value is inserted in place of the original block.
Indentation is preserved via `org-replace-text-while-preserving-indentation'.

This method is part of the Org export pipeline that processes supported
blocks listed in `org--supported-blocks', typically triggered during export
pre-processing steps."
  (-let* (((&org-special-block 'name 'main-arg 'kwdargs 'parameters 'contents 'start-point 'end-point 'anchor) block)
          (handler (intern (format "org-block/%s" name)))
          ;; A block's own keyword of a switch's name wins; e.g., a block declaring `folded' gets :folded itself.
          (declared (--map (intern (format ":%s" it)) (org--block-declared-keywords (get handler 'org-defblock-args))))
          (switch? (lambda (key) (and (org--block-switch-p key) (not (memq key declared)))))
          ((switches . kwdargs) (org--plist-partition kwdargs switch?))
          ;; The #+begin_ line's header, sans switches, for :verbatim to show the block as it is otherwise written.
          (header (cdr (org--header-sans-switches (or parameters "") switch?))))
    (org-replace-text-while-preserving-indentation
     start-point
     end-point
     (org--block-with-switches
      name switches backend contents
      (lambda (contents)
        (let ((org--handler-anchor anchor))
          (apply handler backend contents main-arg kwdargs)))
      header))))

;;;; org--rewrite-special-blocks-by-handlers

(defun org--handler-begin-line-p ()
  "Whether point's #+begin_ line opens a block for `org--rewrite-special-blocks-by-handlers' to replace.
Literal text, such as in a src block, is not; a malformed block is, to reach the parser, which reports it.
One of Org's own blocks, once overridden, is ours too ---save one a handler hands back by `org-builtin-block'."
  (let ((elem (org-element-at-point)))
    (or (memq (org-element-type elem) '(special-block paragraph))
        ;; Its own #+begin_ line, that is: a #+begin_note written within a src block is its text.
        (and (memq (org-element-type elem) org--builtin-block-types)
             (= (org-element-property :post-affiliated elem) (line-beginning-position))
             (not (get-text-property (save-excursion (skip-chars-forward " \t") (point)) 'org-builtin-block))))))

(defun org--rewrite-special-blocks-by-handlers (backend)
  "Replace supported Org special blocks with the result of their handlers, for BACKEND; e.g., \\='html or \\='latex.
A replaced block's output is rescanned, as it may hold blocks too.

Note: This function mutates the current buffer."
  (setq backend (org--base-backend backend))
  (cl-loop for blk in org--supported-blocks
           do (goto-char (point-min))
           (while (re-search-forward (format "^\\s-*\\#\\+begin_%s\\b" blk) nil t)
             (beginning-of-line)
             (if (org--handler-begin-line-p)
                 (org-eval-replace-block (org-special-block-after-point blk) backend)
               (forward-line 1)))))

;;;; LaTeX packages, loaded as needed

(defvar-local org--latex-head-used nil
  "What the blocks this export has rendered need in the LaTeX preamble, by their `:latex-head'; see `org--load-latex-head'.
Local to the buffer an export works in: each export, nested ones included, works in a copy of its own.")

(defconst org--latex-margin-head
  '("marginnote"
    "\\AddToHook{env/tcolorbox/begin}{\\let\\marginpar\\marginnote}"
    "\\AddToHook{env/minipage/begin}{\\let\\marginpar\\marginnote}"
    "\\AddToHook{env/multicols/begin}{\\let\\marginpar\\marginnote}")
  "What a document with margin notes needs: within a box, a minipage, or columns, \\marginpar loses its float, as the
margin is out of reach of floats there; so there, it is marginnote's \\marginnote, which reaches it all the same.
Elsewhere, \\marginpar stays, as it moves stacked notes apart.  See `org--latex-margin-note'.")

(defun org--load-latex-head (output backend _info)
  "(OUTPUT, BACKEND) ↦ LaTeX OUTPUT whose preamble holds, once, each item its blocks need, `org--latex-head-used'.
A package name, \"xcolor\", is \\usepackage{xcolor} unless the preamble loads it already, options and all; any
other line, \"\\\\newcommand{\\\\QED}{∎}\", is added unless the preamble has it.  They go just before \\begin{document};
so after the document's own lines.  A body-only export, sans preamble, is left be.  Margin notes, wherever they
come from, bring `org--latex-margin-head'; by the output itself, as some are made in buffers of their own."
  (let ((needed (append (reverse org--latex-head-used)
                        (and (string-match-p "OrgSpecialBlockExtrasMarginBox" output) org--latex-margin-head))))
    (if (not (and (org-export-derived-backend-p backend 'latex) needed
                  (string-match "^\\\\begin{document}" output)))
        output
      (let* ((preamble (substring output 0 (match-beginning 0)))
             (package? (lambda (item) (string-match-p "\\`[[:alnum:]-]+\\'" item)))
             (present? (lambda (item)
                         (if (not (funcall package? item)) (s-contains? item preamble)
                           (string-match-p (format "\\\\\\(?:usepackage\\|RequirePackage\\)\\(?:\\[[^]]*\\]\\)?{\\(?:[^}]*,\\)?[ \t]*%s[ \t]*[,}]"
                                                   (regexp-quote item))
                                           preamble))))
             (missing (-remove present? (-uniq needed))))
        (if (not missing) output
          (concat preamble
                  (mapconcat (lambda (item) (format (if (funcall package? item) "\\usepackage{%s}\n" "%s\n") item)) missing "")
                  (substring output (length preamble))))))))

(defun org--latex-define-color (name colour)
  "(NAME, COLOUR) ↦ LaTeX defining NAME as COLOUR: via \\definecolor when COLOUR is a hex code such as #e5f5e5, or a
name xcolor lacks but Emacs knows, such as navy; else \\colorlet, as for red or red!5!white.
COLOUR may be a symbol, as block arguments such as “:color red!5!white” are read as Lisp."
  (setq colour (format "%s" colour))
  (-if-let (rgb (and (not (string-prefix-p "#" colour))
                     (not (string-match-p "!" colour))       ; A mix, such as red!5!white, is xcolor's own.
                     (not (and (boundp 'org--named-colors) (memq (intern colour) org--named-colors)))
                     ;; Emacs's X11 colour table, which a display-less batch Emacs reads too; e.g., navy, gold.
                     (tty-color-standard-values colour)))
      (apply #'format "\\definecolor{%s}{rgb}{%.3f,%.3f,%.3f}" name (--map (/ it 65535.0) rgb))
    (if (string-prefix-p "#" colour)
        (format "\\definecolor{%s}{HTML}{%s}" name (substring colour 1))
      (format "\\colorlet{%s}{%s}" name colour))))

;;;; HTML <head> items, loaded as needed

(defvar-local org--html-head-used nil
  "What the blocks, and links, this export has rendered need in an HTML page's <head>; see `org--load-html-head'.
Local to the buffer an export works in, as `org--latex-head-used' is.")

(defun org-html-head-require (item)
  "Note that this HTML export's page needs ITEM in its <head>: HTML, a symbol whose value is HTML, or `mathjax'.
E.g., our tooltips ask for (org-html-head-require \='org--tooltip-html-head); a block asks by its `:html-head'."
  (cl-pushnew item org--html-head-used :test #'equal))

(defun org--html-mathjax (info)
  "INFO ↦ Org's own MathJax setup, by the user's MathJax options, as for a page whose maths is Org's own.
Org sets MathJax up only for maths it parsed itself; ours, such as a math block's, is raw HTML to it."
  (org-html--build-mathjax-config
   (plist-put (copy-sequence info) :parse-tree '(org-data nil (latex-fragment (:value "\\(x\\)"))))))

(defun org--load-html-head (output backend info)
  "(OUTPUT, BACKEND, INFO) ↦ HTML OUTPUT whose <head> holds each item its blocks need, `org--html-head-used', once.
Our own CSS and JS only if `org-special-block-add-html-extra'; MathJax always, else our maths would not render."
  (if (not (and (org-export-derived-backend-p backend 'html) org--html-head-used
                (string-match "</head>" output)))
      output
    (let* ((at (match-beginning 0))
           (heads (-non-nil
                   (--map (cond ((eq it 'mathjax) (unless (string-match-p "window.MathJax" output) (org--html-mathjax info)))
                                ((not org-special-block-add-html-extra) nil)
                                ((symbolp it) (symbol-value it))
                                (t it))
                          (reverse org--html-head-used))))
           (missing (--remove (s-contains? it output) (-uniq heads))))
      (if (not missing) output
        (concat (substring output 0 at) (apply #'concat missing) (substring output at))))))

;;;; Section links ---resolved once Org knows where each section lands

(defun org--resolve-section-links (output backend info)
  "BACKEND's OUTPUT with each section link our blocks wrote ↦ that section's anchor, as INFO places it; e.g., in HTML,
a graph's href=\"*Heading\" and a math block's \\href{id:ID} ↦ #ANCHOR; in LaTeX, a math block's \\hyperref[*Heading],
or [#CUSTOM-ID], ↦ its \\label's.  An unresolvable link stays as it is.  Our blocks render before Org parses the
buffer, so only now is a section's anchor known.  A \\href{#CUSTOM-ID} needs none: HTML's anchor is the custom id."
  (cl-flet ((resolve (regexp anchor)
              (replace-regexp-in-string
               regexp
               (lambda (match)
                 ;; `replace-regexp-in-string' replaces by the match data that REP is handed, so resolving must not
                 ;; clobber it.
                 (save-match-data
                   (string-match regexp match)
                   (let ((open (match-string 1 match)) (link (match-string 2 match)) (close (match-string 3 match)))
                     (condition-case nil
                         (concat open (funcall anchor (org-export-resolve-link (xml-substitute-special link) info)) close)
                       (error match)))))
               output t t)))
    (cond ((org-export-derived-backend-p backend 'html)
           (resolve "\\(href=\"\\|\\\\href{\\)\\(\\*[^\"}]*\\|id:[^\"}]*\\)\\([\"}]\\)"
                    (lambda (datum) (concat "#" (org-html--reference datum info)))))
          ((org-export-derived-backend-p backend 'latex)
           (resolve "\\(\\\\hyperref\\[\\)\\([*#][^]]*\\|id:[^]]*\\)\\(]\\)"
                    (lambda (datum) (org-latex--label datum info t))))
          (t output))))

(defun org--block-note-heads (handler backend result)
  "Note what HANDLER's RESULT needs atop a BACKEND document, by its `:latex-head' or `:html-head'; see `org-defblock'.
A RESULT handing the block back to Org, by `org-builtin-block', needs none."
  (unless (text-property-any 0 (length result) 'org-builtin-block t result)
    (pcase backend
      ('latex (dolist (package (get handler 'org-latex-head))
                (cl-pushnew package org--latex-head-used :test #'equal)))
      ('html (mapc #'org-html-head-require (get handler 'org-html-head))))))

(provide 'org-block-export)
;;; org-block-export.el ends here
