;;; org-special-block-extras.el --- Twenty-four new custom blocks for Org-mode   -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 1.0
;; Package-Requires: ((s "1.12.0") (dash "2.16.0") (emacs "24.4"))
;; Keywords: org, blocks, colors, convenience
;; URL: https://alhassy.github.io/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Common operations such as colouring text for HTML and LaTeX
;; backends are provided. Below is an example.
;;
;; #+begin_red org
;; /This/
;;       *text*
;;              _is_
;;                   red!
;; #+end_red
;;
;; This file has been tangled from a literate, org-mode, file;
;; and so contains further examples demonstrating the special
;; blocks it introduces.
;;
;;
;; The system is extensible:
;; Users register a handler ORG-SPECIAL-BLOCK-EXTRAS/TYPE
;; for a new custom block TYPE, which is then invoked.
;; The handler takes three arguments:
;; - CONTENTS: The string contents delimited by the custom block.
;; - BACKEND:  The current exportation backend; e.g., 'html or 'latex.
;; The handler must return a string.

;;; Code:

;; String and list manipulation libraries
;; https://github.com/magnars/dash.el
;; https://github.com/magnars/s.el

(require 's)               ;; “The long lost Emacs string manipulation library”
(require 'dash)            ;; “A modern list library for Emacs”
(require 'dash-functional) ;; Function library; ‘-const’, ‘-compose’, ‘-orfn’, ‘-not’, ‘-partial’, etc.
(require 'subr-x)          ;; Extra Lisp functions; e.g., ‘when-let’.
(require 'cl-lib)          ;; New Common Lisp library; ‘cl-???’ forms.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core utility

(defun org-special-block-extras--advice (backend blk contents _)
  "Invoke the appropriate custom block handler, if any.

A given custom block BLK has a TYPE extracted from it, then we
send the block CONTENTS along with the current export BACKEND to
the formatting function ORG-SPECIAL-BLOCK-EXTRAS/TYPE if it is
defined, otherwise, we leave the CONTENTS of the block as is.

We also support the seemingly useless blocks that have no
contents at all, not even an empty new line."
  (let* ((type    (nth 1 (nth 1 blk)))
         (handler (intern (format "org-special-block-extras--%s" type))))
    (ignore-errors (apply handler backend (or contents "") nil))))

(advice-add #'org-html-special-block :before-until
            (-partial #'org-special-block-extras--advice 'html))

(advice-add #'org-latex-special-block :before-until
            (-partial #'org-special-block-extras--advice 'latex))

(s-join "\n\n"
(loop for c in org-special-block-extras/colors
      collect (format "#+begin_%s\n This text is %s!\n#+end_%s" c c c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load support for 19 colour custom blocks

(defvar org-special-block-extras--colors
  '(black blue brown cyan darkgray gray green lightgray lime
          magenta olive orange pink purple red teal violet white
          yellow)
  "Colours that should be available on all systems.")

(loop for colour in org-special-block-extras--colors
      do (eval (read (format
                      "(defun org-special-block-extras--%s (backend contents)
                     (format (pcase backend
                     (`latex \"\\\\begingroup\\\\color{%s}%%s\\\\endgroup\")
                     (`html  \"<div style=\\\"color:%s;\\\">%%s</div>\")
                     (t      \"org-special-block-extras: Unsupported backend\"))
                     contents))"
                      colour colour colour))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parallel blocks: parallel<n>[NB] for n:2..5, optionally with ‘N’o ‘b’ar
;; in-between the columns.
;;
;; Common case is to have three columns, and we want to avoid invoking the
;; attribute via org, so making this.

(loop for cols in '("1" "2" "3" "4" "5")
      do (loop for rule in '("solid" "none")
      do (eval (read (concat
"(defun org-special-block-extras--" cols "parallel"
(if (equal rule "solid") "" "NB")
"(backend contents)"
"(format (pcase backend"
"(`html \"<div style=\\\"column-rule-style:" rule ";column-count:" cols ";\\\"%s</div>\")"
"(`latex \"\\\\par \\\\setlength{\\\\columnseprule}{" (if (equal rule "solid") "2" "0") "pt}"
"          \\\\begin{minipage}[t]{\\\\linewidth}"
"          \\\\begin{multicols}{" cols "}"
"          %s"
"          \\\\end{multicols}\\\\end{minipage}\")) contents))")))))

(defalias #'org-special-block-extras--parallel   #'org-special-block-extras--2parallel)
(defalias #'org-special-block-extras--parallelNB #'org-special-block-extras--2parallelNB)

(defun org-special-block-extras--extract-arguments (contents &rest args)
"Get list of CONTENTS string with ARGS lines stripped out and values of ARGS.

Example usage:

    (-let [(contents′ . (&alist 'k₀ … 'kₙ))
           (…extract-arguments contents 'k₀ … 'kₙ)]
          body)

Within ‘body’, each ‘kᵢ’ refers to the ‘value’ of argument
‘:kᵢ:’ in the CONTENTS text and ‘contents′’ is CONTENTS
with all ‘:kᵢ:’ lines stripped out.

+ If ‘:k:’ is not an argument in CONTENTS, then it is assigned value NIL.
+ If ‘:k:’ is an argument in CONTENTS but is not given a value in CONTENTS,
  then it has value the empty string."
  (let ((ctnts contents)
        (values (loop for a in args
                      for regex = (format ":%s:\\(.*\\)" a)
                      for v = (cadr (s-match regex contents))
                      collect (cons a v))))
    (loop for a in args
          for regex = (format ":%s:\\(.*\\)" a)
          do (setq ctnts (s-replace-regexp regex "" ctnts)))
    (cons ctnts values)))

(defvar org-special-block-extras-hide-editor-comments nil
  "Should editor comments be shown in the output or not.")

(defun org-special-block-extras--edcomm (backend contents)
"Format CONTENTS as an first-class editor comment according to BACKEND.

The CONTENTS string has two optional argument switches:
1. :ed: ⇒ To declare an editor of the comment.
2. :replacewith: ⇒ [Nullary] The text preceding this clause
   should be replaced by the text after it."
  (-let* (
           ;; Get arguments
           ((contents₁ . (&alist 'ed))
            (org-special-block-extras--extract-arguments contents 'ed))

           ;; Strip out any <p> tags     
           (_ (setq contents₁ (s-replace-regexp "<p>" "" contents₁)))
           (_ (setq contents₁ (s-replace-regexp "</p>" "" contents₁)))

           ;; Are we in the html backend?
           (html? (equal backend 'html))

           ;; fancy display style
           (boxed (lambda (x)
                    (if html?
                        (concat "<span style=\"border-width:1px"
                                 ";border-style:solid;padding:5px\">"
                                 "<strong>" x "</strong></span>")
                    (concat "\\fbox{\\bf " x "}"))))

           ;; Is this a replacement clause?
           ((this that) (s-split ":replacewith:" contents₁))
           (replacement-clause? that) ;; There is a ‘that’
           (replace-keyword (if html? "&nbsp;<u>Replace:</u>"
                              "\\underline{Replace:}"))
           (with-keyword    (if html? "<u>With:</u>"
                              "\\underline{With:}"))
           (editor (format "[%s:%s"
                           (if (s-blank? ed) "Editor Comment" ed)
                           (if replacement-clause?
                               replace-keyword
                             "")))
           (contents₂ (if replacement-clause?
                          (format "%s %s %s" this
                                  (funcall boxed with-keyword)
                                  that)
                        contents₁))

           ;; “[Editor Comment:”
           (edcomm-begin (funcall boxed editor))
           ;; “]”
           (edcomm-end (funcall boxed "]")))

    (setq org-export-allow-bind-keywords t) ;; So users can use “#+bind” immediately
    (if org-special-block-extras-hide-editor-comments
        ""
      (format (pcase backend
                (`html "<p> %s %s %s</p>")
                (`latex "%s %s %s"))
              edcomm-begin contents₂ edcomm-end))))

(setq org-export-allow-bind-keywords t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'org-special-block-extras)

;;; org-special-block-extras.el ends here
