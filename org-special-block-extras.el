;;; org-special-block-extras.el --- New custom blocks and links for Org-mode   -*- lexical-binding: t; -*-

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

(s-join "\n\n"
(loop for c in org-special-block-extras/colors
      collect (format "#+begin_%s\n This text is %s!\n#+end_%s" c c c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core utility

(defun org-special-block-extras--advice (backend blk contents _)
  "Invoke the appropriate custom block handler, if any.

A given custom block BLK has a TYPE extracted from it, then we
send the block CONTENTS along with the current export BACKEND to
the formatting function ORG-SPECIAL-BLOCK-EXTRAS/TYPE if it is
defined, otherwise, we leave the CONTENTS of the block as is."
  (let* ((type    (nth 1 (nth 1 blk)))
         (handler (intern (format "org-special-block-extras/%s" type))))
    (ignore-errors (apply handler backend contents nil))))

(advice-add #'org-html-special-block :before-until
            (-partial #'org-special-block-extras--advice 'html))

(advice-add #'org-latex-special-block :before-until
            (-partial #'org-special-block-extras--advice 'latex))

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

(provide 'org-special-block-extras)

;;; org-special-block-extras.el ends here
