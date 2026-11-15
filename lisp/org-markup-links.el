;;; org-markup-links.el --- What our markup links mean, for maths and graphs to render alike  -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.11.14
;; Package-Requires: ((dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; In prose, Org exports red:x, [[tooltip:x][a tip]], doc:fold, and their kin through each link's own :export.  Maths
;; and graph labels are no prose: maths wants LaTeX, and a graph's node wants Graphviz's HTML-like label, tooltip, and
;; URL.  So each such link says what it *means*, once, here; e.g.,
;;
;;     red:x                    ⟶  (:color "red")                 the text x, in red
;;     [[tooltip:x][a tip]]     ⟶  (:text "x" :tip "a tip")       the text x, “a tip” hovering over it
;;     [[https://o.org][Org]]   ⟶  (:url "https://o.org")         the text Org, leading to o.org
;;
;; and each sink renders a meaning in its own tongue: `org--math-markup' as LaTeX, `org--graph-item-render' as a label.
;; A new link of ours reaches maths and graphs alike by joining `org-markup-links'.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'dash)
(require 'org-color-links)

(declare-function org--docs-cite "org-links")
(declare-function org--docs-plain-p "org-links")

(defun org--markup-tooltip (path desc)
  "Returns the meaning of [[tooltip:PATH][DESC]]: PATH shown, DESC hovering over it; sans DESC, PATH hovers over
itself, as in prose."
  (list :text path :tip (or desc path)))

(defun org--markup-doc (path desc)
  "Returns the meaning of [[doc:PATH][DESC]]: DESC, else PATH's glossary name, shown, its documentation hovering over
it in HTML; PATH joins the glossary that #+print_glossary: lists, as a doc: link in prose does."
  (-let [(name docs) (org--docs-cite path)]
    (list :text (unless desc name) :tip docs :plain (org--docs-plain-p path) :glossary t)))

(defvar org-markup-links
  (append
   (--map (let ((colour (symbol-name it))) (cons colour (lambda (_path _desc) (list :color colour))))
          org--named-colors)
   `(("color"     . ,(lambda (path _desc) (list :color path)))
     ;; A plain highlight:x has no colour of its own, x being what shows.
     ("highlight" . ,(lambda (path desc) (list :background (or (and desc (org-string-nw-p path)) "yellow"))))
     ("tooltip"   . org--markup-tooltip)
     ("margin"    . org--markup-tooltip)
     ("doc"       . org--markup-doc)
     ;; A web link leads to its URL, which shows sans description.
     ,@(--map (let ((scheme it))
                (cons scheme (lambda (path desc)
                               (let ((url (concat scheme ":" path))) (list :url url :text (unless desc url))))))
              '("https" "http"))))
  "Link types that maths and graph labels know, each TYPE paired with a function of PATH and DESC, the latter nil for
TYPE:PATH and [[TYPE:PATH]], that returns the link's meaning, a plist:
+ :text        what shows, raw; sans it, the sink's rendering of DESC, else PATH.
+ :color       the colour of what shows, a name or #RRGGBB.
+ :background  the colour behind what shows.
+ :url         where what shows leads.
+ :tip         Org text hovering over what shows; :plain, when it is plain text instead, such as a docstring.
+ :glossary    non-nil when the :tip is a glossary's, which LaTeX lists at the end rather than in a margin note.
A TYPE not here shows its DESC, else its PATH.")

(defun org-markup-link-meaning (type path desc)
  "Returns the meaning, as `org-markup-links' says, of a TYPE link to PATH described by DESC; nil for an unknown TYPE."
  (-when-let (meaning (cdr (assoc type org-markup-links)))
    (funcall meaning path desc)))

(provide 'org-markup-links)
;;; org-markup-links.el ends here
