;;; org-remark-block.el --- Editor remarks, which may all be hidden at once  -*- lexical-binding: t; -*-

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

;; A remark block is an editor's comment, shown in colour; `org-hide-editor-comments' hides them all.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'org-defblock)

;;;;;; Remark

(defvar org-hide-editor-comments nil
  "Should editor comments be shown in the output or not.")

(org-defblock remark
              (editor "Editor Remark" color "black" signoff "" strong nil :latex-head ("xcolor")
                      :link [:face '(:foreground "red" :weight bold)])
              "Format CONTENTS as a first-class editor comment according to BACKEND.

The CONTENTS string has an optional switch: If it contains a line
with having only ‘#+replacewith:’, then the text preceding this
clause should be replaced by the text after it; i.e., this is
what the EDITOR (the person editing) intends and so we format the
replacement instruction (to the author) as such.

In Emacs, as links, editor remarks are shown with a bold red; but
the exported COLOR of a remark is black by default and it is not
STRONG ---i.e., bold---. There is an optional SIGNOFF message
that is appended to the remark.

Remarks are hidden from the export when `org-hide-editor-comments' is
non-nil.  For just one file, write “#+bind: org-hide-editor-comments t”
in it; that needs `org-export-allow-bind-keywords' set to t, which we
leave to you, since #+bind lets a document set any variable.

In Markdown, a remark block is GitHub's “> [!NOTE]” alert, titled EDITOR; as a link, it stays HTML."
              (cond
               (org-hide-editor-comments "")
               ((and (org-export-markdown-p) (not contents-occur-as-link-description))
                (-let [(this that) (s-split "\\#\\+replacewith:" raw-contents)]
                  (org-parse (org--markdown-alert "note" editor
                                                  (concat (if that (format "*Replace:* %s\n\n*With:* %s" (s-trim this) (s-trim that)) raw-contents)
                                                          (if (org-string-nw-p signoff) (format "\n\n%s" signoff) ""))))))
               (t
                (-let* (;; Are we in the html backend?
                        (tex? (equal backend 'latex))

                        ;; fancy display style
                        (boxed (lambda (x)
                                 (if tex?
                                     (concat "\\fbox{\\bf " x "}")
                                   (concat "<span style=\"border-width:1px"
                                           ";border-style:solid;padding:5px\">"
                                           "<strong>" x "</strong></span>"))))

                        ;; Is this a replacement clause?
                        ((this that) (s-split "\\#\\+replacewith:" contents))
                        (replacement-clause? that) ;; There is a ‘that’
                        (replace-keyword (if tex?
                                             "\\underline{Replace:}" "&nbsp;<u>Replace:</u>"))
                        (with-keyword    (if tex? "\\underline{With:}" "<u>With:</u>"
                                             ))
                        (editor (format "[%s:%s" editor
                                        (if replacement-clause?
                                            replace-keyword
                                          "")))
                        (contents′ (if replacement-clause?
                                       (format "%s %s %s" this
                                               (org-export (funcall boxed with-keyword))
                                               that)
                                     contents))

                        ;; “[Editor Comment:”
                        (edcomm-begin (funcall boxed editor))
                        ;; “]”
                        (edcomm-end (funcall boxed "]")))

                       (format (pcase backend
                                 ('latex (format "{\\color{%%s}%s %%s %%s %%s %%s}" (if strong "\\bfseries" "")))
                                 (_ (format "<%s style=\"color: %%s;\">%%s %%s %%s %%s</%s>" (if strong "strong" "p") (if strong "strong" "p"))))
                               color edcomm-begin contents′ signoff edcomm-end)))))

(provide 'org-remark-block)
;;; org-remark-block.el ends here
