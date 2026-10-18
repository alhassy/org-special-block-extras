;;; org-thread-blockcall.el --- Build new special blocks by threading text through existing ones  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.18
;; Package-Requires: ((dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; By way of example, the `solution' block of org-solution-block.el is a red
;; `details' inside a blue `box' inside another `details':
;;
;;     (org-defblock solution (title "Solution" reprimand "Did you try?" really "For real")
;;       (org-thread-blockcall raw-contents
;;                             (details really :title-color "red")
;;                             (box reprimand :background-color "blue")
;;                             (details title)))
;;
;; That is, `org-thread-blockcall' pipes a block's contents through a chain of
;; other blocks, much as `->>' pipes a value through functions:
;;
;;     (thread-blockcall x)       = x
;;     (thread-blockcall x (f a)) = (blockcall f a x)
;;     (thread-blockcall x f₁ f₂) ≈ (f₂ (f₁ x))
;;
;; Each form is (block-name main-arg :key value …); `org--blockcall', of
;; org-defblock.el, is to blocks what `funcall' is to functions.
;;
;; Why not simply compose the handlers?  Every (org-defblock B …) gives rise to
;; a function `org-block/B' : backend × raw-contents × main-arg × keys → String,
;; so it seems the body above could just be
;;
;;     (org-block/details backend
;;       (org-block/box backend
;;         (org-block/details backend raw-contents really :title-color "red")
;;         reprimand :background-color "blue")
;;       title)
;;
;; Besides reading inside-out, with `backend' threaded by hand, this exports
;; wrongly: HTML gets a stray literal “#+end_export”.  The culprit is that a
;; handler yields *Org* text ---backend output in “#+begin_export” blocks,
;; between bits of Org that `org-parse' left for Org--- whereas the body of the
;; enclosing `org-defblock' must yield *backend* text, since its result is itself
;; wrapped in an export block.  So composed handlers nest export blocks within
;; an export block, which Org does not support.
;;
;; Hence, `org--blockcall' wraps each call in “#+end_export … #+begin_export”,
;; closing the enclosing export block so its Org result is read as Org; and
;; `org-thread-blockcall' reopens that wrapping between consecutive calls, so
;; each block receives plain Org.  That bookkeeping is the ≈ in the third law.

;;; Code:

(require 'dash)
(require 'org-defblock)

(defmacro org-thread-blockcall (body &rest forms)
  "(BODY, (b₁ a₁ …), …, (bₙ aₙ …)) ↦ BODY in block b₁, that in b₂, …, that in bₙ."
  (if (not forms) body
    `(-let [result (org--blockcall ,@(car forms) ,body)]
       ,@(cl-loop for b in (cdr forms)
                  collect `(setq result (org--blockcall ,@b (concat "#+begin_export\n" result "\n#+end_export"))))
       result)))

(provide 'org-thread-blockcall)
;;; org-thread-blockcall.el ends here
