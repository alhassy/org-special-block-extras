;;; org-callout-blocks.el --- Callouts: note, tip, important, warning, caution  -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 4.1.1
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "27.1") (org "9.6"))
;; Keywords: org, blocks, convenience
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Five callout blocks, after GitHub's alerts: a coloured rule down the left, under a titled heading.
;;
;;   #+begin_warning Mind the gap
;;   Trains do not wait.
;;   #+end_warning
;;
;; Full documentation at https://alhassy.github.io/org-special-block-extras

;;; Code:

(require 'org-defblock)

(defvar org-callouts
  '((note      "#0969da" "ℹ️" "Useful information that readers should know, even when skimming.")
    (tip       "#1a7f37" "💡" "Helpful advice for doing things better or more easily.")
    (important "#8250df" "❗" "Key information readers need to achieve their goal.")
    (warning   "#9a6700" "⚠️" "Urgent information that needs immediate attention, to avoid problems.")
    (caution   "#cf222e" "🛑" "Advises about risks or negative outcomes of certain actions."))
  "The callout blocks, as (NAME COLOUR ICON PURPOSE) entries; the colours and purposes are GitHub's.")

(defun org--callout (backend colour icon title contents)
  "(BACKEND, COLOUR, ICON, TITLE, CONTENTS) ↦ CONTENTS under TITLE, with a COLOUR rule down its left.
HTML prefixes TITLE with ICON; LaTeX, whose fonts rarely have emoji, does not, and uses tcolorbox."
  (pcase backend
    ('latex (concat (org--latex-define-color "osbe-callout" colour)
                    (format (concat "\\begin{tcolorbox}[colframe=osbe-callout,colback=osbe-callout!5!white,"
                                    "colbacktitle=osbe-callout!5!white,coltitle=osbe-callout,fonttitle=\\bfseries,"
                                    "title={%s},sharp corners,leftrule=3pt,toprule=0pt,rightrule=0pt,bottomrule=0pt]\n"
                                    "%s\n\\end{tcolorbox}")
                            title contents)))
    (_ (format (concat "<div class=\"callout\" style=\"border-left: 0.25em solid %s; padding: 0.5em 1em; margin: 1em 0;\">\n"
                       "<p style=\"color: %s; font-weight: bold; margin: 0;\">%s %s</p>\n%s\n</div>")
               colour colour icon title contents))))

;; No links or tags: a callout is a whole paragraph, and a section tagged :note: is too common to claim.
(pcase-dolist (`(,name ,colour ,icon ,purpose) org-callouts)
  (eval `(org-defblock ,name (&optional (title ,(capitalize (symbol-name name))) :link nil :tag nil)
           ,(format "%s\nA callout, titled TITLE, with a %s rule down its left; see `org-callouts'." purpose colour)
           (org--callout backend ,colour ,icon title contents))
        t))

(provide 'org-callout-blocks)
;;; org-callout-blocks.el ends here
