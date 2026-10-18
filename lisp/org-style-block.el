;;; org-style-block.el --- CSS for the exported page  -*- lexical-binding: t; -*-

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

;; A style block styles the exported HTML page with its CSS; elsewhere, it is nothing.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'org-defblock)

;;;;;; Style ---CSS for the exported page

(org-defblock style (main-arg nil :link nil :tag nil)
  "Style the exported HTML page with the block's CSS, taken verbatim; elsewhere, nothing.

  #+begin_style
  h2 { color: teal; }
  #+end_style"
  (if (equal backend 'html) (format "<style>\n%s\n</style>" raw-contents) ""))

(provide 'org-style-block)
;;; org-style-block.el ends here
