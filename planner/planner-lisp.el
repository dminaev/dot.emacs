;;; planner-lisp.el --- Interactive Emacs Lisp integration for Planner mode

;; Copyright (C) 2004, 2005 Free Software Foundation, Inc.

;; Author: Sacha Chua <sacha@free.net.ph>
;; Keywords: planner, gnus
;; URL: http://www.plannerlove.com/

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file allows you to launch Emacs Lisp functions
;; from a planner page.
;;
;; Example:
;; [[lisp:/plan][Plan]] will be rendered as "Plan". Selecting the link
;; will run the plan function interactively.
;;
;; If the link is a lisp form it is evaluated non-interactively:
;; [[lisp:/(man "bash")][bash manual]] will be rendered as "bash
;; manual".  Selecting the link will show the bash man page in Emacs.

;;; Contributors:

;; Gerd Flaig fixed the docstring for `planner-lisp-browse-url'.

;; Jim Ottaway made it such that if the URL like a lisp form, read and
;; evaluate it, otherwise call it interactively as before.

;;; Code:

(require 'planner)

;;;###autoload
(defun planner-lisp-browse-url (url)
  "If this is a LISP URL, evaluate it."
  (when (string-match "\\`lisp:/*\\(.+\\)" url)
    (let ((form (match-string 1 url)))
      (if (string-match "\\`\\s-*\\((.+)\\)\\s-*\\'" form)
          (eval (read (match-string 1 form)))
        (eval (read (concat "(call-interactively '" form ")")))))
    t))

(planner-add-protocol "lisp:/*" 'planner-lisp-browse-url nil)

(provide 'planner-lisp)

;;; planner-lisp.el ends here
