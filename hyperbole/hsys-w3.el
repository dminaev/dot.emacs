;;; hsys-w3.el --- Hyperbole support for Emacs W3 World-Wide Web (WWW) browsing.

;; Copyright (C) 1994, 1995  Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;; Maintainer: Mats Lidell <matsl@contactor.se>
;; Keywords: comm, help, hypermedia

;; This file is part of GNU Hyperbole.

;; GNU Hyperbole is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; GNU Hyperbole is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;   This module defines an implicit button type and associated action and
;;   help types.  A press of the Action Key on a unified resource locator
;;   (URL) displays the referent for the URL.  A press of the Help Key on a
;;   URL displays a history list of previously browsed WWW documents.  Press
;;   the Action Key on any item from the history list to display it.
;;
;;   This requires the Emacs W3 World-Wide-Web browser available from:
;;     ftp://cs.indiana.edu/pub/elisp/w3/.
;;
;;   It assumes that you have set up to have w3 auto-loaded according to the
;;   setup instructions included with W3.  Specifically, `w3-fetch' should be
;;   autoloaded.
;;

;;; Code:

;;;
;;; Other required Elisp libraries
;;;

;;; Requires that 'w3' or other web browser code that is called be available.

;;;
;;; Public functions and types
;;;

(defib www-url ()
  "When not in a w3 buffer, follow any non-ftp url (link) at point.
Customize `browse-url' for what browser that is used."
  (if (not (eq major-mode 'w3-mode))
      (let ((link-and-pos (hpath:www-at-p t)))
	(if link-and-pos
	    (progn (ibut:label-set link-and-pos)
		   (hact 'www-url (car link-and-pos)))))))

(defact www-url (url)
  "Follows a link given by URL using `browse-url'."
  (interactive "sURL to follow: ")
  (or (stringp url)
      (error "(www-url): Link label must be given as a string."))
  (funcall 'browse-url url))

(defun www-url:help (&optional but)
  "Displays history list of www nodes previously visited with the W3 browser."
  (interactive)
  (if (fboundp 'w3-show-history-list)
      (hact 'w3-show-history-list)
    (hact 'error "(www-url:help): W3 must be loaded to display WWW history")))

(provide 'hsys-w3)

;;; hsys-w3.el ends here
