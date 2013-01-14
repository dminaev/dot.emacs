;;; hvar.el --- Variable manipulation routines for Hyperbole.

;; Copyright (C) 1991-1995, Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;; Maintainer: Mats Lidell <matsl@contactor.se>
;; Keywords: extensions, hypermedia

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

;;; Code:

;;;
;;; Other required Elisp libraries
;;;

(require 'set)

;;;
;;; Public functions
;;;

;;;###autoload
(defun var:append (var-symbol-name list-to-add)
  "Appends to value held by VAR-SYMBOL-NAME, LIST-TO-ADD.  Returns new value.
If VAR-SYMBOL-NAME is unbound, it is set to LIST-TO-ADD.
Often used to append to 'hook' variables."
  (let ((val))
    (if (and (boundp var-symbol-name)
	     (setq val (symbol-value var-symbol-name))
	     (or (if (symbolp val) (setq val (cons val nil)))
		 (listp val)))
	;; Don't add if list elts are already there.
	(if (memq nil (mapcar (function
				(lambda (elt) (set:member elt val)))
			      list-to-add))
	    (set-variable var-symbol-name
			  (if (eq (car val) 'lambda)
			      (apply 'list val list-to-add)
			    (append val list-to-add)))
	  val)
      (set-variable var-symbol-name list-to-add))))

(provide 'hvar)


;;; hvar.el ends here
