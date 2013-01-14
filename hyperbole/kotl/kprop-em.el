;;; kprop-em.el --- Koutline text property handling under Emacs 19.

;; Copyright (C) 1993, 1994, 1995  Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;; Maintainer: Mats Lidell <matsl@contactor.se>
;; Keywords: outlines, wp

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

(require 'hversion)

;;;
;;; Public functions
;;;

(fset 'kproperty:get 'get-text-property)

(defun kproperty:map (function property value)
  "Apply FUNCTION to each PROPERTY `eq' to VALUE in the current buffer.
FUNCTION is called with point preceding PROPERTY and receives the list of
properties at point as an argument.  FUNCTION may not modify this list of
properties."
  (let ((result)
	(start (point-min)))
    (save-excursion
      (while (setq start
		   (text-property-any start (point-max) property value))
	(goto-char start)
	(setq result (cons (funcall function (text-properties-at start))
			   result))))
    (nreverse result)))

(fset 'kproperty:next-single-change 'next-single-property-change)

(fset 'kproperty:previous-single-change 'previous-single-property-change)

(fset 'kproperty:properties 'text-properties-at)

(defun kproperty:put (start end property-list &optional object)
  "From START to END, add PROPERTY-LIST properties to the text.
The optional fourth argument, OBJECT, is the string or buffer containing the
text.  Text inserted before or after this region does not inherit the added
properties."
  (add-text-properties
   start end (append property-list '(rear-nonsticky t)) object))

(defun kproperty:remove (start end property-list &optional object)
  "From START to END, remove the text properties in PROPERTY-LIST.
The optional fourth argument, OBJECT, is the string or buffer containing the
text.  PROPERTY-LIST should be a plist; if the value of a property is
non-nil, then only a property with a matching value will be removed.
Returns t if any property was changed, nil otherwise."
  (let ((changed) plist property value next)
    (while property-list
      (setq property (car property-list)
	    value (car (cdr property-list))
	    plist (list property value)
	    property-list (nthcdr 2 property-list)
	    next start)
      (while (setq next (text-property-any next end property value object))
	(remove-text-properties next (1+ next) plist object)
	(setq changed t next (1+ next))))
    changed))

(defun kproperty:replace-separator (pos label-separator old-sep-len)
  "Replace at POS the cell label separator with LABEL-SEPARATOR.
OLD-SEP-LEN is the length of the separator being replaced."
  (let (properties)
    (while (setq pos (kproperty:next-single-change (point) 'kcell))
      (goto-char pos)
      (setq properties (text-properties-at pos))
      ;; Replace label-separator while maintaining cell properties.
      (insert label-separator)
      (add-text-properties pos (+ pos 2) properties)
      (delete-region (point) (+ (point) old-sep-len)))))

(defun kproperty:set (property value)
  "Set PROPERTY of character at point to VALUE."
  (kproperty:put (point) (min (+ 2 (point)) (point-max))
		 (list property value)))

;;; kprop-em.el ends here
