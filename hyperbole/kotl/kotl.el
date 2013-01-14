;;; kotl.el --- Internal representation of outline kcells used by kviews.

;; Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;;         Kellie Clark
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

(mapcar 'require '(klabel knode hinit htz))

;;;
;;; Public variables
;;;

(defvar kcell:read-only-attributes
  '(idstamp creator create-time modifier mod-time)
  "List of kcell attributes which may not be modified by a user.
Add to this list but don't remove any of the default elements.")

;;;
;;; Public functions
;;;

;;;
;;; kcell
;;;

(fset 'kcell:contents     'knode:contents)

(defun kcell:copy (kcell)
  "Return a copy of KCELL."
  (knode:copy kcell))

(defun kcell:create (contents idstamp &optional plist)
  "Return a new kcell which stores CONTENTS (a string or nil), has permanent IDSTAMP (an integer), and optional additional property list, PLIST.
User id of `creator' of cell and `create-time' are added to cell's PLIST if
not already there."
  (and contents (not (stringp contents))
       (error "(kcell:create): Invalid `contents' argument: %s" contents))
  (if (or (not (integerp idstamp)) (< idstamp 0))
      (error "(kcell:create): Invalid `idstamp' argument: %s" idstamp))
  (knode:create
   contents (nconc (list 'idstamp idstamp)
		   (if (memq 'creator plist)
		       nil
		     (list 'creator (concat (user-login-name)
					    hyperb:host-domain)
			   'create-time (htz:date-sortable-gmt)))
		   plist)))

(defun kcell:create-top (&optional file counter)
  "Return a new koutline top cell optionally attached to FILE with current idstamp COUNTER."
  (kcell:create nil 0
		;; id-counter = max idstamp value given out in this kotl
		(list 'id-counter (or counter 0) 'file file)))

(defun kcell:get-attr (kcell attribute)
  "Return the value of KCELL's ATTRIBUTE."
  (knode:get-attr (kcell:plist kcell) attribute))

(defun kcell:idstamp (kcell)
  "Return permanent idstamp of KCELL as an integer."
  (kcell:get-attr kcell 'idstamp))

(fset 'kcell:is-p      'knode:is-p)

(defun kcell:plist (kcell)
  (knode:get-attr kcell 'plist))

(defun kcell:ref-to-id (cell-ref)
  "Returns a CELL-REF string converted to a cell identifier string.
If CELL-REF contains both a relative and a permanent id, the permanent id is
returned.  If CELL-REF is invalid, nil is returned.

CELL-REF may be of any of the following forms:
  1b        - relative id, augment style
  1.2       - relative id, legal style
  012       - permanent idstamp
  1a=012    - both relative and permanent ids (in that order) separated by =
  |viewspec - a viewspec setting, rather than a cell reference
  :viewspec - an augment viewspec, ignored for now.

Optionally, any of the above id forms may be followed by a period and some
alpha characters indicating a location relative to the id.

Optionally, any of these id forms (or the relative form) may be followed by
zero or more whitespace characters, a | and some view specification
characters.  Augment viewspec characters preceded by a colon are ignored, for
now."

  (if (not (stringp cell-ref))
      nil
    (setq cell-ref (hypb:replace-match-string "\\s +" cell-ref "" t))
    (let ((specs) result)
      ;; Ignore Augment :viewspecs.
      (if (string-match ":" cell-ref)
	  (setq cell-ref (substring cell-ref 0 (match-beginning 0))))
      ;; Separate koutline |viewspecs from cell id.
      (if (string-match "\\(\\.[a-zA-Z]\\||\\)" cell-ref)
	  (setq specs (substring cell-ref (match-beginning 1))
		cell-ref (substring cell-ref 0 (match-beginning 0))))
      (setq result
	    (cond
	     ((string-match "[^.= \t\n0-9a-zA-Z]" cell-ref) nil)
	     ((string-match "^\\([.0-9a-zA-Z]+\\)=\\(0[0-9]*\\)$"
			    cell-ref)
	      (substring cell-ref (match-beginning 2) (match-end 2)))
	     ((string-match "^\\([.0-9a-zA-Z]+\\)$" cell-ref)
	      (substring cell-ref (match-beginning 1) (match-end 1)))))
      (cond (result
	     (if specs (concat result specs) result))
	    (specs
	     (if (= ?| (aref specs 0)) specs))))))
	
(defun kcell:remove-attr (kcell attribute)
  "Remove KCELL's ATTRIBUTE, if any, return modified KCELL."
  (knode:set-attr
   kcell 'plist (knode:remove-attr (kcell:plist kcell) attribute)))

(defun kcell:set-attr (kcell attribute value)
  "Set KCELL's ATTRIBUTE to VALUE and return modified KCELL."
  (knode:set-attr
   kcell 'plist (knode:set-attr (kcell:plist kcell)
				attribute value)))

(defun kcell:set-create-time (kcell)
  "Store the time of creation of KCELL."
  (kcell:set-attr kcell 'create-time (htz:date-sortable-gmt)))

(defun kcell:set-creator (kcell)
  "Store the current user's id as the creator of KCELL."
  (kcell:set-attr
   kcell 'creator (concat (user-login-name) hyperb:host-domain)))

(defun kcell:set-idstamp (kcell idstamp)
  "Set KCELL's permanent IDSTAMP (an integer) and return IDSTAMP."
  (kcell:set-attr kcell 'idstamp idstamp)
  (kcell:idstamp kcell))

;;;
;;; kotl-data - Persistent representation of kotl cells (written to files).
;;;

(defun kotl-data:create (cell)
  "Given a kotl CELL, return a kotl-data structure to write to a file.
If CELL, its idstamp, or its property list are nil, this repairs the cell by
assuming it is the cell at point and filling in the missing information."
   (let ((idstamp (kcell:idstamp cell))
	 (plist (nthcdr 2 (kcell:plist cell))))
     (if (and cell idstamp plist)
	 (vector idstamp plist)
       (kotl-data:create
	(kcell:create nil
		      (or idstamp (kview:id-increment kview))
		      plist)))))

(defun kotl-data:idstamp (kotl-data)
  (aref kotl-data 0))

(defun kotl-data:plist-v2 (kotl-data)
  (aref kotl-data 2))

(defun kotl-data:plist-v3 (kotl-data)
  (aref kotl-data 1))

(defun kotl-data:to-kcell-v2 (kotl-data)
  (if (vectorp kotl-data)
      (kcell:create
       ;; Cell contents are no longer put into cells themselves by default
       ;; when a file is read.  The contents are stored within the kview
       ;; buffer, so use nil as a place-holder.
       nil
       ;; Repair invalid idstamps on the fly.
       (or (kotl-data:idstamp kotl-data) (kview:id-increment kview))
       (kotl-data:plist-v2 kotl-data))
    ;; Repair invalid cells on the fly.
    (kcell:create nil (kview:id-increment kview))))

(defun kotl-data:to-kcell-v3 (kotl-data)
  (if (vectorp kotl-data)
      (kcell:create
       ;; Cell contents are no longer put into cells themselves by default
       ;; when a file is read.  The contents are stored within the kview
       ;; buffer, so use nil as a place-holder.
       nil
       ;; Repair invalid idstamps on the fly.
       (or (kotl-data:idstamp kotl-data) (kview:id-increment kview))
       (kotl-data:plist-v3 kotl-data))
    ;; Repair invalid cells on the fly.
    (kcell:create nil (kview:id-increment kview))))

(provide 'kotl)

;;; kotl.el ends here
