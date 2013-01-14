;;; hbmap.el --- Hyperbole button map maintenance for queries and lookups.

;; Copyright (C) 1991-1995 Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;; Maintainer: Mats Lidell <matsl@contactor.se>
;; Keywords: hypermedia, matching

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
;;; Public variables
;;;

(defvar hbmap:filename "HYPB"
  "*Filename used for quick access button files.")

;;;
;;; Public functions
;;;

(defun hbmap:dir-add (dir-name &optional no-save)
  "Adds DIR-NAME to map of all directories in which user has written buttons.
Returns t iff DIR-NAME is not already in map, nil if it is, and some
other value when cannot read or write map.
Optional NO-SAVE disables saving of the map after an add."
  (hbmap:dir-operate (function (lambda (dir) (not (hbmap:dir-member dir))))
		     dir-name
		     '(progn (prin1 (list dir-name) buf) (terpri buf))
		     no-save))

(defun hbmap:dir-list ()
  "Returns list of all directories in which user has written buttons."
  (save-excursion
    (let ((buf (if (and (file-exists-p hbmap:dir-filename)
			(not (file-readable-p hbmap:dir-filename)))
		   nil
		 (find-file-noselect hbmap:dir-filename)))
	  (dirs))
      (if buf
	  (progn (set-buffer buf)
		 (goto-char (point-min))
		 (condition-case ()
		     (while (setq dirs (cons (car (read (current-buffer)))
					     dirs)))
		   (error t))
		 dirs)))))

(defun hbmap:dir-remove (dir-name &optional no-save)
  "Removes DIR-NAME from map of all dirs in which user has written buttons.
Returns t iff DIR-NAME is in the map and is successfully removed, nil if it
is not, and some other value when the map is not readable or writable.
Optional NO-SAVE disables saving of the map after a removal."
(hbmap:dir-operate 'hbmap:dir-member dir-name
		   '(delete-region (point) (progn (forward-line 1) (point)))
		   no-save))

(defun hbmap:dir-member (dir-name)
  "Returns t iff DIR-NAME is a member of user's Hyperbole map, else nil.
If t, point is left at the start of the matching map entry.  If nil,
point is left in a position appropriate for insertion of a new entry."
  (let ((obuf (current-buffer))
	(buf (and (file-exists-p hbmap:dir-filename)
		  (find-file-noselect hbmap:dir-filename)))
	(rtn))
    (if buf
	(progn (set-buffer buf) (widen) (goto-char 1)
	       (if (search-forward (concat "\n(\"" dir-name "\"") nil t)
		   (progn (beginning-of-line) (setq rtn t))
		 (goto-char 1) (or (= (forward-line 1) 0) (insert "\n")))
	       (set-buffer obuf)))
    rtn))

;;;
;;; Private functions
;;;

(defun hbmap:dir-operate (pred dir-name form &optional no-save)
  "If PRED called on DIR-NAME is non-nil, evaluates FORM.
Returns t if PRED evaluation is successful and nil when not, except when
hbmap is not readable or writable, in which case returns a symbol indicating
the error.  Optional NO-SAVE disables saving of the map after operation."
  (save-excursion
    (let ((buf (if (and (file-exists-p hbmap:dir-filename)
			(not (file-readable-p hbmap:dir-filename)))
		   nil
		 (find-file-noselect hbmap:dir-filename))))
      (if buf
	  (progn (set-buffer buf)
		 (if (funcall pred dir-name)
		     (progn
		       (setq buffer-read-only nil)
		       (eval form)
		       (if no-save t
			 (if (file-writable-p buffer-file-name)
			     (progn (save-buffer) t)
			   'hbmap-not-writable)))))
	'hbmap-not-readable))))

;;;
;;; Private variables
;;;

(defvar hbmap:dir-user
  (if (memq system-type '(ms-windows windows-nt ms-dos))
      "c:/_hyperb/" "~/.hyperb/")
  "Per user directory in which to store top level Hyperbole map data.
Must end with a directory separator.
Hyperbole will try to create it whenever 'hyperb:init' is called.")

(defvar hbmap:dir-filename
  (expand-file-name  "HBMAP" hbmap:dir-user)
  "Name of a file that lists all dirs to which a user has written buttons.
See also 'hbmap:dir-user'.
If you change its value, you will be unable to search for buttons created by
others who use a different value!")

(provide 'hbmap)

;;; hbmap.el ends here
