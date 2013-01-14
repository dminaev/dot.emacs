;;; wrolo-logic.el --- Performs logical retrievals on rolodex files

;; Copyright (C) 1989-1995, Free Software Foundation, Inc.
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
;;
;;  INSTALLATION:
;;
;;   See also wrolo.el.  These functions are separated from wrolo.el since many
;;   users may never want or need them.  They can be automatically loaded when
;;   desired by adding the following to one of your Emacs init files:
;;
;;    (autoload 'rolo-logic "wrolo-logic" "Logical rolodex search filters." t)
;;
;;  FEATURES:
;;
;;   1.  One command, 'rolo-logic' which takes a logical search expression as
;;       an argument and displays any matching entries.
;;
;;   2.  Logical 'and', 'or', 'not', and 'xor' rolodex entry retrieval filter
;;       functions. They take any number of string or boolean arguments and
;;       may be nested.  NOTE THAT THESE FUNCTIONS SHOULD NEVER BE CALLED
;;       DIRECTLY UNLESS THE FREE VARIABLES 'start' and 'end' ARE BOUND
;;       BEFOREHAND.
;;
;;  EXAMPLE:
;;
;;     (rolo-logic (function
;;                   (lambda ()
;;                     (rolo-and
;;                        (rolo-not "Tool-And-Die")
;;                        "secretary"))))
;;
;;   would find all non-Tool-And-Die Corp. secretaries in your rolodex.
;;
;;   The logical matching routines are not at all optimal, but then most
;;   rolodex files are not terribly lengthy either.
;;

;;; Code:

(require 'wrolo)

;;;###autoload
(defun rolo-logic (func &optional in-bufs count-only include-sub-entries
			      no-sub-entries-out)
  "Apply FUNC to all entries in optional IN-BUFS, display entries where FUNC is non-nil.
If IN-BUFS is nil, 'rolo-file-list' is used.  If optional COUNT-ONLY is
non-nil, don't display entries, return count of matching entries only.  If
optional INCLUDE-SUB-ENTRIES flag is non-nil, FUNC will be applied across all
sub-entries at once.  Default is to apply FUNC to each entry and sub-entry
separately.  Entries are displayed with all of their sub-entries unless
INCLUDE-SUB-ENTRIES is nil and optional NO-SUB-ENTRIES-OUT flag is non-nil.
FUNC should use the free variables 'start' and 'end' which contain the limits
of the region on which it should operate.  Returns number of applications of
FUNC that return non-nil."
  (interactive "xLogic function of no arguments, (lambda () (<function calls>): ")
  (let ((obuf (current-buffer))
	(display-buf (if count-only
			 nil
		       (prog1 (set-buffer (get-buffer-create rolo-display-buffer))
			 (setq buffer-read-only nil)
			 (erase-buffer)))))
    (let ((result
	    (mapcar
	     (function
	      (lambda (in-bufs)
		 (rolo-map-logic func in-bufs count-only include-sub-entries
				 no-sub-entries-out)))
	      (cond ((null in-bufs) rolo-file-list)
		    ((listp in-bufs) in-bufs)
		    ((list in-bufs))))))
      (let ((total-matches (apply '+ result)))
	(if (or count-only (= total-matches 0))
	    nil
	  (pop-to-buffer display-buf)
	  (goto-char (point-min))
	  (set-buffer-modified-p nil)
	  (setq buffer-read-only t)
	  (let ((buf (get-buffer-window obuf)))
	    (if buf (select-window buf) (switch-to-buffer buf))))
	(if (interactive-p)
	    (message (concat (if (= total-matches 0) "No" total-matches)
			     " matching entr"
			     (if (= total-matches 1) "y" "ies")
			     " found in rolodex.")))
	total-matches))))

(defun rolo-map-logic (func rolo-buf &optional count-only
			    include-sub-entries no-sub-entries-out)
  "Apply FUNC to all entries in ROLO-BUF, write to buffer entries where FUNC is non-nil.
If optional COUNT-ONLY is non-nil, don't display entries, return count of
matching entries only.  If optional INCLUDE-SUB-ENTRIES flag is non-nil, FUNC
will be applied across all sub-entries at once.  Default is to apply FUNC to
each entry and sub-entry separately.  Entries are displayed with all of their
sub-entries unless INCLUDE-SUB-ENTRIES is nil and optional NO-SUB-ENTRIES-OUT
flag is non-nil.  FUNC should use the free variables 'start' and 'end' which
contain the limits of the region on which it should operate.  Returns number
of applications of FUNC that return non-nil."
  (if (or (bufferp rolo-buf)
	  (if (file-exists-p rolo-buf)
	      (setq rolo-buf (find-file-noselect rolo-buf t))))
      (let* ((display-buf (set-buffer (get-buffer-create rolo-display-buffer)))
	     (buffer-read-only))
	(let ((hdr-pos) (num-found 0))
	  (set-buffer rolo-buf)
	  (goto-char (point-min))
	  (if (re-search-forward rolo-hdr-regexp nil t 2)
	      (progn (forward-line)
		     (setq hdr-pos (cons (point-min) (point)))))
	  (let* ((start)
		 (end)
		 (end-entry-hdr)
		 (curr-entry-level))
	    (while (re-search-forward rolo-entry-regexp nil t)
	      (setq start (save-excursion (beginning-of-line) (point))
		    next-entry-exists nil
		    end-entry-hdr (point)
		    curr-entry-level (buffer-substring start end-entry-hdr)
		    end (rolo-to-entry-end include-sub-entries curr-entry-level))
	      (let ((fun (funcall func)))
		(or count-only 
		    (and fun (= num-found 0) hdr-pos
			 (append-to-buffer display-buf
					   (car hdr-pos) (cdr hdr-pos))))
		(if fun 
		    (progn (goto-char end)
			   (setq num-found (1+ num-found)
				 end (if (or include-sub-entries
					     no-sub-entries-out)
					 end
				       (goto-char (rolo-to-entry-end
						    t curr-entry-level))))
			   (or count-only
			       (append-to-buffer display-buf start end)))
		  (goto-char end-entry-hdr)))))
	  (rolo-kill-buffer rolo-buf)
	  num-found))
    0))


;;
;; INTERNAL FUNCTIONS.
;;

;; Do NOT call the following functions directly.
;; Send them as parts of a lambda expression to 'rolo-logic'.

(defun rolo-not (&rest pat-list)
  "Logical <not> rolodex entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a string."
  (let ((pat))
    (while (and pat-list
		(or (not (setq pat (car pat-list)))
		    (and (not (eq pat t))
			 (goto-char start)
			 (not (search-forward pat end t)))))
      (setq pat-list (cdr pat-list)))
    (if pat-list nil t)))

(defun rolo-or (&rest pat-list)
  "Logical <or> rolodex entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a string."
  (if (memq t pat-list)
      t
    (let ((pat))
      (while (and pat-list
		  (or (not (setq pat (car pat-list)))
		      (and (not (eq pat t))
			   (goto-char start)
			   (not (search-forward pat end t)))))
	(setq pat-list (cdr pat-list)))
      (if pat-list t nil))))

(defun rolo-xor (&rest pat-list)
  "Logical <xor> rolodex entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a string."
  (let ((pat)
	(matches 0))
    (while (and pat-list
		(or (not (setq pat (car pat-list)))
		    (and (or (eq pat t)
			     (not (goto-char start))
			     (search-forward pat end t))
			 (setq matches (1+ matches)))
		    t)
		(< matches 2))
      (setq pat-list (cdr pat-list)))
    (= matches 1)))

(defun rolo-and (&rest pat-list)
  "Logical <and> rolodex entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a string."
  (if (memq nil pat-list)
      nil
    (let ((pat))
      (while (and pat-list
		  (setq pat (car pat-list))
		  (or (eq pat t)
		      (not (goto-char start))
		      (search-forward pat end t)))
	(setq pat-list (cdr pat-list)))
      (if pat-list nil t))))

;; Work with regular expression patterns rather than strings

(defun rolo-r-not (&rest pat-list)
  "Logical <not> rolodex entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a string."
  (let ((pat))
    (while (and pat-list
		(or (not (setq pat (car pat-list)))
		    (and (not (eq pat t))
			 (goto-char start)
			 (not (re-search-forward pat end t)))))
      (setq pat-list (cdr pat-list)))
    (if pat-list nil t)))

(defun rolo-r-or (&rest pat-list)
  "Logical <or> rolodex entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a string."
  (if (memq t pat-list)
      t
    (let ((pat))
      (while (and pat-list
		  (or (not (setq pat (car pat-list)))
		      (and (not (eq pat t))
			   (goto-char start)
			   (not (re-search-forward pat end t)))))
	(setq pat-list (cdr pat-list)))
      (if pat-list t nil))))

(defun rolo-r-xor (&rest pat-list)
  "Logical <xor> rolodex entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a string."
  (let ((pat)
	(matches 0))
    (while (and pat-list
		(or (not (setq pat (car pat-list)))
		    (and (or (eq pat t)
			     (not (goto-char start))
			     (re-search-forward pat end t))
			 (setq matches (1+ matches)))
		    t)
		(< matches 2))
      (setq pat-list (cdr pat-list)))
    (= matches 1)))

(defun rolo-r-and (&rest pat-list)
  "Logical <and> rolodex entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a string."
  (if (memq nil pat-list)
      nil
    (let ((pat))
      (while (and pat-list
		  (setq pat (car pat-list))
		  (or (eq pat t)
		      (not (goto-char start))
		      (re-search-forward pat end t)))
	(setq pat-list (cdr pat-list)))
      (if pat-list nil t))))

(provide 'wrolo-logic)

;;; wrolo-logic.el ends here
