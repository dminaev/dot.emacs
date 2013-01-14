;;; hmoccur.el --- Multi-buffer or multi-file regexp occurrence location.

;; Copyright (C) 1991 Markus Freericks
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
;;
;; Modified by Bob Weiner to allow selection of a set of files within a
;; single directory to search.  By default, {M-x moccur RTN} searches
;; current buffers with files attached.
;;
;; Date: 1 Aug 91 15:47:27 GMT
;; From: mfx@cs.tu-berlin.de (Markus Freericks)
;; Subject: moccur - multibuffer occurences
;;
;; While editing some dozen or so files, i had the dire need for
;; something like 'occur' that can cope with multiple buffers. This has
;; probably been done before; but still, here is my try at it. It seems
;; to be very useful.
;; 
;; How to use it: simple say 
;; 	M-x moccur <regexp> 
;; moccur then searches through *all buffers* currently existing that are
;; bound to files and displays the occurences in a buffer that runs in
;; Moccur-mode. Change to that buffer, scroll around, and say C-c C-c
;; to jump to the occurrence. Quite simple.
;; 
;; Incompatibilites to Occur mode: 
;; a) it browses through *all* buffers that have a file name
;; associated with them; those may or may not include the current
;; buffer. Especially, while standard occur works 
;; on 'all lines following point', Moccur does not.
;; b) there is no support for the 'NLINE' argument.
;;
;; Usage:
;; moccur <regexp> shows all occurences of <regexp> in all buffers
;; currently existing that refer to files.
;; the occurences are displayed in a buffer running in Moccur mode;
;; C-c C-c gets you to the occurence
;;

;;; Code:

;;;
;;; Public variables
;;;

(defconst moccur-source-prefix "@loc> "
  "Prefix for lines indicating source of matches.")

;;;
;;; Public functions
;;;

(defun moccur (regexp &optional file-regexp no-fold-search)
  "Show all lines of all buffers containing a match for REGEXP.
With optional FILE-REGEXP, a pattern matching to files in a single
directory, search matching files rather than current buffers.
The lines are shown in a buffer named *Moccur* which serves as a menu to
find any of the occurrences in this buffer.
\\[describe-mode] in that buffer explains how."
  (interactive "sRegexp to find occurrences of: \nsFiles to search (default current file buffers): ")
  (if (equal file-regexp "") (setq file-regexp nil))
  (let*  ((buffers (if file-regexp (directory-files
				    (expand-file-name
				     (or (file-name-directory
					  file-regexp) "."))
				    'full (file-name-nondirectory file-regexp))
		     (buffer-list)))
	  (occbuf (get-buffer-create "*Moccur*"))
	  (matches 0)
	  (firstmatch t))
    (set-buffer occbuf)
    (setq buffer-read-only nil)
    (widen)
    (erase-buffer)
    (insert "Lines matching '" regexp "':\n\n")
    (let ((currbuf) (currfile) (kill-buf))
      (while buffers
	(setq currbuf (car buffers)
	      currfile (if (stringp currbuf) currbuf)
	      kill-buf (and currfile (not (get-file-buffer currfile)))
	      buffers (cdr buffers))
	(if currfile
	    (setq currbuf (find-file-noselect currfile))
	  (setq currfile (buffer-file-name currbuf)))
	(if (or (not currfile) (not currbuf))
	    nil
	  (set-buffer currbuf)
	  (let ((case-fold-search (not no-fold-search)))
	    (save-excursion
	      (goto-char (point-min))
	      (setq firstmatch t)
	      (while (re-search-forward regexp nil t)
		(setq matches (+ matches 1))
		(let* ((linenum (count-lines (point-min)(point)))
		       (tag (format "\n%4d:" linenum)))
		  (set-buffer occbuf)
		  (if firstmatch
		      (progn
			(insert moccur-source-prefix currfile "\n")
			(setq firstmatch nil)))
		  (insert tag)
		  (set-buffer currbuf)
		  (forward-word -1) ;; needed if match goes to eoline
		  (beginning-of-line)
		  (let ((beg (point)))
		    (end-of-line)
		    (append-to-buffer occbuf beg (point)))
		  (forward-line 1)))))
	  (save-excursion
	    (set-buffer occbuf)
	    (if (not firstmatch) (insert "\n\n"))
	    (if kill-buf (kill-buffer currbuf))))))
    (if (> matches 0)
	(progn
	  (set-buffer occbuf)
	  (moccur-mode)
	  (if (fboundp 'outline-minor-mode)
	      (and (progn (goto-char 1)
			  (search-forward "\C-m" nil t))
		   (outline-minor-mode 1)))
	  (goto-char (point-min))
	  (pop-to-buffer occbuf)
	  (message "%d matches." matches)
	  t)
      (message "No matches.")
      nil)))

(defun moccur-to ()
  "Go to the line where this occurrence was found."
  (interactive)
    (if (not (eq major-mode 'moccur-mode))
        (error "'moccur-to' must be called within a moccur buffer.")
	(let ((beg nil)
	      (line nil)
	      (lineno nil)
	      (dstbuf nil))
	  (save-excursion
	    (beginning-of-line)
	    (setq beg (point))
            (end-of-line)
            (setq line (buffer-substring beg (point)))
            (if (string-match "^[ ]*[0-9]+:" line)
                (progn
                  (setq lineno (string-to-int (substring
					       line 0 (match-end 0))))
                  (if (re-search-backward
		       (concat "^" moccur-source-prefix
			       "\"?\\([^\" \n]+\\)\"?") nil t)
                      (progn
                        (setq line (buffer-substring
				    (match-beginning 1) (match-end 1))
			      dstbuf (find-file-noselect line))
			(if (not dstbuf)
			    (message
			     "moccur-to: file '%s' is not readable" line)))
		    (error "No moccur header line for file.")))
	      (error "Not an moccur occurrence line.")))
	  (if (and lineno dstbuf)
	      (progn
		(message "Selection <%s> line %d." line lineno)
		(pop-to-buffer dstbuf)
		(goto-line lineno))))))

(fset 'moccur-mode-goto-occurrence 'moccur-to)


;;;
;;; Private functions
;;;

(defun moccur-mode ()
  "Major mode for output from \\[moccur].
Move point to one of the occurrences in this buffer,
then use \\[moccur-to] to go to the same occurrence
in the buffer that the occurrenc was found in.
\\{occur-mode-map}"
  (kill-all-local-variables)
  (use-local-map moccur-mode-map)
  (setq major-mode 'moccur-mode)
  (setq mode-name "Moccur"))

;;;
;;; Private variables
;;;

(defvar moccur-mode-map ())
(if moccur-mode-map
    ()
    (setq moccur-mode-map (make-sparse-keymap))
    (define-key moccur-mode-map "\C-c\C-c" 'moccur-to)
    (define-key moccur-mode-map " " 'moccur-to)
    (define-key moccur-mode-map "\C-m" 'moccur-to)
)

(provide 'hmoccur)

;;; hmoccur.el ends here
