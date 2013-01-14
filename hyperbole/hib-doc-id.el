;;; hib-doc-id.el --- Implicit button type for document id index entries.

;; Copyright (C) 1992-1995 Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;; Maintainer: Mats Lidell <matsl@contactor.se>
;; Keywords: docs, extensions, hypermedia

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
;;  TO USE:
;;
;;   Pressing the Action Key on a doc id such as, [Emacs-001],
;;   displays the online version of the document, if any.  Pressing the
;;   Assist Key on it displays its document index entry.
;;
;;  TO INSTALL:
;;
;;   Set the value of 'doc-id-indices' before using the 'doc-id'
;;   implicit button type defined herein or you will get an error telling you
;;   to do so.  See the documentation for 'doc-id-indices'.
;;
;;   You must explicitly load this package in order to use it, since
;;   Hyperbole does not load it by default.
;;
;;   At this site, we use doc ids of the form, [Emacs-001], delimited by
;;   brackets, starting with a subject name, followed by a -, followed by a
;;   multi-digit numeric identifier.
;;
;;   Typically an index entry should have links to all available forms of its
;;   document, e.g. online, printed, source.  Below is the index entry form
;;   we use.  The default variable settings herein work with our formats.  If
;;   you prefer different ones, you must change all of the variable values.
;;
;;  --------------------------------------------------------------------------
;;  Title:                                                  ID: []
;;  Email-To:
;;  Distribution:     
;;  
;;  Abstract:         
;;                    
;;                    
;;  References:       
;;  
;;  Author:           
;;  Copyright:        
;;  Keywords:         
;;  
;;  Online-Format:    
;;  Online-Loc:       ""
;;  Printed-Format:
;;  Printed-Loc:      Local Library
;;  Printable-Loc:    ""
;;  Source-Format:
;;  Source-Loc:       ""
;;  
;;  Date:             
;;  Version:          
;;  Version-Changes:  
;;  --------------------------------------------------------------------------
;;   

;;; Code:

;;;
;;; Public implicit button types
;;;
  
;;;
;;; Displays a documentation index entry given an ID.
;;;

(defact link-to-doc (doc-id)
  "Displays online version of a document given by DOC-ID (no delimiters), in other window.
If online version of document is not found in `doc-id-indices', an error is signalled."
  (interactive "sID for document to link to (omit delimiters): ")
  (let ((rolo-display-buffer (hypb:help-buf-name "Doc ID"))
	(delim-doc-id (concat doc-id-start doc-id doc-id-end)))
    (cond ((null doc-id-indices)
	   (error "(doc-id-index-entry): You must set the 'doc-id-indices' variable first."))
	  ((let ((rolo-entry-regexp doc-id-index-entry-regexp))
	     (= 0 (rolo-grep (funcall doc-id-match doc-id)
			     1 doc-id-indices nil 'no-display)))
	   (error "(doc-id-index-entry): %s not found in document index."
		  delim-doc-id))
	  ;; Matching index entry has been put into 'rolo-display-buffer'.
	  (t (save-excursion
	       (set-buffer rolo-display-buffer)
	       (goto-char (point-min))
	       (message "Searching for document %s..." delim-doc-id)
	       (if (re-search-forward doc-id-online-regexp nil t)
		   (progn
		     (goto-char (match-beginning 1))
		     (let ((doc-path (buffer-substring
				      (match-beginning 1) (match-end 1)))
			   (ibut (ibut:at-p)))
		       (if ibut
			   (progn (hbut:act ibut)
				  (message "Displaying %s." delim-doc-id))
			 (error "(link-to-doc): %s invalid online location: %s"
				delim-doc-id doc-path))))
		 (error "(link-to-doc): %s is unavailable in online form."
			delim-doc-id)))))))

(defib doc-id ()
  "Displays an index entry for a site-specific document given its id.
Ids must be delimited by 'doc-id-start' and 'doc-id-end' and must
match the function given by 'doc-id-p'."
  (and (not (bolp))
       (let* ((id-and-pos (hbut:label-p t doc-id-start doc-id-end t))
	      (id (car id-and-pos)))
	 (if (funcall doc-id-p id)
	     (progn (ibut:label-set id-and-pos)
		    (hact 'link-to-doc id))))))


;;;
;;; Displays a doc from SW Process Tree (Motorola Paging Products Specific)
;;;

(if (file-exists-p "/proj/process/ppg/")
    (defib ppg-sw-process ()
      "Display a Paging Products software process document from document id at point."
      (let ((path (hpath:at-p nil t)))
	(if (and path (string-match "/.+%s.+%s" path))
	    (progn (require 'sw-process)
		   (ibut:label-set path)
		   (setq path (format path ppg-sw-process-file-format
				      ppg-sw-process-file-suffix))
		   (if (file-exists-p path)
		       (hact 'link-to-file path)
		     (if (re-search-forward
			  "^Source-Loc:[ \t]*\"\\([^\"]+\\)\"" nil t)
			 (progn
			   (goto-char (match-beginning 1))
			   (let ((path-but (ibut:at-p)))
			     (if path-but
				 (hbut:act path-but)))))))))))

;;;
;;; Public variables
;;;

(defvar doc-id-indices '()
  "List of pathnames in which to search for site-specific document index entries.
Each file must utilize a wrolo record format, with each record start
delimited by 'doc-id-index-entry-regexp'.")

;;;
;;; Private functions
;;;

(defun doc-id:help (but)
  "Displays site-specific document index entry given by doc-id BUT, in other window.
Also displays standard Hyperbole help for implicit button BUT."
  (let ((rolo-entry-regexp doc-id-index-entry-regexp)
	(rolo-display-buffer (hypb:help-buf-name "Doc ID"))
	(doc-id (hbut:key-to-label (hattr:get but 'lbl-key))))
    (cond ((null doc-id-indices)
	   (error "(doc-id-index-entry): You must set the 'doc-id-indices' variable first."))
	  ((= 0 (rolo-grep (funcall doc-id-match doc-id) 1 doc-id-indices))
	   (error
	     "(doc-id-index-entry): No document index entry found for %s%s%s."
		  doc-id-start doc-id doc-id-end)))
    (let* ((report-buf (hypb:help-buf-name))
	   (temp-buffer-show-hook
	     (function
	       (lambda (buffer)
		 (setq *hkey-wconfig*
		       (current-window-configuration)))
	       (let ((wind (get-buffer-create buffer)))
		 (setq minibuffer-scroll-window wind))))
	   (temp-buffer-show-function temp-buffer-show-hook))
      (hbut:report but)
      (save-excursion
	(set-buffer rolo-display-buffer)
	(setq buffer-read-only nil)
	(goto-char (point-max))
	(insert-buffer report-buf)
	(set-buffer-modified-p nil)
	(setq buffer-read-only nil)
	(goto-char (point-min)))
      (kill-buffer report-buf)
      )))

;;;
;;; Private variables
;;;

(defvar doc-id-start "["
  "String which delimits start of a site-specific document id.")
(defvar doc-id-end   "]"
  "String which delimits end of a site-specific document id.")

(defvar doc-id-index-entry-regexp "^------+[ \t\n]+Title:"
  "Regexp which matches start of a site-specific document index entry.")

(defvar doc-id-match
  (function (lambda (doc-id)
	      (concat "ID:[ \t]*\\[" (regexp-quote doc-id) "\\]")))
  "Function which returns regexp which matches only in DOC-ID's index entry.")

(defvar doc-id-p (function
		   (lambda (str)
		     (and (stringp str)
			  (> (length str) 0)
			  (= ?w (char-syntax (aref str 0)))
			  (string-match "\\`\\w+-[0-9][0-9][0-9]+\\'" str))))
  "Boolean function to test whether arg 'str' is a doc id or not.")

(defvar doc-id-online-regexp "^Online-Loc:[ \t]*\"\\([^\"]+\\)\""
  "Regexp whose 1st grouping matches an implicit button which displays an online document within an index entry.")

(provide 'hib-doc-id)

;;; hib-doc-id.el ends here
