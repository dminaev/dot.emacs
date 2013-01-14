;;; kimport.el --- Convert and insert other outline file formats into koutlines.

;; Copyright (C)  Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;;         Kellie Clark
;; Maintainer: Mats Lidell <matsl@contactor.se>
;; Keywords: data, outlines, wp

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

;; kfile.el requires kotl-mode.el which requires kimport.el.
(require 'wrolo)

;;;
;;; Public variables
;;;

;;  kimport:mode-alist and kimport:suffix-alist are defined in
;;  "../hyperbole.el".

;;;
;;; Public functions
;;;

;;;###autoload
(defun kimport:file (import-from output-to &optional children-p)
  "Import a buffer or file IMPORT-FROM into the koutline in buffer or file OUTPUT-TO.

Any suffix in IMPORT-FROM's buffer name is used to determine the type of
importation.  All others are imported as text, one paragraph per cell.

See the documentation for the variable, `kimport:suffix-alist' for
information on specific importation formats."
  (interactive "FImport from buffer/file: \nFInsert into koutline buffer/file: \nP")
  (let ((import-buf-name
	 (cond ((or (bufferp import-from)
		    (get-buffer import-from))
		(buffer-name (get-buffer import-from)))
	       ((get-file-buffer import-from)
		(buffer-name (get-file-buffer import-from)))
	       ((stringp import-from)
		(file-name-nondirectory import-from))
	       (t (error "(kimport:buffer): `%s' is an invalid `import-from' argument"))))
	(function))

    (set-buffer import-buf-name)
    (if (setq function (cdr (assq major-mode kimport:mode-alist)))
	nil
      (let ((import-suffix (if (string-match "\\..+\\'" import-buf-name)
			       (match-string 0 import-buf-name)))
	    (suffix-alist kimport:suffix-alist)
	    suffix-regexp)
	(while (and import-suffix suffix-alist)
	  (setq suffix-regexp (car (car suffix-alist))
		function (cdr (car suffix-alist))
		suffix-alist (cdr suffix-alist))
	  (if (string-match suffix-regexp import-suffix)
	      nil
	    (setq function nil)))
	(if function nil (setq function (cdr (assq t kimport:mode-alist))))))
    (funcall function import-from output-to children-p)))

;;; Augment right-side numbered files, blank line between cells
;;;

;;;###autoload
(defun kimport:aug-post-outline (import-from output-to &optional children-p)
  "Insert Augment outline statements from IMPORT-FROM into koutline OUTPUT-TO.
Displays and leaves point in OUTPUT-TO.  See documentation for
`kimport:initialize' for valid values of IMPORT-FROM and OUTPUT-TO and for
an explanation of where imported cells are placed.

If OUTPUT-TO is a new koutline, the first statement inserted will be the
first cell.  Otherwise, it will be the successor of the current cell.

Each statement to be imported is delimited by an Augment relative id at the
end of the statement.  \"1\" = level 1, \"1a\" = level 2 in outline and so
on."
  (interactive "FImport from Augment post-numbered buffer/file: \nFBuffer/file to insert cells into: \nP")
  (let ((output-level 1) (klabel "1")
	initially-empty-output no-renumber orig-point count total)
    ;; Don't change the order of import-from and output-to inits here.
    (setq import-from (kimport:copy-and-set-buffer import-from)
	  output-to (kimport:initialize output-to)
	  orig-point (point)
	  initially-empty-output (zerop (- (point-max) (point-min)))
	  no-renumber (or initially-empty-output
			  (not (if children-p
				   (kcell-view:child-p)
				 (kcell-view:sibling-p)))))

    (if (eq import-from output-to)
	(error "(kimport:aug-post-outline): Import and output buffers may not be the same."))

    (set-buffer import-from)
    (show-all)
    (save-excursion
      (goto-char (point-min))
      ;; Total number of Augement statements.
      (setq total (read (count-matches
			 " +\\([0-9][0-9a-z]*\\)\n\\(\n\\|\\'\\)")))
      (if initially-empty-output
	  nil
	;; Insert first cell as sibling of current cell.
	(set-buffer output-to)
	(if children-p
	    ;; Insert as children.
	    (progn (setq klabel (klabel:child (kcell-view:label))
			 output-level (klabel:level klabel))
		   ;; Move to end of this cell since cell insertion will
		   ;; occur at point.
		   (goto-char (kcell-view:end)))
	;; Insert as successors.
	(setq klabel (klabel:increment (kcell-view:label))
	      output-level (klabel:level klabel))
	;; Move to start of line of next tree since cell insertion will occur
	;; at point.
	(goto-char (kotl-mode:tree-end))))
      (setq count (kimport:aug-post-statements
		   import-from output-to klabel output-level 1 0 total)))
    (pop-to-buffer output-to)
    (kfile:narrow-to-kcells)
    (if no-renumber nil (klabel-type:update-labels klabel))
    (goto-char orig-point)
    (if (kotl-mode:buffer-empty-p)
	nil
      (kotl-mode:to-valid-position))
    (message "Imported %d of %d Augment statements." count total)))

;;;
;;; Emacs outliner style files, leading '*' cell delimiters
;;;

;;;###autoload
(defun kimport:star-outline (import-from output-to &optional children-p)
  "Insert star outline nodes from IMPORT-FROM into koutline OUTPUT-TO.
Displays and leaves point in OUTPUT-TO.  See documentation for
`kimport:initialize' for valid values of IMPORT-FROM and OUTPUT-TO and for
an explanation of where imported cells are placed.

\"* \" = level 1, \"** \" = level 2 in outline and so on."
  (interactive "FImport from star delimited cells buffer/file: \nFBuffer/file to insert cells into: \nP")
  (let ((output-level 1) (klabel "1")
	initially-empty-output no-renumber orig-point count total) 
    ;; Don't change the order of import-from and output-to inits here.
    (setq import-from (kimport:copy-and-set-buffer import-from)
	  output-to (kimport:initialize output-to)
	  orig-point (point)
	  initially-empty-output (zerop (- (point-max) (point-min)))
	  no-renumber (or initially-empty-output
			  (not (if children-p
				   (kcell-view:child-p)
				 (kcell-view:sibling-p)))))

    (if (eq import-from output-to)
	(error "(kimport:star-outline): Import and output buffers may not be the same."))

    (set-buffer import-from)
    (show-all)
    (save-excursion
      (goto-char (point-min))
      ;; If initial text in buffer is not an star outline node, add a star to
      ;; make it one, so it is not deleted from the import.
      (if (not (looking-at "[ \t]*\\*"))
	  (insert "* "))
      (goto-char (point-min))
      ;; Total number of top-level cells.
      (setq total (read (count-matches "^[ \t]*\\*[ \t\n]")))
      (if initially-empty-output
	  nil
	;; Insert first cell as sibling of current cell.
	(set-buffer output-to)
	(if children-p
	    ;; Insert as children.
	    (progn (setq klabel (klabel:child (kcell-view:label))
			 output-level (klabel:level klabel))
		   ;; Move to end of this cell since cell insertion will
		   ;; occur at point.
		   (goto-char (kcell-view:end)))
	;; Insert as successors.
	(setq klabel (klabel:increment (kcell-view:label))
	      output-level (klabel:level klabel))
	;; Move to start of line of next tree since cell insertion will occur
	;; at point.
	(goto-char (kotl-mode:tree-end))))
      (setq count (kimport:star-entries
		   import-from output-to klabel output-level 1 0 total)))
    (pop-to-buffer output-to)
    (kfile:narrow-to-kcells)
    (if no-renumber nil (klabel-type:update-labels klabel))
    (goto-char orig-point)
    (if (kotl-mode:buffer-empty-p)
	nil
      (kotl-mode:to-valid-position))
    (message "Imported %d of %d star outline trees." count total)))

;;;
;;; Generic text file import or koutline insertion.
;;;

;;;###autoload
(defun kimport:text (import-from output-to &optional children-p)
  "Insert text paragraphs from IMPORT-FROM into koutline OUTPUT-TO.
Displays and leaves point in OUTPUT-TO.  See documentation for
`kimport:initialize' for valid values of IMPORT-FROM and OUTPUT-TO and for
an explanation of where imported cells are placed.

Text paragraphs are imported as a sequence of same level cells.  Koutlines
are imported with their structure intact.

The variable, 'paragraph-start,' is used to determine paragraphs."
  (interactive "FImport from text/koutline buffer/file: \nFInsert cells into koutline buffer/file: \nP")
  (let ((klabel "1") (output-level 1) (count 0) initially-empty-output
	no-renumber orig-point total)
    ;; Don't change the order of import-from and output-to inits here.
    (setq import-from (kimport:copy-and-set-buffer import-from)
	  output-to (kimport:initialize output-to)
	  orig-point (point)
	  initially-empty-output (zerop (- (point-max) (point-min)))
	  no-renumber (or initially-empty-output
			  (not (if children-p
				   (kcell-view:child-p)
				 (kcell-view:sibling-p)))))

    (if (eq import-from output-to)
	(error "(kimport:text): Import and output buffers may not be the same."))

    (set-buffer import-from)
    (let ((kotl-import (eq major-mode 'kotl-mode))
	  visible-cells)
      (save-excursion
	(if initially-empty-output
	    nil
	  ;; Insert first cell as sibling of current cell.
	  (set-buffer output-to)
	  (if children-p
	      ;; Insert as children.
	      (progn (setq klabel (klabel:child (kcell-view:label))
			   output-level (klabel:level klabel))
		     ;; Move to end of this cell since cell insertion will
		     ;; occur at point.
		     (goto-char (kcell-view:end)))
	    ;; Insert as successors.
	    (setq klabel (klabel:increment (kcell-view:label))
		  output-level (klabel:level klabel))
	    ;; Move to start of line of next tree since cell insertion will occur
	    ;; at point.
	    (goto-char (kotl-mode:tree-end)))
	  (set-buffer import-from))

	(if kotl-import
	    ;; Importing from a koutline, so handle specially.
	    (progn (kotl-mode:beginning-of-buffer)
		   ;; Total number of cells.
		   (setq total (read (count-matches "[\n\r][\n\r]"))
			 visible-cells (read (count-matches "\n\n"))
			 count (save-excursion
				 ;; Incredible non-local exit to ensure that
				 ;; recursion ends at the right time.
				 (catch 'end
				   (kimport:kcells import-from output-to klabel
						   output-level 1
						   count total)))))

	  (show-all)
	  (goto-char (point-min))
	  ;; Total number of paragraphs.
	  (setq total (read (count-matches paragraph-start))
		count (kimport:text-paragraphs import-from output-to klabel
					       output-level count total))))
      (pop-to-buffer output-to)
      (kfile:narrow-to-kcells)
      (if no-renumber nil (klabel-type:update-labels klabel))
      (goto-char orig-point)
      (if (kotl-mode:buffer-empty-p)
	  nil
	(kotl-mode:to-valid-position))
      (if kotl-import
	  (message "Imported %d of %d visible cells from a %d cell outline."
		   count visible-cells total)
	(message "Imported %d of %d paragraphs." count total)))))

;;;
;;; Private functions - Don't call these functions from outside of this
;;; module or you may misuse them and cause data corruption.
;;;

(defun kimport:aug-label-lessp (label1 label2)
  "Return non-nil iff Augment-style LABEL1 is less than LABEL2."
  (let ((lev1 (klabel:level-alpha label1))
	(lev2 (klabel:level-alpha label2)))
    (cond ((< lev1 lev2))
	  ((= lev1 lev2) (string-lessp label1 label2))
	  (t nil))))

(defun kimport:aug-post-statements (import-from output-to klabel output-level
 			            import-level count total)
  "Insert post-numbered Augment statements (contents only) from IMPORT-FROM into existing OUTPUT-TO. 

KLABEL is the label to use for the first imported statement.
OUTPUT-LEVEL is the level at which to insert the first statement.
IMPORT-LEVEL is the depth of the current statement in the import file,
\(initially 1).

COUNT of inserted cells starts at 0.  TOTAL is the total number of statements
in IMPORT-FROM, used to show a running tally of the imported statements."
  (set-buffer import-from)
  (let ((cell-end-regexp " +\\([0-9][0-9a-z]*\\)\n\\(\n+\\|\\'\\)")
	contents start subtree-p end end-contents statement-level
	child-label)
    ;; While find cells at import-level or deeper ...
    (while (and (setq start (point))
		(re-search-forward cell-end-regexp nil t)
		(<= import-level
		   (setq statement-level
			 (klabel:level-alpha
			  (buffer-substring
			   (match-beginning 1) (match-end 1))))))
      (setq end-contents (match-beginning 0)
	    end (match-end 0))
      (goto-char start)
      (skip-chars-forward " ")
      (setq contents (kimport:unindent-region (point) end-contents))
      (goto-char end)
      (setq subtree-p (save-excursion
			(if (re-search-forward cell-end-regexp nil t)
			    (< statement-level
			       (klabel:level-alpha
				(buffer-substring
				 (match-beginning 1) (match-end 1)))))))
      (save-excursion
	(set-buffer output-to)
	;; Add the cell starting at point.
	(kview:add-cell klabel output-level contents nil t)
	(if subtree-p (setq child-label (klabel:child klabel)))
	(message "%d of %d statements converted..."
		 (setq count (1+ count)) total)
	(setq klabel (klabel:increment klabel)))
      ;;
      ;; Current buffer returns to `import-from' here.
      ;; Handle each sub-level through recursion.
      (if subtree-p
	  ;; Subtree exists so insert its cells.
	  (setq count
		(kimport:aug-post-statements
		 import-from output-to child-label (1+ output-level)
		 (1+ import-level) count total))))
    (goto-char start))
  count)

(defun kimport:copy-and-set-buffer (source)
  "Copy and untabify SOURCE, set copy buffer as current buffer for this command and return the copy buffer.
SOURCE may be a buffer name, a buffer or a file name.
If SOURCE buffer name begins with a space, it is not copied under the
assumption that it already has been.  If SOURCE is a koutline, it is not
copied since there is no need to copy it to import it."
  ;; This buffer name format is used so that we can easily
  ;; extract any file name suffix from the buffer name.
  (setq source (set-buffer (or (get-buffer source)
			       (find-file-noselect source))))
  (let ((mode (or (if (boundp 'kotl-previous-mode) kotl-previous-mode)
		  major-mode))
	copy)
    (if (or (eq mode 'kotl-mode)
	    (= ?\ (aref (buffer-name source) 0)))
	source
      (setq copy (get-buffer-create
		  (concat " " (if (string-match ".+[|<]" (buffer-name))
				  (substring (buffer-name)
					     0 (1- (match-end 0)))
				(buffer-name)))))
      (set-buffer copy)
      (setq buffer-read-only nil
	    major-mode mode)
      (erase-buffer)
      (insert-buffer source)
      (untabify (point-min) (point-max))
      ;; Ensure buffer ends with a newline so that we don't miss the last
      ;; element during the import.
      (goto-char (point-max))
      (if (/= (preceding-char) ?\n) (insert "\n"))
      (set-buffer-modified-p nil)
      copy)))

(defun kimport:initialize (output-to)
  "Setup to import elements into koutline OUTPUT-TO.
Return OUTPUT-TO buffer and set current buffer for the current command
to OUTPUT-TO.

OUTPUT-TO may be a buffer, buffer-name or file name.  If OUTPUT-TO exists
already, it must be a koutline or an error will be signaled.  For an existing
OUTPUT-TO, the text cells are inserted after the cell at point or after the
first cell for a newly loaded koutline.  If OUTPUT-TO is nil, the current
buffer is used.

If OUTPUT-TO is an existing koutline, the first cell imported will be added
as the successor of the current cell.  If an existing file is read in as
OUTPUT-TO within this function, point is left at the end of this buffer so
that imported cells will be appended to the buffer.  For a new file, this
means the first cell imported will become the first outline cell.

If a non-nil third argument, CHILDREN-P, is given to the caller of this
function and OUTPUT-TO contains at least one cell, then the imported cells
will be added as children of the cell where this function leaves point
\(either the current cell or for a newly read in outline, the last cell)."
  (let* ((output-existing-buffer-p
	  (if output-to
	     (or (get-buffer output-to) (get-file-buffer output-to))))
	 (output-exists-p
	  (if output-to
	     (or output-existing-buffer-p (file-exists-p output-to))
	   ;; current buffer will be used for output and it exists.
	   t)))
    (setq output-to (if output-to
			(or (get-buffer output-to)
			    (find-file-noselect output-to))
		      (current-buffer)))
    (set-buffer output-to)
    (if output-exists-p
	(if (eq major-mode 'kotl-mode)
	    (if (kotl-mode:buffer-empty-p)
		nil
	      ;; Make imported cells be appended if the output buffer was
	      ;; just read in.
	      (if output-existing-buffer-p nil (goto-char (point-max)))
	      (kotl-mode:to-valid-position))
	  (error
	   "(kimport:initialize): Second arg, %s, must be a koutline file."
	   (buffer-name output-to)))
      (if (eq major-mode 'kotl-mode)
	  nil
	(setq kview nil)
	(kotl-mode))
      (delete-region (point-min) (point-max))))
  output-to)

(defun kimport:kcells (import-from output-to klabel output-level
		       import-level count total)
  "Insert visible koutline cells (contents and attributes) from IMPORT-FROM into existing OUTPUT-TO. 

KLABEL is the label to use for the first imported cell.
OUTPUT-LEVEL is the level at which to insert the first cell.
IMPORT-LEVEL is the depth of the current cell in the import file,
\(initially 1).

COUNT of inserted cells starts at 0.  TOTAL is the total number of cells
in IMPORT-FROM, used to show a running tally of the imported cells."
  (set-buffer import-from)
  (goto-char (kcell-view:start))
  (let ((again t) contents subtree-p child-label)
    ;; While find cells at import-level or deeper ...
    (while (<= import-level (kcell-view:level))
      (setq subtree-p (kcell-view:child-p nil t)
	    contents (kcell-view:contents))
      (goto-char (kcell-view:end-contents))
      (save-excursion
	(set-buffer output-to)
	;; Add the cell starting at point.
	(kview:add-cell klabel output-level contents nil t)
	(if subtree-p (setq child-label (klabel:child klabel)))
	(message "%d of %d cells inserted..."
		 (setq count (1+ count)) total)
	(setq klabel (klabel:increment klabel)))
      ;;
      ;; Current buffer returns to `import-from' here.
      ;; Handle each sub-level through recursion.
      (if (and (setq again (kcell-view:next t)) subtree-p)
	  ;; Subtree exists so insert its cells.
	  (setq count
		(kimport:kcells
		 import-from output-to child-label (1+ output-level)
		 (1+ import-level) count total)))
      (if again nil (throw 'end count))))
  count)

(defun kimport:star-entries (import-from output-to klabel output-level
                             import-level count total)
  "Insert visible star outline entries from IMPORT-FROM into existing OUTPUT-TO. 

KLABEL is the label to use for the first imported entry.
OUTPUT-LEVEL is the level at which to insert the first entry.
IMPORT-LEVEL is the depth of the current entry in the import file,
\(initially 1).

COUNT of inserted entries starts at 0.  TOTAL is the total number of entries
in IMPORT-FROM, used to show a running tally of the imported entries."
  (set-buffer import-from)
  (let ((start (point))
	(rolo-entry-regexp "^[ \t]*\\(\\*+\\)")
	subtree-p end contents node-level child-label)
    ;; While find cells at import-level or deeper ...
    (while (and (re-search-forward rolo-entry-regexp nil t)
		(<= import-level
		    (setq node-level
			  (length
			   (buffer-substring
			    (match-beginning 1) (match-end 1))))))
      (skip-chars-forward " \t")
      (setq start (point)
	    end (rolo-to-entry-end)
	    subtree-p (if (looking-at rolo-entry-regexp)
			  (< node-level
			     (length (buffer-substring
				      (match-beginning 1) (match-end 1))))))
      (skip-chars-backward "\n\r")
      (setq contents (kimport:unindent-region start (point)))
      (save-excursion
	(set-buffer output-to)
	;; Add the cell starting at point.
	(kview:add-cell klabel output-level contents nil t)
	(if subtree-p (setq child-label (klabel:child klabel)))
	(message "%d of %d trees converted..."
		 (if (= node-level 1) (setq count (1+ count)) count)
		 total)
	(setq klabel (klabel:increment klabel)))
      ;;
      ;; Current buffer returns to `import-from' here.
      (goto-char end)
      ;;
      ;; Handle each sub-level through recursion.
      (if subtree-p
	  ;; Subtree exists so insert its cells.
	  (setq count
		(kimport:star-entries import-from output-to child-label
				      (1+ output-level) (1+ import-level)
				      count total))))
    (goto-char start))
  count)

(defun kimport:text-paragraphs (import-from output-to klabel
			        output-level count total)
  "Insert text paragraphs from IMPORT-FROM into existing OUTPUT-TO.
First cell is inserted with KLABEL at OUTPUT-LEVEL, as the sibling of the
previous cell, with the COUNT of inserted paragraphs starting at 0.  TOTAL is
the total number of paragraphs in IMPORT-FROM, used to show a running tally
of the imported paragraphs.

The variable, 'paragraph-start' is used to determine paragraphs."
  (set-buffer import-from)
  (let* ((count 0) start end contents)
    ;; Next line is needed when importing into an existing kview.
    (goto-char (point-min))
    ;; Move past blank lines at point.
    (skip-chars-forward " \t\n\r")
    (beginning-of-line)
    (while (and (setq start (point)
		      end (re-search-forward paragraph-start nil t))
		(/= start end))
      (setq contents (kimport:unindent-region start end))
      (set-buffer output-to)
      ;; Add the cell starting at point.
      (kview:add-cell klabel output-level contents nil t)
      (setq count (1+ count))
      (message "%d of %d paragraphs converted..."
	       count total)
      (setq klabel (klabel:increment klabel))
      (set-buffer import-from)
      (goto-char end)
      ;; Move past blank lines separating paragraphs.
      (skip-chars-forward " \t\n\r")
      (beginning-of-line))
    (message "%d of %d paragraphs converted" count total)
    count))

(defun kimport:unindent-region (start end)
  "Calculate indent based upon the second line within the region START to END.
Remove the indent and return the remaining region as a string."
  (save-excursion
    (let (indent-regexp)
      (goto-char start)
      ;; Remove leading indent from lines in paragraph.  Base paragraph
      ;; indent on the 2nd paragraph line since the first line might be
      ;; further indented or outdented.
      (setq indent-regexp
	    (if (re-search-forward "[\n\r][ \t]+" end t)
		(concat "^" (make-string (current-column) ?\ ))))
      (if indent-regexp
	  (hypb:replace-match-string
			  indent-regexp (buffer-substring start end) "" t)
	(buffer-substring start end)))))

(provide 'kimport)


;;; kimport.el ends here
