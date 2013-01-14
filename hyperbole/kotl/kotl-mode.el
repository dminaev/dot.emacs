;;; kotl-mode.el --- Major mode for editing koutlines and associated commands.

;; Copyright (C) 1993, 1994, 1995, 2007  Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;;         Kellie Clark
;; Maintainer: Mats Lidell <matsl@contactor.se>
;; Keywords: data, hypermedia, outlines, wp

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
;;; Other required Lisp Libraries
;;;

(mapcar 'require '(hsite hmail kview kimport kvspec kotl kmenu))

;;;
;;; Public variables
;;;

(defvar kotl-mode:refill-flag nil
  "*Automatically refill cells during move, copy, promotion and demotion operations when non-nil.
Default value is nil.  Cells with a `no-fill' attribute are never refilled
during such operations, regardless of the value of this flag.")

;;;
;;; Public functions
;;;

;;;###autoload
(defun kotl-mode ()
  "The major mode used to edit and view koutlines.
It provides the following keys:
\\{kotl-mode-map}"
  (interactive)
  (use-local-map kotl-mode-map)
  (set-syntax-table text-mode-syntax-table)
  ;; Turn off filladapt minor mode if on, so that it does not interfere with
  ;; the filling code in "kfill.el".
  (and (boundp 'filladapt-mode) filladapt-mode (filladapt-mode -1))
  (if (/= 3 (length (action:params (symbol-function 'fill-paragraph))))
      ;; Some package such as filladapt has overwritten the primitives
      ;; defined in kfill.el, so reload it.
      (load "kfill"))
  ;; Ensure that outline structure data is saved when save-buffer is called
  ;; from save-some-buffers, {C-x s}.
  (add-hook 'local-write-file-hooks 'kotl-mode:update-buffer)
  (mapcar 'make-local-variable
	  '(kotl-previous-mode indent-line-function indent-region-function
            minor-mode-alist selective-display-ellipses))
  ;; Used by kimport.el functions.
  (if (and (boundp 'kotl-previous-mode) kotl-previous-mode)
      nil
    (setq kotl-previous-mode major-mode
	  ;; Remove outline indication due to selective-display.
	  minor-mode-alist (copy-sequence minor-mode-alist)
	  minor-mode-alist (set:remove '(selective-display " Outline")
				       minor-mode-alist)
	  minor-mode-alist (set:remove '(selective-display " Otl")
				       minor-mode-alist)
	;; Remove indication that buffer is ;; narrowed.
	mode-line-format (copy-sequence mode-line-format)
	mode-line-format (set:remove "%n" mode-line-format)))
  ;;
  (setq indent-line-function 'kotl-mode:indent-line
	indent-region-function 'kotl-mode:indent-region
	local-abbrev-table text-mode-abbrev-table
	selective-display t
	selective-display-ellipses t
	paragraph-start "^[ \t]*$\\|^\^L"
	paragraph-separate "^[ \t]*$\\|^\^L")
  ;;
  ;; This major-mode setting must come after the local variable settings but
  ;; before the koutline is formatted.
  (setq major-mode 'kotl-mode
	mode-name "Kotl"
	indent-tabs-mode nil)
  ;; If buffer has not yet been formatted for editing, format it.
  (cond
   ;; Koutline file that has been loaded and formatted for editing.
   ((kview:is-p kview)
    ;; The buffer might have been widened for inspection, so narrow to cells
    ;; only.
    (kfile:narrow-to-kcells))
   ;; Koutline file that has been loaded but not yet formatted for editing.
   ((kfile:is-p)
    (kfile:read
     (current-buffer)
     (and buffer-file-name (file-exists-p buffer-file-name)))
    (kvspec:activate))
   ;; New koutline buffer or a foreign text buffer that must be converted to
   ;; koutline format.
   (t
    (kfile:create (current-buffer))
    (kvspec:activate)))
  ;; We have been converting a buffer from a foreign format to a koutline.
  ;; Now that it is converted, ensure that kotl-previous-mode is set to
  ;; koutline now.
  (setq kotl-previous-mode 'kotl-mode)
  (easy-menu-add kotl-mode-menu kotl-mode-map)
  (run-hooks 'kotl-mode-hook))

(defun kotl-mode:find-file-hook ()
  (if (kview:is-p kview)
      (kotl-mode:to-valid-position))
  nil)

;;; Ensure that point ends up at a valid position whenever a find-file
;;; is done on a kotl-file.
(add-hook 'find-file-hooks 'kotl-mode:find-file-hook)

;;; Ensure that outline structure data is hidden from view after a file save.
(add-hook 'after-save-hook 'kfile:narrow-to-kcells)

;;;
;;; Editing within a single kotl
;;;

(fset 'kotl-mode:backward-delete-char-untabify
      'kotl-mode:delete-backward-char)
(fset 'kotl-mode:backward-delete-char
      'kotl-mode:delete-backward-char)

(defun kotl-mode:backward-kill-word (arg)
  "Kill up to prefix ARG words preceding point within a single cell."
  (interactive "*p")
  (or arg (setq arg 1))
  (cond ((< arg 0)
	 (if (kotl-mode:eocp)
	     (error "(kotl-mode:backward-kill-word): End of cell")))
	((> arg 0)
	 (if (kotl-mode:bocp)
	     (error "(kotl-mode:backward-kill-word): Beginning of cell"))))
  (if (= arg 0)
      nil
    (save-restriction
      (narrow-to-region (kcell-view:start) (kcell-view:end-contents))
      (backward-kill-word arg))))

(defun kotl-mode:center-line ()
  "Center the line point is on, within the width specified by `fill-column'.
This means adjusting the indentation so that it equals the distance between
the end of the text and `fill-column'."
  (interactive "*")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (let ((indent (kcell-view:indent))
	(opoint (point-marker))
	(bocp)
	start)
    (setq start (kotl-mode:beginning-of-line))
    (if (setq bocp (kotl-mode:bocp))
	(progn
	  ;; Add a temporary fill-prefix since this is the 1st line of the cell
	  ;; where label could interfere with centering.
	  (insert "\n\n") (insert-char ?\  indent)))
    (center-line)
    (if bocp
	;; Delete temporary fill prefix.
	(delete-region start (+ start indent 2)))
    (goto-char opoint)
    ;; Move to editable point if need be.
    (kotl-mode:to-valid-position)))

(defun kotl-mode:center-paragraph ()
  "Center each nonblank line in the paragraph at or after point.
See `center-line' for more info."
  (interactive "*")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (let ((indent (kcell-view:indent))
	(opoint (point-marker))
	start)
    (backward-paragraph)
    (kotl-mode:to-valid-position)
    (setq start (point))
    ;; Add a temporary fill-prefix for 1st line in cell which contains a
    ;; label, so is centered properly.
    (insert "\n\n") (insert-char ?\  indent)
    (kcell-view:operate 'center-paragraph)
    ;; Delete temporary fill prefix.
    (delete-region start (+ start indent 2))
    ;; Return to original point.
    (goto-char (min opoint (kcell-view:end-contents)))
    ;; Move to editable point if need be.
    (kotl-mode:to-valid-position)))

(defun kotl-mode:copy-region-as-kill (start end)
  "Copy region between START and END within a single kcell to kill ring."
  (interactive "r")
  (kotl-mode:kill-region start end t))

(defun kotl-mode:copy-to-register (register start end &optional delete-flag)
  "Copy into REGISTER the region START to END.
With optional prefix arg DELETE-FLAG, delete region."
  (interactive "cCopy to register: \nr\nP")
  (let ((indent (kcell-view:indent)))
    (set-register register
		  (hypb:replace-match-string
		   (concat "^" (make-string indent ?\ ))
		   (buffer-substring start end)
		   "" t)))
  (if delete-flag (delete-region start end)))

(defun kotl-mode:delete-backward-char (arg &optional kill-flag)
  "Delete up to the preceding prefix ARG characters.
Return number of characters deleted.
Optional KILL-FLAG non-nil means save in kill ring instead of deleting.
Does not delete across cell boundaries."
  (interactive "*P")
  (if (interactive-p)
      (if current-prefix-arg
	  (setq kill-flag t
		arg (prefix-numeric-value current-prefix-arg))))
  (or arg (setq arg 1))
  (kotl-mode:delete-char (- arg) kill-flag))

(defun kotl-mode:delete-blank-lines ()
  "On blank line within a cell, delete all surrounding blank lines, leaving just one.
On isolated blank line, delete that one.
On nonblank line, delete all blank lines that follow it.

If nothing but whitespace follows point until the end of a cell, delete all
whitespace at the end of the cell."
  (interactive "*")
  ;; If nothing but whitespace from point until the end of cell, remove all
  ;; cell trailing whitespace.
  (let ((end (kcell-view:end-contents))
	start)
    (if (save-excursion
	  (skip-chars-forward " \t\n\r" end)
	  (not (kotl-mode:eocp)))
	(kcell-view:operate (function (lambda () (delete-blank-lines))))
      (setq start (kcell-view:start))
      (goto-char end)
      ;; delete any preceding whitespace
      (skip-chars-backward " \t\n\r" start)
      (delete-region (max start (point)) end)))
  (kotl-mode:to-valid-position))

(defun kotl-mode:delete-char (arg &optional kill-flag)
  "Delete up to prefix ARG characters following point.
Return number of characters deleted.
Optional KILL-FLAG non-nil means save in kill ring instead of deleting.
Does not delete across cell boundaries."
  (interactive "*P")
  (if (interactive-p)
      (if current-prefix-arg
	  (setq kill-flag t
		arg (prefix-numeric-value current-prefix-arg))))
  (or arg (setq arg 1))
  (let ((del-count 0)
	(indent (kcell-view:indent))
	count start end)
    (cond ((> arg 0)
	   (if (kotl-mode:eocp)
	       (error "(kotl-mode:delete-char): End of cell")
	     (setq end (kcell-view:end)
		   arg (min arg (- end (point))))
	     (while (> arg 0)
	       (if (kotl-mode:eolp)
		   (if (/= ?\ (char-syntax (following-char)))
		       (setq arg 0
			     del-count (1- del-count))
		     (delete-char 1 kill-flag)
		     ;; There may be non-whitespace characters in the
		     ;; indent area.  Don't delete them.
		     (setq count indent)
		     (while (and (> count 0)
				 (= ?\ (char-syntax (following-char))))
		       (delete-char 1)
		       (setq count (1- count))))
		 (delete-char 1 kill-flag))
	       (setq arg (1- arg)
		     del-count (1+ del-count)))
	     ))
	  ((< arg 0)
	   (if (kotl-mode:bocp)
	       (error "(kotl-mode:delete-char): Beginning of cell")
	     (setq start (kcell-view:start)
		   arg (max arg (- start (point))))
	     (while (< arg 0)
	       (if (kotl-mode:bolp)
		   (if (/= ?\ (char-syntax (preceding-char)))
		       (setq arg 0
			     del-count (1- del-count))
		     ;; There may be non-whitespace characters in the
		     ;; indent area.  Don't delete them.
		     (setq count indent)
		     (while (and (> count 0)
				 (= ?\ (char-syntax (preceding-char))))
		       (delete-char -1)
		       (setq count (1- count)))
		     (if (zerop count)
			 (delete-char -1 kill-flag)))
		 (delete-char -1 kill-flag))
	       (setq arg (1+ arg)
		     del-count (1+ del-count))))))
    del-count))

(defun kotl-mode:delete-horizontal-space ()
  "Delete all spaces and tabs around point."
  (interactive "*")
  (save-restriction
    (narrow-to-region
     (save-excursion
       (kotl-mode:start-of-line))
     (save-excursion
       (kotl-mode:finish-of-line)))
    (delete-horizontal-space)))

(defun kotl-mode:delete-indentation (&optional arg)
  "Join this line to previous and fix up whitespace at join.
If there is a fill prefix, delete it from the beginning of this line.
With argument, join this line to following line."
  (interactive "*P")
  (kcell-view:operate
   (function
    (lambda ()
      (let ((opoint (point)))
	(beginning-of-line)
	(if arg (forward-line 1))
	(if (eq (preceding-char) ?\n)
	    (progn
	      (delete-region (point) (1- (point)))
	      ;; If the second line started with the fill prefix,
	      ;; delete the prefix.
	      (if (and fill-prefix
		       (<= (+ (point) (length fill-prefix)) (point-max))
		       (string= fill-prefix
				(buffer-substring
				 (point) (+ (point) (length fill-prefix)))))
		  (delete-region (point) (+ (point) (length fill-prefix))))
	      (fixup-whitespace))
	  (goto-char opoint)))))))

(defun kotl-mode:fill-cell (&optional justify ignore-collapsed-p)
  "Fill current cell within current view if it does not have a non-nil `no-fill' attribute.
With optional JUSTIFY, justify cell as well.
IGNORE-COLLAPSED-P is used when caller has already expanded cell, indicating
it is not collapsed."
  (interactive "*P")
  (cond ((kcell-view:get-attr 'no-fill)
	 (if (interactive-p)
	     (progn (beep)
		    (message "Current cell has a `do not fill' attribute.")
		    nil)))
	((string-match "\\`[ \t\n\r]*\\'" (kcell-view:contents))
	  ;; Cell content is all whitespace.
	 nil)
	(t (let* ((indent (kcell-view:indent))
		  (opoint (set-marker (make-marker) (point)))
		  (start  (kcell-view:start))
		  (collapsed-p)
		  (end (kcell-view:end-contents))
		  temp-prefix prev-point)
	     (goto-char start)
	     ;; Expand cell if collapsed so that filling is done properly.
	     (if (and (not ignore-collapsed-p)
		      (setq collapsed-p (search-forward "\r" end t)))
		 (subst-char-in-region start end ?\r ?\n t))
	     (goto-char start)
	     ;; Add a temporary fill-prefix for first labeled line, so is
	     ;; filled properly.
	     (insert (setq temp-prefix
			   (concat "\n\n" (make-string indent ?\ ))))
	     (while (progn (fill-paragraph justify)
			   (setq prev-point (point))
			   (forward-paragraph)
			   (and (/= (point) prev-point)
				(< (point) (kcell-view:end-contents))
				(if (memq (preceding-char) '(?\n ?\r))
				    (not (looking-at "[\n\r]"))
				  t))))
	     ;; Delete temporary fill prefix.
	     (goto-char start)
	     (if (looking-at temp-prefix)
		 (replace-match "" t t))
	     ;; Return to original point.
	     (setq end (kcell-view:end-contents))
	     (goto-char (min opoint end))
	     ;;
	     ;; If cell was collapsed before filling, collapse it again.
	     (if collapsed-p
		 (subst-char-in-region start end ?\n ?\r t))
	     ;;
	     ;; Remove markers
	     (set-marker opoint nil))
	   ;; Move to editable point if need be.
	   (kotl-mode:to-valid-position))))

(defun kotl-mode:fill-paragraph (&optional justify)
  "Fill the current paragraph within the cell.
With optional JUSTIFY, justify the paragraph as well.
Ignore any non-nil no-fill attribute attached to the cell."
  (interactive "*P")
  (let ((indent (kcell-view:indent))
	(opoint (point-marker))
	start end)
    (backward-paragraph)
    (kotl-mode:to-valid-position)
    (setq start (point-marker))
    ;; Add a temporary fill-prefix for 1st line in cell which contains a
    ;; label, so is filled properly.
    (insert "\n\n") (insert-char ?\  indent)
    (setq end (point-marker))
    ;; Return to original paragraph point.  This is the correct formula,
    ;; considering the fill prefix that was just added.
    (goto-char (min (max opoint (point)) (kcell-view:end-contents)))
    (fill-paragraph justify)
    ;; Delete temporary fill prefix.
    (delete-region start end)
    ;; Return to original point.
    (goto-char (min opoint (kcell-view:end-contents)))
    ;; Move to editable point if need be.
    (kotl-mode:to-valid-position)
    ;; Remove markers
    (set-marker opoint nil)
    (set-marker start nil)
    (set-marker end nil)))

;; XEmacs binds this to {M-q}.
(fset 'kotl-mode:fill-paragraph-or-region 'kotl-mode:fill-paragraph)

(defun kotl-mode:fill-tree (&optional top-p)
  "Refill each cell within the tree whose root is at point.
Skip cells with a non-nil no-fill attribute.
With optional prefix argument TOP-P non-nil, refill all cells in the outline."
  (interactive "P")
  ;; Store list of which cells are presently collapsed.
  (let ((collapsed-cells
	 (kview:map-tree
	  (function (lambda (view)
		      ;; Use free variable label-sep-len bound in
		      ;; kview:map-tree for speed.
		      (kcell-view:collapsed-p nil label-sep-len)))
	  kview top-p)))
    ;;
    ;; Expand all cells in tree.
    (if top-p
	(subst-char-in-region (point-min) (point-max) ?\r ?\n t)
      (save-excursion
	(kotl-mode:end-of-tree)
	(subst-char-in-region
	 (point) (kcell-view:end-contents) ?\r ?\n t)))
    ;;
    ;; Refill cells without no-fill property.
    (kview:map-tree (function (lambda (view) (kotl-mode:fill-cell)))
		    kview top-p)
    ;;
    ;; Collapse temporarily expanded cells.
    (if (delq nil collapsed-cells)
	(kview:map-tree
	 (function
	  (lambda (view)
	    (if (car collapsed-cells)
		;; Use free variable label-sep-len bound in
		;; kview:map-tree for speed.
		(kcell-view:collapse nil label-sep-len))
	    (setq collapsed-cells (cdr collapsed-cells))))
	 kview top-p))))

(defun kotl-mode:insert-buffer (buffer)
  "Insert after point the contents of BUFFER.
Puts mark after the inserted text.
BUFFER may be a buffer or a buffer name."
  (interactive "*bInsert buffer: ")
  (insert-buffer buffer)
  (kotl-mode:add-indent-to-region))

(defun kotl-mode:insert-file (import-from children-p)
  "Insert each element in IMPORT-FROM as a separate cell in the current view.
Insert as sibling cells following the current cell unless prefix arg,
CHILDREN-P is non-nil, then insert as the initial children of the current
cell.

IMPORT-FROM may be a buffer name or file name (file name completion is
provided).

See documentation for `kimport:file' for information on how the type of
importation is determined."
  (interactive
   (list (read-file-name
	  (if current-prefix-arg
	      "Buffer or file to insert as children of current cell: "
	    "Buffer or file to insert as siblings of current cell: "))
	 current-prefix-arg))
  (kimport:file import-from (current-buffer) children-p))

(defun kotl-mode:insert-file-contents (filename)
  "Insert contents of file FILENAME into current cell after point.
Set mark after the inserted text."
  (interactive "*fInsert file: ")
  (let ((tem (insert-file-contents filename)))
    (push-mark (+ (point) (car (cdr tem)))))
  (kotl-mode:add-indent-to-region))

(defun kotl-mode:insert-register (register &optional arg)
  "Insert contents of register REGISTER at point in current cell.
REGISTER is a character naming the register to insert.
Normally puts point before and mark after the inserted text.
If optional second arg is non-nil, puts mark before and point after.
Interactively, second arg is non-nil if prefix arg is supplied."
  (interactive "*cInsert register: \nP")
  (push-mark)
  (let ((val (get-register register)))
    (cond ((consp val)
           (insert-rectangle val))
          ((stringp val)
	   (insert val)
	   (kotl-mode:add-indent-to-region))
          ((integerp val)
           (princ val (current-buffer)))
          ((and (markerp val) (marker-position val))
           (princ (marker-position val) (current-buffer)))
          (t
           (error "Register '%c' does not contain text" register))))
  (if (not arg) (exchange-point-and-mark)))

(defun kotl-mode:just-one-space ()
  "Delete all spaces and tabs around point and leave one space."
  (interactive "*")
  (save-restriction
    (narrow-to-region
     (save-excursion
       (kotl-mode:start-of-line))
     (save-excursion
       (kotl-mode:finish-of-line)))
    (just-one-space)))

(defun kotl-mode:kill-line (&optional arg)
  "Kill ARG lines from point."
  (interactive "*P")
  (if (and (null arg)
	   (kotl-mode:bolp)
	   (boundp 'kill-whole-line) kill-whole-line)
      (let ((indent (kcell-view:indent)))
	;; Kill whole line including newline, if any.
	(kcell-view:operate
	 (function
	  (lambda ()
	    (let ((no-newline))
	      (kill-region (point)
			   (progn (setq no-newline
					(not (search-forward "\n" nil 'stay)))
				  (point)))
	      (or no-newline (delete-char indent)))))))
    ;; Kill part of a line or multiple lines.
    (let ((num-arg (prefix-numeric-value arg)))
      (cond
       ((and (null arg) (not (kotl-mode:eolp)))
	;; kill to eol but not newline
	(kill-region (point) (setq arg (kotl-mode:finish-of-line))))
       ((= num-arg 0)
	;; kill to bol
	(kill-region (point) (setq arg (kotl-mode:start-of-line))))
       (t;; (/= num-arg 0)
	;; Find start and end of region to kill
	(let ((start (point))
	      (end (min (kcell-view:end-contents)
			(save-excursion (forward-line num-arg) (point)))))
	  (kotl-mode:kill-region start end))))))
  (setq last-command 'kill-region))

(defun kotl-mode:kill-region (start end &optional copy-p)
  "Kill region between START and END within a single kcell.
With optional COPY-P equal to 't, copy region to kill ring but does not
kill it.  With COPY-P any other non-nil value, return region as a
string without affecting kill ring.

If the buffer is read-only and COPY-P is nil, the region will not be deleted
but it will be copied to the kill ring and then an error will be signaled."
  (interactive "*r")
  (let ((read-only (and (not copy-p) buffer-read-only)))
    (if read-only (setq copy-p t))
    (if (and (number-or-marker-p start)
	     (number-or-marker-p end)
	     (eq (kcell-view:cell start)
		 (kcell-view:cell end)))
	(save-excursion
	  (goto-char start)
	  (let ((indent (kcell-view:indent))
		killed subst-str)
	    ;; Convert region to string
	    ;; Convert all occurrences of newline + indent
	    ;; to just newline, eliminating indent.
	    ;; Then save to kill ring.
	    (setq subst-str (concat "\\([\n\r]\\)" (make-string indent ?\ ))
		  killed
		  (hypb:replace-match-string
		   subst-str (buffer-substring start end) "\\1"))
	    (if copy-p
		nil
	      ;; If last char of region is a newline, then delete indent in
	      ;; following line.
	      (delete-region
	       start (+ end (if (memq (char-after (1- (max start end)))
				      '(?\n ?\r))
				indent
			      0))))
	    (if (and copy-p (not (eq copy-p t)))
		;; Return killed region as a string.
		killed
	      (if (eq last-command 'kill-region)
		  (kill-append killed (< end start))
		(kill-new killed))
	      (setq this-command 'kill-region)
	      (if read-only (barf-if-buffer-read-only))
	      )))
      (error
       "(kotl-mode:kill-region): Bad region or not within a single kcell."))))

(fset 'kotl-mode:kill-ring-save 'kotl-mode:copy-region-as-kill)

(defun kotl-mode:kill-sentence (&optional arg)
  "Kill up to prefix ARG (or 1) sentences following point within a single cell."
  (interactive "*p")
  (or arg (setq arg 1))
  (cond ((> arg 0)
	 (if (kotl-mode:eocp)
	     (error "(kotl-mode:kill-sentence): End of cell")))
	((< arg 0)
	 (if (kotl-mode:bocp)
	     (error "(kotl-mode:kill-sentence): Beginning of cell"))))
  (if (= arg 0)
      nil
    (kotl-mode:kill-region (point)
			   (save-excursion
			     (kotl-mode:forward-sentence arg)))))

(defun kotl-mode:kill-word (arg)
  "Kill up to prefix ARG words following point within a single cell."
  (interactive "*p")
  (or arg (setq arg 1))
  (cond ((> arg 0)
	 (if (kotl-mode:eocp)
	     (error "(kotl-mode:kill-word): End of cell")))
	((< arg 0)
	 (if (kotl-mode:bocp)
	     (error "(kotl-mode:kill-word): Beginning of cell"))))
  (if (= arg 0)
      nil
    (save-restriction
      (narrow-to-region (kcell-view:start) (kcell-view:end-contents))
      (kill-word arg))))

(defun kotl-mode:newline (arg)
  "Insert a newline.  With ARG, insert ARG newlines.
In Auto Fill mode, if no numeric arg, break the preceding line if it is
too long."
  (interactive "*p")
  (let ((indent (kcell-view:indent)))
    (if (equal arg 1)
	(progn
	  (save-excursion
	    (insert ?\n)
	    (insert-char ?\  indent))
	  (do-auto-fill)
	  (forward-line 1)
	  (kotl-mode:start-of-line)
	  )
      (while (> arg 0)
	(insert ?\n)
	(insert-char ?\  indent)
	(setq arg (1- arg))))))

(fset 'kotl-mode:newline-and-indent 'kotl-mode:newline)

(defun kotl-mode:open-line (arg)
  "Insert a newline and leave point before it.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((bolp (and (kotl-mode:bolp) (not (kotl-mode:bocp))))
	 (indent (kcell-view:indent)))
    (while (> arg 0)
      (save-excursion
        (insert ?\n)
	(if (and (not bolp) fill-prefix)
	    (insert fill-prefix)
	  (insert-char ?\  indent)))
      (setq arg (1- arg)))
    (if (and bolp fill-prefix)
	(progn (delete-horizontal-space)
	       (insert fill-prefix)))
    ))

(defun kotl-mode:set-fill-prefix (turn-off)
  "Sets fill prefix to line up to point.
With prefix arg TURN-OFF or at begin of line, turns fill prefix off."
  (interactive "P")
  (set-fill-prefix (or turn-off (kotl-mode:bolp))))

(defun kotl-mode:transpose-chars (arg)
  "Interchange characters around point, moving forward one character.
With prefix ARG, take character before point and drag it forward past ARG
other characters (backward if ARG negative).
If no prefix ARG and at end of line, the previous two characters are
exchanged."
  (interactive "*P")
  (and (null arg) (kotl-mode:eolp) (kotl-mode:forward-char -1))
  (transpose-subr 'kotl-mode:forward-char (prefix-numeric-value arg)))

(defun kotl-mode:transpose-lines (arg)
  "Exchange current line and previous line, leaving point after both.
If no previous line, exchange current with next line.
With prefix ARG, take previous line and move it past ARG lines.
With prefix ARG = 0, interchange the line that contains point with the line
that contains mark."
  (interactive "*p")
  (cond
   ((and (kotl-mode:first-line-p) (kotl-mode:last-line-p))
    (error "(kotl-mode:transpose-lines): Only one line in outline"))
   ;;
   ;; Transpose current and previous lines or current and next lines, if no
   ;; previous line.  Leave point after both exchanged lines.
   ((= arg 1)
    (let* ((point (point-marker))
	   (mark (set-marker (make-marker)
			     (if (kotl-mode:first-line-p)
				 (kotl-mode:next-line 1)
			       (kotl-mode:previous-line 1)))))
      (kotl-mode:transpose-lines-internal point mark)
      (goto-char (max point mark))
      (kotl-mode:next-line 1)
      (set-marker mark nil)))
   ;;
   ;; Transpose point and mark lines, leaving point on the line of text that
   ;; originally contained point.
   ((= arg 0)
    (kotl-mode:transpose-lines-internal (point-marker) (hypb:mark-marker t))
    ;; This is like exchange-point-and-mark, but doesn't activate the
    ;; mark.
    (goto-char (prog1 (hypb:mark t)
		 (set-marker (hypb:mark-marker t) (point)))))
   ;;
   ;; Move previous line past ARG next lines and leave point after previous
   ;; line text.
   (t
    (if (kotl-mode:first-line-p)
	(error "(kotl-mode:transpose-lines): No previous line to transpose"))
    (kotl-mode:previous-line 1)
    (let* ((mark (set-marker (make-marker)
			     (save-excursion (kotl-mode:next-line arg))))
	   (line-to-move (kotl-mode:delete-line)))
      (condition-case ()
	  ;; Delete trailing newline if any, ignoring error.
	  (kotl-mode:delete-char 1)
	(error nil))
      (goto-char mark)
      (set-marker mark nil)
      (kotl-mode:finish-of-line)
      (insert "\n")
      (insert-char ?\  (kcell-view:indent))
      (insert line-to-move)
      (kotl-mode:start-of-line)))))

(defun kotl-mode:transpose-paragraphs (arg)
  "Interchange this (or next) paragraph with previous one."
  (interactive "*p")
  (transpose-subr 'kotl-mode:forward-paragraph (prefix-numeric-value arg)))

(defun kotl-mode:transpose-sentences (arg)
  "Interchange this (next) and previous sentence."
  (interactive "*p")
  (transpose-subr 'kotl-mode:forward-sentence (prefix-numeric-value arg)))

(defun kotl-mode:transpose-words (arg)
  "Interchange words around point, leaving point after both words.
With prefix ARG, take word before or around point and drag it forward past
ARG other words (backward if ARG negative).  If ARG is zero, the words around
or after point and around or after mark are interchanged."
  (interactive "*p")
  (transpose-subr 'kotl-mode:forward-word (prefix-numeric-value arg)))

(defun kotl-mode:zap-to-char (arg char)
  "Kill up to and including prefix ARG'th occurrence of CHAR.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "*p\ncZap to char within current cell: ")
  (kcell-view:operate 
   (function (lambda () (zap-to-char arg char)))))

;;;
;;; Editing across kotls
;;;

(defun kotl-mode:append-cell (contents-cell append-to-cell)
  "Append CONTENTS-CELL to APPEND-TO-CELL.
APPEND-TO-CELL is refilled if neither cell has a no-fill property and
kotl-mode:refill-flag is enabled."
  (interactive
   (let* ((label (kcell-view:label))
	  (hargs:defaults (list label label)))
     (hargs:iform-read
      '(interactive
	"*+KAppend contents of cell: \n+KAppend contents of cell <%s> to cell: "))))
  (save-excursion
    (kotl-mode:goto-cell contents-cell)
    (let ((contents (kcell-view:contents))
	  (no-fill (kcell-view:get-attr 'no-fill)))
      (kotl-mode:goto-cell append-to-cell)
      (if no-fill nil (setq no-fill (kcell-view:get-attr 'no-fill)))
      (goto-char (kcell-view:end-contents))
      (let ((fill-prefix (make-string (kcell-view:indent) ?\ )))
	(if (kotl-mode:bolp)
	    nil
	  ;; Append contents of cell beginning on its own line.
	  (insert "\n" fill-prefix))
	(kview:insert-contents (kcell-view:cell) contents
			       (or no-fill (null kotl-mode:refill-flag))
			       fill-prefix)))))

(defun kotl-mode:copy-after (from-cell-ref to-cell-ref child-p)
  "Copy tree rooted at FROM-CELL-REF to follow tree rooted at TO-CELL-REF.
If prefix arg CHILD-P is non-nil, make FROM-CELL-REF the first child of
TO-CELL-REF, otherwise make it the sibling following TO-CELL-REF.

Leave point at the start of the root cell of the new tree."
  (interactive
   (let* ((label (kcell-view:label))
	  (hargs:defaults (list label label)))
     (append
      (hargs:iform-read
       (list
	'interactive
	(format "*+KCopy tree: \n+KCopy tree <%%s> to follow as %s of cell: "
		(if current-prefix-arg "child" "sibling"))))
      (list current-prefix-arg))))
  ;;
  ;; Copy tree in current view and leave point at the start of the copy.
  (goto-char (kotl-mode:move-after from-cell-ref to-cell-ref child-p t))
  ;; Alter the copied tree so each cell appears to be newly created.
  (kview:map-tree
   (function
    (lambda (view)
      (kcell-view:set-cell
       (kcell:create nil (kview:id-increment view)))))
   kview))

(defun kotl-mode:copy-before (from-cell-ref to-cell-ref parent-p)
  "Copy tree rooted at FROM-CELL-REF to precede tree rooted at TO-CELL-REF.
If prefix arg PARENT-P is non-nil, make FROM-CELL-REF the first child of
TO-CELL-REF's parent, otherwise make it the preceding sibling of TO-CELL-REF.

Leave point at the start of the root cell of the new tree."
  (interactive
   (let* ((label (kcell-view:label))
	  (hargs:defaults (list label label)))
     (append
      (hargs:iform-read
       (list 'interactive
	     (format "*+KCopy tree: \n+KCopy tree <%%s> to be %s of cell: "
		     (if current-prefix-arg "first child of parent"
		       "preceding sibling"))))
      (list current-prefix-arg))))
  ;;
  ;; Copy tree in current view and leave point at the start of the copy.
  (goto-char (kotl-mode:move-before from-cell-ref to-cell-ref parent-p t))
  ;; Alter the copied tree so each cell appears to be newly created.
  (kview:map-tree
   (function
    (lambda (view)
      (kcell-view:set-cell
       (kcell:create nil (kview:id-increment view)))))
   kview))

(defun kotl-mode:copy-to-buffer (cell-ref buffer invisible-flag)
  "Copy outline tree rooted at CELL-REF to a non-koutline BUFFER.
Invisible text is expanded and included in the copy only if INVISIBLE-FLAG is
non-nil.  The tree is inserted before point in BUFFER.  Use \"0\" to copy the
whole outline buffer."
  (interactive
   (let ((label-default (kcell-view:label)))
     (hargs:iform-read
      '(interactive
	(list
	 (hargs:read "Copy tree without attributes: (0 for whole outline) "
		     nil label-default nil 'kcell)
	 (read-buffer "To buffer: "
		      (save-window-excursion
			(if (one-window-p)
			    (select-frame (next-frame))
			  (other-window 1))
			(buffer-name))
		      t)
	 (y-or-n-p "Copy invisible text? "))))))
  (message "") ;; Erase last interactive prompt, if any.
  (setq buffer (get-buffer-create buffer))
  (if (equal cell-ref "0")
      (hypb:insert-region buffer (point-min) (point-max) invisible-flag)
    (let (start end)
      (save-excursion
	(kotl-mode:goto-cell cell-ref t)
	(save-excursion (beginning-of-line) (setq start (point)))
	(setq end (kotl-mode:tree-end)))
      (hypb:insert-region buffer start end invisible-flag))))

(defun kotl-mode:move-after (from-cell-ref to-cell-ref child-p
			     &optional copy-p fill-p)
  "Move tree rooted at FROM-CELL-REF to follow tree rooted at TO-CELL-REF.
If prefix arg CHILD-P is non-nil, make FROM-CELL-REF the first child of
TO-CELL-REF, otherwise make it the sibling following TO-CELL-REF.
With optional COPY-P, copies tree rather than moving it.

Leave point at original location but return the tree's new start point."
  (interactive
   (let* ((label (kcell-view:label))
	  (hargs:defaults (list label label)))
     (append
      (hargs:iform-read
       (list
	'interactive
	(format "*+KMove tree: \n+KMove tree <%%s> to follow as %s of cell: "
		(if current-prefix-arg "child" "sibling"))))
      (list current-prefix-arg))))
  (if (and (not copy-p) (equal from-cell-ref to-cell-ref))
      (error "(kotl-mode:move-after): Can't move tree after itself"))
  (let* ((orig (set-marker (make-marker) (point)))
	 (label-sep-len (kview:label-separator-length kview))
	 (move-to-point (set-marker
			 (make-marker)
			 (kotl-mode:goto-cell to-cell-ref t)))
	 (to-label (kcell-view:label))
	 (to-indent (kcell-view:indent nil label-sep-len))
	 (from-label (progn (kotl-mode:goto-cell from-cell-ref t)
			    (kcell-view:label)))
	 (from-indent (kcell-view:indent nil label-sep-len))
	 (start (kotl-mode:tree-start))
	 (end   (kotl-mode:tree-end))
	 (sib-id (if (= 0 (kotl-mode:forward-cell 1))
		     (kcell-view:idstamp)))
	 new-tree-start)
    ;;
    ;; We can't move a tree to a point within itself, so if that is the case
    ;; and this is not a copy operation, signal an error.
    (if (and (not copy-p) (>= move-to-point start) (<= move-to-point end))
	(error "(kotl-mode:move-after): Can't move tree <%s> to within itself"
	       from-label))
    ;;
    ;; If tree to move has a sibling, point is now at the start of the
    ;; sibling cell.  Mark its label with a property which will be deleted
    ;; whenever the cell label is renumbered.  This tells us whether or not
    ;; to renumber the sibling separately from the tree to move.
    (if sib-id
	;; Move to middle of label and insert klabel-original temp property.
	(progn (goto-char (- (point) label-sep-len 3))
	       (kproperty:set 'klabel-original t)))
    ;;
    ;; Position for insertion before deletion of tree-to-move from old
    ;; position, in case old position precedes new one.
    ;; Skip past either cell or tree at move-to-point.
    (goto-char move-to-point)
    (if child-p
	;; Move to insert position for first child of to-cell-ref.
	(progn (goto-char (kcell-view:end))
	       (setq to-label (klabel:child to-label)
		     to-indent (+ to-indent (kview:level-indent kview))))
      ;; Move to after to-cell-ref's tree for insertion as following sibling.
      (goto-char (kotl-mode:tree-end))
      (setq to-label (klabel:increment to-label)))
    ;;
    ;; Insert tree-to-move at new location
    ;;
    (kview:move start end (point) from-indent to-indent copy-p
		(or fill-p kotl-mode:refill-flag))
    ;;
    ;; Ensure that point is within editable region of cell with to-label.
    (kotl-mode:to-valid-position)
    (setq new-tree-start (point))
    ;;
    ;; Update current cell and new siblings' labels within view.
    (klabel-type:update-labels to-label)
    ;;
    (if copy-p
	nil
      ;;
      ;; Move to sibling of tree-to-move within view and update labels within
      ;; view of tree-to-move's original siblings.
      (if sib-id
	  (progn (kotl-mode:goto-cell sib-id t)
		 ;; Sibling labels may have already been updated if tree was
		 ;; moved somewhere preceding its siblings.
		 (let ((label-middle (- (point) label-sep-len 2)))
		   (if (kproperty:get label-middle 'klabel-original)
		       (klabel-type:update-labels from-label))))))
    ;;
    (goto-char orig)
    ;;
    ;; Ensure that point is within editable region of a cell.
    (kotl-mode:to-valid-position)
    ;;
    (set-marker orig nil)
    (set-marker move-to-point nil)
    new-tree-start))

(defun kotl-mode:move-before (from-cell-ref to-cell-ref parent-p
			      &optional copy-p fill-p)
  "Move tree rooted at FROM-CELL-REF to precede tree rooted at TO-CELL-REF.
If prefix arg PARENT-P is non-nil, make FROM-CELL-REF the first child of
TO-CELL-REF's parent, otherwise make it the preceding sibling of TO-CELL-REF.
With optional COPY-P, copies tree rather than moving it.

Leave point at original location but return the tree's new start point."
  (interactive
   (let* ((label (kcell-view:label))
	  (hargs:defaults (list label label)))
     (append
      (hargs:iform-read
       (list 'interactive
	     (format "*+KMove tree: \n+KMove tree <%%s> to be %s of cell: "
		     (if current-prefix-arg "first child of parent"
		       "preceding sibling"))))
      (list current-prefix-arg))))
  (if (and (not copy-p) (equal from-cell-ref to-cell-ref))
      (error "(kotl-mode:move-before): Can't move tree before itself"))
  (let* ((orig (set-marker (make-marker) (point)))
	 (label-sep-len (kview:label-separator-length kview))
	 (move-to-point (set-marker
			 (make-marker)
			 (kotl-mode:goto-cell to-cell-ref t)))
	 (to-label (kcell-view:label))
	 (to-indent (kcell-view:indent nil label-sep-len))
	 (from-label (progn (kotl-mode:goto-cell from-cell-ref t)
			    (kcell-view:label)))
	 (from-indent (kcell-view:indent nil label-sep-len))
	 (start (kotl-mode:tree-start))
	 (end   (kotl-mode:tree-end))
	 (sib-id (if (= 0 (kotl-mode:forward-cell 1))
		     (kcell-view:idstamp)))
	 new-tree-start)
    ;;
    ;; We can't move a tree to a point within itself, so if that is the case
    ;; and this is not a copy operation, signal an error.
    (if (and (not copy-p) (>= move-to-point start) (<= move-to-point end))
	(error "(kotl-mode:move-before): Can't move tree <%s> to within itself"
	       from-label))
    ;;
    ;; If tree to move has a sibling, point is now at the start of the
    ;; sibling cell.  Mark its label with a property which will be deleted
    ;; whenever the cell label is renumbered.  This tells us whether or not
    ;; to renumber the sibling separately from the tree to move.
    (if sib-id
	;; Move to middle of label and insert klabel-original temp property.
	(progn (goto-char (- (point) label-sep-len 3))
	       (kproperty:set 'klabel-original t)))
    ;;
    ;; Position for insertion at succeeding-tree, before deletion of
    ;; tree-to-move from old position, in case old position precedes new one.
    (goto-char move-to-point)
    (if parent-p
	;; Move to insert position for first child of to-cell-ref's parent.
	(if (kcell-view:parent nil label-sep-len)
	    (progn (setq to-label (klabel:child (kcell-view:label)))
		   (goto-char (kcell-view:end)))
	  (error "(kotl-mode:move-before): to-cell-ref's parent not in current view"))
      ;; Move to before to-cell-ref for insertion as preceding sibling.
      (goto-char (kotl-mode:tree-start)))
    ;;
    ;; Insert tree-to-move at new location
    ;;
    (kview:move start end (point) from-indent to-indent copy-p
		(or fill-p kotl-mode:refill-flag))
    ;;
    ;; Ensure that point is within editable region of root of tree just moved.
    (kotl-mode:to-valid-position)
    (setq new-tree-start (point))
    ;;
    ;; Update current cell and new siblings' labels within view.
    (klabel-type:update-labels to-label)
    ;;
    (if copy-p
	nil
      ;;
      ;; Move to sibling of tree-to-move within view and update labels within
      ;; view of tree-to-move's original siblings.
      (if sib-id
	  (progn
	    (kotl-mode:goto-cell sib-id t)
	    ;; Sibling labels may have already been updated if tree was
	    ;; moved somewhere preceding its siblings.
	    (let ((label-middle (- (point) label-sep-len 2)))
	      (if (kproperty:get label-middle 'klabel-original)
		  (klabel-type:update-labels from-label))))))
    ;;
    (goto-char orig)
    ;;
    ;; Ensure that point is within editable region of a cell.
    (kotl-mode:to-valid-position)
    ;;
    (set-marker orig nil)
    (set-marker move-to-point nil)
    new-tree-start))

(defun kotl-mode:yank (&optional arg)
  "Reinsert the last stretch of killed text.
More precisely, reinsert the stretch of killed text most recently
killed OR yanked.  Put point at end, and set mark at beginning.
With just C-u as argument, same but put point at beginning (and mark at end).
With argument N, reinsert the Nth most recently killed stretch of killed
text.
See also the command \\[kotl-mode:yank-pop]."
  (interactive "*P")
  (push-mark (point))
  (let* ((yank-text (current-kill (cond
				   ((listp arg) 0)
				   ((eq arg '-) -1)
				   (t (1- arg)))))
	 (indent (kcell-view:indent))
	 (indent-str (make-string indent ?\ )))
    ;; Convert all occurrences of newline to newline + cell indent.
    ;; Then insert into buffer.
    (insert (hypb:replace-match-string
	     "[\n\r]" yank-text (concat "\\0" indent-str))))
  (if (consp arg)
      ;; This is like exchange-point-and-mark, but doesn't activate the mark.
      ;; It is cleaner to avoid activation, even though the command
      ;; loop would deactivate the mark because we inserted text.
      (goto-char (prog1 (mark t)
		   (set-marker (hypb:mark-marker t) (point)))))
  nil)

(defun kotl-mode:yank-pop (arg)
  "Replace just-yanked stretch of killed text with a different stretch.
This command is allowed only immediately after a `yank' or a `yank-pop'.
At such a time, the region contains a stretch of reinserted
previously-killed text.  `yank-pop' deletes that text and inserts in its
place a different stretch of killed text.

With no argument, the previous kill is inserted.
With argument N, insert the Nth previous kill.
If N is negative, this is a more recent kill.

The sequence of kills wraps around, so that after the oldest one
comes the newest one."
  (interactive "*p")
  (if (not (eq last-command 'kotl-mode:yank))
      (error "Previous command was not a yank"))
  (setq this-command 'kotl-mode:yank)
  (let ((before (< (point) (mark t))))
    (delete-region (point) (mark t))
    (set-marker (hypb:mark-marker t) (point) (current-buffer))
    (let* ((yank-text (current-kill arg))
	   (indent (kcell-view:indent))
	   (indent-str (make-string indent ?\ )))
      ;; Convert all occurrences of newline to newline + cell indent.
      ;; Then insert into buffer.
      (insert (hypb:replace-match-string
	       "[\n\r]" yank-text (concat "\\0" indent-str))))
    (if before
	;; This is like exchange-point-and-mark, but doesn't activate the mark.
	;; It is cleaner to avoid activation, even though the command
	;; loop would deactivate the mark because we inserted text.
	(goto-char (prog1 (mark t)
		     (set-marker (hypb:mark-marker t) (point) (current-buffer))))))
  nil)

;;;
;;; Movement
;;;

;;; Cursor and keypad key functions aliases for XEmacs.
(if (not (string-match "XEmacs\\|Lucid" emacs-version))
    nil
  (fset 'kotl-mode:fkey-backward-char 'kotl-mode:backward-char)
  (fset 'kotl-mode:fkey-forward-char  'kotl-mode:forward-char)
  (fset 'kotl-mode:fkey-next-line     'kotl-mode:next-line)
  (fset 'kotl-mode:fkey-previous-line 'kotl-mode:previous-line)
  (fset 'kotl-mode:deprecated-scroll-down 'kotl-mode:scroll-down)
  (fset 'kotl-mode:deprecated-scroll-up 'kotl-mode:scroll-up)
  (fset 'kotl-mode:deprecated-bob     'kotl-mode:beginning-of-buffer)
  (fset 'kotl-mode:deprecated-eob     'kotl-mode:end-of-buffer))

(defun kotl-mode:back-to-indentation ()
  "Move point to the first non-read-only non-whitespace character on this line."
  (interactive)
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (back-to-indentation)
  (kotl-mode:to-valid-position))

(defun kotl-mode:backward-cell (arg)
  "Move to prefix ARGth prior visible cell (same level) within current view.
Return number of cells left to move."
  (interactive "p")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (if (< arg 0)
      (kotl-mode:forward-cell (- arg))
    (let ((prior (= arg 0))
	  (label-sep-len (kview:label-separator-length kview)))
      (while (and (> arg 0) (setq prior (kcell-view:backward t label-sep-len)))
	(setq arg (1- arg)))
      (if (or prior (not (interactive-p)))
	  arg
	(error "(kotl-mode:backward-cell): No prior cell at same level")))))

(defun kotl-mode:backward-char (&optional arg)
  "Move point backward ARG (or 1) characters and return point."
  (interactive "p")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (or arg (setq arg 1))
  (if (>= arg 0)
      (while (> arg 0)
	(cond ((kotl-mode:bobp)
	       (error "(kotl-mode:backward-char): Beginning of buffer"))
	      ((kotl-mode:bocp)
	       (if (kcell-view:previous)
		   (kotl-mode:end-of-cell)))
	      ((kotl-mode:bolp)
	       (if (re-search-backward "[\n\r]" nil t)
		   (kotl-mode:to-valid-position t)))
	      (t (backward-char)))
	(setq arg (1- arg)))
    (kotl-mode:forward-char (- arg)))
  (point))

(defun kotl-mode:backward-paragraph (&optional arg)
  "Move backward to start of paragraph.
With arg N, do it N times; negative arg -N means move forward N paragraphs.
Return point.

A paragraph start is the beginning of a line which is a
`first-line-of-paragraph' or which is ordinary text and follows a
paragraph-separating line.

See `forward-paragraph' for more information."
  (interactive "p")
  (setq arg (prefix-numeric-value arg)
	zmacs-region-stays t);; Maintain region highlight for XEmacs.
  (kotl-mode:forward-paragraph (- arg)))

(fset 'kotl-mode:backward-para 'kotl-mode:backward-paragraph)

(defun kotl-mode:backward-sentence (&optional arg)
  "Move point backward ARG (or 1) sentences and return point."
  (interactive "p")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (let* ((label-sep-len (kview:label-separator-length kview))
	 ;; Setting fill prefix makes sentence commands properly recognize
	 ;; indented paragraphs.
	 (fill-prefix (make-string (kcell-view:indent nil label-sep-len) ?\ )))
    (if (kotl-mode:bobp)
	(error "(kotl-mode:backward-sentence): First sentence")
      (if (and (kotl-mode:bocp) (kcell-view:previous nil label-sep-len))
	  (goto-char (kcell-view:end-contents)))
      (or arg (setq arg 1))
      (save-restriction
	(if (= arg 1)
	    (narrow-to-region
	     (- (kcell-view:start nil label-sep-len)
		(kcell-view:indent nil label-sep-len))
	     (kcell-view:end-contents)))
	(unwind-protect
	    (let ((opoint (point)))
	      (backward-sentence arg)
	      (if (= opoint (point))
		  (progn (kcell-view:previous nil label-sep-len)
			 (backward-sentence arg))))
	  (kotl-mode:to-valid-position t)))))
  (point))

(defun kotl-mode:backward-word (&optional arg)
  "Move point backward ARG (or 1) words and return point."
  (interactive "p")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (or arg (setq arg 1))
  (if (>= arg 0)
      (while (> arg 0)
	(cond ((kotl-mode:bobp) (setq arg 0))
	      ((kotl-mode:bocp)
	       (beginning-of-line)
	       (kotl-mode:to-valid-position t)))
	(unwind-protect
	    (backward-word 1)
	  (kotl-mode:to-valid-position t))
	(setq arg (1- arg)))
    (kotl-mode:forward-word (- arg)))
  (point))

(defun kotl-mode:beginning-of-buffer ()
  "Move point to beginning of buffer and return point."
  (interactive)
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (goto-char (point-min))
  ;; To move to cell start.
  (goto-char (kcell-view:start)))

(defun kotl-mode:beginning-of-cell (&optional arg)
  "Move point to beginning of current or ARGth - 1 prior cell and return point."
  (interactive "p")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (or arg (setq arg 1))
  (or (integer-or-marker-p arg)
      (error "(kotl-mode:beginning-of-cell): Wrong type arg, integer-or-marker, '%s'" arg))
  (if (= arg 1)
      (goto-char (kcell-view:start))
    (kotl-mode:backward-cell (1- arg)))
  (point))

;;; Avoid XEmacs byte-compiler bug which inserts nil for calls to this
;;; function if named kotl-mode:beginning-of-line.
;;;
(defun kotl-mode:start-of-line (&optional arg)
  "Move point to beginning of current or ARGth - 1 line and return point."
  (interactive "p")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (or arg (setq arg 1))
  (or (integer-or-marker-p arg)
      (error "(kotl-mode:start-of-line): Wrong type arg, integer-or-marker, '%s'" arg))
  (forward-line (1- arg))
  (if (eolp)
      nil
    (forward-char (prog1 (kcell-view:indent)
		    (beginning-of-line))))
  (point))

(defalias 'kotl-mode:beginning-of-line 'kotl-mode:start-of-line)

(defun kotl-mode:beginning-of-tree ()
  "Move point to the level 1 root of the current cell's tree.
Leave point at the start of the cell."
  (interactive)
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (let ((label-sep-len (kview:label-separator-length kview)))
    (if (/= (kcell-view:level nil label-sep-len) 1)
	;; Enable user to return to this previous position if desired.
	(push-mark nil 'no-msg))
    (while (and (/= (kcell-view:level nil label-sep-len) 1)
		(kcell-view:parent nil label-sep-len)))
    (kotl-mode:beginning-of-cell)))

(defun kotl-mode:down-level (arg)
  "Move down prefix ARG levels lower within current tree."
  (interactive "p")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (if (< arg 0)
      (kotl-mode:up-level (- arg))
    ;; Enable user to return to this previous position if desired.
    (push-mark nil 'no-msg)
    (let ((child))
      (while (and (> arg 0) (kcell-view:child))
	(or child (setq child t))
	(setq arg (1- arg)))
      ;; Signal an error if couldn't move down at least 1 child level.
      (or child
	  (progn
	    (goto-char (hypb:mark t))
	    (pop-mark)
	    (error "(kotl-mode:down-level): No child level to which to move")
	    )))))

(defun kotl-mode:end-of-buffer ()
  "Move point to end of buffer and return point."
  (interactive)
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (goto-char (point-max))
  ;; To move to cell end.
  (kotl-mode:to-valid-position t)
  (point))

(defun kotl-mode:end-of-cell (&optional arg)
  "Move point to end of current or ARGth - 1 succeeding cell and return point."
  (interactive "p")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (or arg (setq arg 1))
  (or (integer-or-marker-p arg)
      (error "(kotl-mode:end-of-cell): Wrong type arg, integer-or-marker, '%s'" arg))
  (if (= arg 1)
      (goto-char (kcell-view:end-contents))
    (kotl-mode:forward-cell (1- arg)))
  (point))

;;; Avoid XEmacs byte-compiler bug which inserts nil for calls to this
;;; function if named kotl-mode:end-of-line.
;;;
(defun kotl-mode:finish-of-line (&optional arg)
  "Move point to end of current or ARGth - 1 line and return point."
  (interactive "p")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (or arg (setq arg 1))
  (or (integer-or-marker-p arg)
      (error "(kotl-mode:finish-of-line): Wrong type arg, integer-or-marker, '%s'" arg))
  (forward-line (1- arg))
  (end-of-line)
  ;; May have to move backwards to before label if support labels
  ;; at end of cells.
  (point))

(defalias 'kotl-mode:end-of-line 'kotl-mode:finish-of-line)

(defun kotl-mode:end-of-tree ()
  "Move point to the last cell in tree rooted at the current cell.
Leave point at the start of the cell."
  (interactive)
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  ;; Enable user to return to this previous position if desired.
  (push-mark nil 'no-msg)
  (let ((label-sep-len (kview:label-separator-length kview)))
    (if (kcell-view:forward nil label-sep-len)
	;; Move to cell preceding start of next tree.
	(kcell-view:previous nil label-sep-len)
      ;; Otherwise, no next tree, so move until find last cell in tree.
      (let ((cell-indent (kcell-view:indent nil label-sep-len))
	    (end-point (point)))
	;; Terminate when no further cells or when reach a cell at an equal
	;; or higher level in the outline than the first cell that we
	;; processed.
	(while (and (kcell-view:next nil label-sep-len)
		    (> (kcell-view:indent nil label-sep-len) cell-indent))
	  (setq end-point (point)))
	(goto-char end-point)))
    (kotl-mode:beginning-of-cell)))

(defun kotl-mode:first-sibling ()
  "Move point to the first sibling of the present cell.
Leave point at the start of the cell or at its present position if it is
already within the first sibling cell."
  (interactive)
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (let ((label-sep-len (kview:label-separator-length kview)))
    (if (save-excursion (kcell-view:backward nil label-sep-len))
	;; Enable user to return to this previous position if desired.
	(push-mark nil 'no-msg))
    (while (kcell-view:backward nil label-sep-len))))

(defun kotl-mode:forward-cell (arg)
  "Move to prefix ARGth following cell (same level) within current view.
Return number of cells left to move."
  (interactive "p")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (if (< arg 0)
      (kotl-mode:backward-cell (- arg))
    (let ((next (= arg 0))
	  (label-sep-len (kview:label-separator-length kview)))
      (while (and (> arg 0) (setq next (kcell-view:forward t label-sep-len)))
	(setq arg (1- arg)))
      (if (or next (not (interactive-p)))
	  arg
	(error "(kotl-mode:forward-cell): No following cell at same level")))))

(defun kotl-mode:forward-char (&optional arg)
  "Move point forward ARG (or 1) characters and return point."
  (interactive "p")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (or arg (setq arg 1))
  (if (>= arg 0)
      (while (> arg 0)
	(cond ((and (kotl-mode:eolp) (kotl-mode:last-line-p))
	       (error "(kotl-mode:forward-char): End of buffer"))
	      ((kotl-mode:eocp)
	       (skip-chars-forward "\n\r")
	       (kotl-mode:start-of-line))
	      ((kotl-mode:eolp)
	       (forward-char)
	       (kotl-mode:start-of-line))
	      (t (forward-char)))
	(setq arg (1- arg)))
    (kotl-mode:backward-char (- arg)))
  (point))

(defun kotl-mode:forward-paragraph (&optional arg)
  "Move point forward until after the last character of the current paragraph.
With arg N, do it N times; negative arg -N means move backward N paragraphs.
Return point.

A line which `paragraph-start' matches either separates paragraphs
\(if `paragraph-separate' matches it also) or is the first line of a paragraph.
A paragraph end is one character before the beginning of a line which is not
part of the paragraph, or the end of the buffer."
  (interactive "p")
  (setq arg (prefix-numeric-value arg)
	zmacs-region-stays t);; Maintain region highlight for XEmacs.
  (if (< arg 0)
      (progn
	(if (kotl-mode:bocp) (setq arg (1- arg)))
	(while (< arg 0)
	  (start-of-paragraph-text)
	  (setq arg (1+ arg))))
    (while (> arg 0) 
      (end-of-paragraph-text)
      (setq arg (1- arg))))
  (kotl-mode:to-valid-position)
  (point))

(fset 'kotl-mode:forward-para 'kotl-mode:forward-paragraph)

(defun kotl-mode:forward-sentence (&optional arg)
  "Move point forward ARG (or 1) sentences and return point."
  (interactive "P")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (let* ((label-sep-len (kview:label-separator-length kview))
	 ;; Setting fill prefix makes sentence commands properly recognize
	 ;; indented paragraphs.
	 (fill-prefix (make-string (kcell-view:indent nil label-sep-len) ?\ )))
    (if (kotl-mode:eobp)
	(error "(kotl-mode:forward-sentence): Last sentence")
      (if (kotl-mode:eocp) (kcell-view:next nil label-sep-len))
      (or arg (setq arg 1))
      (save-restriction
	(if (= arg 1)
	    (narrow-to-region
	     (- (kcell-view:start nil label-sep-len)
		(kcell-view:indent nil label-sep-len))
	     (kcell-view:end-contents)))
	(unwind-protect
	    (let ((opoint (point)))
	      (forward-sentence arg)
	      (if (= opoint (point))
		  (progn (kcell-view:next nil label-sep-len)
			 (forward-sentence arg))))
	  (kotl-mode:to-valid-position)))))
  (point))

(defun kotl-mode:forward-word (&optional arg)
  "Move point forward ARG (or 1) words and return point."
  (interactive "p")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (or arg (setq arg 1))
  (if (>= arg 0)
      (while (> arg 0)
	(cond ((kotl-mode:eobp) (setq arg 0))
	      ((kotl-mode:eocp)
	       (skip-chars-forward "\n\r")
	       (kotl-mode:start-of-line)))
	(unwind-protect
	    (forward-word 1)
	  (kotl-mode:to-valid-position))
	;; If point is at beginning of a cell after moving forward a word,
	;; then we moved over something other than a word (some
	;; punctuation or an outline autonumber); therefore, leave counter as
	;; is in order to move forward over next word.
	(or (kotl-mode:bocp)
	    (setq arg (1- arg))))
    (kotl-mode:backward-word (- arg)))
  (point))

(defun kotl-mode:goto-cell (cell-ref &optional error-p)
  "Move point to start of cell given by CELL-REF.  (See 'kcell:ref-to-id'.)
Return point iff CELL-REF is found within current view.
With a prefix argument, CELL-REF is assigned the argument value for use
as an idstamp.

Optional second arg, ERROR-P, non-nil means signal an error if CELL-REF is
not found within current view.  Will signal same error if called
interactively when CELL-REF is not found."
  (interactive
   (list (if current-prefix-arg
	     (format "0%d" (prefix-numeric-value current-prefix-arg))
	   (read-string "Goto cell label or id: "))))
  (setq cell-ref
	(or (kcell:ref-to-id cell-ref)
	    (error "(kotl-mode:goto-cell): Invalid cell reference, '%s'" cell-ref)))
  (let* ((opoint (point))
	 (found)
	 cell-id kvspec)
    (if (= ?| (aref cell-ref 0))
	;; This is a standalone view spec, not a cell reference.
	(progn (kvspec:activate cell-ref) (setq found (point)))

      ;; !! Remove any relative specs and view specs from
      ;; cell-ref to form cell-id.  Really should account for relative
      ;; specs here, but we don't yet support them.
      (if (string-match "\\(\\.[a-zA-Z]+\\)?\\([|:].*\\)\\|\\.[a-zA-Z]+"
			cell-ref)
	  (setq cell-id (substring cell-ref 0 (match-beginning 0))
		kvspec  (if (match-beginning 2)
			    (substring
			     cell-ref (match-beginning 2) (match-end 2))))
	(setq cell-id cell-ref kvspec nil))

      (goto-char (point-min))
      (cond ((= ?0 (aref cell-id 0))
	     ;; is an idstamp
	     (if (kview:goto-cell-id cell-id)
		 (setq found (point))))
	    ;; is a label
	    ((re-search-forward
	      (format "\\([\n\r][\n\r]\\|\\`\\)[ ]*%s%s"
		      (regexp-quote cell-id)
		      (regexp-quote (kview:label-separator kview)))
	      nil t)
	     (setq found (point)))
	    ;; no match
	    (t (goto-char opoint)
	       nil))
      (if (and (not found) (or error-p (interactive-p)))
	  (error "(kotl-mode:goto-cell): No '%s' cell in this view" cell-ref)
	;; Activate any viewspec associated with cell-ref.
	(if kvspec (kvspec:activate kvspec))))
    found))

(defun kotl-mode:head-cell ()
  "Move point to the start of the first visible cell at the same level as current cell.
If at head cell already, do nothing and return nil."
  (interactive "p")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (let ((moved)
	(label-sep-len (kview:label-separator-length kview)))
    (while (kcell-view:backward t label-sep-len)
      (setq moved t))
    moved))

(defun kotl-mode:last-sibling ()
  "Move point to the last sibling of the present cell.
Leave point at the start of the cell or at its present position if it is
already within the last sibling cell."
  (interactive)
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (let ((label-sep-len (kview:label-separator-length kview)))
    (if (save-excursion (kcell-view:forward nil label-sep-len))
	;; Enable user to return to this previous position if desired.
	(push-mark nil 'no-msg))
    (while (kcell-view:forward nil label-sep-len))))

(defun kotl-mode:mark-paragraph ()
  "Put point at beginning of this paragraph, mark at end.
The paragraph marked is the one that contains point or follows point."
  (interactive)
  (forward-paragraph 1)
  (kotl-mode:to-valid-position t)
  (hypb:push-mark nil t t)
  (backward-paragraph 1)
  (kotl-mode:to-valid-position))

(defun kotl-mode:mark-whole-buffer ()
  "Put point at first editable character in buffer and mark at the last such character."
  (interactive)
  (hypb:push-mark (point))
  (kotl-mode:end-of-buffer)
  (hypb:push-mark (point) nil t)
  (kotl-mode:beginning-of-buffer))

(defun kotl-mode:next-cell (arg)
  "Move to prefix ARGth next cell (any level) within current view."
  (interactive "p")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (if (< arg 0)
      (kotl-mode:previous-cell (- arg))
    (let ((next (= arg 0))
	  (label-sep-len (kview:label-separator-length kview)))
      (while (and (> arg 0) (setq next (kcell-view:next t label-sep-len)))
	(setq arg (1- arg)))
      (if next
	  arg
	(error "(kotl-mode:next-cell): Last cell")))))

(defun kotl-mode:next-line (arg)
  "Move point to ARGth next line and return point."
  (interactive "p")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (kotl-mode:set-temp-goal-column)
  (let ((orig-arg arg))
    (cond ((> arg 0)
	   (while (and (> arg 0) (= 0 (forward-line 1)))
	     (cond ((kotl-mode:eobp)
		    (forward-line -1)
		    (goto-char (kcell-view:end-contents))
		    (and (interactive-p) (= orig-arg arg)
			 (message "(kotl-mode:next-line): Last line") (beep))
		    (setq arg 0)
		    )
		   ((looking-at "^$");; blank line between cells
		    nil);; Don't count this line.
		   (t (setq arg (1- arg)))))
	   (kotl-mode:line-move 0)
	   (kotl-mode:to-valid-position)
	   )
	  ((< arg 0)
	   (kotl-mode:previous-line (- arg)))
	  (t)))
  (setq this-command 'next-line)
  (point))

(defun kotl-mode:next-tree ()
  "Move past current tree to next tree, or to last cell in tree if no next tree.
Return non-nil iff there is a next tree within this koutline."
  (let ((start-indent (kcell-view:indent))
	(label-sep-len (kview:label-separator-length kview))
	(same-tree t))
      (while (and (kcell-view:next nil label-sep-len)
		  (setq same-tree (< start-indent
				     (kcell-view:indent nil label-sep-len)))))
      (not same-tree)))

(defun kotl-mode:previous-line (arg)
  "Move point to ARGth previous line and return point."
  (interactive "p")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (kotl-mode:set-temp-goal-column)
  (cond ((> arg 0)
         (while (and (> arg 0) (= 0 (forward-line -1)))
           (cond ((kotl-mode:bobp)
                  (kotl-mode:beginning-of-cell)
                  (setq arg 0))
                 ((looking-at "^$") ;; blank line between cells
                  nil) ;; Don't count this line.
                 (t (setq arg (1- arg)))))
	 (kotl-mode:line-move 0)
	 (kotl-mode:to-valid-position)
	 )
        ((< arg 0)
         (kotl-mode:next-line (- arg)))
        (t))
  (setq this-command 'previous-line)
  (point))

(defun kotl-mode:previous-cell (arg)
  "Move to prefix ARGth previous cell (any level) within current view."
  (interactive "p")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (if (< arg 0)
      (kotl-mode:next-cell (- arg))
    (let ((previous (= arg 0))
	  (label-sep-len (kview:label-separator-length kview)))
      (while (and (> arg 0) (setq previous
				  (kcell-view:previous t label-sep-len)))
	(setq arg (1- arg)))
      (if previous
	  arg
	(error "(kotl-mode:previous-cell): First cell")))))

(defun kotl-mode:scroll-down (arg)
  "Scroll text of current window downward ARG lines; or a windowful if no ARG."
  (interactive "P")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (scroll-down arg)
  (kotl-mode:to-valid-position t))

(defun kotl-mode:scroll-up (arg)
  "Scroll text of current window upward ARG lines; or a windowful if no ARG."
  (interactive "P")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (scroll-up arg)
  (kotl-mode:to-valid-position))

(defun kotl-mode:tail-cell ()
  "Move point to the start of the last visible cell at the same level as current cell and return t.
If at tail cell already, do nothing and return nil."
  (interactive "p")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (let ((moved)
	(label-sep-len (kview:label-separator-length kview)))
    (while (kcell-view:forward t label-sep-len)
      (setq moved t))
    moved))

(defun kotl-mode:up-level (arg)
  "Move up prefix ARG levels higher in current outline view."
  (interactive "p")
  (setq zmacs-region-stays t) ;; Maintain region highlight for XEmacs.
  (if (< arg 0)
      (kotl-mode:down-level (- arg))
    ;; Enable user to return to this previous position if desired.
    (push-mark nil 'no-msg)
    (let ((parent)
	  (label-sep-len (kview:label-separator-length kview))
	  result)
      (while (and (> arg 0) (setq result (kcell-view:parent t label-sep-len)))
	(or parent (setq parent result))
	(setq arg (if (eq result 0) 0 (1- arg))))
      ;; Signal an error if couldn't move up at least 1 parent level.
      (or (and parent (not (eq parent 0)))
	  (progn
	    (goto-char (hypb:mark t))
	    (pop-mark)
	    (error "(kotl-mode:up-level): No parent level to which to move")
	    )))))

;;;
;;; Predicates
;;;

(defun kotl-mode:bobp ()
  "Return point if at the start of the first cell in kview, else nil."
  (interactive)
  (or (bobp)
      (and (not (save-excursion (re-search-backward "[\n\r]" nil t)))
	   (kotl-mode:bolp))))

(defun kotl-mode:bocp ()
  "Return point if at beginning of a kcell, else nil."
  (and (kotl-mode:bolp)
       (let ((begin-point (kcell-view:plist-point))
	     (bol))
	 (and begin-point
	      (save-excursion
		;; If first line-begin is less than cell begin point,
		;; then we know we are on the first line of the cell.
		(if (setq bol (re-search-backward "^" nil t))
		    (<= bol begin-point)))))
       (point)))

(defun kotl-mode:bolp ()
  "Return point if at beginning of a kview line, else nil."
  (if (= (current-column) (kcell-view:indent))
      (point)))

(defun kotl-mode:buffer-empty-p ()
  "Return non-nil iff there are no outline cells within current buffer."
  (save-excursion
    (goto-char (point-min))
    (looking-at "[\n\r]*\\'")))

(defun kotl-mode:eobp ()
  "Return point if after the end of the last cell in kview, else nil."
  (interactive)
  (if (looking-at "^[\n\r]*\\'") (point)))

(defun kotl-mode:eocp ()
  "Return point if at the end of a kview cell, else nil."
  (or (eobp)
      (looking-at "[\n\r]+\\'")
      (and (eolp)
	   (save-excursion
	     (skip-chars-forward "\n\r")
	     (kotl-mode:start-of-line)
	     (kotl-mode:bocp)))))

(fset 'kotl-mode:eolp 'eolp)

(defun kotl-mode:first-cell-p ()
  "Return t iff point is on the first cell of the outline."
  (save-excursion (not (kcell-view:previous))))

(fset 'kotl-mode:first-line-p 'first-line-p)

(defun kotl-mode:last-cell-p ()
  "Return t iff point is on the last cell of the outline."
  (save-excursion (not (kcell-view:next))))

(defun kotl-mode:last-line-p ()
  "Return t iff point is on the last line of the outline."
  (save-excursion
    (kotl-mode:finish-of-line)
    (looking-at "\n*\\'")))

;;;
;;; Smart Key Support
;;;


(defun kotl-mode:action-key ()
  "Collapses, expands, links to, and scrolls through koutline cells.
Invoked via a key press when in kotl-mode.  It assumes that its caller has
already checked that the key was pressed in an appropriate buffer and has
moved the cursor to the selected buffer.

If key is pressed:
 (1) at the end of buffer, uncollapse and unhide all cells in view;
 (2) within a cell, if its subtree is hidden then show it,
     otherwise hide it;
 (3) between cells or within the read-only indentation region to the left of
     a cell, then move point to prior location and begin creation of a
     klink to some other outline cell; hit the Action Key twice to select the
     link referent cell;
 (4) anywhere else, scroll up a windowful."
  (interactive)
  (cond	((kotl-mode:eobp) (kotl-mode:show-all))
	((kotl-mode:eolp) (smart-scroll-up))
	((not (kview:valid-position-p))
	 (if (markerp action-key-depress-prev-point)
	     (progn (select-window
		     (get-buffer-window
		      (marker-buffer action-key-depress-prev-point)))
		    (goto-char (marker-position action-key-depress-prev-point))
		    (call-interactively 'klink:create))
	   (kotl-mode:to-valid-position)
	   (error "(kotl-mode:action-key): Action Key released at invalid position")))
	(t ;; On a cell line (not at the end of line).
	 (if (smart-outline-subtree-hidden-p)
	     (kotl-mode:show-tree (kcell-view:label))
	   (kotl-mode:hide-tree (kcell-view:label)))))
  (kotl-mode:to-valid-position))

(defun kotl-mode:help-key ()
  "Displays properties of koutline cells, collapses all cells, and scrolls back.
Invoked via an assist-key press when in kotl-mode.  It assumes that its caller
has already checked that the assist-key was pressed in an appropriate buffer
and has moved the cursor to the selected buffer.

If assist-key is pressed:
 (1) at the end of buffer, collapse all cells and hide all non-level-one
     cells;
 (2) on a header line but not at the beginning or end, display properties of
     each cell in tree beginning at point;
 (3) between cells or within the read-only indentation region to the left of
     a cell, then move point to prior location and prompt to move one tree to
     a new location in the outline; hit the Action Key twice to select the
     tree to move and where to move it;
 (4) anywhere else, scroll down a windowful."
  (interactive)
  (cond ((kotl-mode:eobp) (kotl-mode:overview))
	((kotl-mode:eolp) (smart-scroll-down))
	((not (kview:valid-position-p))
	 (if (markerp assist-key-depress-prev-point)
	     (progn (select-window
		     (get-buffer-window
		      (marker-buffer assist-key-depress-prev-point)))
		    (goto-char (marker-position
				assist-key-depress-prev-point))
		    (call-interactively 'kotl-mode:move-after))
	   (kotl-mode:to-valid-position)
	   (error "(kotl-mode:help-key): Help Key released at invalid position")))
	((not (bolp))
	 ;; On an outline header line but not at the start/end of line,
	 ;; show attributes for tree at point.
	 (kotl-mode:cell-help (kcell-view:label) (or current-prefix-arg 2)))
	((smart-scroll-down)))
  (kotl-mode:to-valid-position))

;;;
;;; Structure Editing
;;;

(defun kotl-mode:add-child ()
  "Add a new cell to current kview as first child of current cell."
  (interactive "*")
  (kotl-mode:add-cell '(4)))

(defun kotl-mode:add-parent ()
  "Add a new cell to current kview as sibling of current cell's parent."
  (interactive "*")
  (kotl-mode:add-cell -1))

(defun kotl-mode:add-cell (&optional relative-level contents plist no-fill)
  "Add a cell following current cell at optional RELATIVE-LEVEL with CONTENTS string, attributes in PLIST, a property list, and NO-FILL flag to prevent any filling of CONTENTS. 

Optional prefix arg RELATIVE-LEVEL means add as sibling if nil or >= 0, as
child if equal to universal argument, {C-u}, and as sibling of current cell's
parent, otherwise.  If added as sibling of current level, RELATIVE-LEVEL is
used as a repeat count for the number of cells to add.

Return last newly added cell."
  (interactive "*P")
  (or (stringp contents) (setq contents nil))
  (let ((klabel (kcell-view:label))
	(label-sep-len (kview:label-separator-length kview))
	cell-level new-cell sibling-p child-p start parent
	cells-to-add)
    (setq cell-level (kcell-view:level nil label-sep-len)
	  child-p (equal relative-level '(4))
	  sibling-p (and (not child-p)
			 (cond ((not relative-level) 1)
			       ((>= (prefix-numeric-value relative-level) 0)
				(prefix-numeric-value relative-level))))
	  cells-to-add (or sibling-p 1))
    (if child-p
	(setq cell-level (1+ cell-level))
      (if sibling-p
	  nil
	;; Add as following sibling of current cell's parent.
	;; Move to parent.
	(setq cell-level (1- cell-level)
	      start (point)
	      parent (kcell-view:parent nil label-sep-len))
	(if (not (eq parent t))
	    (progn
	      (goto-char start)
	      (error
	       "(kotl-mode:add-cell): No higher level at which to add cell.")
	      )))
      ;; Skip from point past any children to next cell.
      (if (kotl-mode:next-tree)
	  ;; If found a new tree, then move back to prior cell so can add
	  ;; new cell after it.
	  (kcell-view:previous nil label-sep-len)))
    (goto-char (kcell-view:end))
    ;;
    ;; Insert new cells into view.
    (if (= cells-to-add 1)
	(setq klabel
	      (cond (sibling-p
		     (klabel:increment klabel))
		    (child-p (klabel:child klabel))
		    ;; add as sibling of parent of current cell
		    (t (klabel:increment (klabel:parent klabel))))
	      new-cell (kview:add-cell klabel cell-level contents plist
				       (or no-fill sibling-p
					   (not kotl-mode:refill-flag))))
      ;;
      ;; sibling-p must be true if we are looping here so there is no need to
      ;; conditionalize how to increment the labels.
      (while (>= (setq cells-to-add (1- cells-to-add)) 0)
	(setq klabel (klabel:increment klabel)
	      ;; Since new cells are at the same level as old one, don't fill
	      ;; any of their intial contents.
	      new-cell (kview:add-cell klabel cell-level contents plist t))))
    ;;
    ;; Move back to last inserted cell and then move to its following
    ;; sibling if any.
    (kotl-mode:to-valid-position t)
    (save-excursion
      (if (kcell-view:forward t label-sep-len)
	  ;; Update the labels of these siblings and their subtrees.
	  (klabel-type:update-labels (klabel:increment klabel))))
    ;;
    ;; Leave point within last newly added cell and return this cell.
    (kotl-mode:beginning-of-cell)
    new-cell))

(defun kotl-mode:demote-tree (arg)
  "Move current tree a maximum of prefix ARG levels lower in current view.
Each cell is refilled iff its `no-fill' attribute is nil and
kotl-mode:refill-flag is non-nil.  With prefix ARG = 0, cells are demoted up
to one level and kotl-mode:refill-flag is treated as true."
  (interactive "*p")
  (if (< arg 0)
      (kotl-mode:promote-tree (- arg))
    (let* ((label-sep-len (kview:label-separator-length kview))
	   (orig-level (kcell-view:level nil label-sep-len))
	   (orig-point (point))
	   (orig-id (kcell-view:idstamp))
	   (fill-p (= arg 0))
	   (orig-pos-in-cell
	    (- (point) (kcell-view:start nil label-sep-len)))
	   prev prev-level)
      (if fill-p (setq arg 1))
      (unwind-protect
	  (progn
	    (backward-char 1)
	    (while (and (> arg 0)
			(setq prev
			      (kcell-view:previous nil label-sep-len)))
	      (if prev
		  (progn (setq prev-level
			       (kcell-view:level nil label-sep-len))
			 (cond ((> prev-level (+ orig-level arg))
				;; Don't want to demote this far
				;; so keep looking at prior nodes.
				nil)
			       ((= arg (- prev-level orig-level))
				;; Demote to be sibling of this kcell.
				(setq arg -1))
			       ((< prev-level orig-level)
				;; prev is at higher level then
				;; orig, so can't demote
				(setq prev nil
				      arg 0))
			       (t
				;; Demote below this kcell.  This is
				;; as far we can demote, though it may
				;; not be the full amount of arg.
				(setq arg 0))))))
	    (if prev
		(kotl-mode:move-after
		 (kcell-view:label orig-point)
		 (kcell-view:label) (= arg 0)
		 nil fill-p)))
	;; Move to start of original cell
	(kotl-mode:goto-cell orig-id)
	;; Move to original pos within cell
	(forward-char orig-pos-in-cell)
	(kotl-mode:to-valid-position))
      (if (not prev)
	  (error "(kotl-mode:demote-tree): Cannot demote any further")))))

(defun kotl-mode:exchange-cells (cell-ref-1 cell-ref-2)
  "Exchange CELL-REF-1 with CELL-REF-2 in current view.  Don't move point."
  (interactive
   (let ((hargs:defaults
	  (save-excursion
	    (list (kcell-view:label)
		  (cond
		   ((kcell-view:previous t)
		    (kcell-view:label))
		   ((kcell-view:next t)
		    (kcell-view:label))
		   (t (error
		       "(kotl-mode:exchange-cells): No 2 visible cells")))))))
     (hargs:iform-read
      '(interactive "*+KExchange cell: \n+KExchange cell <%s> with cell: "))))
  (save-excursion
    (let (kcell-1 contents-1
	  kcell-2 contents-2)
      ;;
      ;; Save cell-1 attributes
      (kotl-mode:goto-cell cell-ref-1 t)
      (setq kcell-1 (kcell-view:cell)
	    contents-1 (kcell-view:contents))
      ;;
      ;; Save cell-2 attributes
      (kotl-mode:goto-cell cell-ref-2 t)
      (setq kcell-2 (kcell-view:cell)
	    contents-2 (kcell-view:contents))
      ;;
      ;; Substitute cell-1 attributes into cell-2 location.
      ;;
      ;; Set kcell properties.
      (kcell-view:set-cell kcell-1)
      ;; If idstamp labels are on, then must exchange labels in view.
      (if (eq (kview:label-type kview) 'id)
	  (klabel:set (kcell-view:idstamp)))
      ;; Exchange cell contents.
      (delete-region (kcell-view:start) (kcell-view:end-contents))
      (insert
       (hypb:replace-match-string
	"\\([\n\r]\\)"
	contents-1 (concat "\\1" (make-string (kcell-view:indent) ?\ ))))
      (if kotl-mode:refill-flag (kotl-mode:fill-cell))
      ;;
      ;; Substitute cell-2 attributes into cell-1 location.
      ;;
      ;; Set kcell properties.
      (kotl-mode:goto-cell cell-ref-1 t)
      (kcell-view:set-cell kcell-2)
      ;; If idstamp labels are on, then must exchange labels in view.
      (if (eq (kview:label-type kview) 'id)
	  (klabel:set (kcell-view:idstamp)))
      ;; Exchange cell contents.
      (delete-region (kcell-view:start) (kcell-view:end-contents))
      ;; Add indentation to all but first line.
      (insert
       (hypb:replace-match-string
	"\\([\n\r]\\)"
	contents-2 (concat "\\1" (make-string (kcell-view:indent) ?\ ))))
      (if kotl-mode:refill-flag (kotl-mode:fill-cell)))))

(defun kotl-mode:kill-contents (arg)
  "Kill contents of cell from point to cell end.
With prefix ARG, kill entire cell contents."
  (interactive "*P")
  (kotl-mode:kill-region
   (if arg (kcell-view:start) (point))
   (kcell-view:end-contents)))

(defun kotl-mode:kill-tree (&optional arg)
  "Kill ARG following trees starting with tree rooted at point.
If ARG is not a non-positive number, nothing is done."
  (interactive "*p")
  (or (integerp arg) (setq arg 1))
  (let ((killed) (label (kcell-view:label))
	(label-sep-len (kview:label-separator-length kview))
	start end sib)
    (while (> arg 0)
      (setq start (kotl-mode:tree-start)
	    end   (kotl-mode:tree-end)
	    sib   (kcell-view:sibling-p nil nil label-sep-len)
	    arg (1- arg)
	    killed t)
      ;; Don't want to delete any prior cells, so if on last cell, ensure
      ;; this is the last one killed.
      (if (kotl-mode:last-cell-p)
	  (progn (setq arg 0)
		 (kview:delete-region start end))
	(kview:delete-region start end)
	(kotl-mode:to-valid-position)))
    (if killed
	(progn
	  (cond (sib (klabel-type:update-labels label))
		((kotl-mode:buffer-empty-p)
		 ;; Always leave at least 1 visible cell within a view.
		 (kview:add-cell "1" 1)))
	  (kotl-mode:to-valid-position)))))

(defun kotl-mode:mail-tree (cell-ref invisible-flag)
  "Mail outline tree rooted at CELL-REF.  Use \"0\" for whole outline buffer.
Invisible text is expanded and included in the mail only if INVISIBLE-FLAG is
non-nil."
  (interactive
   (let ((label-default (kcell-view:label)))
     (hargs:iform-read
      '(interactive
	(list 
	 (hargs:read "Mail tree: (0 for whole outline) "
		     nil label-default nil 'kcell)
	 (y-or-n-p "Include invisible text? "))))))
  (if (equal cell-ref "0")
      (hmail:buffer nil invisible-flag)
    (let (start end)
      (save-excursion
	(kotl-mode:goto-cell cell-ref t)
	(beginning-of-line)
	(setq start (point))
	(or (= (kotl-mode:forward-cell 1) 0) (goto-char (point-max)))
	(forward-line -1)
	(setq end (point)))
      (hmail:region start end nil invisible-flag))))

(defun kotl-mode:promote-tree (arg)
  "Move current tree a maximum of prefix ARG levels higher in current view.
Each cell is refilled iff its `no-fill' attribute is nil and
kotl-mode:refill-flag is non-nil.  With prefix ARG = 0, cells are promoted up
to one level and kotl-mode:refill-flag is treated as true."
  (interactive "*p")
  (if (< arg 0)
      (kotl-mode:demote-tree (- arg))
    (let* ((parent) (result)
	   (label-sep-len (kview:label-separator-length kview))
	   (orig-point (point))
	   (orig-id (kcell-view:idstamp))
	   (fill-p (= arg 0))
	   (orig-pos-in-cell
	    (- (point) (kcell-view:start nil label-sep-len))))
      (if fill-p (setq arg 1))
      (unwind-protect
	  (progn
	    (backward-char 1)
	    (while (and (> arg 0)
			(setq result (kcell-view:parent
				      nil label-sep-len))
			(not (eq result 0)))
	      (setq parent result
		    arg (1- arg)))
	    (if parent
		(kotl-mode:move-after
		 (kcell-view:label orig-point)
		 (kcell-view:label) nil
		 nil fill-p)))
	;; Move to start of original cell
	(kotl-mode:goto-cell orig-id)
	;; Move to original pos within cell
	(forward-char orig-pos-in-cell)
	(kotl-mode:to-valid-position))
      (if (not parent)
	  (error "(kotl-mode:promote-tree): Cannot promote any further")))))

(defun kotl-mode:set-cell-attribute (attribute value &optional pos)
  "Include ATTRIBUTE VALUE with the current cell or the cell at optional POS.
Replaces any existing value that ATTRIBUTE has.
When called interactively, it displays the setting in the minibuffer as
confirmation."
  (interactive
   (let* ((plist (copy-sequence (kcell-view:plist)))
	  (existing-attributes plist)
	  attribute value)
     (barf-if-buffer-read-only)
     ;; Remove attribute values
     (while plist
       (setcdr plist (cdr (cdr plist)))
       (setq plist (cdr plist)))
     ;; Remove read-only attributes
     (setq existing-attributes (set:create existing-attributes)
	   existing-attributes (set:difference
				existing-attributes
				kcell:read-only-attributes))

     (while (zerop (length (setq attribute
				 (completing-read
				  "Current cell attribute to set: "
				  (mapcar 'list
					  (mapcar 'symbol-name
						  existing-attributes))))))
       (beep))
     (setq attribute (intern attribute)
	   value (kcell-view:get-attr attribute))
     (if value
	 (setq value (read-expression
		      (format "Change value of \"%s\" to: " attribute)
		      (prin1-to-string value)))
       (setq value (read-expression
		    (format "Set value of \"%s\" to: " attribute))))
     (list attribute value nil)))
  (kcell-view:set-attr attribute value pos)
  ;; Note that buffer needs to be saved to store new attribute value.
  (set-buffer-modified-p t)
  (if (interactive-p)
      (message "Attribute \"%s\" set to `%s' in cell <%s>."
	       attribute value (kcell-view:label pos))))

(defun kotl-mode:split-cell (&optional arg)
  "Split the current cell into two cells and move to the new cell.
The cell contents after point become part of the newly created cell.
The default is to create the new cell as a sibling of the current cell.
With optional universal ARG, {C-u}, the new cell is added as the child of
the current cell."
  (interactive "*P")
  (let ((new-cell-contents (kotl-mode:kill-region
			    (point) (kcell-view:end-contents) 'string))
	(start (kcell-view:start)))
    ;; delete any preceding whitespace
    (skip-chars-backward " \t\n\r" start)
    (delete-region (max start (point)) (kcell-view:end-contents))
    (kotl-mode:add-cell arg new-cell-contents (kcell-view:plist))))

(defun kotl-mode:transpose-cells (arg)
  "Exchange current and previous visible cells, leaving point after both.
If no previous cell, exchange current with next cell.
With prefix ARG, take current tree and move it past ARG visible cells.
With prefix ARG = 0, interchange the cell that contains point with the cell
that contains mark."
  (interactive "*p")
  (let ((label-sep-len (kview:label-separator-length kview)))
    (cond
     ((save-excursion (not (or (kcell-view:next t label-sep-len)
			       (kcell-view:previous t label-sep-len))))
      (error "(kotl-mode:transpose-cells): Only one visible cell in outline"))
     ;;
     ;; Transpose current and previous cells or current and next cells, if no
     ;; previous cell.  Leave point after both exchanged cells or within last
     ;; visible cell.
     ((= arg 1)
      (let ((label-1 (kcell-view:label))
	    (prev (kcell-view:previous t label-sep-len))
	    label-2)
	(or prev (kcell-view:next t label-sep-len))
	(setq label-2 (kcell-view:label))
	(kotl-mode:exchange-cells label-1 label-2)
	(kcell-view:next t label-sep-len)
	(if prev (kcell-view:next t label-sep-len))))
     ;;
     ;; Transpose point and mark cells, moving point to the new location of the
     ;; cell which originally contained point.
     ((= arg 0)
      (let ((label-1 (kcell-view:label))
	    label-2)
	;; This is like exchange-point-and-mark, but doesn't activate the
	;; mark.
	(goto-char (prog1 (hypb:mark t)
		     (set-marker (hypb:mark-marker t) (point))))
	(setq label-2 (kcell-view:label))
	(kotl-mode:exchange-cells label-1 label-2)))
     ;;
     ;; Move current tree past ARG next visible cells and leave point after
     ;; original cell text.
     (t
      (let ((mark (set-marker (make-marker)
			      (save-excursion (kotl-mode:next-line arg)))))
	(kotl-mode:move-after
	 (kcell-view:label)
	 (progn (while (and (> arg 0) (kcell-view:next t label-sep-len))
		  (setq arg (1- arg)))
		(kcell-view:label))
	 nil)
	(goto-char mark)
	(set-marker mark nil))))))

;;;
;;; Structure Viewing
;;;

(defun kotl-mode:collapse-tree (&optional all-flag)
  "Collapse to one line each visible cell of tree rooted at point.
With optional ALL-FLAG non-nil, collapse all cells visible within the current
view."
  (interactive "P")
  (kotl-mode:is-p)
  (let (buffer-read-only)
    (if all-flag
	(progn (kvspec:show-lines-per-cell 1)
	       (kvspec:update t))
      (kview:map-tree
       (function
	(lambda (kview)
	  ;; Use free variable label-sep-len bound in kview:map-tree for speed.
	  (goto-char (kcell-view:start nil label-sep-len))
	  (subst-char-in-region (point) (kcell-view:end-contents) ?\n ?\r t)))
       kview nil t))))

(defun kotl-mode:expand-tree (&optional all-flag)
  "Expand each visible cell of tree rooted at point.
With optional ALL-FLAG non-nil, expand all cells visible within the current
view."
  (interactive "P")
  (kotl-mode:is-p)
  (let (buffer-read-only)
    (if all-flag
	(progn (kvspec:show-lines-per-cell 0)
	       (kvspec:update t))
      (kview:map-tree
       (function
	(lambda (kview)
	  ;; Use free variable label-sep-len bound in kview:map-tree for speed.
	  (goto-char (kcell-view:start nil label-sep-len))
	  (subst-char-in-region (point) (kcell-view:end-contents) ?\r ?\n t)))
       kview nil t))))

(defun kotl-mode:toggle-tree-expansion (&optional all-flag)
  "Collapse or expand each cell of tree rooted at point or all visible cells if optional prefix arg ALL-FLAG is given.
If current cell is collapsed, cells will be expanded, otherwise they will be
collapsed."
  (interactive "P")
  (if (kcell-view:collapsed-p)
       ;; expand cells
      (kotl-mode:expand-tree all-flag)
    (kotl-mode:collapse-tree all-flag)))

;;; 
(defun kotl-mode:overview ()
  "Show the first line of each cell."
  (interactive)
  (kotl-mode:show-all)
  (kotl-mode:collapse-tree t))

(defun kotl-mode:show-all ()
  "Show (expand) all cells in current view."
  (interactive)
  (if (kotl-mode:is-p)
      (progn (kview:set-attr kview 'levels-to-show 0)
	     (kview:set-attr kview 'lines-to-show 0)
	     (show-all)
	     (kvspec:update t))))

(defun kotl-mode:top-cells ()
  "Collapse all level 1 cells in view and hide any deeper sublevels."
  (interactive)
  (kotl-mode:is-p)
  (let ((modified-p (buffer-modified-p))
	(buffer-read-only))
    (kvspec:levels-to-show 1)
    (kvspec:show-lines-per-cell 1)
    (kvspec:update t)
    ;; Restore buffer modification status
    (set-buffer-modified-p modified-p)))

;;; 
(defun kotl-mode:hide-sublevels (levels-to-keep)
  "Hide all cells in outline at levels deeper than LEVELS-TO-KEEP (a number).
Shows any hidden cells within LEVELS-TO-KEEP.  1 is the first level.  0 means
display all levels of cells."
  (interactive "P")
  (kvspec:levels-to-show levels-to-keep)
  ;; The prior call might have shown more lines per cell than the current
  ;; viewspec supports, so reset lines per cell.
  (kvspec:lines-to-show)
  (kvspec:update t))

(defun kotl-mode:hide-subtree (&optional cell-ref show-flag)
  "Hide subtree, ignoring root, at optional CELL-REF (defaults to cell at point)."
  (interactive)
  (kotl-mode:is-p)
  (save-excursion
    (if cell-ref
	(kotl-mode:goto-cell cell-ref t)
      (kotl-mode:beginning-of-cell))
    (let ((start (kcell-view:end-contents))
	  (end (kotl-mode:tree-end t))
	  (buffer-read-only))
      (if show-flag
	  (subst-char-in-region start end ?\r ?\n t)
	(subst-char-in-region start end ?\n ?\r t)))))

(defun kotl-mode:show-subtree (&optional cell-ref)
  "Show subtree, ignoring root, at optional CELL-REF (defaults to cell at point)."
  (interactive)
  (kotl-mode:hide-subtree cell-ref t))

(defun kotl-mode:hide-tree (&optional cell-ref show-flag)
  "Collapse tree rooted at optional CELL-REF (defaults to cell at point)."
  (interactive)
  (kotl-mode:is-p)
  (save-excursion
    (let ((start (if cell-ref
		     (kotl-mode:goto-cell cell-ref t)
		   (kotl-mode:beginning-of-cell)))
	  (end (kotl-mode:tree-end t))
	  (buffer-read-only))
      (if show-flag
	  (subst-char-in-region start end ?\r ?\n t)
	(subst-char-in-region start end ?\n ?\r t)))))

(defun kotl-mode:show-tree (&optional cell-ref)
  "Display fully expanded tree rooted at CELL-REF."
  (interactive)
  (kotl-mode:hide-tree cell-ref t))

;;; 
(defun kotl-mode:cell-attributes (all-flag)
  "Display a temporary buffer with the attributes of the current kcell.
With prefix arg ALL-FLAG non-nil, display the attributes of all visible
kcells in the current buffer.

See also the documentation for `kotl-mode:cell-help'."
  (interactive "P")
  (with-output-to-temp-buffer
      (hypb:help-buf-name "Kotl")
    (save-excursion
      (if (not all-flag)
	  (kotl-mode:print-attributes kview)
	(let ((label-sep-len (kview:label-separator-length kview)))
	  (kotl-mode:beginning-of-buffer)
	  (while (progn (kotl-mode:print-attributes kview)
			(kcell-view:next t label-sep-len))))))))

(defun kotl-mode:cell-help (&optional cell-ref cells-flag)
  "Display a temporary buffer with CELL-REF's attributes.
CELL-REF defaults to current cell.
Optional prefix arg CELLS-FLAG selects the cells to print:
  If = 1, print CELL-REF's cell only;
  If > 1, print CELL-REF's visible tree (the tree rooted at CELL-REF);
  If < 1, print all visible cells in current view (CELL-REF is not used).

See also the documentation for `kotl-mode:cell-attributes'."
  (interactive
   (let* ((label (kcell-view:label))
	  (hargs:defaults (list label label)))
     (append
      (let ((arg (prefix-numeric-value current-prefix-arg)))
	(if (< arg 1)
	    0
	  (hargs:iform-read
	   (list 'interactive
		 (format "+KDisplay properties of koutline %s: "
			 (if (= arg 1) "cell" "tree"))))))
      (list current-prefix-arg))))
  (or (integerp cells-flag)
      (setq cells-flag (prefix-numeric-value cells-flag)))
  (or (stringp cell-ref) (setq cell-ref (kcell-view:label)))
  (with-output-to-temp-buffer
      (hypb:help-buf-name "Koutline")
    (save-excursion
      (if (equal cell-ref "0")
	  (progn
	    (hattr:report (kcell:plist (kview:top-cell kview)))
	    (terpri)
	    (cond ((= cells-flag 1) nil)
		  ((> cells-flag 1)
		   (kview:map-tree 'kotl-mode:print-attributes kview t t))
		  ;; (< cells-flag 1)
		  (t (kotl-mode:cell-attributes t))))
	(cond ((= cells-flag 1)
	       (kotl-mode:goto-cell cell-ref)
	       (kotl-mode:print-attributes kview))
	      ((> cells-flag 1)
	       (kotl-mode:goto-cell cell-ref)
	       (kview:map-tree 'kotl-mode:print-attributes kview nil t))
	      ;; (< cells-flag 1)
	      (t (kotl-mode:cell-attributes t)))))))

(defun kotl-mode:get-cell-attribute (attribute &optional pos)
  "Return ATTRIBUTE's value for the current cell or the cell at optional POS.
When called interactively, it displays the value in the minibuffer."
  (interactive "SCurrent cell attribute to get: ")
  (let ((value (kcell-view:get-attr attribute pos)))
    (if (interactive-p)
	(message "Attribute \"%s\" = `%s' in cell <%s>."
		 attribute value (kcell-view:label pos)))
    value))

;;;
;;; Private functions
;;;

(defun kotl-mode:add-indent-to-region (&optional indent start end)
  "Add current cell's indent to current region.
Optionally, INDENT and region START and END may be given."
  (or (integerp indent) (setq indent (kcell-view:indent)))
  (save-excursion
    (save-restriction
      (narrow-to-region (or start (point)) (or end (hypb:mark t)))
      (goto-char (point-min))
      (replace-regexp "\n" (concat "\n" (make-string indent ?\ ))))))

(defun kotl-mode:delete-line (&optional pos)
  "Delete and return contents of cell line at point or optional POS as a string.
Does not delete newline at end of line."
  (save-excursion
    (if pos (goto-char pos))
    (if (kview:valid-position-p)
	(let ((bol (kotl-mode:start-of-line))
	      (eol (kotl-mode:finish-of-line)))
	  (prog1
	      (buffer-substring bol eol)
	    (delete-region bol eol)))
      (error "(kotl-mode:delete-line): Invalid position, '%d'" (point)))))

(defun kotl-mode:indent-line (arg)
  ;; Disallow the indent-line command.
  (error "(kotl-mode:indent-line): Insert spaces to indent each line."))

(defun kotl-mode:indent-region (start end)
  ;; User might try to indent across cells.  This would be bad, so disallow
  ;; the indent-region command.
  (error "(kotl-mode:indent-region): Insert spaces to indent each line."))

(defun kotl-mode:is-p ()
  "Signal an error if current buffer is not a Hyperbole outline, else return t."
  (if (kview:is-p kview)
      t
    (hypb:error
     "(kotl-mode:is-p): Command requires a current Hyperbole outline.")))

(defun kotl-mode:tree-end (&optional omit-end-newlines)
  "Return end point of current cell's tree within this view.
If optional OMIT-END-NEWLINES is non-nil, point returned precedes any
newlines at end of tree."
  (let* ((label-sep-len (kview:label-separator-length kview))
	 (start-indent (kcell-view:indent nil label-sep-len))
	 (next))
    (save-excursion
      (while (and (setq next (kcell-view:next nil label-sep-len))
		  (< start-indent (kcell-view:indent nil label-sep-len))))
      (cond (next
	     (goto-char (progn (kcell-view:previous nil label-sep-len)
			       (kcell-view:end))))
	    ;; Avoid skipping too far at end of file.
	    ((re-search-forward "[\n\r][\n\r]" nil t))
	    (t (goto-char (point-max))))
      (if omit-end-newlines (skip-chars-backward "\n\r"))
      (point))))

(defun kotl-mode:tree-start ()
  "Return beginning of line position preceding current cell's start point."
  (save-excursion (goto-char (kcell-view:start)) (beginning-of-line)
		  (point)))

(defun kotl-mode:line-move (arg)
  "Move point ARG visible lines forward within an outline."
  (if (not (integerp selective-display))
      (forward-line arg)
    ;; Move by arg lines, but ignore invisible ones.
    (while (> arg 0)
      (vertical-motion 1)
      (forward-char -1)
      (forward-line 1)
      (setq arg (1- arg)))
    (while (< arg 0)
      (vertical-motion -1)
      (beginning-of-line)
      (setq arg (1+ arg))))
  (move-to-column (or goal-column temporary-goal-column))
  nil)

(defun kotl-mode:print-attributes (kview)
  "Print to the `standard-output' stream the attributes of the current visible kcell. 
Takes argument KVIEW (so it can be used with 'kview:map-tree' and so that
KVIEW is bound correctly) but always operates upon the current view."
  ;; Move to start of visible cell to avoid printing attributes for an
  ;; invisible kcell which point may be over.
  ;; Print first line of cell for reference.
  (save-excursion
    (princ
     (buffer-substring (progn (beginning-of-line) (point))
		       (progn (kview:end-of-actual-line)
			      (point)))))
  (terpri)
  (hattr:report (kcell:plist (kcell-view:cell)))
  (terpri))

(defun kotl-mode:set-temp-goal-column ()
  (if (not (or (eq last-command 'next-line)
	       (eq last-command 'previous-line)))
      (setq temporary-goal-column
	    (if (and track-eol (eolp)
		     ;; Don't count beg of empty line as end of line
		     ;; unless we just did explicit end-of-line.
		     (or (not (bolp)) (eq last-command 'end-of-line)))
		9999
	      (current-column)))))

(defun kotl-mode:to-valid-position (&optional backward-p)
  "Move point to the nearest non-read-only position within current koutline view.
With optional BACKWARD-P, move backward if possible to get to valid position."
  (if (kview:valid-position-p)
      nil
    (let ((label-sep-len (kview:label-separator-length kview)))
      (cond ((kotl-mode:bobp)
	     (goto-char (kcell-view:start nil label-sep-len)))
	    ((kotl-mode:eobp)
	     (skip-chars-backward "\n\r"))
	    (t (let ((indent (kcell-view:indent nil label-sep-len)))
		 (if (bolp)
		     (if backward-p
			 (skip-chars-backward "\n\r")
		       (skip-chars-forward "\n\r")))
		 (setq indent (kcell-view:indent nil label-sep-len))
		 (if (< (current-column) indent)
		     (move-to-column indent))))))))

(defun kotl-mode:transpose-lines-internal (start end)
  "Transpose lines at START and END markers within an outline.
Leave point at end of line now residing at START."
  (if (and start end
	   (kview:valid-position-p start)
	   (kview:valid-position-p end))
      (let* ((pline (kotl-mode:delete-line start))
	     mline)
	(goto-char end)
	(setq mline (kotl-mode:delete-line))
	(insert pline)
	(goto-char start)
	(insert mline))
    ;; Set non-point and non-mark markers to point nowhere before signalling
    ;; an error.
    (or (eq start (point-marker))
	(eq start (hypb:mark-marker t))
	(set-marker start nil))
    (or (eq end (point-marker))
	(eq end (hypb:mark-marker t))
	(set-marker start nil))
    (error "(kotl-mode:transpose-lines): Point or mark is at an invalid position")))

(defun kotl-mode:update-buffer ()
  "Update current view buffer in preparation for saving."
  (if (kview:is-p kview)
      (let ((mod-p (buffer-modified-p))
	    (start (window-start)))
	(save-excursion
	  (kfile:update)
	  (set-buffer-modified-p mod-p))
	(set-window-start nil (max (point-min) start) t)
	nil)))

;;; ----

(defvar kotl-mode-map nil
  "Keymap containing koutliner editing and viewing commands.")
(if kotl-mode-map
    nil
  (setq kotl-mode-map
	(if (string-match "XEmacs\\|Lucid" emacs-version)
	    (make-keymap)
	  (copy-keymap text-mode-map)))
  ;; Overload edit keys to deal with structure and labels.
  (let (local-cmd)
    (mapcar 
     (if (string-match "XEmacs\\|Lucid" emacs-version)
	 ;; XEmacs
	 (function
	  (lambda (cmd)
	    (setq local-cmd (intern-soft
			     (concat "kotl-mode:" (symbol-name cmd))))
	    ;; Only bind key locally if kotl-mode local-cmd has already
	    ;; been defined and cmd is a valid function.
	    (if (and local-cmd (fboundp cmd))
		(progn
		  ;; Make local-cmd have the same property list as cmd,
		  ;; e.g. so pending-delete property is the same.
		  (setplist local-cmd (symbol-plist cmd)) 
		  (mapcar
		   (function
		    (lambda (key) (define-key kotl-mode-map key local-cmd)))
		   (where-is-internal cmd))))))
       ;; GNU Emacs 19
       (function
	(lambda (cmd)
	  (setq local-cmd (intern-soft
			   (concat "kotl-mode:" (symbol-name cmd))))
	  ;; Only bind key locally if kotl-mode local-cmd has already
	  ;; been defined and cmd is a valid function.
	  (if (and local-cmd (fboundp cmd))
	      (progn
		;; Make local-cmd have the same property list as cmd,
		;; e.g. so pending-delete property is the same.
		(setplist local-cmd (symbol-plist cmd)) 
		(substitute-key-definition
		 cmd local-cmd kotl-mode-map global-map))))))
     '(
       back-to-indentation
       backward-char
       backward-delete-char
       backward-delete-char-untabify
       backward-kill-word
       backward-para
       backward-paragraph
       backward-sentence
       backward-word
       beginning-of-buffer
       beginning-of-line
       copy-region-as-kill
       copy-to-register
       delete-blank-lines
       delete-backward-char
       delete-char
       delete-horizontal-space
       delete-indentation
       end-of-buffer
       end-of-line
       fill-paragraph
       fill-paragraph-or-region
       ;; cursor keys
       fkey-backward-char
       fkey-forward-char
       fkey-next-line
       fkey-previous-line
       ;;
       forward-char
       forward-word
       forward-para
       forward-paragraph
       forward-sentence
       insert-buffer
       insert-file
       insert-register
       just-one-space
       kill-word
       kill-line
       kill-region
       kill-ring-save
       kill-sentence
       mark-paragraph
       mark-whole-buffer
       newline
       newline-and-indent
       next-line
       open-line
       previous-line
       scroll-down
       scroll-up
       set-fill-prefix
       transpose-chars
       transpose-lines
       transpose-paragraphs
       transpose-sentences
       transpose-words
       yank
       yank-pop
       zap-to-char
       )))


  ;; kotl-mode keys
  (define-key kotl-mode-map "\C-c@"     'kotl-mode:mail-tree)
  (define-key kotl-mode-map "\C-c+"     'kotl-mode:append-cell)
  (define-key kotl-mode-map "\C-c,"     'kotl-mode:beginning-of-cell)
  (define-key kotl-mode-map "\C-c."     'kotl-mode:end-of-cell)
  (define-key kotl-mode-map "\C-c<"     'kotl-mode:first-sibling)
  (define-key kotl-mode-map "\C-c>"     'kotl-mode:last-sibling)
  (define-key kotl-mode-map "\C-c^"     'kotl-mode:beginning-of-tree)
  (define-key kotl-mode-map "\C-c$"     'kotl-mode:end-of-tree)
  (define-key kotl-mode-map "\C-ca"     'kotl-mode:add-child)
  (define-key kotl-mode-map "\C-c\C-a"  'kotl-mode:show-all)
  (define-key kotl-mode-map "\C-cb"     'kvspec:toggle-blank-lines)
  (define-key kotl-mode-map "\C-c\C-b"  'kotl-mode:backward-cell)
  (define-key kotl-mode-map "\C-cc"     'kotl-mode:copy-after)
  (define-key kotl-mode-map "\C-c\C-c"  'kotl-mode:copy-before)
  (define-key kotl-mode-map "\C-c\M-c"  'kotl-mode:copy-to-buffer)
  (define-key kotl-mode-map "\C-cd"     'kotl-mode:down-level)
  (define-key kotl-mode-map "\C-c\C-d"  'kotl-mode:down-level)
  (define-key kotl-mode-map "\C-ce"     'kotl-mode:exchange-cells)
  (define-key kotl-mode-map "\C-c\C-f"  'kotl-mode:forward-cell)
  (define-key kotl-mode-map "\C-cg"     'kotl-mode:goto-cell)
  (define-key kotl-mode-map "\C-ch"     'kotl-mode:cell-help)
  (define-key kotl-mode-map "\C-c\C-h"  'kotl-mode:hide-tree)
  (define-key kotl-mode-map "\M-\C-h"   'kotl-mode:hide-subtree)
  ;; Override this global binding for set-selective-display with a similar
  ;; function appropriate for kotl-mode.
  (define-key kotl-mode-map "\C-x$"     'kotl-mode:hide-sublevels)
  (define-key kotl-mode-map "\C-i"      'kotl-mode:demote-tree)
  (define-key kotl-mode-map "\M-\C-i"   'kotl-mode:promote-tree)
  (define-key kotl-mode-map "\C-j"      'kotl-mode:add-cell)
  (define-key kotl-mode-map "\M-j"      'kotl-mode:fill-paragraph)
  (define-key kotl-mode-map "\C-c\M-j"  'kotl-mode:fill-cell)
  (define-key kotl-mode-map "\M-\C-j"   'kotl-mode:fill-tree)
  (define-key kotl-mode-map "\C-c\C-k"  'kotl-mode:kill-tree)
  (define-key kotl-mode-map "\C-ck"     'kotl-mode:kill-contents)
  (define-key kotl-mode-map "\C-c\C-i"  'kotl-mode:set-cell-attribute)
  (define-key kotl-mode-map "\C-cl"     'klink:create)
  (define-key kotl-mode-map "\C-c\C-l"  'kview:set-label-type)
  (define-key kotl-mode-map "\C-c\M-l"  'kview:set-label-separator)
  (define-key kotl-mode-map "\C-m"      'kotl-mode:newline)
  (define-key kotl-mode-map "\C-cm"     'kotl-mode:move-after)
  (define-key kotl-mode-map "\C-c\C-m"  'kotl-mode:move-before)
  (define-key kotl-mode-map "\C-c\C-n"  'kotl-mode:next-cell)
  (define-key kotl-mode-map "\C-c\C-o"  'kotl-mode:overview)
  (define-key kotl-mode-map "\C-c\C-p"  'kotl-mode:previous-cell)
  (define-key kotl-mode-map "\C-cp"     'kotl-mode:add-parent)
  (if (memq (global-key-binding "\M-q") '(fill-paragraph
					  fill-paragraph-or-region))
      (progn
	(define-key kotl-mode-map "\C-c\M-q" 'kotl-mode:fill-cell)
	(define-key kotl-mode-map "\M-\C-q"  'kotl-mode:fill-tree)))
  (define-key kotl-mode-map "\C-cs"     'kotl-mode:split-cell)
  (define-key kotl-mode-map "\C-c\C-s"  'kotl-mode:show-tree)
  (define-key kotl-mode-map "\C-c\C-\\" 'kotl-mode:show-tree)
  (define-key kotl-mode-map "\M-s"      'kotl-mode:center-line)
  (define-key kotl-mode-map "\M-S"      'kotl-mode:center-paragraph)
  (define-key kotl-mode-map "\C-ct"     'kotl-mode:transpose-cells)
  (define-key kotl-mode-map "\C-c\C-t"  'kotl-mode:top-cells)
  (define-key kotl-mode-map "\C-cu"     'kotl-mode:up-level)
  (define-key kotl-mode-map "\C-c\C-u"  'kotl-mode:up-level)
  (define-key kotl-mode-map "\C-c\C-v"  'kvspec:activate)
  (define-key kotl-mode-map "\C-x\C-w"  'kfile:write))

(easy-menu-define kotl-mode-menu kotl-mode-map "Kotl Mode Menu." kmenu:menu)

(provide 'kotl-mode)

;;; kotl-mode.el ends here
