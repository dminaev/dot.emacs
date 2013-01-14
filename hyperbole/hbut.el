;;; hbut.el --- Hyperbole button constructs.

;; Copyright (C) 1991-1995 Free Software Foundation, Inc.
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

(require 'hmoccur)
(require 'hbmap)
(require 'htz)
(require 'hbdata)
(require 'hact)

;;;
;;; Public definitions
;;;

;;;
;;; ebut class - Explicit Hyperbole buttons
;;;

(defvar   ebut:hattr-save t
  "*Non-nil value saves button data when button source is saved.
Nil disables saving.")

(defconst ebut:max-len 100
  "Maximum length of a hyper-button label.")


(defun    ebut:alist (&optional file)
  "Returns alist with each element a list containing a button label.
For use as a completion table.  Gets labels from optional FILE or current
buffer."
  (mapcar 'list (ebut:list file)))

(defun    ebut:at-p (&optional start-delim end-delim)
  "Returns explicit Hyperbole button at point or nil.
Assumes point is within first line of button label, if at all.
Optional START-DELIM and END-DELIM are strings that override default
button delimiters."
  (let ((key (ebut:label-p nil start-delim end-delim)))
    (and key (ebut:get key))))

(defun    ebut:create (&optional but-sym)
  "Creates Hyperbole explicit button based on optional BUT-SYM.
Default is 'hbut:current'.
Button should hold the following attributes (see 'hattr:set'): 
   lbl-key (normalized button label string),
   loc     (filename or buffer where button is located),
   dir     (directory name where button is located),
   actype  (action type that provides a default action for the button),
   action  (optional action that overrides the default),
   args    (list of arguments for action, if action takes a single
            argument of the button lbl-key, args may be nil).

If successful returns any instance number to append to button label
except when instance number would be 1, then returns t.  On failure,
returns nil.

If successful, leaves point in button data buffer, so caller should use
'save-excursion'.  Does not save button data buffer."
  (let ((lbl-instance (hbdata:write nil but-sym)))
    (run-hooks 'ebut:create-hook)
    lbl-instance))

(defun    ebut:delete (&optional but-sym)
  "Deletes Hyperbole explicit button based on optional BUT-SYM.
Default is 'hbut:current'.
Returns entry deleted (a list of attribute values) or nil."
  (if (null but-sym) (setq but-sym 'hbut:current))
  (if (ebut:is-p but-sym)
      (let* ((but-key (hattr:get but-sym 'lbl-key)) 
	     (loc     (hattr:get but-sym 'loc))
	     (entry   (hbdata:delete-entry but-key loc)))
	(run-hooks 'ebut:delete-hook)
	entry)))

(defun    ebut:get (&optional lbl-key buffer key-src)
  "Returns explicit Hyperbole button symbol given by LBL-KEY and BUFFER.
KEY-SRC is given when retrieving global buttons and is full source pathname.
Retrieves button data, converts into a button object and returns a symbol
which references the button.

All arguments are optional.  When none are given, returns symbol for
button that point is within or nil.  BUFFER defaults to the current
buffer."
  (hattr:clear 'hbut:current)
  (save-excursion
    (let ((key-file) (key-dir) (but-data) (actype))
      (or lbl-key (setq lbl-key (ebut:label-p)))
      (if buffer
	  (if (bufferp buffer) (set-buffer buffer)
	    (error "(ebut:get): Invalid buffer argument: %s" buffer)))
      (if key-src
	  nil
	(if (equal lbl-key (ebut:label-p))
	    nil
	  (goto-char (point-min))
	  (ebut:next-occurrence lbl-key))
	(if (setq key-src (ebut:key-src 'full))
	    ;; 'ebut:key-src' sets current buffer to key-src buffer.
	    (setq buffer (current-buffer)))
	)
      (if (and (stringp lbl-key) key-src)
	  (progn
	    (if (stringp key-src)
		(setq key-dir (file-name-directory key-src)
		      key-file (file-name-nondirectory key-src)))
	    (setq but-data (and key-src
				(hbdata:get-entry lbl-key (or key-file key-src)
						  key-dir)))
	    (if (null but-data)
		nil
	      (hattr:set 'hbut:current 'lbl-key lbl-key)
	      (hattr:set 'hbut:current 'loc key-src)
	      (hattr:set 'hbut:current 'categ 'explicit)
	      (hattr:set 'hbut:current 'action nil)
	      (hattr:set 'hbut:current 'actype
			 (intern (setq actype (hbdata:actype but-data))))
	      ;; Hyperbole V1 referent compatibility
	      (if (= (length actype) 2)
		  (hattr:set 'hbut:current 'referent
			     (hbdata:referent but-data)))
	      (hattr:set 'hbut:current 'args (hbdata:args but-data))
	      (hattr:set 'hbut:current 'creator (hbdata:creator but-data))
	      (hattr:set 'hbut:current
			 'create-time (hbdata:create-time but-data))
	      (hattr:set 'hbut:current
			 'modifier (hbdata:modifier but-data))
	      (hattr:set 'hbut:current
			 'mod-time (hbdata:mod-time but-data))
	      'hbut:current)
	    )))))

(defun    ebut:is-p (object)
  "Returns non-nil if OBJECT denotes an explicit Hyperbole button."
  (and (symbolp object)
       (eq (hattr:get object 'categ) 'explicit)))

(defun    ebut:key-of-label-p (key label)
  "Returns t iff KEY matches to LABEL in a case insensitive manner."
  (and (stringp key) (stringp label)
       (equal key (downcase (ebut:label-to-key label)))))

(defun    ebut:key-src (&optional full)
  "Return key source (usually unqualified) for current Hyperbole button.
Also sets current buffer to key source.
With optional FULL when source is a pathname, the full pathname is returned."
  (let ((src (cond ((hmail:mode-is-p) (current-buffer))
		   ((ebut:key-src-fmt))
		   ((save-excursion
		      (save-restriction
			(widen)
			(if (and (search-backward hbut:source-prefix nil t)
				 (or (memq (preceding-char) '(?\n ?\^M))
				     (= (point) (point-min))))
			    (hbut:source full)))))
		   (buffer-file-name
		    (if full buffer-file-name
		      (file-name-nondirectory buffer-file-name)))
		   (t (current-buffer))
		   )))
    (cond ((null src) nil)
	  ((bufferp src)
	   (set-buffer src)
	   src)
	  ((file-readable-p src)
	   (set-buffer (find-file-noselect src))
	   src)
	  ((file-readable-p (setq src (hpath:symlink-referent src)))
	   (set-buffer (find-file-noselect src))
	   src))))

(defun    ebut:key-src-fmt ()
  "Returns unformatted filename associated with formatted current buffer.
This is used to obtain the source of explicit buttons for buffers that
represent the output of particular document formatters."
  (cond ((or (eq major-mode 'Info-mode)
	     (string-match "\\.info\\(-[0-9]+\\)?$" (buffer-name)))
	 (let ((src (and buffer-file-name
			 (substring
			  buffer-file-name
			  0 (string-match "\\.[^.]+$" buffer-file-name)))))
	   (cond ((file-exists-p (concat src ".texi"))
		  (concat src ".texi"))
		 ((file-exists-p (concat src ".texinfo"))
		  (concat src ".texinfo"))
		 ((current-buffer)))))
	))

(defun    ebut:key-to-label (lbl-key)
  "Unnormalizes LBL-KEY and returns a label string approximating actual label."
  (if lbl-key
      (let* ((pos 0) (len (length lbl-key)) (lbl) c)
	(while (< pos len)
	  (setq c (aref lbl-key pos)
		lbl (concat lbl 
			    (if (= c ?_)
				(if (or (= (1+ pos) len)
					(/= (aref lbl-key (1+ pos)) ?_))
				    " "
				  (setq pos (1+ pos))
				  "_")
			      (char-to-string c)))
		pos (1+ pos)))
	lbl)))

(defun    ebut:label-p (&optional as-label start-delim end-delim pos-flag)
  "Returns key for Hyperbole button label that point is within.
Returns nil if not within a label.
Assumes point is within first line of button label, if at all.
If optional AS-LABEL is non-nil, label is returned rather than the key
derived from the label.  Optional START-DELIM and END-DELIM are strings
that override default button delimiters.  With optional POS-FLAG non-nil,
returns list of label-or-key, but-start-position, but-end-position.
Positions include delimiters."
  (let ((opoint (point))
	(npoint (1+ (point)))
	(quoted "\\(^\\|[^\\{]\\)")
	(start)
	lbl-key end but-start but-end)
    (or start-delim (setq start-delim ebut:start))
    (or end-delim (setq end-delim ebut:end))
    (save-excursion
      (beginning-of-line)
      (while (and (progn
		    (while (re-search-forward
			    (concat quoted (regexp-quote start-delim))
			    npoint t)
		      (setq start t))
		    start)
		  (re-search-forward (concat "[^\\{]" (regexp-quote end-delim))
				     npoint t))
	(setq start nil))
      (if start
	  (progn
	    (setq start (point)
		  but-start (match-end 1))
	    (if (= ?\( (char-syntax (preceding-char)))
		(condition-case ()
		    (progn
		      (forward-char -1)
		      (forward-list)
		      (forward-char -2))
		  (error (goto-char (1- opoint))))
	      (goto-char (1- opoint)))
	    (and (< (point) (+ start ebut:max-len))
		 (re-search-forward (concat quoted (regexp-quote end-delim))
				    (+ start ebut:max-len) t)
		 (setq but-end (point)
		       end (- (point) (length end-delim))
		       lbl-key (ebut:label-to-key (buffer-substring start end)))
		 (cond (pos-flag
			(if as-label
			    (list (ebut:key-to-label lbl-key) but-start but-end)
			  (list lbl-key but-start but-end)))
		       (t (if as-label (ebut:key-to-label lbl-key) lbl-key)))))))))

(defun    ebut:label-regexp (lbl-key &optional no-delim)
  "Unnormalizes LBL-KEY.  Returns regular expr matching delimited but label.
Optional NO-DELIM leaves off delimiters and leading and trailing space."
  (if lbl-key
      (let* ((pos 0)
	     (len (length lbl-key))
	     (c)
	     (sep0 "[ \t\n\^M]*")
	     (sep "[ \t\n\^M]+")
	     (regexp (if no-delim "" (concat (regexp-quote ebut:start) sep0)))
	     (case-fold-search))
	(while (< pos len)
	  (setq c (aref lbl-key pos)
		regexp (concat regexp 
			       (if (= c ?_)
				   (if (or (= (1+ pos) len)
					   (/= (aref lbl-key (1+ pos)) ?_))
				       sep
				     (setq pos (1+ pos))
				     "_")
				 (regexp-quote (char-to-string c))))
		pos (1+ pos)))
	(if no-delim regexp 
	  (setq regexp (concat regexp sep0 (regexp-quote ebut:end)))))))

(defun    ebut:label-to-key (label)
  "Normalizes LABEL for use as a Hyperbole button key and returns key.
Eliminates any fill prefix in the middle of the label, replaces '_' with
'__', removes leading and trailing whitespace and replaces each other
whitespace sequence with '_'."
  (if (null label)
      nil
    (setq label (hbut:fill-prefix-remove label)
	  ;; Remove leading and trailing space.
	  label (hypb:replace-match-string "\\`[ \t\n\^M]+\\|[ \t\n\^M]+\\'"
					   label "" t)
	  label (hypb:replace-match-string "_" label "__" t))
    (hypb:replace-match-string "[ \t\n\^M]+" label "_" t)))

(defun    ebut:list (&optional file loc-p)
  "Returns list of button labels from given FILE or current buffer.
Removes duplicate labels if optional LOC-P is omitted.  With LOC-P, returns
list of elements (label start end) where start and end are the buffer
positions at which the starting button delimiter begins and ends."
  (interactive)
  (setq file (if file (and (file-exists-p file) (find-file-noselect file))
	       (current-buffer)))
  (if file
      (progn
	(set-buffer file)
	(let ((buts (ebut:map (if loc-p
				  (function
				   (lambda (lbl start end)
				     ;; Normalize label spacing
				     (list (ebut:key-to-label
					    (ebut:label-to-key lbl))
					   start end)))
				(function
				 (lambda (lbl start end)
				   ;; Normalize label spacing
				   (ebut:key-to-label
				    (ebut:label-to-key lbl))))))))
	  (if loc-p buts (nreverse (set:create buts)))))))

(fset    'map-ebut 'ebut:map)
(defun    ebut:map (but-func &optional start-delim end-delim
			     regexp-match include-delims)
  "Applies BUT-FUNC to buttons delimited by optional START-DELIM and END-DELIM.
If REGEXP-MATCH is non-nil, only buttons which match this argument are
considered.
Maps over portion of buffer visible under any current restriction.
BUT-FUNC must take precisely three arguments: the button label, the
start position of the delimited button label and its end position (positions
include delimiters when INCLUDE-DELIMS is non-nil).
If END-DELIM is a symbol, e.g. t, then START-DELIM is taken as a regular
expression which matches an entire button string."
  (or start-delim (setq start-delim ebut:start))
  (or end-delim (setq end-delim ebut:end))
  (let* ((regexp (symbolp end-delim))
	 (end-sym (or regexp (substring end-delim -1)))
	 (rtn)
	 (quoted)
	 start end but lbl)
    (save-excursion
      (goto-char (point-min))
      (setq include-delims (if include-delims 0 1))
      (while (re-search-forward
	      (if regexp start-delim
		(concat (regexp-quote start-delim)
			"\\([^" end-sym "\"][^" end-sym "]*\\)"
			(regexp-quote end-delim)))
	      nil t)
	(setq start (match-beginning include-delims)
	      end (match-end include-delims)
	      but (buffer-substring (match-beginning 0) (match-end 0))
	      lbl (buffer-substring (match-beginning 1) (match-end 1)))
	(save-excursion
	  (goto-char start)
	  (if (or (= (preceding-char) ?\\) (= (preceding-char) ?\{))
	      ;; Ignore matches with quoted delimiters.
	      (setq quoted t)))
	(cond (quoted (setq quoted nil))
	      ((or (not regexp-match)
		   (string-match regexp-match but))
	       (setq rtn (cons (funcall but-func lbl start end) rtn))))))
    (nreverse rtn)))

(defun    ebut:modify (&optional lbl-key but-sym)
  "Modifies existing Hyperbole button from optional LBL-KEY and BUT-SYM.
Defaults are the key for any button label at point and 'hbut:current'.
If successful, returns button's instance number except when instance
number is 1, then returns t.  On failure, as when button does not exist,
returns nil.

If successful, leaves point in button data buffer, so caller should use
'save-excursion'.  Does not save button data buffer."
  (save-excursion
    (let ((lbl-instance (hbdata:write lbl-key but-sym)))
      (run-hooks 'ebut:modify-hook)
      lbl-instance)))

(defun    ebut:next-occurrence (lbl-key &optional buffer)
  "Moves point to next occurrence of button with LBL-KEY in optional BUFFER.
BUFFER defaults to current buffer.  It may be a buffer name.
Returns non-nil iff occurrence is found.

Remember to use (goto-char (point-min)) before calling this in order to
move to the first occurrence of the button."
  (if buffer
      (if (not (or (bufferp buffer)
		   (and (stringp buffer) (get-buffer buffer))))
	  (error "(ebut:next-occurrence): Invalid buffer arg: %s" buffer)
	(switch-to-buffer buffer)))
  (if (re-search-forward (ebut:label-regexp lbl-key) nil t)
      (goto-char (+ (match-beginning 0) (length ebut:start)))))

(defun    ebut:operate (curr-label new-label)
  "Operates on a new or existing Hyperbole button given by CURR-LABEL.
When NEW-LABEL is non-nil, this is substituted for CURR-LABEL and the
associated button is modified.  Otherwise, a new button is created.
Returns instance string appended to label to form unique label, nil if
label is already unique.  Signals an error when no such button is found
in the current buffer."
  (let* ((lbl-key (ebut:label-to-key curr-label))
	 (lbl-regexp (ebut:label-regexp lbl-key))
	 (modify new-label)
	 (instance-flag))
    (or new-label (setq new-label curr-label))
    (hattr:set 'hbut:current 'lbl-key (ebut:label-to-key new-label))
    (save-excursion
      (if (setq instance-flag
		(if modify (ebut:modify lbl-key) (ebut:create)))
	  (if (hmail:editor-p) (hmail:msg-narrow))))
    (if instance-flag
	(progn
	  ;; Rename all occurrences of button - those with same label.
	  (if modify
	      (let* ((but-key-and-pos (ebut:label-p nil nil nil 'pos))
		     (at-but (equal (car but-key-and-pos)
				    (ebut:label-to-key new-label))))
		(if at-but
		    (ebut:delimit (nth 1 but-key-and-pos)
				  (nth 2 but-key-and-pos)
				  instance-flag))
		(cond ((ebut:map
			(function
			 (lambda (lbl start end)
			   (delete-region start end)
			   (ebut:delimit
			    (point)
			    (progn (insert new-label) (point))
			    instance-flag)))
			nil nil lbl-regexp 'include-delims))
		      (at-but)
		      ((hypb:error "(ebut:operate): No button matching: %s" curr-label))))
	    ;; Add a new button.
	    (let (start end buf-lbl)
	      (cond ((and (marker-position (hypb:mark-marker t))
			  (setq start (region-beginning)
				end (region-end)
				buf-lbl (buffer-substring start end))
			  (equal buf-lbl curr-label))
		     nil)
		    ((looking-at (regexp-quote curr-label))
		     (setq start (point)
			   end (match-end 0)))
		    (t (setq start (point))
		       (insert curr-label)
		       (setq end (point))))
	      (ebut:delimit start end instance-flag))
	    )
	  ;; Position point
	  (let ((new-key (ebut:label-to-key new-label)))
	    (cond ((equal (ebut:label-p) new-key)
		   (forward-char 1) (search-backward ebut:start nil t)
		   (goto-char (match-end 0)))
		  ((let ((regexp (ebut:label-regexp new-key)))
		     (or (re-search-forward  regexp nil t)
			 (re-search-backward regexp nil t)))
		   (goto-char (+ (match-beginning 0) (length ebut:start))))))
	  ;; instance-flag might be 't which we don't want to return.
	  (if (stringp instance-flag) instance-flag))
      (hypb:error
       "(ebut:operate): Operation failed.  Check button attribute permissions: %s"
       hattr:filename))))

(defun    ebut:search (string out-buf &optional match-part)
  "Writes explicit button lines matching STRING to OUT-BUF.
Uses Hyperbole space into which user has written buttons for the search.
By default, only matches for whole button labels are found, optional MATCH-PART
enables partial matches."
  (let*  ((buffers (mapcar (function
			    (lambda (dir)
			      (expand-file-name hattr:filename dir)))
			   (hbmap:dir-list)))
	  (total 0)
	  (firstmatch))
    (save-excursion
      (set-buffer out-buf)
      (setq buffer-read-only nil)
      (widen)
      (erase-buffer)
      (let (currbuf currfile kill-buf src-matches dir)
	(while buffers
	  (setq currbuf (car buffers)
		currfile (if (stringp currbuf) currbuf)
		kill-buf (and currfile (not (get-file-buffer currfile)))
		buffers (cdr buffers))
	  (if currfile
	      (setq currbuf (and (file-readable-p currfile)
				 (find-file-noselect currfile))
		    dir (file-name-directory currfile))
	    (setq currfile (buffer-file-name currbuf)))
	  (and currfile currbuf
	       (unwind-protect
		   (setq src-matches
			 (hbdata:search currbuf string match-part))
		 (if kill-buf (kill-buffer currbuf))))
	  (if src-matches
	      (let (elt matches)
		(while src-matches
		  (setq elt (car src-matches))
		  (if (null elt) nil
		    (setq src-matches (cdr src-matches)
			  currfile (expand-file-name (car elt) dir)
			  matches (cdr elt)
			  currbuf (get-file-buffer currfile)
			  kill-buf (not currbuf)
			  currbuf (or currbuf
				      (and (file-readable-p currfile)
					   (find-file-noselect currfile))))
		    (if (null currbuf)
			(progn (set-buffer out-buf)
			       (insert "ERROR: (ebut:search): \"" currfile
				       "\" is not readable.\n\n"))
		      (set-buffer currbuf)
		      (unwind-protect
			  (save-excursion
			    (widen) (goto-char 1)
			    (let ((case-fold-search t)
				  (regexp
				   (ebut:match-regexp matches match-part)))
			      (setq firstmatch t)
			      (while (re-search-forward regexp nil t)
				(setq total (1+ total))
				(let* ((linenum (count-lines (point-min)
							     (point)))
				       (tag (format "\n%4d:" linenum))
				       lns start end)
				  (setq end (progn (end-of-line) (point))
					start (progn
						(goto-char (match-beginning 0))
						(beginning-of-line) (point))
					lns (buffer-substring start end))
				  (goto-char end)
				  (save-excursion
				    (set-buffer out-buf)
				    (if firstmatch
					(progn
					  (insert hbut:source-prefix "\"" 
						  currfile "\"\n")
					  (setq firstmatch nil)))
				    (insert tag lns))))
			      (set-buffer out-buf)
			      (if (not firstmatch) (insert "\n\n"))))
			(if kill-buf (kill-buffer currbuf)))))))))))
    total))

;;; ----
(defun    ebut:delimit (start end instance-str)
  "Delimits button label spanning region START to END in current buffer.
If button is already delimited or delimit fails, returns nil, else t.
Inserts INSTANCE-STR after END, before ending delimiter."
  (goto-char start)
  (if (looking-at (regexp-quote ebut:start))
      (forward-char (length ebut:start)))
  (if (ebut:label-p)
      nil
    (if (not (stringp instance-str)) (setq instance-str ""))
    (insert ebut:start)
    (goto-char (setq end (+ end (length ebut:start))))
    (insert instance-str ebut:end)
    (setq end (+ end (length instance-str) (length ebut:end)))
    (and (fboundp 'hproperty:but-add) (hproperty:but-add start end hproperty:but))
    (hbut:comment start end)
    (goto-char end)
    t))

(defun    ebut:match-regexp (match-keys match-part)
  "Returns regexp to match to all explicit button keys from MATCH-KEYS."
  (setq match-part (if match-part
		       (concat "[^" (substring ebut:end -1) "]*")
		     "[ \t\n]*"))
  (concat
   (regexp-quote ebut:start) match-part
   "\\(" (mapconcat (function
		     (lambda (key) (ebut:label-regexp key 'no-delim)))
		    match-keys "\\|")
   "\\)" match-part (regexp-quote ebut:end)))

(defconst ebut:start "<("
  "String matching the start of a hyper-button.")
(defconst ebut:end   ")>"
  "String matching the end of a hyper-button.")
(defconst ebut:instance-sep ":"
  "String of one character, separates an ebut label from its instance num.")

;;;
;;; gbut class - Global Hyperbole buttons - activated by typing label name
;;;

(defvar gbut:file (expand-file-name hbmap:filename hbmap:dir-user)
  "File that stores Hyperbole buttons accessible by name, global buttons.")

(defun gbut:act (label)
  "Activates Hyperbole global button with LABEL."
  (interactive (list (hargs:read-match "Activate global button labeled: "
				       (mapcar 'list (gbut:lbl-list))
				       nil t nil 'ebut)))
  (let* ((lbl-key (hbut:label-to-key label))
	 (but (ebut:get lbl-key nil gbut:file)))
    (if but
	(hbut:act but)
      (error "(gbut:act): No global button labeled: %s" label))))

(defun gbut:help (label)
  "Displays help for Hyperbole global button with LABEL."
  (interactive (list (hargs:read-match "Report on global button labeled: "
				       (mapcar 'list (gbut:lbl-list))
				       nil t nil 'ebut)))
  (let* ((lbl-key (hbut:label-to-key label))
	 (but (ebut:get lbl-key nil gbut:file)))
    (if but
	(hbut:report but)
      (error "(gbut:help): No global button labeled: %s" label))))

;;; ----
(defun gbut:key-list ()
  "Returns list of global button label keys."
  (save-excursion
    (if (hbdata:to-entry-buf gbut:file)
	(let ((gbuts))
	  (save-restriction
	    (narrow-to-region (point) (if (search-forward "\^L" nil t)
					  (point) (point-max)))
	    (goto-char (point-min))
	    (condition-case ()
		(while (setq gbuts (cons (car (read (current-buffer))) gbuts)))
	      (error nil))
	    gbuts)))))

(defun gbut:lbl-list ()
  "Returns list of global button labels."
  (mapcar 'hbut:key-to-label (gbut:key-list)))

;;;
;;; hattr class
;;;

(defun    hattr:attributes (obj-symbol)
  "Returns a list of OBJ-SYMBOL's attributes as symbols."
  (if (symbolp obj-symbol)
      (let* ((attr-val-list (symbol-plist obj-symbol))
	     (i -1))
	(delq nil (mapcar (function
			   (lambda (elt)
			     (setq i (1+ i))
			     (and (= (% i 2) 0) elt)))
			  attr-val-list)))))

(defun    hattr:clear (hbut)
  "Removes all of HBUT's attributes except `variable-documentation'."
  (let (sublist)
    (or (symbolp hbut)
	(error "(hattr:clear): Argument not a Hyperbole button: %s" hbut))
    (if (setq sublist (memq 'variable-documentation (symbol-plist hbut)))
	(progn
	  (setcdr (cdr sublist) nil)
	  (setplist hbut sublist))
      (setplist hbut nil)
      )))

(defun    hattr:copy (from-hbut to-hbut)
  "Copies attributes FROM-HBUT TO-HBUT, eliminating attributes TO-HBUT had.
Returns TO-HBUT."
  (mapcar
   (function
    (lambda (hbut)
      (or (and hbut (symbolp hbut))
	  (error "(hattr:clear): Argument not a Hyperbole button: %s" hbut))))
   (list from-hbut to-hbut))
  (unwind-protect
      nil
    (hattr:clear to-hbut)
    (setplist to-hbut (copy-sequence (symbol-plist from-hbut))))
  to-hbut)

(defun    hattr:get (obj-symbol attr-symbol)
  "Returns value of OBJ-SYMBOL's attribute ATTR-SYMBOL."
  (get obj-symbol attr-symbol))

(defun    hattr:list (obj-symbol)
  "Returns a property list of OBJ-SYMBOL's attributes.
Each pair of elements is: <attrib-name> <attrib-value>."
  (if (symbolp obj-symbol)
      (symbol-plist obj-symbol)
    (error "(hattr:list): Argument not a symbol: %s" obj-symbol)))

(defun    hattr:memq (attr-symbol obj-symbol)
  "Returns t if ATTR-SYMBOL is in OBJ-SYMBOL's attribute list, else nil."
  (and (symbolp obj-symbol) (symbolp attr-symbol)
       (let* ((attr-val-list (symbol-plist obj-symbol))
	      (attr-list (let ((i -1))
			   (delq nil (mapcar
				      (function
				       (lambda (elt)
					 (setq i (1+ i))
					 (and (= (% i 2) 0) elt)))
				      attr-val-list)))))
	 (if (memq attr-symbol attr-list) t))))

(defun    hattr:report (attrib-list)
  "Pretty prints to standard-output attribute-value pairs from ATTRIB-LIST.
Ignores nil valued attributes.  Returns t unless no attributes are printed."
  (let ((has-attr) attr val len)
    (if (or (null attrib-list) (not (listp attrib-list))
	    ;; odd number of elements?
	    (= (% (length attrib-list) 2) 1))
	nil
      (while (setq attr (car attrib-list))
	(setq val (car (setq attrib-list (cdr attrib-list)))
	      attrib-list (cdr attrib-list))
	(if val
	    (progn
	      (setq has-attr t
		    attr (symbol-name attr)
		    len (max (- 16 (length attr)) 1))
	      (princ "   ") (princ attr) (princ ":")
	      (princ (make-string len ? ))
	      (let (str)
		(prin1 (cond ((string-match "time" attr)
			      (htz:date-unix val
					     (and (>= (aref val 0) ?0)
						  (<= (aref val 0) ?9)
						  "GMT") htz:local))
			     ((and (setq str (if (stringp val) val
					       (prin1-to-string val)))
				   (string-match "\\`actypes::" str))
			      (intern (substring str (match-end 0))))
			     (t val))))
	      (terpri))))
      has-attr)))

(defun    hattr:save ()
  "Saves button attribute file for current directory, if modified.
Suitable for use as part of 'write-file-hooks'."
  (let* ((bd-file (expand-file-name hattr:filename default-directory))
	 (buf (and (stringp default-directory)
		   (get-file-buffer bd-file))))
    (if (and ebut:hattr-save buf (not (eq buf (current-buffer))))
	(let ((ebut:hattr-save));; Prevents 'write-file-hooks' looping.
	  (and (buffer-modified-p buf) 
	       (save-excursion
		 (set-buffer buf) (save-buffer)
		 ;; Unlock button attribute file; kill buffer so user is
		 ;; never holding a buffer which is out of sync with file,
		 ;; due to some other user's edits.
		 ;; Maybe this should be user or site configurable.
		 (or (buffer-modified-p buf) (kill-buffer buf))
		 )))))
  ;; Must return nil, so can be used as part of write-file-hooks.
  nil)

(defun    hattr:set (obj-symbol attr-symbol attr-value)
  "Sets OBJ-SYMBOL's attribute ATTR-SYMBOL to ATTR-VALUE."
  (put obj-symbol attr-symbol attr-value))

(fset    'hattr:summarize 'hattr:report)

(defvar   hattr:filename
  (if (memq system-type '(ms-windows windows-nt ms-dos)) "_hypb" ".hypb")
  "Per directory file name in which explicit button attributes are stored.
If you change its value, you will be unable to use buttons created by
others who use a different value!")

;;;
;;; hbut class - abstract
;;;

(defun    hbut:act (hbut)
  "Performs action for explicit or implicit Hyperbole button symbol HBUT."
  (and hbut (apply 'actype:act (hattr:get hbut 'actype)
		   (hattr:get hbut 'args))))

(defun    hbut:action (hbut)
  "Returns appropriate action for Hyperbole button symbol HBUT."
  (let ((categ (hattr:get hbut 'categ)) (atype) (action))
    (if (eq categ 'explicit)
	(progn (setq action (hattr:get hbut 'action)
		     atype  (hattr:get hbut 'actype))
	       (if (= (length (symbol-name atype)) 2)
		   atype
		 (or action (actype:action atype))))
      ;; Must be an implicit button.
      (if (fboundp atype) atype))))

(defun    hbut:at-p ()
  "Returns symbol for explicit or implicit Hyperbole button at point or nil."
  (or (ebut:at-p) (ibut:at-p)))


(defun    hbut:comment (start end)
  "Comment button label spanning region START to END in current buffer.
Use buffer commenting grammar, if any, otherwise don't comment."
  (save-excursion
    (if comment-start
	(if (or (equal comment-end "")
		(null comment-end))
	    (progn
	      (beginning-of-line)
	      (if (search-forward comment-start start t)
		  nil
		(goto-char start)
		(insert comment-start)
		(if (/= (preceding-char) ? )
		    (insert ? ))))
	  ;; Comments have both start and end delimiters
  	  (if (and (re-search-backward
		    (concat (regexp-quote comment-start) "\\|"
			    (regexp-quote comment-end))
		    nil t)
		   (looking-at (regexp-quote comment-start)))
	      nil
	    (goto-char start)
	    (insert comment-start)
	    (if (/= (preceding-char) ? )
		(insert ? ))
	    (goto-char (+ (point) (- end start)))
	    (if (/= (following-char) ? )
		(insert ? ))
	    (insert comment-end)
	    )))))

;;; Regexps derived in part from "filladapt.el" under the GPL, Copyright
;;; 1989 Kyle E. Jones.
(defvar   hbut:fill-prefix-regexps
  '(
    ;; Included text in news or mail messages
    "\n[ \t]*\\([:|<>]+ *\\)+"
    ;; Included text generated by SUPERCITE.  We can't hope to match all
    ;; the possible variations.
    "\n[ \t]*[^'`\"< \t]*> *"
    ;; Lisp comments
    "\n[ \t]*\\(;+[ \t]*\\)+"
    ;; UNIX shell comments
    "\n[ \t]*\\(#+[ \t]*\\)+"
    ;; C++ comments
    "\n[ \t]*//[/ \t]+"
    ;; C or Pascal comments, one open and close per line, so match close
    ;; then open.
    "\\*+[/\)][ \t]*\n+[ \t]*[/\(]\\*+"
    "}[ \t]*\n+[ \t]*{"
    ;; Eiffel or Sather comments
    "\n[ \t]*--[ \t]+"
    ;; Fortran comments
    "\n[Cc][ \t]+"
    ;; Postscript comments
    "\n[ \t]*\\(%+[ \t]*\\)+"
    )
  "List of regexps of fill prefixes to remove from the middle of buttons.")

(defun    hbut:fill-prefix-remove (label)
  "Removes any recognized fill prefix from within LABEL.
'hbut:fill-prefix-regexps' is a list of fill prefixes to recognize."
  (if (string-match "\n" label)
      (mapcar
       (function
	(lambda (fill-prefix)
	  (and (string-match "\n" label)
	       (setq label
		     (hypb:replace-match-string fill-prefix label " " t)))))
       hbut:fill-prefix-regexps))
  label)

(defun    hbut:is-p (object)
  "Returns non-nil if object denotes a Hyperbole button."
  (and (symbolp object) (hattr:get object 'categ)))

(fset    'hbut:key-src      'ebut:key-src)
(fset    'hbut:key-to-label 'ebut:key-to-label)

(defun    hbut:label (hbut)
  "Returns the label for Hyperbole button symbol HBUT."
  (if (hbut:is-p hbut)
      (hbut:key-to-label (hattr:get hbut 'lbl-key))
    (error "(hbut:label): Argument is not a Hyperbole button symbol, '%s'"
	   hbut)))

(fset    'hbut:label-p      'ebut:label-p)
(fset    'hbut:label-to-key 'ebut:label-to-key)

(defun    hbut:report (&optional arg)
  "Pretty prints the attributes of a button or buttons.

Takes an optional ARG interpreted as follows:
  a button symbol - report on that button;
  nil             - report on button at point, if any;
  integer > 0     - report on all explicit buttons in buffer, alphabetize;
  integer < 1     - report on all explicit buttons in occurrence order;

Returns number of buttons reported on or nil if none."
  (setq arg (cond ((or (integerp arg) (symbolp arg)) arg)
		  ((listp arg)
		   (if (integerp (setq arg (car arg))) arg 1))
		  (t 1)))
  (let* ((but (if (and arg (symbolp arg)) arg (hbut:at-p)))
	 (curr-key (and but (hattr:get but 'lbl-key)))
	 (key-src (or (and but (hattr:get but 'loc)) (hbut:key-src)))
	 (lbl-lst (cond ((not arg)
			 (if curr-key (list (ebut:key-to-label curr-key))))
			((symbolp arg) (if curr-key
					   (list (hbut:key-to-label
						  (hattr:get arg 'lbl-key)))))
			((< arg 1) (ebut:list))
			(t (sort (ebut:list)
				 (function
				  (lambda (s1 s2)
				    (string< (downcase s1) (downcase s2))))))))
	 (key-buf (current-buffer))
	 (buf-name (hypb:help-buf-name))
	 (attribs))
    (if lbl-lst
	(progn
	  (with-output-to-temp-buffer buf-name
	    (princ hbut:source-prefix)
	    (prin1 key-src)
	    (terpri)
	    (terpri)
	    (mapcar
	     (function
	      (lambda (lbl)
		(if (setq but
			  (cond ((or (null arg) (symbolp arg)) but)
				(t (ebut:get (ebut:label-to-key lbl) key-buf)))
			  attribs (hattr:list but))
		    (progn
		      (princ (if (ibut:is-p but)
				 lbl
			       (concat ebut:start lbl ebut:end)))
		      (terpri)
		      (let ((doc (actype:doc but (= 1 (length lbl-lst)))))
			(if doc
			    (progn
			      (princ "  ")
			      (princ doc)
			      (terpri))))
		      (hattr:report
;;		       (if (eq (car (cdr (memq 'categ attribs))) 'explicit)
;;			   (memq 'action attribs)
;;			 (memq 'categ attribs))
		       attribs)
		      (terpri))
		  )))
	     lbl-lst))
	  (length lbl-lst)))))

(defun    hbut:source (&optional full)
  "Returns Hyperbole source buffer or file given at point.
If a file, always returns a full path if optional FULL is non-nil."
  (goto-char (match-end 0))
  (cond ((looking-at "#<buffer \\([^ \n]+\\)>")
	 (get-buffer (buffer-substring (match-beginning 1)
				       (match-end 1))))
	((looking-at "\".+\"")
	 (let* ((file (buffer-substring (1+ (match-beginning 0))
					(1- (match-end 0))))
		(absolute (file-name-absolute-p file)))
	   (if (and full (not absolute))
	       (expand-file-name file default-directory)
	     file)))))

(fset    'hbut:summarize 'hbut:report)

(defvar   hbut:current nil
  "Currently selected Hyperbole button.
Available to action routines.")

(defconst hbut:source-prefix moccur-source-prefix
  "String found at start of a buffer containing only a hyper-button menu.
   This expression should be followed immediately by a file-name indicating the
source file for the buttons in the menu, if any.")

;;;
;;; htype class - Hyperbole Types, e.g. action and implicit button types
;;;

(require 'set)

(defun    htype:body (htype-sym)
  "Return body for HTYPE-SYM.  If HTYPE-SYM is nil, return nil."
  (and htype-sym (hypb:indirect-function htype-sym)))

(defun    htype:category (type-category)
  "Return list of symbols in Hyperbole TYPE-CATEGORY in priority order.
Symbols contain category component.
TYPE-CATEGORY should be 'actypes, 'ibtypes or nil for all."
  (let ((types (symset:get type-category 'symbols))
	(categ-name (symbol-name type-category)))
    (mapcar (function
	     (lambda (type)
	       (intern (concat categ-name "::" (symbol-name type)))))
	    types)))

;; Thanks to JWZ for help on this.
(defmacro htype:create (type type-category doc params body property-list)
  "Create a new Hyperbole TYPE within TYPE-CATEGORY (both unquoted symbols).
Third arg DOC is a string describing the type.
Fourth arg PARAMS is a list of parameters to send to the fifth arg BODY,
which is a list of forms executed when the type is evaluated.
Sixth arg PROPERTY-LIST is attached to the new type's symbol.

This symbol is returned."
  (let* ((sym (htype:symbol type type-category))
	(action (nconc (list 'defun sym params doc) body)))
    (` (progn
	 (, action)
	 (setplist '(, sym) (, property-list))
	 (symset:add '(, type) '(, type-category) 'symbols)
	 (run-hooks 'htype:create-hook)
	 '(, sym)))))

(defun    htype:delete (type type-category)
  "Delete a Hyperbole TYPE derived from TYPE-CATEGORY (both symbols).
Return the Hyperbole symbol for the TYPE if it existed, else nil."
  (let* ((sym (htype:symbol type type-category))
	 (exists (fboundp 'sym)))
    (setplist sym nil)
    (symset:delete type type-category 'symbols)
    (fmakunbound sym)
    (run-hooks 'htype:delete-hook)
    (and exists sym)))

(defun    htype:doc (type)
  "Return documentation for Hyperbole TYPE, a symbol."
  (documentation type))

(defun    htype:names (type-category &optional sym)
  "Return list of current names for Hyperbole TYPE-CATEGORY in priority order.
Names do not contain category component.
TYPE-CATEGORY should be 'actypes, 'ibtypes or nil for all.
When optional SYM is given, return the name for that symbol only, if any."
  (let ((types (symset:get type-category 'symbols))
	(sym-name (and sym (symbol-name sym))))
    (if sym-name
	;; Strip category from sym-name before looking for a match.
	(progn (if (string-match "::" sym-name)
		   (setq sym (intern (substring sym-name (match-end 0)))))
	       (if (memq sym types) (symbol-name sym)))
      (mapcar 'symbol-name types))))

;;; ----

(defun   htype:symbol (type type-category)
  "Return Hyperbole type symbol composed from TYPE and TYPE-CATEGORY (both symbols)."
  (intern (concat (symbol-name type-category) "::"
		  (symbol-name type))))

;;;
;;; ibut class - Implicit Hyperbole Buttons
;;;

(defun    ibut:at-p (&optional key-only)
  "Returns symbol for implicit button at point, else nil.
With optional KEY-ONLY, returns only the label key for button."
  (let ((types (htype:category 'ibtypes))
	;; Global var used in (hact) function, don't delete.
	(hrule:action 'actype:identity)
	(itype)
	(args)
	(is-type))
    (or key-only (hattr:clear 'hbut:current))
    (while (and (not is-type) types)
      (setq itype (car types))
      (if (setq args (funcall itype))
	  (setq is-type itype)
	(setq types (cdr types))))
    (if is-type
	(if key-only
	    (hattr:get 'hbut:current 'lbl-key)
	  (hattr:set 'hbut:current 'loc (save-excursion
					  (hbut:key-src 'full)))
	  (hattr:set 'hbut:current 'categ is-type)
	  (or (hattr:get 'hbut:current 'args)
	      (not (listp args))
	      (progn
		(hattr:set 'hbut:current 'actype
			   (or
			     ;; Hyperbole action type
			     (intern-soft (concat "actypes::"
						  (symbol-name (car args))))
			     ;; Regular Emacs Lisp function symbol
			     (car args)
			     ))
		(hattr:set 'hbut:current 'args (cdr args))))
	  'hbut:current))))

(defun    ibut:is-p (object)
  "Returns non-nil if object denotes an implicit Hyperbole button."
  (if (symbolp object)
      (let ((categ (hattr:get object 'categ)))
	(and categ (string-match "^ibtypes::" (symbol-name categ))))))

(defun    ibut:label-p ()
  "Returns key for Hyperbole implicit button label that point is on or nil."
  (ibut:at-p 'key-only))

(defun    ibut:label-set (label &optional start end)
  "Sets current implicit button attributes from LABEL and START, END position.
START and END are optional.  When given, they specify the region in the buffer
to flash when this implicit button is activated or queried for its attributes.
If LABEL is a list, it is assumed to contain all arguments."
  (cond ((stringp label)
	 (hattr:set 'hbut:current 'lbl-key (hbut:label-to-key label))
	 (and start (hattr:set    'hbut:current 'lbl-start start))
	 (and end   (hattr:set    'hbut:current 'lbl-end   end)))
	((and label (listp label))
	 (hattr:set 'hbut:current 'lbl-key (hbut:label-to-key (car label)))
	 (hattr:set    'hbut:current 'lbl-start    (nth 1 label))
	 (hattr:set    'hbut:current 'lbl-end      (nth 2 label)))
	(t (error "(ibut:label-set): Invalid label arg: '%s'" label)))
  t)

;;;
;;; ibtype class - Implicit button types
;;;

(fset    'defib 'ibtype:create)
(put     'ibtype:create 'lisp-indent-function 'defun)
(defmacro ibtype:create (type params doc at-p &optional to-p style)
  "Creates implicit button TYPE (unquoted sym) with PARAMS, described by DOC.
PARAMS are presently ignored.

  AT-P is a boolean form of no arguments which determines whether or not point
is within a button of this type.
  Optional TO-P is a boolean form which moves point immediately after the next
button of this type within the current buffer and returns a list of (button-
label start-pos end-pos), or nil when none is found.
  Optional STYLE is a display style specification to use when highlighting
buttons of this type; most useful when TO-P is also given.

Returns symbol created when successful, else nil.  Nil indicates that action
type for ibtype is presently undefined."
  (if type
      (let ((to-func (if to-p (action:create nil (list to-p))))
	    (at-func (list at-p)))
	(` (htype:create (, type) ibtypes (, doc) nil (, at-func)
			 (list 'to-p (, to-func) 'style (, style)))))))

(defun    ibtype:delete (type)
  "Deletes an implicit button TYPE (a symbol).
Returns TYPE's symbol if it existed, else nil."
  (htype:delete type 'ibtypes))

;;;
;;; symset class - Hyperbole internal symbol set maintenance
;;;

(require 'set)

(defun    symset:add (elt symbol prop)
  "Adds ELT to SYMBOL's PROP set.
Returns nil iff ELT is already in SET.  Uses 'eq' for comparison."
  (let* ((set (get symbol prop))
	 (set:equal-op 'eq)
	 (new-set (set:add elt set)))
    (and new-set (put symbol prop new-set))))

(fset    'symset:delete 'symset:remove)

(defun    symset:get (symbol prop)
  "Returns SYMBOL's PROP set."
  (get symbol prop))

(defun    symset:remove (elt symbol prop)
  "Removes ELT from SYMBOL's PROP set and returns the new set.
Assumes PROP is a valid set.  Uses 'eq' for comparison."
  (let ((set (get symbol prop))
	(set:equal-op 'eq))
    (put symbol prop (set:remove elt set))))


(provide 'hbut)

;;; hbut.el ends here
