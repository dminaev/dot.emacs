;;; wrolo.el --- Hierarchical, multi-file, easy to use rolodex system

;; Copyright (C) 1989, 1990, 1991, 1992, 1995, 2006 Free Software
;; Foundation, Inc.
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
;;  The `put whatever you feel like into it' rolodex.
;;
;;  FEATURES:
;;
;;   1.  Multiple rolodex files with free text lookup.  No structured
;;       fields are used.
;;
;;   2.  Hierarchical rolodex entries as in:
;;        *    Company
;;        **     Manager
;;        ***      Underlings
;;
;;       Searching for Manager turns up all Underlings.  Searching for
;;       Company retrieves all listed employees.
;;
;;       This hierarchical system has proved very effective for retrieving
;;       computer system administration problem reports by vendor name,
;;       problem number or by subject area, without having to resort to a
;;       database system, and also for extraction of relevant text
;;       sections from reports.
;;
;;   3.  String and regular expression searching capabilities.  Matches are
;;       found anywhere within entries, so entries may be of any format you
;;       like without the bother of fixed field restrictions.
;;       Ability to restrict number of matches or to report number of matches
;;       without displaying entries.
;;
;;   4.  Smart addition, editing and sorting of entries by hierarchy level.
;;
;;   5.  Support for Hyperbole buttons within rolodex entries.
;;
;;   See "wrolo-logic.el" for logical search functions (and, or, not, xor).
;;   See "wrolo-menu.el" for menu handling functions.  (If you received
;;   wrolo as part of Hyperbole, this file in unneeded and so not included.)
;;
;;
;;  SETUP:
;;
;;   The variable 'rolo-file-list' is a list of files to search for
;;   matching rolodex entries.  To add personal files to rolo-file-list,
;;   when you find these functions are useful for any sort of list lookup,
;;   add the following to your ~/.emacs file (substituting where you see
;;   <fileN>):
;;
;;      (setq rolo-file-list (append rolo-file-list '("<file1>" "<file2>")))
;;
;;   We recommend that entries in 'rolo-file-list' have ".otl" suffixes
;;   so that they do not conflict with file names that other rolodex
;;   programs might use and so that they are edited in 'outline-mode' by
;;   default.  If you want the latter behavior, uncomment and add something
;;   like the following to one of your GNU Emacs initialization files:
;;
;;     ;; Add to the list of suffixes that causes automatic mode invocation
;;     (setq auto-mode-alist
;;        (append '(("\\.otl$" . outline-mode)) auto-mode-alist))
;;
;;   The buffers containing the rolodex files are not killed after a search
;;   on the assumption that another search is likely to follow within this
;;   Emacs session.  You may wish to change this behavior with the following
;;   setting:
;;
;;     (setq rolo-kill-buffers-after-use t)
;;
;;   After an entry is killed, the modified rolodex file is automatically
;;   saved.  If you would rather always save files yourself, use this
;;   setting:
;;
;;     (setq rolo-save-buffers-after-use nil)
;;
;;   When adding an entry from within a buffer containing a mail
;;   message, the rolodex add function will extract the sender's name
;;   and e-mail address and prompt you with the name as a default.  If
;;   you accept it, it will enter the name and the email address using
;;   the format given by the 'rolo-email-format' variable.  See its
;;   documentation if you want to change its value.
;;
;;
;;   If you use Hyperbole V2.3 or greater, then no other rolodex setup
;;   is necessary, simply select the "Rolo/" menu item from the top
;;   level Hyperbole menu.  Otherwise, add the following to your
;;   "~/.emacs" file: 
;;
;;     (autoload 'rolo-menu "rolo-menu" "Load wrolo system." t)
;;     (global-set-key "\C-x4r" 'rolo-menu)
;;
;;   And then simply invoke the rolodex menu with {C-x 4 r} after Emacs
;;   has read those lines in your init file.
;;
;;
;;  SUMMARY OF USE:
;;
;;   The rolo menu provides access to the following commands:
;;
;;     Menu Item       Function              Description
;;     ====================================================================
;;     Add             rolo-add              Adds a rolodex entry
;;     Display         rolo-display-matches  Displays last matches again
;;     Edit            rolo-edit             Edits an existing rolodex entry
;;     Info                                  Displays Rolodex manual entry
;;     Kill            rolo-kill             Removes an entry from the rolodex
;;     Order           rolo-sort             Sorts all levels in rolodex
;;     RegexFind       rolo-grep             Finds all entries containing
;;                                             a regular expression
;;     StringFind      rolo-fgrep            Finds all entries containing
;;                                             a string
;;     WordFind        rolo-word             Finds all entries containing
;;                                             a string of whole words
;;     Yank            rolo-yank             Inserts first matching rolodex
;;                                             entry at point
;;
;;   For any of these commands that prompt you for a name, you may use the form
;;   parent/child to locate a child entry below a parent entry, e.g.
;;   from the example near the top, we could give Company/Manager/Underlings.
;;
;;   Here is a snippet from our group rolodex file.  The ';'s should be
;;   removed of course and the '*'s should begin at the start of the
;;   line.  If a rolodex file begins with two separator lines whose
;;   first three characters are "===", then these lines and any text
;;   between them are prepended to the output buffer whenever any
;;   entries are retrieved from that file.
;;
;;=============================================================================
;;			      GROUP ROLODEX
;; <Last Name>, <First Name>  <Co/Categ>   W<Work #>   H<Home #>  P<Pager #>
;;				           F<Fax #>    M<Modem #> C<Cellular #>
;;        <Address>	   <Miscellaneous Info, Key Words>
;;=============================================================================
;;*   EX594, Digital-Systems-Research
;;**  Weiner, Bob	      Motorola     W2087                  P7-7489
;;	  FL19, L-1035
;;
;;
;;  FOR PROGRAMMERS:
;;
;;   Entries in rolodex files are separated by patterns matching
;;   'rolo-entry-regexp'.  Each entry may have any number of sub-entries
;;   which represent the next level down in the entry hierarchy.
;;   Sub-entries' separator patterns are always longer than their parents'.
;;   For example, if an entry began with '*' then its sub-entries would begin
;;   with '**' and so on.  Blank lines in rolodex files will not end up where
;;   you want them if you use the rolo-sort commands; therefore, blank lines
;;   are not recommended.  If you change the value of
;;   'rolo-entry-regexp', you will have to modify 'rolo-sort'.
;;
;;   The following additional functions are provided:
;;
;;     'rolo-sort-level' sorts a specific level of entries in a rolodex file;
;;     'rolo-map-level' runs a user specified function on a specific level of
;;       entries in a rolodex file;
;;     'rolo-fgrep-file', same as 'rolo-fgrep' but operates on a single file;
;;     'rolo-grep-file', same as 'rolo-grep' but operates on a single file;
;;     'rolo-display-matches', display last set of rolodex matches, if any;
;;     'rolo-toggle-narrow-to-entry' toggles between display of current entry
;;       and display of all matching entries.
;;
;;
;;  MOD HISTORY:
;;
;;   12/17/89
;;     Added internal 'rolo-shrink-window' function for use in
;;     compressing/uncompressing the rolo view window to/from a size just
;;     large enough for the selected entry.  This is useful when a search
;;     turns up more entries than desired.
;;
;;   02/21/90
;;     Modified 'rolo-grep-file' and 'rolo-map-level' so they only set buffers
;;     read-only the first time they are read in.  This way, if someone edits a
;;     rolodex file and then does a rolo-fgrep or other function, the buffer
;;     will not be back in read-only mode.
;;
;;   04/18/91
;;     Modified 'rolo-grep-file' to expand any hidden entries in rolo file
;;     before doing a search.
;;
;;   12/24/91
;;     Added Hyperbole button support.
;;
;;   12/30/91
;;     Added convenient support for entry add, edit, kill and yank.
;;
;;   01/10/91
;;     Fixed bug in rolo-to that ended search too early.
;;

;;; Code:

;;;
;;; Other required Elisp libraries
;;;

(require 'hversion)
(require 'hmail)

;;;
;;; Public variables
;;;

(defvar rolo-email-format "%s\t\t<%s>"
  "Format string to use when adding an entry with e-mail addr from a mail msg.
It must contain a %s indicating where to put the entry name and a second
%s indicating where to put the e-mail address.")

(defvar rolo-file-list
  (if (memq system-type '(ms-windows windows-nt ms-dos))
      '("c:/_rolodex.otl") '("~/.rolodex.otl"))
  "*List of files containing rolodex entries.
The first file should be a user-specific rolodex file, typically in the home
directory.  The second file is often a shared, group-specific rolodex file.

A rolo-file consists of:
   (1) an optional header beginning with and ending with a line which matches
       rolo-hdr-regexp;
   (2) one or more rolodex entries which each begin with
       rolo-entry-regexp and may be nested.")

(defvar rolo-highlight-face nil
  "*Face used to highlight rolodex search matches.")
(if rolo-highlight-face
    nil
  (setq rolo-highlight-face
	(cond (hyperb:emacs19-p
	       (if (fboundp 'make-face)
		   (progn (make-face 'rolo-highlight-face)
			  'rolo-highlight-face)))
	      (hyperb:epoch-p (make-style))
	      (t (if (fboundp 'make-face)
		     (face-name (make-face 'rolo-highlight-face))))))
  (if (fboundp 'hproperty:set-item-highlight)
      (hproperty:set-item-highlight)))

(defvar rolo-kill-buffers-after-use nil
  "*Non-nil means kill rolodex file buffers after searching them for entries.
Only unmodified buffers are killed.")

(defvar rolo-save-buffers-after-use t
  "*Non-nil means save rolodex file after an entry is killed.")

(defvar wrolo-yank-reformat-function nil
  "*Value is a function of two arguments, START and END, invoked after a rolo-yank.
It should reformat the region given by the arguments to some preferred style.
Default value is nil, meaning no reformmating is done.")

;;;
;;; Commands
;;;

;;;###autoload
(defun rolo-add (name &optional file)
  "Adds a new entry in personal rolodex for NAME.
Last name first is best, e.g. \"Smith, John\".
With prefix argument, prompts for optional FILE to add entry within.
NAME may be of the form: parent/child to insert child below a parent
entry which begins with the parent string."
  (interactive
   (progn
     (or (fboundp 'mail-fetch-field) (require 'mail-utils))
     (let* ((lst (rolo-name-and-email))
	    (name (car lst))
	    (email (car (cdr lst)))
	    (entry (read-string "Name to add to rolo: "
				(or name email))))
       (list (if (and email name
		      (string-match (concat "\\`" (regexp-quote entry)) name))
		 (format rolo-email-format entry email) entry)
	     current-prefix-arg))))
  (if (or (not (stringp name)) (string= name ""))
      (error "(rolo-add): Invalid name: '%s'" name))
  (if (and (interactive-p) file)
      (setq file (completing-read "File to add to: "
				  (mapcar 'list rolo-file-list))))
  (if (null file) (setq file (car rolo-file-list)))
  (cond ((and file (or (not (stringp file)) (string= file "")))
	 (error "(rolo-add): Invalid file: '%s'" file))
	((and (file-exists-p file) (not (file-readable-p file)))
	 (error "(rolo-add): File not readable: '%s'" file))
	((not (file-writable-p file))
	 (error "(rolo-add): File not writable: '%s'" file)))
  (set-buffer (or (get-file-buffer file) (find-file-noselect file)))
  (if (interactive-p) (message "Locating insertion point for '%s'..." name))
  (let ((parent "") (level "") end)
    (widen) (goto-char 1)
    (while (setq end (string-match "/" name))
      (setq parent (substring name 0 end)
	    name (substring name (min (1+ end) (length name))))
      (if (re-search-forward
	   (concat "\\(" rolo-entry-regexp "\\)[ \t]*" 
		   (regexp-quote parent)) nil t)
	  (setq level (buffer-substring (match-beginning 1)
					(match-end 1)))
	(error "(rolo-add): '%s' category not found in \"%s\"."
	       parent file)))
    (narrow-to-region (point)
		      (progn (rolo-to-entry-end t level) (point)))
    (goto-char (point-min))
    (let* ((len (length name))
	   (name-level (concat level "*"))
	   (level-len (length name-level))
	   (entry "")
	   (entry-spc "")
	   (entry-level)
	   (match)
	   (again t))
      (while (and again
		  (re-search-forward
		   (concat "\\(" rolo-entry-regexp "\\)\\([ \t]*\\)")
		   nil 'end))
	(setq entry-level (buffer-substring (match-beginning 1)
					    (match-end 1)))
	(if (/= (length entry-level) level-len)
	    (rolo-to-entry-end t entry-level)
	  (setq entry (buffer-substring (point) (+ (point) len))
		entry-spc (buffer-substring (match-beginning 2)
					    (match-end 2)))
	  (cond ((string< entry name)
		 (rolo-to-entry-end t entry-level))
		((string< name entry)
		 (setq again nil) (beginning-of-line))
		(t ;; found existing entry matching name
		 (setq again nil match t)))))
      (setq buffer-read-only nil)
      (if match
	  nil
	(insert (or entry-level (concat level "*"))
		(if (string= entry-spc "") "   " entry-spc)
		name "\n")
	(backward-char 1))
      (widen)
      (rolo-to-buffer (current-buffer))
      ;; Fixes non-display update bug when buf is on screen before
      ;; interactive command invocation. 
      (goto-char (point))
      (if (interactive-p)
	  (message "Edit entry at point.")))))

;;;###autoload
(defun rolo-display-matches (&optional display-buf return-to-buffer)
  "Display optional DISPLAY-BUF buffer of previously found rolodex matches.
If DISPLAY-BUF is nil, use the value in 'rolo-display-buffer'.
Second arg RETURN-TO-BUFFER is the buffer to leave point within after the display."
  (interactive)
  (or display-buf (setq display-buf (get-buffer rolo-display-buffer)))
  (if display-buf nil
    (error "(rolo-display-matches): Search the rolodex first."))
  ;; Save current window configuration if rolodex match buffer is not
  ;; displayed in one of the windows already.
  (or
   ;; Handle both Emacs V18 and V19 versions of get-buffer-window.
   (condition-case ()
       (get-buffer-window display-buf (selected-frame))
     (error (get-buffer-window display-buf)))
   (setq *rolo-wconfig* (current-window-configuration)))
  (rolo-to-buffer display-buf t)
  (if (eq major-mode 'wrolo-mode) nil (wrolo-mode))
  (setq buffer-read-only nil)
  (if (fboundp 'hproperty:but-create) (hproperty:but-create))
  (rolo-shrink-window)
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (run-hooks 'wrolo-display-hook)
  ;; Leave point in match buffer unless a specific RETURN-TO-BUFFER has
  ;; been specified.  Use {q} to quit and restore display.
  (if return-to-buffer (rolo-to-buffer return-to-buffer t)))

;;;###autoload
(defun rolo-edit (&optional name file)
  "Edits a rolodex entry given by optional NAME within 'rolo-file-list'.
With prefix argument, prompts for optional FILE to locate entry within.
With no NAME arg, simply displays FILE or first entry in 'rolo-file-list' in an
editable mode.  NAME may be of the form: parent/child to edit child below a
parent entry which begins with the parent string."
  (interactive "sName to edit in rolo: \nP")
  (if (string-equal name "") (setq name nil))
  (and name (not (stringp name))
       (error "(rolo-edit): Invalid name: '%s'" name))
  (if (and (interactive-p) current-prefix-arg)
      (if (= (length rolo-file-list) 1)
	  (setq file (car rolo-file-list))
	(setq file (completing-read "Entry's File: "
				    (mapcar 'list rolo-file-list)))))
  (let ((found-point) (file-list (if file (list file) rolo-file-list)))
    (or file (setq file (car file-list)))
    (if (null name)
	(progn (if (not (file-writable-p file))
		  (error "(rolo-edit): File not writable: '%s'" file))
	       (find-file-other-window file) (setq buffer-read-only nil))
      (if (setq found-point (rolo-to name file-list))
	  (progn
	    (setq file buffer-file-name)
	    (if (file-writable-p file)
		(setq buffer-read-only nil)
	      (message
	       "(rolo-edit): Entry found but file not writable: '%s'" file)
	      (beep))
	    (rolo-to-buffer (current-buffer)))
	(message "(rolo-edit): '%s' not found." name)
	(beep)
	(rolo-to-buffer (or (get-file-buffer (car file-list))
			    (find-file-noselect (car file-list))))
	(setq buffer-read-only nil))
      (widen)
      ;; Fixes display update bug in some Emacs versions.  When buf is
      ;; on screen before interactive command invocation, point is not
      ;; moved to proper location.
      (if found-point (goto-char found-point)))))

(defun rolo-edit-entry ()
  "Edit the source entry of the rolodex match buffer entry at point.
Returns entry name if found, else nil."
  (interactive)
  (let ((name (rolo-name-at)))
    (if name (progn (rolo-edit name (hbut:key-src))
		    name))))

;;;###autoload
(defun rolo-fgrep (string
		    &optional max-matches rolo-file count-only no-display)
  "Display rolodex entries matching STRING.
To a maximum of optional prefix arg MAX-MATCHES, in file(s) from optional
ROLO-FILE or rolo-file-list.  Default is to find all matching entries.  Each
entry is displayed with all of its sub-entries.  Optional COUNT-ONLY non-nil
means don't retrieve and don't display matching entries.  Optional NO-DISPLAY
non-nil means retrieve entries but don't display.

Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.

Returns number of entries matched.  See also documentation for the variable
rolo-file-list."
  (interactive "sRolodex string to match: \nP")
  (let ((total-matches (rolo-grep (regexp-quote string) max-matches
				  rolo-file count-only no-display)))
    (if (interactive-p)
	(message "%s matching entr%s found in rolodex."
		 (if (= total-matches 0) "No" total-matches)
		 (if (= total-matches 1) "y" "ies")))
    total-matches))

;;;###autoload
(defun rolo-grep (regexp &optional max-matches rolo-bufs count-only no-display)
  "Display rolodex entries matching REGEXP.
To a maximum of prefix arg MAX-MATCHES, in buffer(s) from optional ROLO-BUFS or
rolo-file-list.  Default is to find all matching entries.  Each entry is
displayed with all of its sub-entries.  Optional COUNT-ONLY non-nil means don't
retrieve and don't display matching entries.  Optional NO-DISPLAY non-nil
means retrieve entries but don't display.

Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.

Returns number of entries matched.  See also documentation for the variable
rolo-file-list."
  (interactive "sRolodex regular expression to match: \nP")
  (let ((rolo-file-list
	 (cond ((null rolo-bufs) rolo-file-list)
	       ((listp rolo-bufs) rolo-bufs)
	       ((list rolo-bufs))))
	(display-buf (if count-only
			 nil
		       (set-buffer (get-buffer-create rolo-display-buffer))))
	(total-matches 0)
	(num-matched 0)
	(inserting (or (eq max-matches t)
		       (and (integerp max-matches) (< max-matches 0))))
	(file))
    (if count-only nil
      (setq buffer-read-only nil)
      (or inserting (erase-buffer)))
    (while (and (setq file (car rolo-file-list))
		(or (not (integerp max-matches))
		    (< total-matches (max max-matches (- max-matches)))))
      (setq rolo-file-list (cdr rolo-file-list)
	    num-matched (rolo-grep-file file regexp max-matches count-only)
	    total-matches (+ total-matches num-matched))
      (if (integerp max-matches)
	  (setq max-matches
		(if (>= max-matches 0)
		    (- max-matches num-matched)
		  (+ max-matches num-matched)))))
    (if (or count-only no-display inserting (= total-matches 0))
	nil
      (rolo-display-matches display-buf))
    (if (interactive-p)
	(message "%s matching entr%s found in rolodex."
		 (if (= total-matches 0) "No" total-matches)
		 (if (= total-matches 1) "y" "ies")
		 ))
    total-matches))

(defun rolo-isearch ()
  "Interactively search forward for next occurrence of current match regexp.
Use this to add characters to further narrow the search."
  (interactive)
  (if (equal (buffer-name) rolo-display-buffer)
      (execute-kbd-macro (concat "\e\C-s" rolo-match-regexp))
    (error "(rolo-isearch): Use this command in the %s match buffer"
	   rolo-display-buffer)))

;;;###autoload
(defun rolo-kill (name &optional file)
  "Kills a rolodex entry given by NAME within 'rolo-file-list'.
With prefix argument, prompts for optional FILE to locate entry within.
NAME may be of the form: parent/child to kill child below a parent entry
which begins with the parent string.
Returns t if entry is killed, nil otherwise."
  (interactive "sName to kill in rolo: \nP")
  (if (or (not (stringp name)) (string= name ""))
      (error "(rolo-kill): Invalid name: '%s'" name))
  (if (and (interactive-p) current-prefix-arg)
      (setq file (completing-read "Entry's File: "
				  (mapcar 'list rolo-file-list))))
  (let ((file-list (if file (list file) rolo-file-list))
	(killed))
    (or file (setq file (car file-list)))
    (if (rolo-to name file-list)
	(progn
	  (setq file buffer-file-name)
	  (if (file-writable-p file)
	      (let ((kill-op
		     (function (lambda (start level)
				 (kill-region
				  start (rolo-to-entry-end t level))
				 (setq killed t)
				 (rolo-save-buffer)
				 (rolo-kill-buffer))))
		    start end level)
		(setq buffer-read-only nil)
		(re-search-backward rolo-entry-regexp nil t)
		(setq end (match-end 0))
		(beginning-of-line)
		(setq start (point)
		      level (buffer-substring start end))
		(goto-char end)
		(skip-chars-forward " \t")
		(if (interactive-p)
		    (let ((entry-line (buffer-substring
				       (point)
				       (min (+ (point) 60)
					    (progn (end-of-line) (point))))))
		      (if (y-or-n-p (format "Kill `%s...' " entry-line))
			  (progn
			    (funcall kill-op start level)
			    (message "Killed"))
			(message "Aborted")))
		  (funcall kill-op start level)))
	    (message
	     "(rolo-kill): Entry found but file not writable: '%s'" file)
	    (beep)))
      (message "(rolo-kill): '%s' not found." name)
      (beep))
    killed))

(defun rolo-mail-to ()
  "Start composing mail addressed to the first e-mail address at or after point."
  (interactive)
  (let ((opoint (point)) button)
    (skip-chars-backward "^ \t\n\r<>")
    (if (and (re-search-forward mail-address-regexp nil t)
	     (goto-char (match-beginning 1))
	     (setq button (ibut:at-p)))
	(hui:hbut-act button)
      (goto-char opoint)
      (beep)
      (message "(rolo-mail-to): Invalid buffer or no e-mail address found"))))

(defun rolo-next-match ()
  "Move point forward to the start of the next rolodex search match."
  (interactive)
  (if (not (stringp rolo-match-regexp))
      (error "(rolo-next-match): Invoke a rolodex search expression first"))
  (let ((start (point))
	(case-fold-search t))
    (if (looking-at rolo-match-regexp)
	(goto-char (match-end 0)))
    (if (re-search-forward rolo-match-regexp nil t)
	(goto-char (match-beginning 0))
      (goto-char start)
      (error
       "(rolo-next-match): No following matches for \"%s\"" rolo-match-regexp))))

(defun rolo-previous-match ()
  "Move point back to the start of the previous rolodex search match."
  (interactive)
  (if (not (stringp rolo-match-regexp))
      (error "(rolo-previous-match): Invoke a rolodex search expression first"))
  (let ((case-fold-search t))
    (if (re-search-backward rolo-match-regexp nil t)
	nil
      (error
       "(rolo-previous-match): No prior matches for \"%s\"" rolo-match-regexp))))

(defun rolo-quit ()
  "Quit from the rolodex match buffer and restore the prior frame display."
  (interactive)
  (bury-buffer)
  (if (and *rolo-wconfig*
	   (if (fboundp 'window-configuration-p)
	       (window-configuration-p *rolo-wconfig*)
	     t))
      (set-window-configuration *rolo-wconfig*)))

;;;###autoload
(defun rolo-sort (&optional rolo-file)
  "Sorts up to 14 levels of entries in ROLO-FILE (default is personal rolo).
Assumes entries are delimited by one or more '*'characters.
Returns list of number of groupings at each entry level." 
  (interactive
   (list (let ((default "")
	       (file))
	 (setq file
	       (completing-read
		(format "Sort rolo file (default %s): "
			(file-name-nondirectory
			 (setq default
			       (if (and buffer-file-name
					(memq
					 t (mapcar
					    (function
					     (lambda (file)
					       (equal buffer-file-name
						      (expand-file-name file))))
					    rolo-file-list)))
				   buffer-file-name
				 (car rolo-file-list)))))
		(mapcar 'list rolo-file-list)))
	 (if (string= file "") default file))))
  (if (or (not rolo-file) (equal rolo-file ""))
      (setq rolo-file (car rolo-file-list)))
  (if (not (and (stringp rolo-file) (file-readable-p rolo-file)))
      (error "(rolo-sort): Invalid or unreadable file: %s" rolo-file))
  (let ((level-regexp (regexp-quote "**************"))
	(entries-per-level-list)
	(n))
    (while (not (equal level-regexp ""))
      (setq n (rolo-sort-level rolo-file level-regexp))
      (if (or (/= n 0) entries-per-level-list)
	  (setq entries-per-level-list
		(append (list n) entries-per-level-list)))
      (setq level-regexp (substring level-regexp 0 (- (length level-regexp) 2))))
    entries-per-level-list))

(defun rolo-sort-level (rolo-file level-regexp &optional max-groupings)
  "Sorts groupings of entries in ROLO-FILE at hierarchy level LEVEL-REGEXP.
To a maximum of optional MAX-GROUPINGS.  Nil value of MAX-GROUPINGS means all
groupings at the given level.  LEVEL-REGEXP should simply match the text of
any rolodex entry of the given level, not the beginning of a line (^); an
example, might be (regexp-quote \"**\") to match level two.  Returns number
of groupings sorted."
  (interactive "sRolodex file to sort: \nRegexp for level's entries: \nP")
  (rolo-map-level
   (function (lambda (start end) (sort-lines nil start end)))
   rolo-file
   level-regexp
   max-groupings))

(defun rolo-toggle-narrow-to-entry ()
  "Toggle between display of current entry and display of all matched entries.
Useful when bound to a mouse key."
  (interactive)
  (if (rolo-narrowed-p)
      (widen)
    (if (or (looking-at rolo-entry-regexp)
	    (re-search-backward rolo-entry-regexp nil t))
	(progn (forward-char)
	       (narrow-to-region (1- (point)) (rolo-display-to-entry-end)))))
  (rolo-shrink-window)
  (goto-char (point-min)))

(defun rolo-word (string
		  &optional max-matches rolo-file count-only no-display)
  "Display rolodex entries with whole word matches for STRING.
To a maximum of optional prefix arg MAX-MATCHES, in file(s) from optional
ROLO-FILE or rolo-file-list.  Default is to find all matching entries.  Each
entry is displayed with all of its sub-entries.  Optional COUNT-ONLY non-nil
means don't retrieve and don't display matching entries.  Optional NO-DISPLAY
non-nil means retrieve entries but don't display.

Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.

Returns number of entries matched.  See also documentation for the variable
rolo-file-list."
  (interactive "sRolodex whole words to match: \nP")
  (let ((total-matches (rolo-grep (format "\\b%s\\b" (regexp-quote string))
				  max-matches
				  rolo-file count-only no-display)))
    (if (interactive-p)
	(message "%s matching entr%s found in rolodex."
		 (if (= total-matches 0) "No" total-matches)
		 (if (= total-matches 1) "y" "ies")))
    total-matches))

;;;###autoload
(defun rolo-yank (name &optional regexp-p)
  "Inserts at point the first rolodex entry matching NAME.
With optional prefix arg, REGEXP-P, treats NAME as a regular expression instead
of a string."
  (interactive "sName to insert record for: \nP")
  (let ((rolo-display-buffer (current-buffer))
	(start (point))
	found)
    (save-excursion
      (setq found (if regexp-p
		      (rolo-grep name -1)
		    (rolo-grep (regexp-quote name) -1))))
    ;; Let user reformat the region just yanked.
    (if (and (= found 1) (fboundp wrolo-yank-reformat-function))
	(funcall wrolo-yank-reformat-function start (point)))
    found))

;;;
;;; Public functions
;;;

(defun rolo-fgrep-file (rolo-buf string &optional max-matches count-only)
  "Retrieve entries in ROLO-BUF matching STRING to a maximum of optional MAX-MATCHES.
Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.  Optional COUNT-ONLY non-nil
means don't retrieve matching entries.
Returns number of matching entries found."
  (rolo-grep-file rolo-buf (regexp-quote string) max-matches count-only))

(defun rolo-grep-file (rolo-buf regexp &optional max-matches count-only)
  "Retrieve entries in ROLO-BUF matching REGEXP to a maximum of optional MAX-MATCHES.
Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.  Optional COUNT-ONLY non-nil
means don't retrieve matching entries.
Returns number of matching entries found."
  ;;
  ;; Save regexp as last rolodex search expression.
  (setq rolo-match-regexp regexp)
  ;;
  (let ((new-buf-p) (actual-buf))
    (if (and (or (null max-matches) (eq max-matches t) (integerp max-matches))
	     (or (setq actual-buf (rolo-buffer-exists-p rolo-buf))
		 (if (file-exists-p rolo-buf)
		     (setq actual-buf (find-file-noselect rolo-buf t)
			   new-buf-p t))))
	(let ((hdr-pos) (num-found 0) (curr-entry-level)
	      (incl-hdr t))
	  (if max-matches
	      (cond ((eq max-matches t)
		     (setq incl-hdr nil max-matches nil))
		    ((< max-matches 0)
		     (setq incl-hdr nil
			   max-matches (- max-matches)))))
	  (set-buffer actual-buf)
	  (if new-buf-p (setq buffer-read-only t))
	  (save-excursion
	    (save-restriction
	      (widen)
	      (goto-char 1)
	      ;; Ensure no entries in outline mode are hidden.
	      ;; Uses 'show-all' function from outline.el.
	      (and (search-forward "\C-m" nil t)
		   (show-all))
	      (if (re-search-forward rolo-hdr-regexp nil t 2)
		  (progn (forward-line)
			 (setq hdr-pos (cons (point-min) (point)))))
	      (re-search-forward rolo-entry-regexp nil t)
	      (while (and (or (null max-matches) (< num-found max-matches))
			  (re-search-forward regexp nil t))
		(re-search-backward rolo-entry-regexp nil t)
		(let ((start (point))
		      (next-entry-exists))
		  (re-search-forward rolo-entry-regexp nil t)
		  (setq curr-entry-level (buffer-substring start (point)))
		  (rolo-to-entry-end t curr-entry-level)
		  (or count-only
		      (if (and (= num-found 0) incl-hdr)
			  (let* ((src (or (buffer-file-name actual-buf)
					  actual-buf))
				 (src-line
				   (format
				     (concat (if (boundp 'hbut:source-prefix)
						 hbut:source-prefix
					       "@loc> ")
					     "%s")
				     (prin1-to-string src))))
			    (set-buffer rolo-display-buffer)
			    (goto-char (point-max))
			    (if hdr-pos
				(progn
				  (insert-buffer-substring
				    actual-buf (car hdr-pos) (cdr hdr-pos))
				  (insert src-line "\n\n"))
			      (insert (format rolo-hdr-format src-line)))
			    (set-buffer actual-buf))))
		  (setq num-found (1+ num-found))
		  (or count-only
		      (rolo-add-match rolo-display-buffer regexp start (point)))))))
	  (rolo-kill-buffer actual-buf)
	  num-found)
      0)))

(defun rolo-map-level (func rolo-buf level-regexp &optional max-groupings)
  "Perform FUNC on groupings of ROLO-BUF entries at level LEVEL-REGEXP,
to a maximum of optional argument MAX-GROUPINGS.  Nil value of MAX-GROUPINGS
means all groupings at the given level.  FUNC should take two arguments, the
start and the end of the region that it should manipulate.  LEVEL-REGEXP
should simply match the text of any rolodex entry of the given level, not the
beginning of a line (^); an example, might be (regexp-quote \"**\") to match
level two.  Returns number of groupings matched."
  (let ((actual-buf))
    (if (and (or (null max-groupings) (< 0 max-groupings))
	     (or (setq actual-buf (rolo-buffer-exists-p rolo-buf))
		 (if (file-exists-p rolo-buf)
		     (progn (setq actual-buf (find-file-noselect rolo-buf t))
			    t))))
	(progn
	  (set-buffer actual-buf)
	  (let ((num-found 0)
		(exact-level-regexp (concat "^\\(" level-regexp "\\)[ \t\n]"))
		(outline-regexp rolo-entry-regexp)
		(buffer-read-only)
		(level-len))
	    ;; Load 'outline' library since its functions are used here.
	    (if (not (boundp 'outline-mode-map))
		(load-library "outline"))
	    (goto-char (point-min))
	    ;; Pass buffer header if it exists
	    (if (re-search-forward rolo-hdr-regexp nil t 2)
		(forward-line))
	    (while (and (or (null max-groupings) (< num-found max-groupings))
			(re-search-forward exact-level-regexp nil t))
	      (setq num-found (1+ num-found))
	      (let* ((opoint (prog1 (point) (beginning-of-line)))
		     (grouping-start (point))
		     (start grouping-start)
		     (level-len (or level-len (- (1- opoint) start)))
		     (next-level-len)
		     (next-entry-exists)
		     (grouping-end)
		     (no-subtree))
		(while (and (progn
			      (if (setq next-entry-exists
					(re-search-forward
					 rolo-entry-regexp nil t 2))
				  (setq next-level-len
					(- (point)
					   (progn (beginning-of-line)
						  (point)))
					grouping-end
					(< next-level-len level-len)
					no-subtree
					(<= next-level-len level-len))
				(setq grouping-end t no-subtree t)
				(goto-char (point-max)))
			      (let ((end (point)))
				(goto-char start)
				(hide-subtree) ; And hide multiple entry lines
				;; Move to start of next entry at equal
				;; or higher level.
				(setq start
				      (if no-subtree
					  end
					(if (re-search-forward
					     rolo-entry-regexp nil t)
					    (progn (beginning-of-line) (point))
					  (point-max))))
				;; Remember last expression in 'progn'
				;; must always return non-nil.
				(goto-char start)))
			    (not grouping-end)))
		(let ((end (point)))
		  (goto-char grouping-start)
		  (funcall func grouping-start end)
		  (goto-char end))))
	    (show-all)
	    (rolo-kill-buffer actual-buf)
	    num-found))
      0)))

;;;
;;; Private functions
;;;

(defun rolo-add-match (rolo-matches-buffer regexp start end)
  "Insert before point in ROLO-MATCHES-BUFFER an entry matching REGEXP from the current region between START to END."
  (let ((rolo-buf (current-buffer))
	opoint)
    (set-buffer (get-buffer-create rolo-matches-buffer))
    (setq opoint (point))
    (insert-buffer-substring rolo-buf start end)
    (rolo-highlight-matches regexp opoint (point))
    (set-buffer rolo-buf)))

(defun rolo-buffer-exists-p (rolo-buf)
  "Returns buffer given by ROLO-BUF or nil.
ROLO-BUF may be a file-name, buffer-name, or buffer."
  (car (memq (get-buffer (or (and (stringp rolo-buf)
				  (get-file-buffer rolo-buf))
			     rolo-buf))
	     (buffer-list))))

(defun rolo-display-to-entry-end ()
  "Go to end of current entry, ignoring sub-entries."
  (if (re-search-forward (concat rolo-hdr-regexp "\\|"
				 rolo-entry-regexp) nil t)
      (progn (beginning-of-line) (point))
    (goto-char (point-max))))

	  
(defun rolo-format-name (name-str first last)
  "Reverse order of NAME-STR field given my regexp match field FIRST and LAST."
  (if (match-beginning last)
      (concat (substring name-str (match-beginning last) (match-end last))
	      ", "
	      (substring name-str (match-beginning first) (match-end first)))))

(defun rolo-highlight-matches (regexp start end)
  "Highlight matches for REGEXP in region from START to END."
  (if (fboundp 'hproperty:but-add)
      (let ((hproperty:but-emphasize-p))
	(save-excursion
	  (goto-char start)
	  (while (re-search-forward regexp nil t)
	    (hproperty:but-add (match-beginning 0) (match-end 0)
			       (or rolo-highlight-face
				   hproperty:highlight-face)))))))

(defun rolo-kill-buffer (&optional rolo-buf)
  "Kills optional ROLO-BUF if unchanged and 'rolo-kill-buffers-after-use' is t.
Default is current buffer."
  (or rolo-buf (setq rolo-buf (current-buffer)))
  (and rolo-kill-buffers-after-use (not (buffer-modified-p rolo-buf))
       (kill-buffer rolo-buf)))

(defun rolo-name-and-email ()
  "If point is in a mail message, returns list of (name email-addr) of sender.
Name is returned as 'last, first-and-middle'."
  (let ((email) (name) (from))
    (save-window-excursion
      (if (or (hmail:lister-p) (hnews:lister-p))
	  (other-window 1))
      (save-excursion
	(save-restriction
	  (goto-char (point-min))
	  (if (search-forward "\n\n" nil t)
	      (narrow-to-region (point-min) (point)))
	  (setq email (mail-fetch-field "reply-to")
		from  (mail-fetch-field "from")))))
    (if from
	(cond
	 ;; Match: email, email (name), email "name"
	 ((string-match
	   (concat "^\\([^\"<>() \t\n]+\\)"
		   "\\([ \t]*[(\"][ \t]*\\([^\"()]+\\)[ \t]+"
		   "\\([^\" \t()]+\\)[ \t]*[)\"]\\)?[ \t]*$")
	   from)
	  (setq name (rolo-format-name from 3 4))
	  (or email (setq email (substring from (match-beginning 1)
					   (match-end 1)))))
	 ;; Match: <email>, name <email>, "name" <email>
	 ((string-match
	   (concat "^\\(\"?\\([^\"<>()\n]+\\)[ \t]+"
		   "\\([^\" \t()<>]+\\)\"?[ \t]+\\)?"
		   "<\\([^\"<>() \t\n]+\\)>[ \t]*$")
	   from)
	  (setq name (rolo-format-name from 2 3))
	  (or email (setq email (substring from (match-beginning 4)
					   (match-end 4)))))))
    (if (or name email)
	(list name email))))

(defun rolo-name-at ()
  "If point is within an entry in 'rolo-display-buffer', returns entry, else nil."
  (if (string-equal (buffer-name) rolo-display-buffer)
      (save-excursion
	(if (or (looking-at rolo-entry-regexp)
		(progn (end-of-line)
		       (re-search-backward rolo-entry-regexp nil t)))
	    (progn (goto-char (match-end 0))
		   (skip-chars-forward " \t")
		   (if (or (looking-at "[^ \t\n\^M]+ ?, ?[^ \t\n\^M]+")
			   (looking-at "\\( ?[^ \t\n\^M]+\\)+"))
		       (buffer-substring (match-beginning 0)
					 (match-end 0))))))))

(defun rolo-narrowed-p ()
  (or (/= (point-min) 1) (/= (1+ (buffer-size)) (point-max))))

(defun rolo-save-buffer (&optional rolo-buf)
  "Saves optional ROLO-BUF if changed and 'rolo-save-buffers-after-use' is t.
Default is current buffer.  Used, for example, after a rolo entry is killed."
  (or rolo-buf (setq rolo-buf (current-buffer)))
  (and rolo-save-buffers-after-use (buffer-modified-p rolo-buf)
       (set-buffer rolo-buf) (save-buffer)))

(defun rolo-shrink-window ()
  (let* ((lines (count-lines (point-min) (point-max)))
	 (height (window-height))
	 (window-min-height 2)
	 (desired-shrinkage (1- (min (- height lines)))))
    (and (>= lines 0)
	 (/= desired-shrinkage 0)
	 (> (frame-height) (1+ height))
	 (shrink-window 
	   (if (< desired-shrinkage 0)
	       (max desired-shrinkage (- height (/ (frame-height) 2)))
  (min desired-shrinkage (- height window-min-height)))))))

(defun rolo-to (name &optional file-list)
  "Moves point to entry for NAME within optional FILE-LIST.
'rolo-file-list' is used as default when FILE-LIST is nil.
Leaves point immediately after match for NAME within entry.
Switches internal current buffer but does not alter the frame.
Returns point where matching entry begins or nil if not found."
  (or file-list (setq file-list rolo-file-list))
  (let ((found) file)
    (while (and (not found) file-list)
      (setq file (car file-list)
	    file-list (cdr file-list))
      (cond ((and file (or (not (stringp file)) (string= file "")))
	     (error "(rolo-to): Invalid file: '%s'" file))
	    ((and (file-exists-p file) (not (file-readable-p file)))
	     (error "(rolo-to): File not readable: '%s'" file)))
      (set-buffer (or (get-file-buffer file) (find-file-noselect file)))
      (let ((case-fold-search t) (real-name name) (parent "") (level) end)
	(widen) (goto-char 1)
	(while (setq end (string-match "/" name))
	  (setq level nil
		parent (substring name 0 end)
		name (substring name (min (1+ end) (length name))))
	  (cond ((progn
		   (while (and (not level) (search-forward parent nil t))
		     (save-excursion
		       (beginning-of-line)
		       (if (looking-at
			    (concat "\\(" rolo-entry-regexp "\\)[ \t]*" 
				    (regexp-quote parent)))
			   (setq level (buffer-substring (match-beginning 1)
							 (match-end 1))))))
		   level))
		((equal name real-name));; Try next file.
		(t;; Found parent but not child
		 (setq buffer-read-only nil)
		 (rolo-to-buffer (current-buffer))
		 (error "(rolo-to): '%s' part of name not found in \"%s\"."
			parent file)))
	  (if level
	      (narrow-to-region (point)
				(save-excursion
				  (rolo-to-entry-end t level) (point)))))
	(goto-char (point-min))
	(while (and (search-forward name nil t)
		    (not (save-excursion
			   (beginning-of-line)
			   (setq found
				 (if (looking-at
				      (concat "\\(" rolo-entry-regexp
					      "\\)[ \t]*"
					      (regexp-quote name)))
				     (point))))))))
      (or found (rolo-kill-buffer))) ;; conditionally kill
    (widen)
    found))

(defun rolo-to-buffer (buffer &optional other-window-flag frame)
  "Pop to BUFFER."
  (cond (hyperb:xemacs-p
	  (pop-to-buffer buffer other-window-flag
			 ;; default is to use selected frame
			 (or frame (selected-frame))))
	(t (pop-to-buffer buffer other-window-flag))))

(defun rolo-to-entry-end (&optional include-sub-entries curr-entry-level)
"Goes to end of whole entry if optional INCLUDE-SUB-ENTRIES is non-nil.
CURR-ENTRY-LEVEL is a string whose length is the same as the last found entry
header.  If INCLUDE-SUB-ENTRIES is nil, CURR-ENTRY-LEVEL is not needed.
Returns current point."
  (while (and (setq next-entry-exists
		    (re-search-forward rolo-entry-regexp nil t))
	      include-sub-entries
	      (> (- (point) (save-excursion
			      (beginning-of-line)
			      (point)))
		 (length curr-entry-level))))
  (if next-entry-exists
      (progn (beginning-of-line) (point))
    (goto-char (point-max))))

(defun wrolo-mode ()
  "Major mode for the rolodex match buffer.
Calls the functions given by `wrolo-mode-hook'.
\\{wrolo-mode-map}"
  (interactive)
  (setq major-mode 'wrolo-mode
	mode-name "Rolodex")
  (use-local-map wrolo-mode-map)
  ;;
  ;; Loads menus under non-tty InfoDock, XEmacs or Emacs19; does nothing
  ;; otherwise.
  (and (not (featurep 'wrolo-menu)) hyperb:window-system
       (or hyperb:xemacs-p hyperb:emacs19-p) (require 'wrolo-menu))
  ;;
  (if (not (fboundp 'outline-minor-mode))
      nil
    (outline-minor-mode 1))
  (run-hooks 'wrolo-mode-hook))

;;;
;;; Private variables
;;;

(defvar rolo-display-buffer "*Rolodex*"
  "Buffer used to display set of last matching rolodex entries.")

(defvar rolo-entry-regexp "^\\*+"
  "Regular expression to match the beginning of a rolodex entry.
This pattern must match the beginning of the line.  Entries may be nested
through the use of increasingly longer beginning patterns.")

(defconst rolo-hdr-format
  (concat
   "======================================================================\n"
   "%s\n"
   "======================================================================\n")
  "Header to insert preceding a file's first rolodex entry match when
file has none of its own.  Used with one argument, the file name."
)

(defconst rolo-hdr-regexp "^==="
  "Regular expression to match the first and last lines of rolodex file headers.
This header is inserted into rolo-display-buffer before any entries from the
file are added.")

(defconst rolo-match-regexp nil
  "Last regular expression used to search the rolodex.
Nil before a search is done.
String search expressions are converted to regular expressions.")

(defvar *rolo-wconfig* nil
  "Saves frame's window configuration prior to a rolodex search.")

(defvar wrolo-mode-map nil
  "Keymap for the rolodex match buffer.")

(if wrolo-mode-map
    nil
  (setq wrolo-mode-map (make-keymap))
  (if (fboundp 'set-keymap-name)
      (set-keymap-name wrolo-mode-map 'wrolo-mode-map))
  (suppress-keymap wrolo-mode-map)
  (define-key wrolo-mode-map "<"        'beginning-of-buffer)
  (define-key wrolo-mode-map ">"        'end-of-buffer)
  (define-key wrolo-mode-map "."        'beginning-of-buffer)
  (define-key wrolo-mode-map ","        'end-of-buffer)
  (define-key wrolo-mode-map "?"        'describe-mode)
  (define-key wrolo-mode-map "\177"     'scroll-down)
  (define-key wrolo-mode-map " "        'scroll-up)
  (define-key wrolo-mode-map "a"        'show-all)
  (define-key wrolo-mode-map "b"        'outline-backward-same-level)
  (define-key wrolo-mode-map "e"        'rolo-edit-entry)
  (define-key wrolo-mode-map "f"        'outline-forward-same-level)
  (define-key wrolo-mode-map "h"        'hide-subtree)
  (define-key wrolo-mode-map "m"        'rolo-mail-to)
  (define-key wrolo-mode-map "n"        'outline-next-visible-heading)
  (define-key wrolo-mode-map "p"        'outline-previous-visible-heading)
  (define-key wrolo-mode-map "q"        'rolo-quit)
  (define-key wrolo-mode-map "r"        'rolo-previous-match)
  (define-key wrolo-mode-map "s"        'show-subtree)
  (define-key wrolo-mode-map "\M-s"     'rolo-isearch)
  (define-key wrolo-mode-map "t"        'hide-body)
  (define-key wrolo-mode-map "\C-i"     'rolo-next-match)      ;; {TAB}
  (define-key wrolo-mode-map "\M-\C-i"  'rolo-previous-match)  ;; {M-TAB}
  (define-key wrolo-mode-map "u"        'outline-up-heading)
  )

(provide 'wrolo)

;;; wrolo.el ends here
