;;; hibtypes.el --- Hyperbole System Implicit Button Types.

;; Copyright (C) 2004 Free Software Foundation, Inc.
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

(require 'hactypes)

;;;
;;; Public implicit button types
;;;
  
(run-hooks 'hibtypes:begin-load-hook)

;;;
;;; Follows URLs by invoking a browser.
;;;

(require 'hsys-w3)

;;;
;;; Handles internal references within an annotated bibliography, delimiters=[]
;;;

(defib annot-bib ()
  "Displays annotated bibliography entries referenced internally.
References must be delimited by square brackets, must begin with a word
constituent character, and must not be in buffers whose names begin with a
' ' or '*' character or which do not have an attached file."
  (and (not (bolp))
       buffer-file-name
       (let ((chr (aref (buffer-name) 0)))
	 (not (or (= chr ? ) (= chr ?*))))
       (let* ((ref-and-pos (hbut:label-p t "[" "]" t))
	      (ref (car ref-and-pos)))
	 (and ref (= ?w (char-syntax (aref ref 0)))
	      (progn (ibut:label-set ref-and-pos)
		     (hact 'annot-bib ref))))))

;;;
;;; Summarizes an Internet rfc for random access browsing by section.
;;;

(defib rfc-toc ()
  "Summarizes contents of an Internet rfc from anywhere within rfc buffer.
Each line in summary may be selected to jump to section."
  (let ((case-fold-search t)
	(toc)
	(opoint (point)))
    (if (and (string-match "rfc" (buffer-name))
	     (goto-char (point-min))
	     (progn (setq toc (search-forward "Table of Contents" nil t))
		    (re-search-forward "^[ \t]*1.0?[ \t]+[^ \t\n]" nil t
				       (and toc 2))))
	(progn (beginning-of-line)
	       (ibut:label-set (buffer-name))
	       (hact 'rfc-toc (buffer-name) opoint))
      (goto-char opoint)
      nil)))

;;;
;;; Jumps to C/C++ source line associated with Cscope C analyzer output line.
;;;

(defib cscope ()
  "Jumps to C/C++ source line associated with Cscope C analyzer output line.
Requires pre-loading of the cscope.el Lisp library available from the Emacs
Lisp archives and the commercial cscope program available from UNIX System
Laboratories.  Otherwise, does nothing."
  (and (boundp 'cscope:bname-prefix)  ;; (featurep 'cscope)
       (stringp cscope:bname-prefix)
       (string-match (regexp-quote cscope:bname-prefix)
		     (buffer-name))
       (= (match-beginning 0) 0)
       (save-excursion
	 (beginning-of-line)
	 (looking-at cscope-output-line-regexp))
       (let (start end)
	 (skip-chars-backward "^\n\^M")
	 (setq start (point))
	 (skip-chars-forward "^\n\^M")
	 (setq end (point))
	 (ibut:label-set (buffer-substring start end)
			 start end)
	 (hact 'cscope-interpret-output-line))))

;;;
;;; Makes README table of contents entries jump to associated sections.
;;;

(defib text-toc ()
  "Jumps to the text file section referenced by a table of contents entry at point.
File name must contain README and there must be a `Table of Contents' or
`Contents' label on a line by itself (it may begin with an asterisk),
preceding the table of contents.  Each toc entry must begin with some
whitespace followed by one or more asterisk characters.  Each file section
name line must start with one or more asterisk characters at the very
beginning of the line."
  (let (section)
    (if (and (string-match "README" (buffer-name))
	     (save-excursion
	       (beginning-of-line)
	       (if (looking-at
		    "[ \t]+\\*+[ \t]+\\(.*[^ \t]\\)[ \t]*$")
		   (setq section (buffer-substring (match-beginning 1)
						   (match-end 1)))))
	     (progn (ibut:label-set section (match-beginning 1) (match-end 1))
		    t)
	     (save-excursion (re-search-backward
			      "^\\**[ \t]*\\(\\|Table of \\)Contents[ \t]*$"
			      nil t)))
	(hact 'text-toc section))))

;;;
;;; Makes directory summaries into file list menus.
;;;

(defib dir-summary ()
  "Detects filename buttons in files named \"MANIFEST\" or \"DIR\".
Displays selected files.  Each file name must be at the beginning of the line
or may be preceded by some semicolons and must be followed by one or more
spaces and then another non-space, non-parenthesis, non-brace character."
  (if buffer-file-name
      (let ((file (file-name-nondirectory buffer-file-name))
	    entry start end)
	(if (or (string= file "DIR") (string= file "MANIFEST"))
	    (save-excursion
	      (beginning-of-line)
	      (if (looking-at
		   "\\(;+[ \t]*\\)?\\([^(){}* \t\n]+\\)[ \t]+[^(){}* \t\n]")
		  (progn
		    (setq entry (buffer-substring
				 (match-beginning 2) (match-end 2))
			  start (match-beginning 2)
			  end (match-end 2))
		    (if (file-exists-p entry)
			(progn (ibut:label-set entry start end)
			       (hact 'link-to-file entry))))))))))

;;;
;;; Executes or documents command bindings of brace delimited key sequences.
;;;

(require 'hib-kbd)

;;;
;;; Makes Internet RFC references retrieve the RFC.
;;;

(defib rfc ()
  "Retrieves and displays an Internet rfc referenced at point.
Requires ange-ftp or efs when needed for remote retrievals.  The following
formats are recognized: RFC822, rfc-822, and RFC 822.  The 'hpath:rfc'
variable specifies the location from which to retrieve RFCs."
  (let ((case-fold-search t)
	(rfc-num nil))
    (and (not (memq major-mode '(dired-mode monkey-mode)))
	 (boundp 'hpath:rfc)
	 (stringp hpath:rfc)
	 (save-excursion
	   (skip-chars-backward "-rRfFcC0-9")
	   (if (looking-at "rfc[- ]?\\([0-9]+\\)")
	       (progn
		 (setq rfc-num 
		       (buffer-substring
			(match-beginning 1) (match-end 1)))
		 (ibut:label-set
		  (buffer-substring (match-beginning 0) (match-end 0)))
		 t)))
	 ;; Ensure ange-ftp is available for retrieving a remote
	 ;; RFC, if need be.
	 (if (string-match "^/.+:" hpath:rfc)
	     ;; This is a remote path.
	     (hpath:ange-ftp-available-p)
	   ;; local path
	   t)
	 (hact 'link-to-rfc rfc-num))))

;;;
;;; Makes Hyperbole mail addresses output Hyperbole envir info.
;;;

(defib hyp-address ()
  "Turns a Hyperbole e-mail list address into an implicit button which inserts Hyperbole environment information.
Useful when sending mail to a Hyperbole mail list.
See also the documentation for `actypes::hyp-config'."
  (if (memq major-mode (list hmail:composer hnews:composer))
      (let ((addr (find-tag-default)))
	(cond ((set:member addr (list "hyperbole" "hyperbole-users@gnu.org"))
	       (hact 'hyp-config))
	      ))))

;;;
;;; Makes source entries in Hyperbole reports selectable.
;;;

(defib hyp-source ()
  "Turns source location entries in Hyperbole reports into buttons that jump to the associated location."
  (save-excursion
    (beginning-of-line)
    (if (looking-at hbut:source-prefix)
	(let ((src (hbut:source)))
	  (if src
	      (progn (if (not (stringp src)) (setq src (prin1-to-string src)))
		     (ibut:label-set src (point) (progn (end-of-line) (point)))
		     (hact 'hyp-source src)))))))

;;;
;;; Shows man page associated with a man apropos entry.
;;;

(defib man-apropos ()
  "Makes man apropos entries display associated man pages when selected."
  (save-excursion
    (beginning-of-line)
    (let ((nm "[^ \t\n!@,][^ \t\n,]*")
	  topic)
      (and (looking-at
	    (concat
	     "^\\(\\*[ \t]+[!@]\\)?\\(" nm "[ \t]*,[ \t]*\\)*\\(" nm "\\)[ \t]*"
	     "\\(([-0-9a-zA-z]+)\\)\\(::\\)?[ \t]+-[ \t]+[^ \t\n]"))
	   (setq topic
		 (concat (buffer-substring (match-beginning 3) (match-end 3))
			 (buffer-substring (match-beginning 4) (match-end 4))))
	   (ibut:label-set topic (match-beginning 3) (match-end 4))
	   (hact 'man-show topic)))))

;;;
;;; Follows links to Hyperbole outliner cells.
;;;

(if hyperb:kotl-p (require 'klink))

;;;
;;; Displays files and directories when double quoted pathname is activated.
;;;

(defib pathname ()
  "Makes a delimited, valid pathname display the path entry.
Also works for delimited and non-delimited ange-ftp and efs pathnames.
See `hpath:at-p' function documentation for possible delimiters.
See `hpath:suffixes' variable documentation for suffixes that are added to or
removed from pathname when searching for a valid match.
See `hpath:find' function documentation and `hpath:display-alist' and
`hpath:find-alist' variable documentation for special file display options."
     (let ((path (hpath:at-p)))
       (if path
	   (progn (ibut:label-set path)
		  (hact 'link-to-file path)))))

;;;
;;; Jumps to source line associated with debugger stack frame or breakpoint
;;; lines.  Supports gdb, dbx, and xdb.
;;;

(defib debugger-source ()
  "Jumps to source line associated with debugger stack frame or breakpoint lines.
This works with gdb, dbx, and xdb.  Such lines are recognized in any buffer."
  (save-excursion
    (beginning-of-line)
    (cond  ((looking-at ".+ \\(at\\|file\\) \\([^ :]+\\):\\([0-9]+\\)\\.?$")
	   ;; GDB
	   (let* ((file (buffer-substring (match-beginning 2)
					  (match-end 2)))
		  (line-num (buffer-substring (match-beginning 3)
					      (match-end 3)))
		  (but-label (concat file ":" line-num)))
	     (setq line-num (string-to-int line-num))
	     (ibut:label-set but-label)
	     (hact 'link-to-file-line file line-num)))
	   ((looking-at ".+ (file=[^\"\n]+\"\\([^\"\n]+\\)\", line=\\([0-9]+\\),")
	   ;; XEmacs assertion failure
	   (let* ((file (buffer-substring (match-beginning 1)
					  (match-end 1)))
		  (line-num (buffer-substring (match-beginning 2)
					      (match-end 2)))
		  (but-label (concat file ":" line-num)))
	     (setq line-num (string-to-int line-num))
	     (ibut:label-set but-label)
	     (hact 'link-to-file-line file line-num)))
	  ((looking-at ".+ line \\([0-9]+\\) in \"\\([^\"]+\\)\"$")
	   ;; New DBX
	   (let* ((file (buffer-substring (match-beginning 2)
					  (match-end 2)))
		  (line-num (buffer-substring (match-beginning 1)
					      (match-end 1)))
		  (but-label (concat file ":" line-num)))
	     (setq line-num (string-to-int line-num))
	     (ibut:label-set but-label)
	     (hact 'link-to-file-line file line-num)))
	  ((or (looking-at ".+ \\[\"\\([^\"]+\\)\":\\([0-9]+\\),") ;; Old DBX
	       (looking-at ".+ \\[\\([^: ]+\\): \\([0-9]+\\)\\]")) ;; HP-UX xdb
	   (let* ((file (buffer-substring (match-beginning 1)
					  (match-end 1)))
		  (line-num (buffer-substring (match-beginning 2)
					      (match-end 2)))
		  (but-label (concat file ":" line-num)))
	     (setq line-num (string-to-int line-num))
	     (ibut:label-set but-label)
	     (hact 'link-to-file-line file line-num))))))

;;;
;;; Jumps to source line associated with grep or compilation error messages.
;;; With credit to Michael Lipp and Mike Williams for the idea.
;;;

(defib grep-msg ()
  "Jumps to line associated with grep or compilation error msgs.
Messages are recognized in any buffer."
  (progn
    (if (equal (buffer-name) "*compilation*")
	(progn
	  (require 'compile)
	  ;; Make sure we have a parsed error-list
	  (if (eq compilation-error-list t)
	      (progn (compilation-forget-errors)
		     (setq compilation-parsing-end 1)))
	  (if (not compilation-error-list)
	      (save-excursion
		(set-buffer-modified-p nil)
		(condition-case ()
		    ;; Emacs V19 incompatibly adds two non-optional arguments
		    ;; over V18.
		    (compilation-parse-errors nil nil)
		  (error (compilation-parse-errors)))))))
    ;; Locate and parse grep messages found in any buffer.
    (save-excursion
      (beginning-of-line)
      (if (or
	    ;; UNIX C compiler and Introl 68HC11 C compiler errors
	    (looking-at "\\([^ \t\n\^M:]+\\): ?\\([0-9]+\\)[ :]")
	    ;; BSO/Tasking 68HC08 C compiler errors
	    (looking-at
	     "[a-zA-Z 0-9]+: \\([^ \t\n\^M]+\\) line \\([0-9]+\\)[ \t]*:")
	    ;; UNIX Lint errors
	    (looking-at "[^:]+: \\([^ \t\n\^M:]+\\): line \\([0-9]+\\):")
	    ;; SparcWorks C compiler errors (ends with :)
	    ;; IBM AIX xlc C compiler errors (ends with .)
	    (looking-at "\"\\([^\"]+\\)\", line \\([0-9]+\\)[:.]")
	    ;; Introl as11 assembler errors
	    (looking-at " \\*+ \\([^ \t\n\^M]+\\) - \\([0-9]+\\) ")
	    ;; perl5: ... at file.c line 10
	    (looking-at ".+ at \\([^ \t\n]+\\) line +\\([0-9]+\\)")
	    )
	  (let* ((file (buffer-substring (match-beginning 1)
					 (match-end 1)))
		 (line-num (buffer-substring (match-beginning 2)
					     (match-end 2)))
		 (but-label (concat file ":" line-num))
		 (source-loc (hbut:key-src t)))
	    (if (stringp source-loc)
		(setq file (expand-file-name
			    file (file-name-directory source-loc))))
	    (setq line-num (string-to-int line-num))
	    (ibut:label-set but-label)
	    (hact 'link-to-file-line file line-num))))))

;;;
;;; Jumps to source of Emacs Lisp V19 byte-compiler error messages.
;;;

(defib elisp-compiler-msg ()
  "Jumps to source code for definition associated with byte-compiler error message.
Works when activated anywhere within an error line."
  (if (or (equal (buffer-name) "*Compile-Log*")
	  (equal (buffer-name) "*compilation*")
	  (save-excursion
	    (and (re-search-backward "^[^ \t\n\r]" nil t)
		 (looking-at "While compiling"))))
      (let (src buffer-p label)
	(and (save-excursion
	       (re-search-backward
		"^While compiling [^\t\n]+ in \\(file\\|buffer\\) \\([^ \n]+\\):$"
		nil t))
	     (setq buffer-p
		   (equal (buffer-substring (match-beginning 1) (match-end 1))
			  "buffer")
		   src (buffer-substring (match-beginning 2) (match-end 2)))
	     (save-excursion
	       (end-of-line)
	       (re-search-backward "^While compiling \\([^ \n]+\\)\\(:$\\| \\)"
				   nil t))
	     (progn
	       (setq label (buffer-substring
			    (match-beginning 1) (match-end 1)))
	       (ibut:label-set label (match-beginning 1) (match-end 1))
	       ;; Remove prefix generated by actype and ibtype definitions.
	       (setq label (hypb:replace-match-string "[^:]+::" label "" t))
	       (hact 'link-to-regexp-match
		     (concat "^\(def[a-z \t]+" (regexp-quote label)
			     "[ \t\n\(]")
		     1 src buffer-p))))))

;;;
;;; Jumps to source associated with a line of output from 'patch'.
;;;

(defib patch-msg ()
  "Jumps to source code associated with output from the 'patch' program.
Patch applies diffs to source code."
  (if (save-excursion
	(beginning-of-line)
	(looking-at "Patching \\|Hunk "))
      (let ((opoint (point))
	    (file) line)
	(beginning-of-line)
	(cond ((looking-at "Hunk .+ at \\([0-9]+\\)")
	       (setq line (buffer-substring (match-beginning 1)
					    (match-end 1)))
	       (ibut:label-set line (match-beginning 1) (match-end 1))
	       (if (re-search-backward "^Patching file \\(\\S +\\)" nil t)
		   (setq file (buffer-substring (match-beginning 1)
						(match-end 1)))))
	      ((looking-at "Patching file \\(\\S +\\)")
	       (setq file (buffer-substring (match-beginning 1)
					    (match-end 1))
		     line "1")
	       (ibut:label-set file (match-beginning 1) (match-end 1))))
	(goto-char opoint)
	(if (null file)
	    nil
	  (setq line (string-to-int line))
	  (hact 'link-to-file-line file line)))))

;;;
;;; Composes mail, in another window, to the e-mail address at point.
;;;

(defib mail-address ()
  "If on an e-mail address in a specific buffer type, mail to that address in another window.
Applies to the rolodex match buffer, any buffer attached to a file in
'rolo-file-list', or any buffer with \"mail\" or \"rolo\" (case-insensitive)
within its name."
  (if (or (and (let ((case-fold-search t))
		 (string-match "mail\\|rolo" (buffer-name)))
	       ;; Don't want this to trigger in a mail/news summary buffer.
	       (not (or (hmail:lister-p) (hnews:lister-p))))
	  (if (boundp 'rolo-display-buffer)
	      (equal (buffer-name) rolo-display-buffer))
	  (and buffer-file-name
	       (boundp 'rolo-file-list)
	       (set:member (current-buffer)
			   (mapcar 'get-file-buffer rolo-file-list))))
      (let ((address (mail-address-at-p)))
	(if address
	    (progn
	      (ibut:label-set address (match-beginning 1) (match-end 1))
	      (hact 'mail-other-window nil address))))))

(defconst mail-address-regexp
  "\\([_a-zA-Z][-_a-zA-Z0-9.!@+%]*@[-_a-zA-Z0-9.!@+%]+\\.[a-zA-Z][-_a-zA-Z][-_a-zA-Z]?\\|[a-zA-Z][-_a-zA-Z0-9.!+%]+@[-_a-zA-Z0-9@]+\\)\\($\\|[^a-zA-Z0-9.!@%]\\)"
  "Regexp with group 1 matching an Internet email address.")

(defun mail-address-at-p ()
  "Return e-mail address, a string, that point is within or nil."
  (save-excursion
    (skip-chars-backward "^ \t\n\^M\"\'(){}[];<>|")
    (if (looking-at mail-address-regexp)
	(buffer-substring (match-beginning 1) (match-end 1)))))
  
;;;
;;; Displays Info nodes when double quoted "(file)node" button is activated.
;;;

(defib Info-node ()
  "Makes \"(file)node\" buttons display the associated Info node."
  (let* ((node-ref-and-pos (hbut:label-p t "\"" "\"" t))
	 (node-ref (hpath:is-p (car node-ref-and-pos) nil t)))
    (and node-ref (string-match "([^\)]+)" node-ref)
	 (ibut:label-set node-ref-and-pos)
	 (hact 'link-to-Info-node node-ref))))

;;;
;;; Inserts completion into minibuffer or other window.
;;;

(defib completion ()
  "Inserts completion at point into minibuffer or other window."
  (let ((completion (hargs:completion t)))
    (and completion
	 (ibut:label-set completion)
	 (hact 'completion))))


(run-hooks 'hibtypes:end-load-hook)
(provide 'hibtypes)

;;; hibtypes.el ends here
