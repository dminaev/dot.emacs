;;; hmouse-tag.el --- Smart Key support of programming language tags location.

;; Copyright (C) 1991-1995, 2006 Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;; Maintainer: Mats Lidell <matsl@contactor.se>
;; Keywords: c, hypermedia, mouse, oop, tools

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
;;   Supports C, C++, Objective-C, Lisp, Fortran, and Assembly.
;;   See the GNU Emacs manual for information on how to create a TAGS file
;;     from the `etags' program.
;;   Does not support the `ctags' tags file format.
;;
;;   YOU MUST APPROPRIATELY SET THE PUBLIC VARIABLES BELOW BEFORE USE.
;;

;;; Code:

;;;
;;; Other required Elisp libraries
;;;

(if (cond ((or (featurep 'etags) (featurep 'tags))
	   nil)
	  ((or hyperb:xemacs-p hyperb:emacs19-p)
	   ;; Force use of .elc file here since otherwise the bin/etags
	   ;; executable might be found in a user's load-path by the load
	   ;; command.
	   (or (load "etags.elc" t nil t)
	       (load "tags-fix" t)))
	  ((load "tags" t)))
    (provide 'tags))

;;;
;;; Public variables
;;;

(defvar smart-asm-include-dirs nil
  "*Ordered list of directories to search for assembly language include files.
Each directory must end with a directory separator.")

(defconst smart-asm-include-regexp
  "[ \t*#|;]*\\(include\\|lib\\)[ \t]+\\([^ \t\n\^M]+\\)"
  "Regexp to match to assembly language include file lines.
Include keyword matched is grouping 1.  File name is grouping 2 but may be
missing its suffix, so add \".ins\" or \".inc\" if need be.
Examples include:
       INCLUDE GLOBALS
         should jump to file \"globals.ins\"
       lib conditionals_equ.inc
         should include \"conditionals_equ.inc\"")

(defvar smart-c-cpp-include-dirs '("/usr/include/")
  "*Ordered list of include directories by default searched by C/C++ preprocessor.
Each directory must end with a directory separator.  See also
'smart-c-include-dirs'.")

(defvar smart-c-include-dirs nil
  "*Ordered list of directories to search for C/C++ include files.
Each directory must end with a directory separator.  Directories normally
searched by the C/C++ pre-processor should be set instead in
'smart-c-cpp-include-dirs'.")

(defvar smart-c-use-lib-man nil
  "When non-nil makes 'smart-c' and 'smart-c++' display man pages for recognized lib symbols.
When nil, 'smart-c' and 'smart-c++' look up only symbols defined in an etags
TAGS file.

Create the file ~/.CLIBS-LIST and populate it with the full pathnames (one per
line) of all of the C/C++ libraries whose symbols you want to match against.
Your MANPATH environment variable must include paths for the man pages of
these libraries also.

Your smart-clib-sym executable script must output a 1 if a symbol is from a
C/C++ library listed in ~/.CLIBS-LIST or 0 if not!  Otherwise, don't set this
variable to t.")

(defconst smart-c-include-regexp
  "[ \t/*]*#[ \t]*\\(include\\|import\\)[ \t]+\\([\"<]\\)\\([^\">]+\\)[\">]"
  "Regexp to match to C, C++, or Objective-C include file lines.
Include keyword matched is grouping 1.  Type of include, user-specified via
double quote, or system-related starting with '<' is given by grouping 2.
File name is grouping 3.")

(defvar smart-emacs-tags-file nil
  "*Full path name of etags file for GNU Emacs source.")

;;;
;;; Public functions
;;;

(defun smart-asm (&optional identifier next)
  "Jumps to the definition of optional assembly IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching assembly tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If:
 (1) on an include statement, the include file is displayed;
     Look for include file in directory list 'smart-asm-include-dirs'.
 (2) on an identifier, the identifier definition is displayed,
     assuming the identifier is found within an 'etags' generated tag file
     in the current directory or any of its ancestor directories."

  (interactive)
  (or
   (if identifier nil (smart-asm-include-file))
   (let ((tag (or identifier (smart-asm-at-tag-p))))
     ;; Set free variable tags-file-name so that next 'find-tag' command uses
     ;; whatever tags file is set here.
     (setq tags-file-name (smart-tags-file buffer-file-name))
     (message "Looking for '%s' in '%s'..." tag tags-file-name)
     (condition-case ()
	 (progn
	   (funcall (if (br-in-browser)
			'find-tag 'find-tag-other-window)
		    tag next)
	   (message "Found definition for '%s'." tag))
       (error (message "'%s' not found in '%s'." tag tags-file-name)
	      (beep))))))

;;;###autoload
(defun smart-asm-at-tag-p ()
  "Return assembly tag name that point is within, else nil."
  (let* ((identifier-chars "_.$a-zA-Z0-9")
	 (identifier (concat "[_.$a-zA-Z][" identifier-chars "]*")))
    (save-excursion
      (skip-chars-backward identifier-chars)
      (if (looking-at identifier)
	  (buffer-substring (point) (match-end 0))))))

(defun smart-asm-include-file ()
  "If point is on an include file line, tries to display file.
Returns non-nil iff on an include file line, even if file is not found.
Look for include file in 'smart-asm-include-dirs' and add suffix \".ins\" or
\".inc\" to filename if it lacks a suffix." 
  (let ((opoint (point)))
    ;; Some assemblers utilize the C preprocessor, so try that first.
    (cond ((smart-c-include-file))
	  ((progn (beginning-of-line)
		  (looking-at smart-asm-include-regexp))
	   (let ((file (buffer-substring (match-beginning 2) (match-end 2)))
		 (path)
		 (dir-list smart-asm-include-dirs))
	     (goto-char opoint)
	     (setq dir-list (cons (file-name-directory buffer-file-name)
				  dir-list))
	     (if (string-match "\\." file)
		 (setq file (regexp-quote file))
	       (setq file (concat (regexp-quote file) "\\.in[sc]$")))
	     (while dir-list
	       (setq dir-list
		     (if (setq path (car (directory-files
					   (car dir-list) t file)))
			 nil
		       (cdr dir-list))))
	     ;;
	     ;; If path exists, display file
	     ;;
	     (if path
		 (if (and (file-readable-p path)
			  (progn
			    (if (br-in-browser)
				(find-file path)
			      (find-file-other-window path))
			    (cond ((featurep 'asm-mode) t)
				  ((load "asm-mode" nil 'nomessage)
				   (provide 'asm-mode))
				  (t
				    (beep)
				    (message
				      "(smart-asm-include-file):  asm-mode undefined.")
				    nil
				    ))))
		     nil
		   (beep)
		   (message "(smart-asm-include-file):  '%s' unreadable." path))
	       (beep)
	       (message "(smart-asm-include-file):  '%s' not found." file))
	     path))
	  ;; not on an include file line
	  (t (goto-char opoint)
	     nil))))


(defun smart-c (&optional identifier next)
  "Jumps to the definition of optional C IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching C tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If:
 (1) on a '#include' statement, the include file is displayed;
     Look for include file in directory lists 'smart-c-cpp-include-dirs'
     and 'smart-c-include-dirs'.
 (2) on a C identifier, the identifier definition is displayed,
     assuming the identifier is found within an 'etags' generated tag file
     in the current directory or any of its ancestor directories.
 (3) if 'smart-c-use-lib-man' is non-nil, the C identifier is
     recognized as a library symbol, and a man page is found for the
     identifier, then the man page is displayed."

  (interactive)
  (or
   (if identifier nil (smart-c-include-file))
   (let ((tag (or identifier (smart-c-at-tag-p))))
     ;; Set free variable tags-file-name so that next 'find-tag' command uses
     ;; whatever tags file is set here.
     (setq tags-file-name (smart-tags-file buffer-file-name))
     (message "Looking for '%s' in '%s'..." tag tags-file-name)
     (condition-case ()
	 (progn
	   (funcall (if (br-in-browser)
			'find-tag 'find-tag-other-window)
		    tag next)
	   (message "Found definition for '%s'." tag))
       (error
	(if (not smart-c-use-lib-man)
	    (progn (message "'%s' not found in '%s'." tag tags-file-name)
		   (beep))
	  (message "Checking if '%s' is a C library function..." tag)
	  (if (smart-library-symbol tag)
	      (progn (message "Displaying C library man page for '%s'." tag)
		     (manual-entry tag))
	    (message "'%s' not found in '%s' or C libraries."
		     tag tags-file-name)
	    (beep))))))))

;;;###autoload
(defun smart-c-at-tag-p ()
  "Return C tag name that point is within, else nil."
  (let* ((identifier-chars "_a-zA-Z0-9")
	 (identifier (concat "[_a-zA-Z][" identifier-chars "]*")))
    (save-excursion
      (skip-chars-backward identifier-chars)
      (if (looking-at identifier)
	  (buffer-substring (point) (match-end 0))))))

(defun smart-c-include-file ()
  "If point is on an include file line, tries to display file.
Returns non-nil iff on an include file line, even if file is not found.
Look for include file in 'smart-c-cpp-include-dirs' and in directory list
'smart-c-include-dirs'."
  (let ((opoint (point)))
    (beginning-of-line)
    (if (looking-at smart-c-include-regexp)
	(let ((incl-type (string-to-char
			  (buffer-substring (match-beginning 2)
					    (1+ (match-beginning 2)))))
	      (file (buffer-substring (match-beginning 3) (match-end 3)))
	      (path)
	      (dir-list smart-c-include-dirs)
	      (found))
	  (goto-char opoint)
	  (setq dir-list (if (= incl-type ?<)
			     (append dir-list smart-c-cpp-include-dirs)
			   (cons (file-name-directory buffer-file-name)
				 dir-list)))
	  (while dir-list
	    (setq path (expand-file-name file (car dir-list))
		  dir-list (if (setq found (file-exists-p path))
			       nil
			     (cdr dir-list))))
	  ;;
	  ;; If found, display file
	  ;;
	  (if found
	      (if (and (file-readable-p path)
		       (progn
			 (if (br-in-browser)
			     (find-file path)
			   (find-file-other-window path))
			 (cond ((or (featurep 'cc-mode)
				    (featurep 'c-mode))
				t)
			       ((or (load "cc-mode" 'missing-ok 'nomessage)
				    (load "c-mode" 'missing-ok 'nomessage))
				(provide 'c-mode))
			       (t
				(beep)
				(message
				 "(smart-c-include-file):  c-mode undefined.")
				nil
				))))
		  nil
		(beep)
		(message "(smart-c-include-file):  '%s' unreadable." path))
	    (beep)
	    (message "(smart-c-include-file):  '%s' not found." file))
	  path)
      (goto-char opoint)
      nil)))


;;;###autoload
(defun smart-c++ (&optional identifier next)
  "Jumps to the definition of optional C++ IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching C++ tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If:
 (1) on a '#include' statement, the include file is displayed;
     Look for include file in directory lists 'smart-c-cpp-include-dirs'
     and 'smart-c-include-dirs'.
 (2) on a C++ identifier, the identifier definition is displayed,
     assuming the identifier is found within an 'etags' generated tag file
     in the current directory or any of its ancestor directories.
 (3) if 'smart-c-use-lib-man' is non-nil, the C++ identifier is
     recognized as a library symbol, and a man page is found for the
     identifier, then the man page is displayed."

  (interactive)
  (or
   (if identifier nil (smart-c-include-file))
   (let ((tag (or identifier (smart-c++-at-tag-p))))
     ;; Set free variable tags-file-name so that next 'find-tag' command uses
     ;; whatever tags file is set here.
     (setq tags-file-name (smart-tags-file buffer-file-name))
     (message "Looking for '%s' in '%s'..." tag tags-file-name)
     (condition-case ()
	 (progn
	   (funcall (if (br-in-browser)
			'find-tag 'find-tag-other-window)
		    tag next)
	   (message "Found definition for '%s'." tag))
       (error
	(if (not smart-c-use-lib-man)
	    (progn (message "'%s' not found in '%s'." tag tags-file-name)
		   (beep))
	  (message "Checking if '%s' is a C++ library function..." tag)
	  (if (smart-library-symbol tag)
	      (progn (message "Displaying C++ library man page for '%s'." tag)
		     (manual-entry tag))
	    (message "'%s' not found in '%s' or C++ libraries."
		     tag tags-file-name)
	    (beep))))))))

;;; The following should be called only if the OO-Browser is available.
;;;###autoload
(defun smart-c++-oobr (&optional junk)
  "Jumps to the definition of selected C++ construct via OO-Browser support.
Optional JUNK is ignored.  Does nothing if the OO-Browser is not available.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If key is pressed:
 (1) on a '#include' statement, the include file is displayed;
     Look for include file in directory lists 'smart-c-cpp-include-dirs'
     and 'smart-c-include-dirs'.
 (2) within a method declaration, its definition is displayed;
 (3) on a class name, the class definition is shown.

 (2) and (3) require that an OO-Browser Environment has been loaded with
     the {M-x br-env-load RTN} command."

  (interactive)
  (c++-to-definition 'other-win))

(defun smart-c++-at-tag-p ()
  "Return C++ tag name that point is within, else nil."
  (let* ((identifier-chars "_:~<>a-zA-Z0-9")
	 (identifier (concat "\\([_~:<a-zA-Z][" identifier-chars "]*"
			     "[ \t]*[^]) \t:;.,?~{}][^[( \t:;.,~^!|?{}]?[=*]?\\)[ \t\n]*(")))
    (save-excursion
      (skip-chars-backward identifier-chars)
      (if (looking-at identifier)
	  (buffer-substring (point) (match-end 1))))))

(defun smart-emacs-lisp-mode-p ()
  "Return t if in a mode which uses Emacs Lisp symbols."
  (or (eq major-mode 'emacs-lisp-mode)
      (eq major-mode 'lisp-interaction-mode)
      (eq major-mode 'debugger-mode)
      ;; Emacs Lisp symbols appear in Help buffers frequently.
      (string-match "Help\\*$" (buffer-name))))

(defun smart-fortran (&optional identifier next)
  "Jumps to the definition of optional Fortran IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching Fortran tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If on a Fortran identifier, the identifier definition is displayed,
assuming the identifier is found within an 'etags' generated tag file
in the current directory or any of its ancestor directories."
  (interactive)
  (let ((tag (or identifier (smart-fortran-at-tag-p))))
    ;; Set free variable tags-file-name so that next 'find-tag' command uses
    ;; whatever tags file is set here.
    (setq tags-file-name (smart-tags-file buffer-file-name))
    (message "Looking for '%s' in '%s'..." tag tags-file-name)
    (condition-case ()
	(progn
	  (funcall (if (br-in-browser)
		       'find-tag 'find-tag-other-window)
		   tag next)
	  (message "Found definition for '%s'." tag))
      (error
       (message "'%s' not found in '%s'." tag tags-file-name)
       (beep)))))

;;;###autoload
(defun smart-fortran-at-tag-p ()
  "Return Fortran tag name that point is within, else nil."
  (let* ((identifier-chars "_a-zA-Z0-9")
	 (identifier (concat "[_a-zA-Z][" identifier-chars "]*")))
    (save-excursion
      (skip-chars-backward identifier-chars)
      (if (looking-at identifier)
	  (buffer-substring (point) (match-end 0))))))

(defun smart-lisp (&optional next)
  "Jumps to the definition of any selected Lisp construct.
If on an Emacs Lisp require, load, or autoload clause and 'find-library'
from load-library package by Hallvard Furuseth (hallvard@ifi.uio.no) has
been loaded, jumps to library source, if possible.

Otherwise, the construct must be found within an 'etags' generated tag file
in the current directory or any of its ancestor directories in order for its
definition to be located.

Optional NEXT means jump to next matching Lisp tag.  When matching to an Emacs
Lisp tag using 'wtags' (Bob Weiner's personal modifications to 'etags'),
there is no next tag, so display documentation for current tag instead.

This command assumes that its caller has already checked that the key was
pressed in an appropriate buffer and has moved the cursor to the selected
buffer."

  (interactive)
  ;; Handle 'require', 'load', and 'autoload' clauses in Emacs Lisp.
  (or (and (fboundp 'find-library)
	   (smart-emacs-lisp-mode-p)
	   (let ((req)
		 (opoint (point)))
	     (setq req (and (search-backward "\(" nil t)
			    (looking-at (concat
					 "(\\(require\\|load\\|autoload\\)"
					 "[ \t]+.*['\"]"
					 "\\([^][() \t\n\^M`'\"]+\\)"))))
	     (goto-char opoint)
	     (if req (progn
		       (setq req (buffer-substring (match-beginning 2)
						   (match-end 2)))
		       (pop-to-buffer nil t)
		       (find-library req)
		       t))))
      (let ((tag (smart-lisp-at-tag-p)))
	;; Set free variable tags-file-name so that next 'find-tag' command
	;; uses whatever tags file is set here.
	(setq tags-file-name (smart-tags-file default-directory))
	;; This part only works properly for Emacs Lisp, so is conditionalized.
	(if (and next (smart-emacs-lisp-mode-p) (featurep 'wtags))
	    (progn (setq tag (intern tag))
		   (cond ((fboundp tag) (describe-function tag))
			 ((boundp tag) (describe-variable tag))
			 (t (error "(smart-lisp): Unbound symbol: %s" tag))))
	  (condition-case ()
	      (funcall (if (br-in-browser)
			   'find-tag 'find-tag-other-window)
		       tag next)
	    (error (if (equal tags-file-name smart-emacs-tags-file)
		       nil
		     (setq tags-file-name smart-emacs-tags-file)
		     (funcall (if (br-in-browser)
				  'find-tag 'find-tag-other-window)
			      tag next))))))))

(defun smart-lisp-at-tag-p ()
  "Returns Lisp tag name that point is within, else nil.
Returns nil when point is on the first line of a 'def' form past the first 4
characters."
  (let* ((identifier-chars "-_*:+%$#!<>a-zA-Z0-9")
	 (identifier (concat "[-<*a-zA-Z][" identifier-chars "]*"))
	 (opoint (point)))
    (save-excursion
      (beginning-of-line)
      (if (and (looking-at "\\(;*[ \t]*\\)?(def[^- \n\t]+[ \n\t]")
	       (> opoint (match-end 0)))
	  nil
	(goto-char opoint)
	(skip-chars-backward identifier-chars)
	(if (looking-at identifier)
	    (buffer-substring (point) (match-end 0)))))))

(defun smart-lisp-mode-p ()
  "Return t if in a mode which uses Lisp symbols."
  (or (smart-emacs-lisp-mode-p)
      (eq major-mode 'lisp-mode)
      (eq major-mode 'scheme-mode)))

;;;###autoload
(defun smart-objc (&optional identifier next)
  "Jumps to the definition of optional Objective-C IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching Objective-C tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If:
 (1) on a '#include' statement, the include file is displayed;
     Look for include file in directory lists 'smart-c-cpp-include-dirs'
     and 'smart-c-include-dirs'.
 (2) on an Objective-C identifier, the identifier definition is displayed,
     assuming the identifier is found within an 'etags' generated tag file
     in the current directory or any of its ancestor directories.
 (3) if 'smart-c-use-lib-man' is non-nil, the Objective-C identifier is
     recognized as a library symbol, and a man page is found for the
     identifier, then the man page is displayed."

  (interactive)
  (or
   (if identifier nil (smart-c-include-file))
   (let ((tag (or identifier (smart-objc-at-tag-p))))
     ;; Set free variable tags-file-name so that next 'find-tag' command uses
     ;; whatever tags file is set here.
     (setq tags-file-name (smart-tags-file buffer-file-name))
     (message "Looking for '%s' in '%s'..." tag tags-file-name)
     (condition-case ()
	 (progn
	   (funcall (if (br-in-browser)
			'find-tag 'find-tag-other-window)
		    tag next)
	   (message "Found definition for '%s'." tag))
       (error
	(if (not smart-c-use-lib-man)
	    (progn (message "'%s' not found in '%s'." tag tags-file-name)
		   (beep))
	  (message
	   "Checking if '%s' is an Objective-C library function..." tag)
	  (if (smart-library-symbol tag)
	      (progn
		(message
		 "Displaying Objective-C library man page for '%s'." tag)
		(manual-entry tag))
	    (message "'%s' not found in '%s' or Objective-C libraries."
		     tag tags-file-name)
	    (beep))))))))

;;; The following should be called only if the OO-Browser is available.
;;;###autoload
(defun smart-objc-oobr (&optional junk)
  "Jumps to the definition of selected Objective-C construct via OO-Browser support.
Optional JUNK is ignored.  Does nothing if the OO-Browser is not available.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If key is pressed:
 (1) on a '#include' statement, the include file is displayed;
     Look for include file in directory lists 'smart-c-cpp-include-dirs'
     and 'smart-c-include-dirs'.
 (2) within a method declaration, its definition is displayed;
 (3) on a class name, the class definition is shown.

 (2) and (3) require that an OO-Browser Environment has been loaded with
     the {M-x br-env-load RTN} command."

  (interactive)
  (objc-to-definition 'other-win))

(defun smart-objc-at-tag-p ()
  "Return Objective-C tag name that point is within, else nil."
  (let* ((identifier-chars "_a-zA-Z0-9")
	 (identifier
	  (concat "\\([-+][ \t]*\\)?\\([_a-zA-Z][" identifier-chars "]*\\)")))
    (save-excursion
      (skip-chars-backward identifier-chars)
      (if (looking-at identifier)
	  (buffer-substring (match-beginning 2) (match-end 2))))))

;;;
;;; Private functions
;;;

(defun smart-library-symbol (tag)
  "Return non-nil if TAG is a library symbol listed in cache of such symbols.
See the \"${hyperb:dir}/smart-clib-sym\" script for more information."
  (let ((buf (get-buffer-create "*junk*"))
	(found))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (call-process (expand-file-name "smart-clib-sym" hyperb:dir)
		    nil buf nil tag)
      (setq found (string-equal (buffer-substring 1 2) "1"))
      (set-buffer-modified-p nil)
      (kill-buffer buf)
      found)))

;;;###autoload
(defun smart-tags-file-path (file)
  "Expand relative FILE name by looking it up in the nearest tags file.
Return FILE unchanged if it exists relative to the current directory or
cannot be expanded via a tags file."
  (or (cond ((or (file-exists-p file) (file-name-absolute-p file)) file)
	    (t (let ((tags-file (smart-tags-file default-directory))
		     (file-regexp
		      (concat "\^L\n\\(.*/\\)?" (regexp-quote file) ",")))
		 (if tags-file
		     (progn
		       (set-buffer (find-file-noselect tags-file))
		       (goto-char (point-min))
		       (if (re-search-forward file-regexp nil t)
			   (expand-file-name
			    (buffer-substring (1- (match-end 0))
					      (progn (beginning-of-line)
						     (point))))))))))
      file))

;;;###autoload
(defun smart-tags-file (curr-filename)
  "Return appropriate tags file name for CURR-FILENAME or 'tags-file-name'."
  (let ((path curr-filename)
	(tags-file))
    (while (and
	    (stringp path)
	    (setq path (file-name-directory path))
	    (setq path (directory-file-name path))
	    ;; Not at root directory
	    (not (string-match ":?/\\'" path))
	    ;; No tags file
	    (not (file-exists-p
		  (setq tags-file (expand-file-name "TAGS" path)))))
      (setq tags-file nil))
    (if (and (not tags-file)
	     (stringp curr-filename)
	     (smart-emacs-lisp-mode-p)
	     (let ((path (file-name-directory curr-filename)))
	       (delq nil (mapcar
			  (function
			   (lambda (p)
			     (and p (equal (file-name-as-directory p)
					   path))))
			  load-path))))
	(setq tags-file smart-emacs-tags-file))
    (or tags-file tags-file-name
	(call-interactively 'visit-tags-table))))

;;;
;;; Private variables
;;;

(provide 'hmouse-tag)

;;; hmouse-tag.el ends here
