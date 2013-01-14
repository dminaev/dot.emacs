;;; hsite-ex.el --- Site-specific setup for Hyperbole

;; Copyright (C) 1991-1995, 2004, 2006 Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;; Maintainer: Mats Lidell <matsl@contactor.se>
;; Keywords: hypermedia, local

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
;;   See the "README" file for installation instructions.
;;
;;   "hsite.el" may be byte-compiled if you like but normally it is not.
;;
;;   Be sure to have users load any personal mail/news initializations
;;   before they load this file if any of Hyperbole's mail or news
;;   support features are enabled either herein or within their personal
;;   Hyperbole initializations.  Otherwise, the mail/news support may
;;   not be configured properly.
;;

;;; Code:

;;; 
;;; Read the comments and modify as desired.
;;; 

(message "Initializing Hyperbole, please wait...")

;;;
;;; SMART SETTINGS
;;;

(defvar hkey-always-display-menu nil
  "*Non-nil means always display the Smart Menu window when the Action or Assist Key is pressed and the Smart Menu system has been loaded.
If a Smart Menu is already displayed, perform another Action or Assist Key
function.")

(defcustom smart-scroll-proportional t
  "*Non-nil means Smart Keys should scroll relative to current line when pressed at the end of a line.
Action Key moves current line to top of window.  Assist Key moves current
line to bottom of window.  Repeated presses then scroll up or down a
windowful.  Nil value instead ignores current line and always scrolls up or
down a windowful."
  :group 'hyperbole
  :type 'boolean
  :link '(custom-manual "(hyperbole)Glossary"))

;;;
;;; HYPERBOLE DIRECTORY SETTING
;;;

(require 'hyperbole)

;;;
;;; INTERNET SETTINGS
;;;

;; String to be used in the call: (hpath:rfc rfc-num) to create an ange-ftp
;; path to the RFC document for 'rfc-num'.  Uncomment and alter this setting
;; if another site is closer for you.
;; (setq hpath:rfc "/anonymous@ds.internic.net:rfc/rfc%s.txt")

;; When a user creates an explicit button, Hyperbole tries to store her
;; Internet e-mail address with the button by using the formula, email-id =
;; <user-id>@<domainname>.  Not every system has its domainname set
;; up properly, however.  If you do a {M-x load-file hypb.elc RET} and then
;; hit {C-x C-e} after the closing paren of the following function,
;; (hypb:domain-name), you will see whether or not yours is configured
;; properly.  If it is not, uncomment the following line and set it to the
;; proper value.

;; (setenv "DOMAINNAME" "mot.com")

;;;
;;; XEMACS, GNU EMACS 19, AND EPOCH CONFIGURATION
;;;

;; No-op unless set by one of the conditionals below.
(defun hui:but-flash ())

(if (and hyperb:emacs19-p window-system)
    (progn
      (require 'hui-em19-b)
      ;; Highlight explicit buttons whenever a file is read in.
      (var:append 'find-file-hooks '(hproperty:but-create))
      (fset 'hui:but-flash 'hproperty:but-flash)
      ;;
      ;; Substitute for the nil argument below a valid X color name with
      ;; which to highlight buttons if the default highlighting does not
      ;; appeal to you. See "hui-em19-b.el" for how this works.
      (hproperty:cycle-but-color nil)
      ;;
      ;; Non-nil means visually emphasize that button under mouse cursor is
      ;; selectable.
      (setq hproperty:but-emphasize-p nil)
      ;;
      ;; If you find that the Hyperbole button flash time is too slow
      ;; or too fast, adjust it here.
      (setq hproperty:but-flash-time 1000)
      ))

(if (and hyperb:xemacs-p (not noninteractive))
    (progn
      (require 'hui-xe-but)
      ;;
      ;; If running XEmacs 19.8 or below, don't highlight explicit buttons
      ;; whenever a file is read in since this can cause a sporadic crash
      ;; when find-files are done.
      (if hyperb:kotl-p (var:append 'find-file-hooks '(hproperty:but-create)))
      (fset 'hui:but-flash 'hproperty:but-flash)
      ;;
      ;; Substitute for the nil argument below a valid X color name with
      ;; which to highlight buttons if the default highlighting does not
      ;; appeal to you. See "hui-xe-but.el" for how this works.
      (hproperty:cycle-but-color nil)
      ;;
      ;; Non-nil means visually emphasize that button under mouse cursor is
      ;; selectable.
      (setq hproperty:but-emphasize-p nil)
      ;;
      ;; If you find that the Hyperbole button flash time is too slow
      ;; or too fast, adjust it here.
      (setq hproperty:but-flash-time 1000)
      ))

(if (and hyperb:epoch-p (string= hyperb:epoch-p "V4"))
    (progn
      (require 'hui-epV4-b)
      ;; Highlight explicit buttons whenever a file is read in.
      (var:append 'find-file-hooks '(hproperty:but-create))
      (fset 'hui:but-flash 'hproperty:but-flash)
      ;; Substitute for the nil argument below a valid X color name with
      ;; which to highlight buttons if the default highlighting does not
      ;; appeal to you. See "hui-epV4-b.el" for how this works.
      (hproperty:cycle-but-color nil)
      ;; If you use Epoch and find that the Hyperbole button flash time is
      ;; too slow or too fast, adjust it here.
      (defvar hproperty:but-flash-time 1000
	"Machine specific value for empty loop counter, Epoch but flash delay.")
      ))

;;;
;;; EXTERNAL SYSTEM ENCAPSULATIONS
;;;

;;; Support for encapsulations of any of these external systems may be
;;; enabled here.  You should be familiar with the external system before
;;; you try to use the Hyperbole support for it.
;;; Possible system encapsulations to include within the innermost set of
;;; parentheses are:
;;;   hsys-wais hsys-hbase
;;; See files with the same name, e.g. "hsys-wais.el" for details on each
;;; system.
;;;
;;; Note: hsys-w3 is automatically loaded by default by Hyperbole.
(setq hibtypes:begin-load-hook
      (list (function (lambda () (mapcar 'require '())))))

;;;
;;; ONLINE LIBRARY CONFIGURATION
;;;

;;; Support for online library document id references is loaded here but
;;; requires some additional configuration before use.  See the DESCRIPTION
;;; section in "hib-doc-id.el" for complete installation and use information.
;;;
(setq hibtypes:end-load-hook
      (list (function (lambda () (mapcar 'require '(hib-doc-id))))))

;;;
;;; HYPERBOLE INITIALIZATION
;;;

;;; This call loads the whole Hyperbole system.
;;; You may want to look at this file just to see what it does.
;;;
(require 'hinit)
;;;
;;; This call initializes the Hyperbole system for use.
;;;
(hyperb:init)

;;;
;;; FILE VIEWER COMMAND SETTINGS
;;;

(defvar hpath:display-alist
  (let ((info-suffix "\\.info\\(-[0-9]+\\)?\\(\\.gz\\|\\.Z\\|-z\\)?$"))
    (list
     ;; Run the OO-Browser on OOBR or OOBR-FTR Environment files.
     '("OOBR\\(-FTR\\)?$" . br-env-browse)
     ;; Display the top node from Info online manuals.
     (cons
      (concat (` (, info-suffix)) "\\|/info/[^.]+$\\|/info-local/[^.]+$")
      (` (lambda (file)
	   (if (and (string-match (, info-suffix) file)
		    (match-beginning 1))
	       ;; Removed numbered trailer to get basic filename.
	       (setq file (concat (substring file 0 (match-beginning 1))
				  (substring file (match-end 1)))))
	   (require 'info)
	   (condition-case ()
	       (Info-find-node file "Top")
	     (error (if (and file (file-exists-p file))
			(progn
			  (if (get-buffer "*info*")
			      (kill-buffer "*info*"))
			  (Info-find-node file "*" nil t))
		      (error "Invalid file")))))))
     ))
  "*Alist of (FILENAME-REGEXP . EDIT-FUNCTION) elements for calling special
functions to display particular file types within Emacs.  See also
'hpath:file-alist' for external display program settings.")

;;; `hyperb:window-system' variable from "hversion.el" must be defined
;;; prior to this variable definition.
;;;
(defvar hpath:find-alist
  (let ((nextstep-suffixes '(("\\.\\(adaptor\\|app\\|bshlf\\|clr\\|concur\\|create\\|diagram\\|dp\\|e?ps\\|frame\\|gif\\|locus\\|Mesa\\|nib\\|project\\|rtf\\|sense\\|tiff\\|tree\\)$" . "open")))
	(x-suffixes '(("\\.e?ps$" . "ghostview")
		      ("\\.ps\\.g?[zZ]$" . "zcat %s | ghostview -")
		      ("\\.\\(gif\\|tiff?\\|xbm\\|pm\\|pbm\\|jpe?g\\)"  . "xv")
		      ("\\.xwd$" . "xwud -noclick -in")
		      ("\\.ra?s$" . "snapshot -l")
		      ("\\.xpm$" . "sxpm")
		      ("\\.\\(fm\\|frame\\|mif\\)$" . "maker")
		      ("\\.\\(doc\\|boo\\)$" . "ileaf")
		      )))
    (if (memq window-system '(dps ns))
	nextstep-suffixes
      (cdr (assoc hyperb:window-system
		  (list (cons "emacs19" x-suffixes)  ; GNU Emacs V19 under X
			(cons "xemacs"  x-suffixes)  ; XEmacs under X
			(cons "xterm"   x-suffixes)  ; GNU Emacs V18 under X
			(cons "epoch"   x-suffixes)  ; UofI Epoch under X
			'("sun"     . nil)       ; SunView
			(cons "next" nextstep-suffixes)
			'("apollo"  . nil)       ; Display Manager
			)))))
  "*Alist of (FILENAME-REGEXP . EDIT-PROGRAM) elements for using window system
dependent external programs to edit/display particular file types.  See also
'hpath:display-alist' for internal, window-system independent display
settings.")

;;;
;;; LINK PATH VARIABLE SUBSTITUTION SETTINGS
;;;

;;; The following variable permits sharing of links over wide areas, where
;;; links may contain variable references whose values may differ between
;;; link creator and link activator.
;;;
;;; When a link is created, if its path contains a match for any of the
;;; variable values in hpath:variables, then the variable's symbol is
;;; substituted for the literal value.  Hyperbole then replaces the variable
;;; with a matching value when the link is resolved.
;;;
(defvar hpath:variables
  '(hyperb:dir Info-directory Info-directory-list sm-directory load-path exec-path)
  "*List of Emacs Lisp variable symbols to substitute within matching link paths.
Each variable value, if bound, must be either a pathname or a list of pathnames.")

;;;
;;; HYPERBOLE LOCAL VARIABLE SUPPORT
;;;

;;; Uncomment this if you really need to be able to use Hyperbole variables
;;; (and others with colons in their names) within file local variable lists.
;;; See the source file for more details.
;;;
;;  (require 'hlvar)

;;;
;;; SITE-SPECIFIC ADDITIONS - Add your Hyperbole configuration additions here.
;;;

;;;
;;; END OF HYPERBOLE CONFIGURATION
;;;

(provide 'hsite)

(message "Hyperbole is ready for action.")

;;; hsite-ex.el ends here
