;;; hsys-www.el --- Hyperbole support for old CERN command line WWW browsing.

;; Copyright (C) 1991-1995 Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;; Maintainer: Mats Lidell <matsl@contactor.se>
;; Keywords: comm, help, hypermedia

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
;;   You must first build the www line mode browser executable before you can
;;   use this system encapsulation.  The browser MUST be configured so that
;;   the final part of its prompt is a line beginning with "==> " without a
;;   trailing newline, like so:
;;
;;   <ref.number>, Back, Quit, or Help.
;;   ==> 
;;
;;
;;   Then, a Hyperbole button should be created that has 'hwww:start' as its
;;   action type.  It may optionally contain a file name argument as
;;   the initial file to display.  When selected, it starts a 'www'
;;   process and displays the initial file.
;;
;;   The 'hwww:link-follow' implicit button type is then used when the
;;   user clicks inside the buffer containing the 'www' output.  It
;;   passes commands to the 'hwww:link-follow' action type.
;;

;;; Code:

;;;
;;; Other required Elisp libraries
;;;

;;; Requires external 'www' executable available via anonymous ftp
;;; from info.cern.ch.

;;;
;;; Public variables
;;;

(defib hwww:link-follow ()
  "When in a www buffer, returns a link follow or history recall command."
  (let* ((www (get-buffer-process (current-buffer)))
	 (www-proc-nm (and www (process-name www)))
	 (selection)
	 (act (function
	       (lambda (&optional prefix)
		 (setq selection
		       (buffer-substring (match-beginning 1)
					 (match-end 1)))
		 (ibut:label-set selection (match-beginning 1)
				 (match-end 1))
		 (hact 'hwww:link-follow (concat prefix selection))))))
    (if (and www-proc-nm (equal (string-match "www" www-proc-nm) 0))
	(cond (;; Hyper ref
	       (save-excursion
		 (skip-chars-backward "^ \t\n")
		 (looking-at "[^][ \t\n]*\\[\\([0-9]+\\)\\]"))
	       (funcall act))
	      (;; History list entry
	       (save-excursion
		 (beginning-of-line)
		 (looking-at "[ \t]*\\([0-9]+\\)\)[ \t]+[^ \t\n]"))
	       (funcall act "recall "))
	      (;; Hyper ref list
	       (save-excursion
		 (beginning-of-line)
		 (looking-at "[ \t]*\\[\\([0-9]+\\)\\][ \t]+[^ \t\n]"))
	       (funcall act ))))))

(defact hwww:link-follow (link-num-str)
  "Follows a link given by LINK-NUM-STR or displays a www history list."
  (interactive "sNumber of WWW link to follow: ")
  (or (stringp link-num-str)
      (error "(hwww:link-follow): Link number must be given as a string."))
  (let ((www (get-buffer-process (current-buffer))))
    (if www
	(progn
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (process-send-string www (concat link-num-str "\n"))
	  )
      (error "(hwww:link-follow): No current WWW process.  Use 'hwww:start'."))))

(defun hwww:link-follow:help (&optional but)
  "Displays history list of www nodes previously visited."
  (interactive)
  (hact 'hwww:link-follow "recall"))

(defact hwww:start (&optional file)
  "Starts a www process and displays optional FILE.
Without FILE (an empty string), displays default initial www file."
  (interactive "FWWW file to start with: ")
  (or (stringp file)
      (error "(hwww:start): FILE argument is not a string."))
  (let ((www-buf (get-buffer-create "WWW"))
	(www-proc (get-process "www")))
    (save-excursion
      (set-buffer www-buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      )
    (if www-proc
	(pop-to-buffer www-buf)
      (if (setq www-proc
		(if (or (equal file "") (equal file "\"\""))
		    (start-process "www" www-buf "www" "-p")
		  (start-process "www" www-buf "www" "-p" file)))
	  (progn (set-process-sentinel www-proc 'hwww:sentinel)
		 (set-process-filter www-proc 'hwww:filter)
		 (process-kill-without-query www-proc)
		 (pop-to-buffer www-buf)
		 (shell-mode)
		 (make-local-variable 'explicit-shell-file-name)
		 (setq explicit-shell-file-name "www")
		 (use-local-map hwww:mode-map)
		 (if hwww:mode-map
		     nil
		   (setq hwww:mode-map (copy-keymap shell-mode-map))
		   (define-key hwww:mode-map "\C-m" 'hwww:send-input)
		   (define-key hwww:mode-map " " 'hwww:scroll-up)
		   (define-key hwww:mode-map "\177" 'hwww:scroll-down)
		   )
		 (goto-char (point-min))
		 )))))

;;;
;;; Private functions
;;;

(defun hwww:filter (process str)
  (if (and (> (length str) 3)
	   (equal "==> " (substring str -4)))
      (progn
	(insert str)
	(goto-char (point-min))
	(hproperty:but-create (concat "\\([^ \t\n]*\\[[0-9]+\\]\\|"
				      "^[ \t]*\\[\\([0-9]+\\)\\][ \t]+[^ \t\n]+\\|"
				      "^[ ]+[0-9]+\).*\\)")
			      'regexp))
    (insert str)))

(defun hwww:scroll-up (&optional arg)
  "If on last line of buffer, insert space, else scroll up a page."
  (interactive "P")
  (if (last-line-p) (insert " ") (scroll-up arg)))

(defun hwww:scroll-down (&optional arg)
  "If on last line of buffer, delete char backwards, else scroll down a page."
  (interactive "P")
  (if (last-line-p) (backward-delete-char-untabify (or arg 1))
    (scroll-down arg)))

(defun hwww:send-input ()
  (interactive)
  (cond ((eobp)
	 (let ((www (get-buffer-process (current-buffer))))
	   (if www
	       (progn
		 (beginning-of-line)
		 ;; Exclude the shell prompt, if any.
		 (re-search-forward shell-prompt-pattern
				    (save-excursion (end-of-line) (point))
				    t)
		 (let ((cmd (concat (buffer-substring (point)
						      (progn (forward-line 1)
							     (point)))
				    "\n")))
		   (erase-buffer)
		   (process-send-string www cmd)
		   ))
	     (error "(hwww:link-follow): No current WWW process.  Use 'hwww:start'."))))
	((ibut:at-p) (hui:hbut-act))
	(t (end-of-buffer))
	))

(defun hwww:sentinel (process signal)
  (princ
   (format "Process: %s received the msg: %s" process signal))
  (or (string-match "killed" signal)
      (pop-to-buffer (process-buffer process))))

;;;
;;; Private variables
;;;

(defvar hwww:mode-map nil)

(provide 'hsys-www)

;;; hsys-www.el ends here
