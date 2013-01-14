;;; hmouse-mod.el --- Action Key acts as CONTROL modifier and Assist Key as META modifier.

;; Copyright (C) 1992-1995 Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;; Maintainer: Mats Lidell <matsl@contactor.se>
;; Keywords: hypermedia, mouse

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
;;   This module is meant to be used with a chord keyboard in one hand for
;;   typing and a mouse in the other.  It requires that Hyperbole be loaded
;;   in order to work.  Hyperbole defines two Smart Keys, the Action Key and
;;   the Assist Key, on the middle and right buttons by default.
;;
;;   If the Action Key is held down while alpha characters are typed,
;;   they are translated into Control keys instead.  The Assist
;;   Key translates them into Meta keys.  When both Smart Keys
;;   are depressed, Control-Meta keys are produced.  The commands bound
;;   to the characters produced are then run.
;;
;;   So the Smart Keys modify the keys typed, e.g. Action Key + {a}
;;   runs the function for {C-a}.
;;
;;   If no keys are typed while the Smart Keys are down, they operate as
;;   normally under Hyperbole.
;;
;;   TO INVOKE:
;;
;;       (hmouse-mod-set-global-map)
;;

;;; Code:

;;;
;;; Other required Elisp libraries
;;;

(require 'hyperbole)

;;;
;;; Public variables
;;;

(defvar hmouse-mod-global-map nil
  "Global key map installed by hmouse-mod-set-global-map function.
Translates self-insert-command characters into control and meta characters if
the Action or Assist Keys are depressed at the time of key press.")

;;;
;;; Public functions
;;;

(defun hmouse-mod-insert-command (count)
  "Surrogate function for self-insert-command.  Accounts for modifier Smart Keys."
  (interactive "p")
  (if (and (boundp 'action-key-depressed-flag)
	   (boundp 'assist-key-depressed-flag))
      (cond ((and action-key-depressed-flag assist-key-depressed-flag)
	     (setq action-key-cancelled t
		   assist-key-cancelled t)
	     (let* ((c (downcase last-command-char))
		    (key (char-to-string (+ 128 (% (- c ?\`) 128)))))
	       (if (and (or (= c ?\C-@)
			    (>= c ?a) (<= c ?z)))
		   (hmouse-mod-execute-command key)
		 (beep)))
	     )
	    ;; Control keys
	    (action-key-depressed-flag
	      (setq action-key-cancelled t)
	      (let ((c (downcase last-command-char)))
		(if (and (or (= c ?\C-@)
			     (>= c ?a) (<= c ?z)))
		    (hmouse-mod-execute-command
		      (char-to-string (- c ?\`)))
		  (beep)))
	      )
	    ;; Meta keys
	    (assist-key-depressed-flag
	      (setq assist-key-cancelled t)
	      (hmouse-mod-execute-command
		(char-to-string (+ 128 (% last-command-char 128))))
	      )
	    (t (call-interactively 'self-insert-command)))
    (call-interactively 'self-insert-command))
  )

(defun hmouse-mod-keyboard-quit ()
  "Surrogate function for keyboard-quit.  Cancels any hmouse-mod-prefix."
  (interactive)
  (setq hmouse-mod-prefix nil)
  (keyboard-quit))

(defun hmouse-mod-set-global-map ()
  "Creates 'hmouse-mod-global-map' and installs as current global map.
It accounts for modifier Smart Keys."
  (interactive)
  (setq hmouse-mod-global-map (copy-keymap global-map))
  (substitute-key-definition
    'self-insert-command 'hmouse-mod-insert-command hmouse-mod-global-map)
  (substitute-key-definition
    'keyboard-quit 'hmouse-mod-keyboard-quit hmouse-mod-global-map)
  (use-global-map hmouse-mod-global-map))

;;;
;;; Private functions
;;;

(defun hmouse-mod-execute-command (key)
  "Executes command associated with keyboard KEY or if KEY prefix, records it."
  (setq key (concat hmouse-mod-prefix key))
  (let ((binding (key-binding key)))
    (cond ((and (not (or (vectorp binding) (stringp binding)))
		(commandp binding))
	   (if (> (length key) 1)
	       (or noninteractive (message (key-description key))))
	   (setq hmouse-mod-prefix nil)
	   (call-interactively binding))
	  ((symbolp binding)
	   (setq hmouse-mod-prefix nil)
	   (error "(hmouse-mod-execute-command): {%s} not bound to a command."
		  (key-description key)))
	  ((integerp binding)
	   (setq hmouse-mod-prefix nil)
	   (error "(hmouse-mod-execute-command): {%s} invalid key sequence."
		  (key-description key)))
	  (t (or noninteractive (message (key-description key)))
	     (setq hmouse-mod-prefix key)))))

;;;
;;; Private variables
;;;

(defvar hmouse-mod-prefix nil
  "Prefix key part of current key sequence.")

(provide 'hmouse-mod)

;;; hmouse-mod.el ends here
