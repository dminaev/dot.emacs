;;; hmh.el --- Support for Hyperbole buttons in mail reader: Mh.

;; Copyright (C) 1991-1995 Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;; Maintainer: Mats Lidell <matsl@contactor.se>
;; Keywords: hypermedia, mail

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
;;   Automatically configured for use in "hsite.el".
;;   If hsite loading fails prior to initializing Hyperbole Mh support,
;;
;;       {M-x Mh-init RTN}
;;
;;   will do it.
;;
;;
;;     Have not yet overloaded 'mh-yank-cur-msg' to yank and hide
;;   button data from mail reader buffer.
;;     Have not yet overloaded 'mh-insert-letter' to highlight buttons
;;   and to merge its button data.
;;

;;; Code:

;;;
;;; Other required Elisp libraries
;;;
(require 'hmail)
(load "hsmail")
(require 'mh-e)

;;;
;;; Public variables
;;;

;;;
;;; Public functions
;;;

(defun Mh-init ()
  "Initializes Hyperbole support for Mh mail reading."
  (interactive)
  (setq hmail:composer  'mh-letter-mode
	hmail:lister    'mh-folder-mode
	hmail:modifier  'mh-letter-mode
	hmail:reader    'mh-show-mode)
  (var:append 'mh-show-hook '(hmail:msg-narrow Mh-hbut-highlight))
  ;;
  ;;
  ;; Setup public abstract interface to Hyperbole defined mail
  ;; reader-specific functions used in "hmail.el".
  ;;
  (rmail:init)
  ;;
  ;; Setup private abstract interface to mail reader-specific functions
  ;; used in "hmail.el".
  ;;
  (fset 'rmail:get-new       'mh-inc-folder)
  (fset 'rmail:msg-forward   'mh-redistribute)
  (fset 'rmail:summ-msg-to   'mh-goto-msg)
  (fset 'rmail:summ-new      'mh-rescan-folder)
  (if (interactive-p)
      (message "Hyperbole MH mail reader support initialized."))
  )

(defun Mh-hbut-highlight ()
  "Highlight any Hyperbole buttons in buffer for which display support exists."
  (if (fboundp 'hproperty:but-create) (hproperty:but-create)))

(defun Mh-msg-hdrs-full (toggled)
  "If TOGGLED is non-nil, toggle full/hidden headers, else show full headers.
For now, a no-op."
  )

(defun Mh-msg-narrow ()
  "Narrows mail reader buffer to current message.
This includes Hyperbole button data."
  (Mh-msg-widen))

(defun Mh-msg-next ()           (mh-next-undeleted-msg 1))

(defun Mh-msg-num ()
  "Returns number of mail message that point is within."
  (interactive)
  (mh-get-msg-num nil))

(defun Mh-msg-prev ()           (mh-previous-undeleted-msg 1))

(defun Mh-msg-to-p (mail-msg-id mail-file)
  "Sets current buffer to start of msg with MAIL-MSG-ID in MAIL-FILE.
Returns t if successful, else nil."
  (if (not (file-readable-p mail-file))
      nil
    (find-file mail-file)
    (hmail:msg-narrow)
    (goto-char 1)
    t))

(defun Mh-msg-widen ()
  "Widens buffer to full current message including Hyperbole button data."
  (Mh-to) (widen))

(defun Mh-to ()
  "Sets current buffer to a mail reader buffer."
  (and (eq major-mode 'Mh-folder-mode)
       (set-buffer (Mh-get-buffer Mh-show-buffer))))

(defun Mh-Summ-delete ()        (mh-delete-msg (mh-get-msg-num t)))

(fset 'Mh-Summ-expunge          'mh-execute-commands)

(defun Mh-Summ-goto ()
  (let ((msg-num (mh-get-msg-num nil)))
    (mh-goto-msg msg-num nil t)
    (mh-show msg-num)))

(defun Mh-Summ-to ()
  "Sets current buffer to a mail listing buffer."
  (let ((summ-buf))
    (save-excursion
      (mapcar (function
		(lambda (window)
		  (if summ-buf
		      nil
		    (set-buffer (window-buffer window))
		    (if (eq major-mode 'Mh-folder-mode)
			(setq summ-buf (current-buffer))))))
	      (hypb:window-list 'no-mini)))
    (if summ-buf (set-buffer summ-buf))))

(defun Mh-Summ-undelete-all ()
  (message
   "(Mh-Summ-undelete-all: I don't think mh-e has an undelete operator."))

;;;
;;; Private functions
;;;
;;;
;;; Overlay version of this function from mh-e.el to run mh-show-hook at end.
;;; This hook may already be run, depending on the version of mh-e you are
;;; running, but running it twice shouldn't do any harm.  Comment this out if
;;; you know that your mh-e.el already runs the hook.
(hypb:function-overload 'mh-display-msg nil
			'(run-hooks 'mh-show-hook))

;;;
;;; Overlay version of 'mh-regenerate-headers' to highlight Hyperbole
;;; buttons when possible.
;;;
(hypb:function-overload 'mh-regenerate-headers nil
			'(if (fboundp 'hproperty:but-create)
			     (hproperty:but-create)))

;;;
;;; Set 'mh-send-letter' hook to widen to include button data before sending.
;;;
(var:append 'mh-before-send-letter-hook '(widen))

;;;
;;; Private variables
;;;

(provide 'hmh)

;;; hmh.el ends here
