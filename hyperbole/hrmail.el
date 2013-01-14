;;; hrmail.el --- Support for Hyperbole buttons in mail reader: Rmail.

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
;;   Automatically configured for use in "hyperbole.el".
;;   If hsite loading fails prior to initializing Hyperbole Rmail support,
;;
;;       {M-x Rmail-init RTN}
;;
;;   will do it.
;;

;;; Code:

;;;
;;; Other required Elisp libraries
;;;

(require 'hmail)
(require 'hact)
(load "hsmail")
(require 'rmail)
(load "rmailedit")
(provide 'rmailedit)

;;;
;;; Public variables
;;;

;;;
;;; Public functions
;;;

(defun Rmail-init ()
  "Initializes Hyperbole support for Rmail mail reading."
  (interactive)
  (setq hmail:composer  'mail-mode
	hmail:lister    'rmail-summary-mode
	hmail:modifier  'rmail-edit-mode
	hmail:reader    'rmail-mode)
  (var:append 'rmail-show-message-hook '(hmail:msg-narrow))
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
  (fset 'rmail:get-new       'rmail-get-new-mail)
  (fset 'rmail:msg-forward   'rmail-forward)
  (fset 'rmail:summ-msg-to   'rmail-summary-goto-msg)
  (fset 'rmail:summ-new      'rmail-new-summary)
  (if (interactive-p)
      (message "Hyperbole RMAIL mail reader support initialized."))
  )

(defun Rmail-msg-hdrs-full (toggled)
  "If TOGGLED is non-nil, toggle full/hidden headers, else show full headers."
  (save-excursion
    (if (or toggled
	    (let ((tog nil))
	      (save-excursion
		(save-restriction
		  (rmail-maybe-set-message-counters)
		  (narrow-to-region (rmail-msgbeg rmail-current-message)
				    (point-max))
		  (let ((buffer-read-only nil))
		    (goto-char (point-min))
		    (forward-line 1)
		    ;; Need to show full header
		    (if (= (following-char) ?1)
			(setq tog t)))))
	      tog))
	(progn (rmail-toggle-header)
	       (setq toggled t)))
    toggled))

(defun Rmail-msg-narrow ()
  "Narrows mail reader buffer to current message.
This includes Hyperbole button data."
  (let ((beg (rmail-msgbeg rmail-current-message))
	(end (rmail-msgend rmail-current-message)))
    (narrow-to-region beg end)))

(defun Rmail-msg-next ()        (rmail-next-undeleted-message 1))

(defun Rmail-msg-num ()
  "Returns number of Rmail message that point is within."
  (interactive)
  (let ((count 0) opoint)
    (save-excursion
     (while (and (not (eobp))
		 (progn (setq opoint (point))
			(re-search-backward "^\^_" nil t)))
       (if (= opoint (point))
	   (backward-char 1)
	 (setq count (1+ count)))))
    count))

(defun Rmail-msg-prev ()        (rmail-previous-undeleted-message 1))

(defun Rmail-msg-to-p (mail-msg-id mail-file)
  "Sets current buffer to start of msg with MAIL-MSG-ID in MAIL-FILE.
Returns t if successful, else nil."
  (if (not (file-readable-p mail-file))
      nil
    (let ((buf (get-file-buffer mail-file)))
      (cond (buf
	     (switch-to-buffer buf)
	     (or (eq major-mode 'rmail-mode)
		 (rmail mail-file)))
	    (t (rmail mail-file))))
    (widen)
    (goto-char 1)
    (if (re-search-forward (concat rmail:msg-hdr-prefix
				   (regexp-quote mail-msg-id)) nil t)
	;; Found matching msg
	(progn
	  (setq buffer-read-only t)
	  (rmail-show-message (Rmail-msg-num))
	  t))))


(defun Rmail-msg-widen ()
  "Widens buffer to full current message including Hyperbole button data."
  (let ((start (point-min))
	(end (point-max)))
    (unwind-protect
	(save-excursion
	  (widen)
	  (if (re-search-forward "^\^_" nil t)
	      (progn (forward-char -1)
		     (setq end (point)))))
      (narrow-to-region start end))))

(defun Rmail-to ()
  "Sets current buffer to a mail reader buffer."
  (and (eq major-mode 'rmail-summary-mode) (set-buffer rmail-buffer)))

(fset 'Rmail-Summ-delete        'rmail-summary-delete-forward)

(fset 'Rmail-Summ-expunge       'rmail-summary-expunge)

(fset 'Rmail-Summ-goto          'rmail-summary-goto-msg)

(defun Rmail-Summ-to ()
  "Sets current buffer to a mail listing buffer."
  (and (eq major-mode 'rmail-mode) (set-buffer rmail-summary-buffer)))

(fset 'Rmail-Summ-undelete-all  'rmail-summary-undelete-many)

;;;
;;; Private functions
;;;

;;;
;;; Overlay version of this function from "rmailedit.el" to include any
;;; hidden Hyperbole button data when computing message length.
(defun rmail-cease-edit ()
  "Finish editing message; switch back to Rmail proper."
  (interactive)
  ;; Make sure buffer ends with a newline.
  (save-excursion
    (Rmail-msg-widen)
    (goto-char (point-max))
    (if (/= (preceding-char) ?\n)
	(insert "\n"))
    ;; Adjust the marker that points to the end of this message.
    (set-marker (aref rmail-message-vector (1+ rmail-current-message))
		(point))
    (hmail:msg-narrow)
    )
  (let ((old rmail-old-text))
    ;; Update the mode line.
    (set-buffer-modified-p (buffer-modified-p))
    (rmail-mode-1)
    (if (and (= (length old) (- (point-max) (point-min)))
	     (string= old (buffer-substring (point-min) (point-max))))
	()
      (setq old nil)
      (rmail-set-attribute "edited" t)
      (if (boundp 'rmail-summary-vector)
	  (progn
	    (aset rmail-summary-vector (1- rmail-current-message) nil)
	    (save-excursion
	      (rmail-widen-to-current-msgbeg
	        (function (lambda ()
			    (forward-line 2)
			    (if (looking-at "Summary-line: ")
				(let ((buffer-read-only nil))
				  (delete-region (point)
						 (progn (forward-line 1)
							(point))))))))
	      (rmail-show-message))))))
  (setq buffer-read-only t))


;;; Overlay version of this function from "rmail.el" to include any
;;; Hyperbole button data.
(defun rmail-forward (&optional resend)
  "Forward the current message to another user."
  (interactive)
  ;; Resend argument is ignored but for now but is there for Emacs V19 call
  ;; compatibility.
  ;;>> this gets set even if we abort. Can't do anything about it, though.
  (rmail-set-attribute "forwarded" t)
  (let ((forward-buffer (current-buffer))
	(subject (concat "["
			 (mail-strip-quoted-names (mail-fetch-field "From"))
			 ": " (or (mail-fetch-field "Subject") "") "]")))
    (save-restriction
      (Rmail-msg-widen)
      ;; If only one window, use it for the mail buffer.
      ;; Otherwise, use another window for the mail buffer
      ;; so that the Rmail buffer remains visible
      ;; and sending the mail will get back to it.
      (if (if (one-window-p t)
	      (mail nil nil subject)
	    (mail-other-window nil nil subject))
	  (save-excursion
	    (goto-char (point-max))
	    (forward-line 1)
	    (insert-buffer forward-buffer)
	    (hmail:msg-narrow)
	    )))))

;;; Overlay version of 'rmail-get-new-mail' from "rmail.el" to highlight
;;; Hyperbole buttons when possible.
;;;
(hypb:function-overload 'rmail-get-new-mail nil
			'(if (fboundp 'hproperty:but-create)
			     (progn (widen) (hproperty:but-create)
				    (rmail-show-message))))

;;; Overlay version of 'rmail-new-summary' from "rmailsum.el" to
;;; highlight Hyperbole buttons when possible.
;;;
(or (fboundp 'rmail-new-summary) (load "rmailsum"))
(hypb:function-overload 'rmail-new-summary nil
			'(if (fboundp 'hproperty:but-create)
			     (hproperty:but-create)))

;;;
;;; Private variables
;;;

(provide 'hrmail)

;;; hrmail.el ends here
