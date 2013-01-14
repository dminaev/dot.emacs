;;; mmdual.el --- MIME entity module for dual buffers

;; Copyright (C) 1998,1999 Free Software Foundation, Inc.
;; Copyright (C) 1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: MIME, multimedia, mail, news

;; This file is part of FLIM (Faithful Library about Internet Message).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'mime)

(eval-and-compile
  (luna-define-class mime-dual-entity (mime-entity)
		     (header-buffer
		      body-buffer))

  (luna-define-internal-accessors 'mime-dual-entity)
  )

(luna-define-method initialize-instance :after ((entity mime-dual-entity)
						&rest init-args)
  (let (buf)
    (setq buf (mime-dual-entity-header-buffer-internal entity))
    (if buf
	(with-current-buffer buf
	  (if (mime-root-entity-p entity)
	      (setq mime-message-structure entity))
	  (or (mime-entity-content-type-internal entity)
	      (mime-entity-set-content-type-internal
	       entity
	       (let ((str (std11-fetch-field "Content-Type")))
		 (if str
		     (mime-parse-Content-Type str)
		   ))))))
    (setq buf (mime-dual-entity-body-buffer-internal entity))
    (if buf
	(with-current-buffer buf
	  (if (mime-root-entity-p entity)
	      (setq mime-message-structure entity))))
    ) entity)

(luna-define-method mime-entity-name ((entity mime-dual-entity))
  (buffer-name (mime-dual-entity-header-buffer-internal entity))
  )


(defun mime-visible-field-p (field-name visible-fields invisible-fields)
  (or (catch 'found
	(while visible-fields
	  (let ((regexp (car visible-fields)))
	    (if (string-match regexp field-name)
		(throw 'found t)
	      ))
	  (setq visible-fields (cdr visible-fields))
	  ))
      (catch 'found
	(while invisible-fields
	  (let ((regexp (car invisible-fields)))
	    (if (string-match regexp field-name)
		(throw 'found nil)
	      ))
	  (setq invisible-fields (cdr invisible-fields))
	  )
	t)))

(defun mime-insert-header-from-buffer (buffer start end
					      &optional invisible-fields
					      visible-fields)
  (let ((the-buf (current-buffer))
	(mode-obj (mime-find-field-presentation-method 'wide))
	field-decoder
	f-b p f-e field-name len field field-body)
    (save-excursion
      (set-buffer buffer)
      (save-restriction
	(narrow-to-region start end)
	(goto-char start)
	(while (re-search-forward std11-field-head-regexp nil t)
	  (setq f-b (match-beginning 0)
		p (match-end 0)
		field-name (buffer-substring f-b p)
		len (string-width field-name)
		f-e (std11-field-end))
	  (when (mime-visible-field-p field-name
				      visible-fields invisible-fields)
	    (setq field (intern
			 (capitalize (buffer-substring f-b (1- p))))
		  field-body (buffer-substring p f-e)
		  field-decoder (inline (mime-find-field-decoder-internal
					 field mode-obj)))
	    (with-current-buffer the-buf
	      (insert field-name)
	      (insert (if field-decoder
			  (funcall field-decoder field-body len)
			;; Don't decode
			field-body))
	      (insert "\n")
	      )))))))

(luna-define-method mime-insert-header ((entity mime-dual-entity)
					&optional invisible-fields
					visible-fields)
  (let* ((buf (mime-dual-entity-header-buffer-internal entity))
	 header-start header-end)
    (with-current-buffer buf
      (setq header-start (point-min)
	    header-end (point-max)))
    (mime-insert-header-from-buffer buf header-start header-end
				    invisible-fields visible-fields)
    ))

(luna-define-method mime-entity-content ((entity mime-dual-entity))
  (mime-decode-string
   (with-current-buffer (mime-dual-entity-body-buffer-internal entity)
     (buffer-string))
   (mime-entity-encoding entity)))

(luna-define-method mime-entity-fetch-field :around
  ((entity mime-dual-entity) field-name)
  (or (luna-call-next-method)
      (with-current-buffer (mime-dual-entity-header-buffer-internal entity)
	(let ((ret (std11-fetch-field field-name)))
	  (when ret
	    (or (symbolp field-name)
		(setq field-name
		      (intern (capitalize (capitalize field-name)))))
	    (mime-entity-set-original-header-internal
	     entity
	     (put-alist field-name ret
			(mime-entity-original-header-internal entity)))
	    ret)))))

(luna-define-method mime-insert-entity-content ((entity mime-dual-entity))
  (insert
   (mime-decode-string
    (with-current-buffer (mime-dual-entity-body-buffer-internal entity)
      (buffer-substring (point-min)(point-max)))
    (mime-entity-encoding entity))))

(luna-define-method mime-write-entity-content ((entity mime-dual-entity)
					       filename)
  (with-current-buffer (mime-dual-entity-body-buffer-internal entity)
    (mime-write-decoded-region (point-min)
			       (point-max)
			       filename
			       (or (mime-entity-encoding entity) "7bit"))))

(luna-define-method mime-insert-entity ((entity mime-dual-entity))
  (let (buf)
    (setq buf (mime-dual-entity-header-buffer-internal entity))
    (when buf
      (insert-buffer (mime-dual-entity-header-buffer-internal entity))
      (setq buf (mime-dual-entity-body-buffer-internal entity))
      (when buf
	(insert "\n")
	(insert-buffer buf)))))

(luna-define-method mime-write-entity ((entity mime-dual-entity) filename)
  (let (buf)
    (setq buf (mime-dual-entity-header-buffer-internal entity))
    (if (null buf)
	(error "No header buffer.")
      (with-current-buffer buf
	(write-region-as-raw-text-CRLF
	 (point-min)(point-max) filename))
      (setq buf (mime-dual-entity-body-buffer-internal entity))
      (when buf
	(with-temp-buffer
	  (insert "\n")
	  (write-region-as-raw-text-CRLF
	   (point-min)(point-max)
	   filename 'append))
	(with-current-buffer buf
	  (write-region-as-raw-text-CRLF
	   (point-min)(point-max)
	   filename 'append))))))

(luna-define-method mime-write-entity-body ((entity mime-dual-entity) filename)
  (with-current-buffer (mime-dual-entity-body-buffer-internal entity)
    (write-region-as-binary (point-min)(point-max)
			    filename)))


;;; @ buffer
;;;

(luna-define-method mime-entity-header-buffer ((entity mime-dual-entity))
  (mime-dual-entity-header-buffer-internal entity))

(luna-define-method mime-entity-body-buffer ((entity mime-dual-entity))
  (mime-dual-entity-body-buffer-internal entity))

(luna-define-method mime-entity-buffer ((entity mime-dual-entity))
  (message "mime-dual-entity does not have mime-entity-buffer.")
  nil)

(luna-define-method mime-entity-body-start-point ((entity mime-dual-entity))
  (with-current-buffer (mime-entity-body-buffer entity)
    (point-min)))

(luna-define-method mime-entity-body-end-point ((entity mime-dual-entity))
  (with-current-buffer (mime-entity-body-buffer entity)
    (point-max)))

(luna-define-method mime-entity-point-min ((entity mime-dual-entity))
  (message "mime-dual-entity does not have mime-entity-point-min.")
  nil)

(luna-define-method mime-entity-point-max ((entity mime-dual-entity))
  (message "mime-dual-entity does not have mime-entity-point-max.")
  nil)

(luna-define-method mime-goto-header-start-point ((entity mime-dual-entity))
  (set-buffer (mime-dual-entity-header-buffer-internal entity))
  (goto-char (point-min)))

(luna-define-method mime-goto-body-start-point ((entity mime-dual-entity))
  (set-buffer (mime-dual-entity-body-buffer-internal entity))
  (goto-char (point-min)))

(luna-define-method mime-goto-body-end-point ((entity mime-dual-entity))
  (set-buffer (mime-dual-entity-body-buffer-internal entity))
  (goto-char (point-max)))


;;; @ end
;;;

(provide 'mmdual)

;;; mmdual.el ends here
