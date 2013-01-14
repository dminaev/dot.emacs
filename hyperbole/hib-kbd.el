;;; hib-kbd.el --- Implicit button type for key sequences delimited with {}.

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
;;
;;   A click of the Hyperbole execution key on a key sequence executes its
;;   command binding.
;;
;;   A click of the Hyperbole help key on a key sequence displays the
;;   documentation for its command binding.
;;
;;   Key sequences should be in human readable form, e.g. {C-b}.
;;   Forms such as {\C-b}, {\^b}, and {^b} will not be recognized.
;;

;;; Code:

;;;
;;; Public implicit button types
;;;
  
(defact kbd-key (key-sequence)
  "Executes the function binding for KEY-SEQUENCE, delimited by {}.
Returns t if a KEY-SEQUENCE has a binding, else nil."
  (interactive "kKeyboard key to execute (no {}): ")
  (kbd-key:act key-sequence))

(defib kbd-key ()
  "Executes a key sequence delimited by curly braces.
Key sequences should be in human readable form, e.g. {C-b}.
Forms such as {\C-b}, {\^b}, and {^b} will not be recognized."
  (if (br-in-browser)
      nil
    (let* ((seq-and-pos (or (hbut:label-p t "{`" "'}" t)
			    (hbut:label-p t "{" "}" t)))
	   (key-sequence (car seq-and-pos))
	   (binding (and (stringp key-sequence)
			 (key-binding (kbd-key:normalize key-sequence)))))
      (and binding (not (integerp binding))
	   (ibut:label-set seq-and-pos)
	   (hact 'kbd-key key-sequence)))))

;;;
;;; Public functions
;;;

(defun kbd-key:act (key-sequence)
  "Executes the command binding for KEY-SEQUENCE.
Returns t if KEY-SEQUENCE has a binding, else nil."
  (interactive "kKeyboard key to execute (no {}): ")
  (setq current-prefix-arg nil) ;; kbd-key:normalize below sets it.
  (let ((binding (key-binding (kbd-key:normalize key-sequence))))
    (cond ((null binding) nil)
	  ((memq binding '(action-key action-mouse-key hkey-either))
	   (beep)
	   (message "(kbd-key:act): This key does what the Action Key does.")
	   t)
	  (t (call-interactively binding) t))))

(defun kbd-key:doc (key &optional full)
  "Shows first line of doc for binding of keyboard KEY in minibuffer.
With optional FULL, displays full documentation for command."
  (interactive "kKey sequence: \nP")
  (let* ((cmd (let ((cmd (key-binding (kbd-key:normalize key))))
		(if (not (integerp cmd)) cmd)))
	 (doc (and cmd (documentation cmd)))
	 (end-line))
    (if doc
	(or full
	    (setq end-line (string-match "[\n]" doc)
		  doc (substitute-command-keys (substring doc 0 end-line))))
      (setq doc (format "No documentation for {%s} %s" key (or cmd ""))))
    (if (and cmd doc)
	(if full
	    (describe-function cmd)
	  (message doc)))))

(defun kbd-key:help (but)
  "Display documentation for binding of keyboard key given by BUT's label."
  (let ((kbd-key (hbut:key-to-label (hattr:get but 'lbl-key))))
    (and kbd-key (kbd-key:doc kbd-key 'full))))

(defun kbd-key:normalize (key-sequence)
  "Returns KEY-SEQUENCE normalized into a form that can be parsed by commands."
  (interactive "kKeyboard key sequence to normalize (no {}): ")
  (let ((norm-key-seq (copy-sequence key-sequence))
	(case-fold-search nil) (case-replace t))
    ;; Quote Control and Meta key names
    (setq norm-key-seq (hypb:replace-match-string
			"[ \t\n\^M]+" norm-key-seq "" t)
	  norm-key-seq (hypb:replace-match-string
			"@key{SPC}\\|SPC" norm-key-seq "\040" t)
	  norm-key-seq (hypb:replace-match-string
			"@key{DEL}\\|DEL" norm-key-seq "\177" t)
	  norm-key-seq (hypb:replace-match-string
			"@key{RET}\\|@key{RTN}\\|RET\\|RTN"
			norm-key-seq "\015" t)
	  norm-key-seq (hypb:replace-match-string
			"ESCESC" norm-key-seq "\233" t)
	  norm-key-seq (hypb:replace-match-string
			"@key{ESC}\\|ESC" norm-key-seq "M-" t)
	  ;; Unqote special {} chars.
	  norm-key-seq (hypb:replace-match-string "\\\\\\([{}]\\)"
						  norm-key-seq "\\1")
	  )
    (while (string-match "\\`\\(C-u\\|M-\\)\\(-?[0-9]+\\)" norm-key-seq)
      (setq current-prefix-arg
	    (string-to-int (substring norm-key-seq (match-beginning 2)
				      (match-end 2)))
	    norm-key-seq (substring norm-key-seq (match-end 0))))
    (let (arg-val)
      (while (string-match "\\`C-u" norm-key-seq)
	(if (or (not (listp current-prefix-arg))
		(not (integerp (setq arg-val (car current-prefix-arg)))))
	    (setq current-prefix-arg '(1)
		  arg-val 1))
	(setq arg-val (* arg-val 4)
	      current-prefix-arg (cons arg-val nil)
	      norm-key-seq (substring norm-key-seq (match-end 0)))))
    (setq norm-key-seq (hypb:replace-match-string
			"C-\\(.\\)" norm-key-seq
			(function
			 (lambda (str)
			   (char-to-string
			    (1+ (- (downcase
				    (string-to-char
				     (substring str (match-beginning 1)
						(1+ (match-beginning 1)))))
				   ?a)))))))
    (hypb:replace-match-string
     "M-\\(.\\)" norm-key-seq
     (function
      (lambda (str)
	(char-to-string (+ (downcase (string-to-char
				      (substring str (match-beginning 1)
						 (1+ (match-beginning 1)))))
			   128)))))))

(provide 'hib-kbd)

;;; hib-kbd.el ends here
