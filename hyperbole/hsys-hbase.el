;;; hsys-hbase.el --- Hyperbole support for the Hyperbase system.

;; Copyright (C) 1991, 1995, Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;; Maintainer: Mats Lidell <matsl@contactor.se>
;; Keywords: comm, hypermedia

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
;;   For information and the source to HyperBase and follow-on hypermedia
;;   work, see:  ftp://ftp.iesd.auc.dk/pub/packages/hypertext/
;;
;;   In order to use this package, you must have the HyperBase system
;;   and must start up a HyperBase server and then load the HyperBase
;;   Epoch support software that comes with the HyperBase system.
;;
;;   Then load this package and Hyperbole will do the following when
;;   in a Hyperbase buffer:
;;
;;     Action Key press on a button follows the link, within any other
;;     text, closes current Epoch screen and kills node buffer.
;;
;;     Assist Key press shows attributes for the current button or
;;     for the current node buffer, if no current button.
;;

;;; Code:

;;;
;;; Other required Elisp libraries
;;;

(require 'hbut)

;;;
;;; Public variables
;;;

(defib hyperbase ()
  "Detects link buttons in buffers that communicate with the Hyperbase system.
Hyperbase is a hypertext database system that interfaces to Emacs."
  (and (boundp 'ehts-mode) ehts-mode
       (let ((lbl (or (ebut:label-p 'as-label "[-> " "]")
		      "no-but")))
	 (ibut:label-set lbl)
	 (hact 'hyperbase lbl))))

(defact hyperbase (linkname)
  "Follows LINKNAME in a buffer that communicates with the Hyperbase system.
If LINKNAME equals t, closes the current Epoch screen and kill the
buffer of the current Hyperbase node.
Hyperbase is a hypertext database system that interfaces to Emacs."
  ;; From hb-EHTS.el by:
  ;;	Uffe Kock Wiil 		(kock@iesd.auc.dk)
  ;;	Claus Bo Nielsen 	(cbn@cci.dk)
  ;;
  (if (equal linkname "no-but")
      (progn (ehts-mouse-kill-screen-and-buffer t)
	     (and (fboundp 'epoch::select-screen)
		  (epoch::select-screen)))
    (let ((linknum (cdr (assoc linkname ehts-node-link-alist))) tonode)
      (ehts-command t)
      (if (< (ehts-hb-sys-call "read" linknum "to data node no" nil t) 0)
	  (progn
	    (ehts-command nil)
	    (error "Can't read \"to data node no\" in link, panic !!!")))
      (ehts-read-4bytes)
      (setq tonode (ehts-read-4bytes))
      (if (< (ehts-hb-sys-call "read" tonode "n name" nil t) 0)
	  (progn
	    (ehts-command nil)
	    (error "Can't read \"name\" in data node, panic !!!")))
      (ehts-get-node (ehts-read-null-string))
      (and (fboundp 'hproperty:but-create-all)
	   (hproperty:but-create-all "[-> " "]"))
      (ehts-command nil))))

;;;
;;; Public functions
;;;

(defun hyperbase:init ()
  "Show initial set of Hyperbase buttons."
  (if (assoc (user-full-name) ehts-node-name-alist)
      (progn
	(ehts-get-node (user-full-name))
	(let (buffer screen)
	  (setq buffer "*Ehts Welcome*")
	  (setq screen (ehts-find-buffer-screen buffer))
	  (kill-buffer buffer)
	  (switch-to-buffer (user-full-name))
	  (remove-screen screen)))
    (if (assoc "dir ehts help" ehts-node-name-alist)
	(progn
	  (ehts-get-node "dir ehts help")
	  (let (buffer screen)
	    (setq buffer "*Ehts Welcome*")
	    (setq screen (ehts-find-buffer-screen buffer))
	    (kill-buffer buffer)
	    (switch-to-buffer "dir ehts help")
	    (remove-screen screen)
	    (hproperty:but-create "[-> " "]"))))))

(defun hyperbase:help (&optional but)
  "Displays attributes of a link button BUT if on one or of the current node.
Hyperbase is a hypertext database system that interfaces to Emacs."
  (interactive (list (ibut:at-p)))
  (or (and (boundp 'ehts-mode) ehts-mode)
      (error "(hyperbase:help): Not in a Hyperbase mode buffer."))
  (hyperbase:attr-help
   (or (and (symbolp but) 
	    (let ((lbl (ebut:key-to-label (hattr:get but 'lbl-key))))
	      (if (not (equal lbl "no-but")) lbl)))
       (current-buffer))))

;;;
;;; Private functions
;;;

(defun hyperbase:already-displayed-p (name)
  "Test if a buffer allready is displayed."
  (let (screenid)
    (setq screenid (ehts-find-buffer-screen name))
    (if screenid
	(progn
	  (switch-screen screenid)
	  t)
      nil)))

(defun hyperbase:attr-help (node-link-spec)
  "Show the attributes of a node or a button link from NODE-LINK-SPEC.
A string value of NODE-LINK-SPEC means show attributes for that button link.
A buffer value means show attributes for the node in that buffer."
  (interactive)
  (or (stringp node-link-spec) (bufferp node-link-spec)
      (error "(hyperbase-show-attributes): Non-string or buffer argument."))
  (let (entity name string number buffer screenid)
    (setq buffer (if (bufferp node-link-spec) (buffer-name node-link-spec))
	  entity (cdr (assoc (if buffer "node" "link") node-link-list))
	  buffer (or buffer (buffer-name)))
    (if (eq (string-match "Attributes - " buffer) 0)
	nil
      (if (= entity 0)
	  (progn
	    (setq name (concat "Attributes - " buffer))
	    (if (not (hyperbase:already-displayed-p name))
		(progn
		  (setq number (cdr (assoc buffer ehts-node-name-alist))
			string (ehts-create-node-attribute-string number))
		  (ehts-setup-attribute-screen name string entity buffer))))
	(if (eq ehts-node-link-alist '())
	    (error "No links in this node."))
	(setq name (concat "Attributes - "
			   (car (assoc node-link-spec ehts-node-link-alist))))
	(if (not (hyperbase:already-displayed-p name))
	    (progn
	      (setq number (cdr (assoc (substring name 13)
				       ehts-node-link-alist))
		    string (ehts-create-link-attribute-string number))
	      (ehts-setup-attribute-screen name string entity buffer)))))))

;;;
;;; Private variables
;;;

(provide 'hsys-hbase)

;;; hsys-hbase.el ends here
