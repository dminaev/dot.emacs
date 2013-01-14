;;; hui-epV4-b.el --- Support color and flashing of hyper-buttons under Epoch V4

;; Copyright (C) 1991-1995, Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;; Maintainer: Mats Lidell <matsl@contactor.se>
;; Keywords: faces, hypermedia

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
;;   Requires Epoch 4.0a or greater.
;;
;;   This is truly prototype code.
;;

;;; Code:

(if (and (boundp 'epoch::version) (stringp epoch::version)
	 (or noninteractive (not (string-lessp epoch::version "Epoch 4"))))
    nil
  (error "(hui-epV4-b.el): Load only under Epoch V4 or higher."))

(load "button")
(require 'hui-ep-but)

(defun hproperty:background ()
  "Returns default background color for selected frame."
  (epoch::background))

(defun hproperty:foreground ()
  "Returns default foreground color for selected frame."
  (epoch::foreground))

;;;
;;; Public variables
;;;

(defvar hproperty:item-highlight-color (foreground)
  "Color with which to highlight list/menu selections.
Call (hproperty:set-item-highlight <color>) to change value.")

;;;
;;; Public functions
;;;

(defun hproperty:but-create (&optional start-delim end-delim regexp-match)
  "Mark all hyper-buttons in buffer as Epoch buttons, for later highlighting.
Will use optional strings START-DELIM and END-DELIM instead of default values.
If END-DELIM is a symbol, e.g. t, then START-DELIM is taken as a regular
expression which matches an entire button string.
If REGEXP-MATCH is non-nil, only buttons matching this argument are
highlighted."
  ;; Clear out Hyperbole button zones.
  (hproperty:but-clear)
  ;; Then recreate them.
  (hproperty:but-create-all start-delim end-delim regexp-match))

(defun hproperty:but-clear ()
  "Delete all Hyperbole button zones from current buffer."
  (interactive)
  (mapcar (function (lambda (zone)
		      (if (eq (epoch::zone-style zone) hproperty:but)
			  (epoch::delete-zone zone))))
	  (epoch::zone-list)))

(defun hproperty:cycle-but-color (&optional color)
  "Switches button color to optional COLOR name or next item referenced by hproperty:color-ptr."
  (interactive "sHyperbole button color: ")
  (if (<= (epoch::number-of-colors) 2)
      nil
    (if color (setq hproperty:color-ptr nil))
    (epoch::set-style-foreground
     hproperty:but
     (or color (car (hproperty:list-cycle
		     hproperty:color-ptr hproperty:good-colors))))
    (hproperty:set-flash-color)
    (redraw-display)
    t))

(defun hproperty:but-flash ()
  "Flash a Hyperbole button at point to indicate selection, when using Epoch."
  (interactive)
  (let ((ibut) (prev)
	(start (hattr:get 'hbut:current 'lbl-start))
	(end   (hattr:get 'hbut:current 'lbl-end))
	(b) (a))
    (if (and start end (setq prev (epoch::button-at start)
			     ibut t))
	(progn (if (not prev) (hproperty:but-add start end hproperty:but))
	       (setq b (and start (epoch::button-at start))))
      (setq b (button-at (point))))
    (if (setq a (and (epoch::buttonp b) (epoch::button-style b)))
	(progn
	  (epoch::set-button-style b hproperty:flash-face)
	  (epoch::redisplay-screen)
	  ;; Delay before redraw button
	  (let ((i 0)) (while (< i hproperty:but-flash-time) (setq i (1+ i))))
	  (epoch::set-button-style b a)
	  (epoch::redisplay-screen)
	  ))
    (if (and ibut (not prev)) (hproperty:but-delete start))
    ))

(defun hproperty:set-item-highlight (&optional background foreground)
  "Setup or reset item highlight style using optional BACKGROUND and FOREGROUND."
  (make-local-variable 'hproperty:item-face)
  (if (stringp background) (setq hproperty:item-highlight-color background))
  (if (not hproperty:highlight-face)
      (progn 
	(setq hproperty:highlight-face (make-style))
	(set-style-foreground hproperty:highlight-face (background))
	(set-style-underline hproperty:highlight-face nil)))

  (let ((update-rolo-highlight-flag
	 (and (boundp 'rolo-highlight-face) (stylep rolo-highlight-face)
	      (or (null (style-foreground rolo-highlight-face))
		  (equal (style-foreground hproperty:highlight-face)
			 (style-foreground rolo-highlight-face))))))
    (if (not (equal (style-background hproperty:highlight-face)
		    (get-color hproperty:item-highlight-color)))
	(set-style-background hproperty:highlight-face
			      hproperty:item-highlight-color))
    (and background (not (equal (style-background hproperty:highlight-face)
				(get-color background)))
	 (set-style-background hproperty:highlight-face background))
    (and foreground (not (equal (style-foreground hproperty:highlight-face)
				(get-color foreground)))
	 (set-style-foreground hproperty:highlight-face foreground))
    (setq hproperty:item-face hproperty:highlight-face)
    (if update-rolo-highlight-flag
	(progn
	  (set-style-background rolo-highlight-face
				(style-background hproperty:highlight-face))
	  (set-style-foreground rolo-highlight-face
				(style-foreground hproperty:highlight-face))
	  (set-style-font rolo-highlight-face
			  (style-font hproperty:highlight-face))
	  (set-style-underline rolo-highlight-face
			       (style-underline hproperty:highlight-face))))))

(defun hproperty:select-item (&optional pnt)
  "Select item in current buffer at optional position PNT using hproperty:item-face."
  (or hproperty:item-button
      (setq hproperty:item-button (add-button (point) (point) hproperty:item-face)))
  (if pnt (goto-char pnt))
  (skip-chars-forward " \t")
  (skip-chars-backward "^ \t\n")
  (let ((start (point)))
    (save-excursion
      (skip-chars-forward "^ \t\n")
      (move-button hproperty:item-button start (point))
      ))
  (epoch::redisplay-screen)
  )

(defun hproperty:select-line (&optional pnt)
  "Select line in current buffer at optional position PNT using hproperty:item-face."
  (or hproperty:item-button
      (setq hproperty:item-button (add-button (point) (point) hproperty:item-face)))
  (if pnt (goto-char pnt))
  (save-excursion
    (beginning-of-line)
    (move-button hproperty:item-button (point) (progn (end-of-line) (point)))
    )
  (epoch::redisplay-screen)
  )

;;;
;;; Private functions
;;;

(defun hproperty:set-flash-color ()
  "Set button flashing colors based upon current color set."
  (if (<= (epoch::number-of-colors) 2)
      nil
    (epoch::set-style-background hproperty:flash-face (hproperty:but-color))
    (epoch::set-style-foreground hproperty:flash-face (hproperty:background))
    ))

;;;
;;; Private variables
;;;

(defvar hproperty:but (epoch::make-style) "Style for hyper-buttons.")
(epoch::set-style-foreground hproperty:but (hproperty:but-color))
(epoch::set-style-background hproperty:but (hproperty:background))

(defvar hproperty:flash-face (epoch::make-style)
  "Style for flashing hyper-buttons.")
(hproperty:set-flash-color)

(defvar hproperty:item-button nil
  "Button used to highlight an item in a listing buffer.")
(make-variable-buffer-local 'hproperty:item-button)
(defvar hproperty:item-face nil "Style for item marking.")
(defvar hproperty:highlight-face nil
  "Item highlighting face.  Use (hproperty:set-item-highlight) to set.")
(if hproperty:highlight-face
    nil
  ;; Reverse foreground and background colors for default block-style highlighting.
  (hproperty:set-item-highlight (hproperty:foreground) (hproperty:background)))

(provide 'hui-epV4-b)

;;; hui-epV4-b.el ends here
