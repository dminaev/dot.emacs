;;; hhist.el --- Maintains history of Hyperbole buttons selected.

;; Copyright (C) 1991-1995 Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;; Maintainer: Mats Lidell <matsl@contactor.se>
;; Keywords: hypermedia

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
;;   This is minimal right now and will be extended.
;;   Currently, it implements a push-pop stack of traversed locations.
;;
;;   It will be extended to allow random access to previous locations
;;   and to store traversal histories for later recall.
;;

;;; Code:

;;;
;;; Public functions
;;;

(defun hhist:add (elt)
  "Adds ELT to hyper-history list if not the same as current or previous loc.
ELT must have been created via a call to 'hhist:element'."
  ;; Even though this next line looks useless, it cures a problem with
  ;; window buffer correspondences on startup, so don't remove it.
  (set-buffer (window-buffer (selected-window)))
  (let ((prev-buf (car elt)))
    (if (or (equal prev-buf (buffer-name))
	    (equal prev-buf (car (car *hhist*))))
	nil
      (setq *hhist* (cons elt *hhist*)))))

(defun hhist:element ()
  "Returns a history element for current point location."
  (list (current-buffer) (point)))

(defun hhist:remove (&optional arg)
  "Removes optional prefix ARG entries from history, returns to ARGth location.
The command is ignored with ARG < 1."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((prev-buf-line))
    (if (null *hhist*)
	(and (> arg 0)
	     (message "(hhist:remove): No previous source to which to return.")
	     (beep))
      (while (and (> arg 0) *hhist*)
	(setq prev-buf-line (car *hhist*)
	      *hhist* (cdr *hhist*)
	      arg (1- arg)))
      (switch-to-buffer (car prev-buf-line))
      (goto-char (car (cdr prev-buf-line)))
      )))

(defun hhist:init ()
  "Resets history list."
  (interactive)
  (setq *hhist* nil))

;;;
;;; Private functions
;;;

(defun hhist:wind-line ()
  "Returns window relative line number that point is on."
  (max 0 (1- (- (count-lines 1 (1+ (point)))
		(count-lines 1 (window-start))))))

;;;
;;; Private variables
;;;

(defconst *hhist* nil
  "List of previously visited Hyperbole button source locations.
Car of list is most recent.")

(provide 'hhist)

;;; hhist.el ends here
