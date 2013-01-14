;;; wconfig.el --- Saves and yanks from save ring of window configurations.

;; Copyright (C) 1989-1995, Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;; Maintainer: Mats Lidell <matsl@contactor.se>
;; Keywords: frames, hypermedia

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
;;   This library provides two unrelated means of managing window
;;   configurations, (the set of windows and associated buffers within a
;;   frame).  The first means associates a name with each stored window
;;   configuration.  The name can then be used to retrieve the window
;;   configuration later.  The following functions provide this behavior:
;;
;;      wconfig-add-by-name
;;      wconfig-delete-by-name
;;      wconfig-restore-by-name
;;
;;   The second means of window configuration management is through
;;   the use of a ring structure, just like the Emacs kill ring except
;;   the elements stored are window configurations instead of textual
;;   regions. These configurations are local for a frame so there is a
;;   ring of configurations for each frame.  The following functions
;;   support storage and sequential retrieval of window
;;   configurations:
;;
;;      wconfig-ring-save
;;      wconfig-yank-pop
;;      wconfig-delete-pop
;;
;;   None of this information is stored between Emacs sessions, so your
;;   window configurations will last only through a single session of use.
;;
;;   Based in part on kill-ring code from simple.el.
;;

;;; Code:

;;;
;;; Recommended key bindings
;;;

;;; Set up in local "hyperbole.el".

;;;
;;; Other required Elisp libraries
;;;
(require 'ring)
(require 'hargs)
(require 'set)

;;;
;;; Public variables
;;;

(defconst wconfig-ring-max 10
  "*Maximum length of window configuration ring before oldest elements are deleted.")

;;;
;;; Public functions
;;;

;;; Handling of name associations with each stored window configuration.
;;;###autoload
(defun wconfig-add-by-name (name)
  "Saves the current window configuration under the string NAME.
When called interactively and a window configuration already exists under
NAME, confirms whether or not to replace it."
  (interactive "sName for current window configuration: ")
  (or (stringp name)
      (error "(wconfig-add-by-name): NAME argument is not a string: %s" name))
  (let ((set:equal-op (function (lambda (key elt)
				  (equal key (car elt))))))
    (if (or (not (interactive-p))
	    (not (set:member name wconfig-names))
	    (y-or-n-p
	      (format "Replace existing '%s' window configuration: " name)))
	(progn (setq wconfig-names
		     (set:replace name (current-window-configuration)
				  wconfig-names))
	       (if (interactive-p)
		   (message "Window configuration '%s' saved.  Use 'wconfig-restore-by-name' to restore." name))))))

;;;###autoload
(defun wconfig-delete-by-name (name)
  "Deletes window configuration saved under NAME."
  (interactive (list (hargs:read-match "Delete window configuration named: "
				       wconfig-names nil t)))
  (or (stringp name)
      (error "(wconfig-delete-by-name): NAME argument is not a string: %s" name))
  (let ((set:equal-op (function (lambda (key elt)
				  (equal key (car elt))))))
    (setq wconfig-names (set:remove name wconfig-names))))

;;;###autoload
(defun wconfig-restore-by-name (name)
  "Restores window configuration saved under NAME."
  (interactive (list (hargs:read-match "Restore window configuration named: "
				       wconfig-names nil t)))
  (or (stringp name)
      (error "(wconfig-restore-by-name): NAME argument is not a string: %s" name))
  (let ((wconfig (set:get name wconfig-names)))
    (if wconfig
	(set-window-configuration wconfig)
      (error "(wconfig-restore-by-name): No window configuration named '%s'" name))))

;;; Window configuration ring management (like text kill ring).
;;;###autoload
(defun wconfig-delete-pop ()
  "Replaces current window config with most recently saved config in ring.
Then deletes this new configuration from the ring."
  (interactive)
  (let ((ring (wconfig-get-ring)))
    (if (ring-empty-p ring)
	(error "(wconfig-delete-pop): Window configuration save ring is empty")
      (ring-remove ring 0)
      (if (ring-empty-p ring)
	  (message "(wconfig-delete-pop): Window configuration save ring is empty")
	(set-window-configuration (ring-ref ring 0))))))

;;;###autoload
(defun wconfig-ring-save ()
  "Saves the current window configuration onto the save ring.
Use {\\[wconfig-yank-pop]} to restore it at a later time."
  (interactive)
  (ring-insert (wconfig-get-ring) (current-window-configuration))
  (if (interactive-p)
      (message "Window configuration saved.  Use 'wconfig-yank-pop' to restore.")))

;;;###autoload
(defun wconfig-yank-pop (n)
  "Replaces current window config with prefix arg Nth prior one in save ring.
Interactively, default value of N = 1, meaning the last saved window
configuration is displayed.

The sequence of window configurations wraps around, so that after the oldest
one comes the newest one."
  (interactive "p")
  (let ((ring (wconfig-get-ring)))
    (if (ring-empty-p ring)
	(error "(wconfig-yank-pop): Window configuration save ring is empty")
      (let ((prev (ring-remove ring (- 1 n))))
	(ring-insert-at-beginning ring prev)
	(set-window-configuration (ring-ref ring 0))))))

;;; Frames
(defun wconfig-get-ring ()
  (let* ((frame (selected-frame))
	 (ring (frame-property frame 'wconfig-ring)))
    (if (not ring)
	(set-frame-property frame 'wconfig-ring (setq ring (make-ring wconfig-ring-max))))
    ring))

;;;
;;; Private variables
;;;

(defvar wconfig-names (set:create)
  "Set of (name . window-configuration) elements.")

(run-hooks 'wconfig-load-hook)

(provide 'wconfig)

;;; wconfig.el ends here
