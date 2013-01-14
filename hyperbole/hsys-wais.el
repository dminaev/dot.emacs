;;; hsys-wais.el --- Hyperbole support for WAIS browsing.

;; Copyright (C) 1991, 1995 Free Software Foundation, Inc.
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
;;   For information on WAIS, see ftp://ftp.wais.com:/pub/.
;;   The freeware/ subdirectory there contains free source code to support
;;   WAIS on most standard architectures.
;;

;;; Code:

;;;
;;; Other required Elisp libraries
;;;

;; Autoload wais.
(autoload 'wais              "wais" "Client-server full-text retrieval"  t)
(autoload 'waisd-mode        "wais" "Wide Area Information Services"     t)
(autoload 'wais-select-question "wais" "Select a new WAIS question."     t)
(autoload 'wais-create-question "wais" "Create a new WAIS question."     t)

;;;
;;; Public implicit button types
;;;

(defib wais-smart ()
  "Handles context-sensitive Smart Key in WAIS buffers."
  (if (featurep 'wais)
      (let ((b (buffer-name)))
	(cond ((string-match ": Find Documents On\\|: Similar To" b)
	       (hact 'wais-smart 'wais-query))
	      ((equal "*Source List*" b)
	       (hact 'wais-smart 'source-menu-view))
	      ((string-match ": On Sources" b)
	       (hact 'wais-smart
		     (if (eobp) 'wais-view-source 'source-menu-view)))
	      ((string-match ": Results" b)
	       (hact 'wais-smart 'wais-edit))
	      ))))

;;;
;;; Public button action types
;;;

(defact wais-ques (question-name)
  "Loads a Wais Question QUESTION-NAME, displays it with WAIS Emacs interface."
  (interactive "sWAIS Question name: ")
  (if (or (featurep 'wais) (load "wais"))
      (progn
	(display-question question-name)
	(wais)
	(display-question question-name)
	)
    (error "(wais-ques): WAIS interface not available so can't load question.")
    ))

(defact wais-smart (interactive-func)
  "Performs INTERACTIVE-FUNC in a Wais buffer."
  (call-interactively interactive-func))

(provide 'hsys-wais)

;;; hsys-wais.el ends here
