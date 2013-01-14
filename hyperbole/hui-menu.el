;;; hui-menu.el --- InfoDock/Emacs menubar menu of Hyperbole commands.

;; Copyright (C) 1994, 1995, 2007  Free Software Foundation, Inc.
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

;;; Code:

;;;
;;; Other required Elisp libraries
;;;

(require 'wrolo-menu)
(require 'easymenu)

;;;
;;; Public functions
;;;

;; Add Hyperbole menu to menubar.
(defun hyperbole-menubar-menu ()
  "Add the Hyperbole menu to the global menubar."
  (and hyperb:xemacs-p
       (add-menu nil (car hui-menu-global-menu) (cdr hui-menu-global-menu))))

(defun hui-menu-remove ()
  "Remove Hyperbole menu from the global menubars."
  (if hyperb:xemacs-p
      (delete-menu-item (list (car hui-menu-global-menu)))
    (easy-menu-remove-item nil nil (car hui-menu-global-menu))))

;;;
;;; Public variables
;;;

(defconst hui-menu-global-menu
  (delq nil
	(list
	 "Hyperbole"
	 '["Activate-Button-at-Point" hui:hbut-act t]
	 '["Back-to-Prior-Location" (hhist:remove current-prefix-arg) t]
	 '("Button-File"
	   ["Edit-Per-Directory-File" (find-file hbmap:filename) t]
	   ["Edit-Personal-File" (find-file
				  (expand-file-name
				   hbmap:filename hbmap:dir-user)) t]
	   "----"
	   ["Manual"  (id-info "(hyperbole.info)Button Files") t]
	   )
	 '("Customize"
	   ["Customize Hyperbole..." hyperb:customize t]
	   "---"
	   ["Display URLs in ..." (customize-variable 'browse-url-browser-function) t] 
	   )
	 '("Documentation"
	   ["Manual"      (id-info "(hyperbole.info)Top") t]
	   "----"
	   ["Copyright"      (id-info "(hyperbole.info)Top") t]
	   ["Demonstration"  (find-file-read-only
			      (expand-file-name "DEMO" hyperb:dir)) t]
	   ["Manifest"       (find-file-read-only
			      (expand-file-name "MANIFEST" hyperb:dir)) t]
	   ["Glossary"    (id-info "(hyperbole.info)Glossary") t]
	   ["Mail-Lists"  (id-info "(hyperbole.info)Mail Lists") t]
	   ["New-Features" (progn
			     (hact 'link-to-regexp-match
				   "\\*[ \t]+What's New" 2
				   (expand-file-name "README" hyperb:dir))
			     (setq buffer-read-only nil)
			     (toggle-read-only)) t]
	   ["Smart-Key-Summary" (id-browse-file (hypb:mouse-help-file)) t]
	   )
	 '("Explicit-Button"
	   ("Activate" :filter hui-menu-explicit-buttons-filter)
	   "----"
	   ["Activate-at-Point" hui:hbut-act t]
	   ["Create" hui:ebut-create t]
	   ["Delete" hui:ebut-delete t]
	   ["Edit"   hui:ebut-modify t]
	   ("Help"  
	    ["Buffer-Buttons"   (hui:hbut-report -1) t]
	    ["Current-Button"   (hui:hbut-report)    t]
	    ["Ordered-Buttons"  (hui:hbut-report 1)  t]
	    "----"
	    ["Manual"   (id-info "(hyperbole.info)Location") t]
	    )
	   ["Modify" hui:ebut-modify t]
	   ["Rename" hui:ebut-rename t]
	   ["Search" hui:ebut-search t]
	   "----"
	   ["Manual"   (id-info "(hyperbole.info)Explicit Buttons") t]
	   )
	 '("Global-Button"
	   ("Activate" :filter hui-menu-global-buttons-filter)
	   "----"
	   ["Create" hui:gbut-create t]
	   ["Edit"   hui:gbut-modify t]
	   ["Help"   gbut:help t]
	   ["Modify" hui:gbut-modify t]
	   "----"
	   ["Manual" (id-info "(hyperbole.info)Global Buttons") t]
	   )
	 '("Implicit-Button"
	   ["Activate-at-Point"    hui:hbut-act t]
	   ["Delete-Type"         (hui:htype-delete 'ibtypes) t]
	   ["Help"   hui:hbut-help t]
	   ["Types"  (hui:htype-help 'ibtypes 'no-sort) t]
	   "----"
	   ["Manual"   (id-info "(hyperbole.info)Implicit Buttons") t]
	   )
	 '("Mail-Lists"
	   "----"
	   ["Mail To Hyperbole-Users ..."
	    (hmail:compose "hyperbole-users@gnu.org" '(hact 'hyp-config)) t]
	   ["Manage Hyperbole-Users Subscription"
	    (browse-url "http://lists.gnu.org/mailman/listinfo/hyperbole-users") t]
	   "----"
	   ["Send Bug Report ..."
	    (hmail:compose "bug-hyperbole@gnu.org" '(hact 'hyp-config)) t]
	   ["Manage Bug-Hyperbole Subscription"
	    (browse-url "http://lists.gnu.org/mailman/listinfo/bug-hyperbole") t]
	   "----"
	   ["Manual" (id-info "(hyperbole.info)Suggestion or Bug Reporting") t]
	   )
	 (if hyperb:kotl-p
	     '("Outline"
	       ["Create-File"    kfile:find t]
	       ["View-File"      kfile:view t]
	       "----"
	       ["Collapse-Tree" (progn (kotl-mode:is-p)
				       (kotl-mode:hide-tree
					(kcell-view:label))) t]
	       ["Create-Link" klink:create t]
	       ["Expand-All-Trees" kotl-mode:show-all t]
	       ["Expand-Tree" (progn (kotl-mode:is-p)
				     (kotl-mode:show-tree
				      (kcell-view:label))) t]
	       ["Show-Top-Level-Only" kotl-mode:hide-body t]
	       "----"
	       ["Manual" (id-info "(hyperbole.info)Outliner") t]
	       ["Example"   (find-file-read-only
			     (expand-file-name
			      "EXAMPLE.kotl" (concat hyperb:dir "kotl/")))
		t]
	       ))
	 infodock-wrolo-menu
	 '("Types"
	   ["Action-Types-Manual"
	    (id-info "(hyperbole.info)Action Types") t]
	   ["Implicit-Button-Types-Manual"
	    (id-info "(hyperbole.info)Implicit Buttons") t]
	   "----"
	   ["Action-Types"      (hui:htype-help 'actypes) t]
	   ["Implicit-Button-Types" (hui:htype-help 'ibtypes 'no-sort) t]
	   )
	 '("Window-Configuration"
	   ["Name-Configuration" wconfig-add-by-name     t]
	   ["Delete-Name"        wconfig-delete-by-name  t]
	   ["Restore-Name"       wconfig-restore-by-name t]
	   "----"
	   ["Pop-from-Ring"      wconfig-delete-pop      t]
	   ["Save-to-Ring"       wconfig-ring-save       t]
	   ["Yank-from-Ring"     wconfig-yank-pop        t]
	   "----"
	   ["Manual" (id-info "(hyperbole.info)Window Configurations") t]
	   )
	 "----"
	 '["Browse-Manual"      (id-info "(hyperbole.info)Top") t]
	 "----"
	 '["Quit" (progn
		    ;; Delete Hyperbole menu item from all menubars.
		    (mapcar
		     (function
		      (lambda (buf)
			(set-buffer buf)
			(hui-menu-remove)))
		     (buffer-list))
		    ;;
		    ;; Remove Hyperbole button comment from future
		    ;; outgoing mail.
		    (if (boundp 'smail:comment)
			(setq smail:comment "")))
	   t]
	 )))

;; Dynamic menus for Global and Explicit buttons.
(defvar hui-menu-max-list-length 24
  "Limits the length of a Hyperbole dynamic menu lists.")

;; List existing global buttons for menu activation.
(defun hui-menu-global-buttons-filter (rest-of-menu)
  (append
   (let ((labels (delq nil (gbut:lbl-list)))
	 (cutoff))
     (if labels
	 (progn
	   ;; Cutoff list if too long.
	   (if (setq cutoff (nthcdr (1- hui-menu-max-list-length) labels))
	       (setcdr cutoff nil))
	   (append
	    (mapcar (function (lambda (label)
				(vector label `(gbut:act ,label) t)))
		    (sort labels 'string-lessp))
	    (if cutoff '("..."))
	    ))))
   rest-of-menu))

(defun hui-menu-explicit-but-act (label)
  "Activate explicit button with LABEL."
  (hbut:act (ebut:get (hbut:label-to-key label))))

;; List existing explicit buttons for menu activation.
(defun hui-menu-explicit-buttons-filter (rest-of-menu)
  (append
   (let ((labels (delq nil (ebut:list)))
	 (cutoff))
     (if labels
	 (progn
	   ;; Cutoff list if too long.
	   (if (setq cutoff (nthcdr (1- hui-menu-max-list-length) labels))
	       (setcdr cutoff nil))
	   (append
	    (mapcar (function (lambda (label)
				(vector label `(hui-menu-explicit-but-act ,label) t)))
		    (sort labels 'string-lessp))
	    (if cutoff '("..."))
	    ))))
   rest-of-menu))

;; Create the menu
(if hyperb:xemacs-p
    (easy-menu-define hui-menu-global-menu nil "Hyperbole" hui-menu-global-menu)
  (easy-menu-add-item nil nil (easy-menu-create-menu 
			       (car hui-menu-global-menu)
			       (cdr hui-menu-global-menu)) 
		      'props))

;;;
;;; Private variables
;;;

(provide 'hui-menu)

;;; hui-menu.el ends here
