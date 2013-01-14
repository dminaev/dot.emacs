;;; wrolo-menu.el --- Pulldown and popup menus of Hyperbole rolodex commands.

;; Copyright (C) 1994-1995, 2006 Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;; Maintainer: Mats Lidell <matsl@contactor.se>
;; Keywords: hypermedia, matching, mouse

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
;;; Public variables
;;;

(require 'wrolo)

;;; This definition is used by InfoDock and XEmacs.
(defconst infodock-wrolo-menu
  '("Rolodex"
    ["Add-Entry"         (id-tool-invoke 'rolo-add) t]
    ["Delete-Entry"      (id-tool-invoke 'rolo-kill) t]
    ["Display-Prior-Matches" (id-tool-invoke 'rolo-display-matches) t]
    ["Edit-Entry"        (id-tool-invoke 'rolo-edit) t]
    ["Edit-Rolodex"      (id-tool-invoke
			  '(progn (require 'wrolo)
				  (find-file (car rolo-file-list))
				  (setq buffer-read-only nil)))
     t]
    ["Insert-Entry-at-Point" (id-tool-invoke 'rolo-yank) t]
    ["Mail-to-Address"   (id-tool-invoke 'rolo-mail-to) t]
    ["Search-for-Regexp" (id-tool-invoke 'rolo-grep)  t]
    ["Search-for-String" (id-tool-invoke 'rolo-fgrep) t]
    ["Search-for-Word"   (id-tool-invoke 'rolo-word)  t]
    ["Sort-Entries"      (id-tool-invoke 'rolo-sort)  t]
    "----"
    ["Manual"            (id-tool-invoke id-man-rolodex) t]
    ))

;;; This definition is used by InfoDock only.
(defconst id-menubar-wrolo
  (list
   '("Wrolo"
     ["Help"                describe-mode                  t]
     ["Manual"              (id-info "(hyperbole.info)Rolo Keys") t]
     "----"
     ["Toggle-Read-Only"    toggle-read-only               t]
     ["Write (Save as)"     write-file                     t]
     "----"
     ["Quit"                (id-tool-quit '(kill-buffer nil))  t]
     )
   '["Edit-Entry-at-Point"  rolo-edit-entry         t]
    ["Mail-to-Address"      (id-tool-invoke 'rolo-mail-to) t]
   '("Move"
     ["Scroll-Backward"     scroll-down             t]
     ["Scroll-Forward"      scroll-up               t]
     ["To-Beginning"        beginning-of-buffer     t]
     ["To-End"              end-of-buffer           t]
     "----"
     ["To-Next-Entry"          outline-next-visible-heading t]
     ["To-Next-Same-Level"     outline-forward-same-level t]
     ["To-Previous-Entry"      outline-previous-visible-heading t]
     ["To-Previous-Same-Level" outline-backward-same-level t]
     ["Up-a-Level"             outline-up-heading t]
     )
   '("Outline"
     ["Hide (Collapse)"      hide-subtree           t]
     ["Show (Expand)"        show-subtree           t]
     ["Show-All"             show-all               t]
     ["Show-Only-First-Line" hide-body              t]
     )
   '["Next-Match"          rolo-next-match         t]
   '["Previous-Match"      rolo-previous-match     t]
   infodock-wrolo-menu
   ))

;;; This definition is used by InfoDock and XEmacs.
(defconst id-popup-wrolo-menu
  (list
    "Wrolo"
    '["Edit-Entry-at-Point" rolo-edit-entry         t]
    "----"
    '["Next-Match"          rolo-next-match         t]
    '["Previous-Match"      rolo-previous-match     t]
    "----"
    '("Move"
      ["Scroll-Backward"     scroll-down             t]
      ["Scroll-Forward"      scroll-up               t]
      ["To-Beginning"        beginning-of-buffer     t]
      ["To-End"              end-of-buffer           t]
      "----"
      ["To-Next-Entry"          outline-next-visible-heading t]
      ["To-Next-Same-Level"     outline-forward-same-level t]
      ["To-Previous-Entry"      outline-previous-visible-heading t]
      ["To-Previous-Same-Level" outline-backward-same-level t]
      ["Up-a-Level"             outline-up-heading t]
      )
    '("Outline"
      ["Hide (Collapse)"      hide-subtree           t]
      ["Show (Expand)"        show-subtree           t]
      ["Show-All"             show-all               t]
      ["Show-Only-First-Line" hide-body              t]
      )
    infodock-wrolo-menu
    "----"
    '["Help"                describe-mode           t]
    '["Manual"              (id-info "(hyperbole.info)Rolo Keys") t]
    "----"
    '["Quit"                (id-tool-invoke 'rolo-quit) t]
    ))

;;;
;;; Public functions
;;;

;;; This definition is used only by XEmacs and Emacs19.
(defun wrolo-menubar-menu ()
  "Add a Hyperbole Rolodex menu to the rolodex match buffer menubar."
  (cond ((fboundp 'popup-mode-menu)
	 (setq mode-popup-menu id-popup-wrolo-menu))
	(hyperb:xemacs-p
	 (define-key wrolo-mode-map 'button3 'wrolo-popup-menu))
	(t ;; hyperb:emacs19-p
	 (define-key wrolo-mode-map [down-mouse-3] 'wrolo-popup-menu)
	 (define-key wrolo-mode-map [mouse-3] nil)))
  (if (and (boundp 'current-menubar)
	   (or hyperb:emacs19-p current-menubar)
	   (not (car (find-menu-item current-menubar '("Wrolo")))))
      (progn
	(set-buffer-menubar (copy-sequence current-menubar))
	(if (fboundp 'add-submenu)
	    (add-submenu nil id-popup-wrolo-menu)
	  (add-menu nil (car id-popup-wrolo-menu)
		    (cdr id-popup-wrolo-menu))))))

;;; This definition is used only by XEmacs and Emacs19.
(defun wrolo-popup-menu (event)
  "Popup the Hyperbole Rolodex match buffer menu."
  (interactive "@e")
  (mouse-set-point event)
  (if (fboundp 'popup-mode-menu)
      (popup-mode-menu)
    (popup-menu id-popup-wrolo-menu)))

(cond ((null hyperb:window-system))
      ((fboundp 'id-menubar-set)
       ;; InfoDock under a window system
       (require 'id-menubars)
       (id-menubar-set 'wrolo-mode 'id-menubar-wrolo))
      (hyperb:xemacs-p
       ;; XEmacs under a window system
       (add-hook 'wrolo-mode-hook 'wrolo-menubar-menu))
      (hyperb:emacs19-p
       ;; Emacs 19 under a window system
       t))

(require 'easymenu)
(easy-menu-define wrolo-mode-menubar-menu wrolo-mode-map "Wrolo-mode menu" id-popup-wrolo-menu)

(provide 'wrolo-menu)

;;; wrolo-menu.el ends here
