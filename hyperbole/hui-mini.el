;;; hui-mini.el --- One line command menus for Hyperbole

;; Copyright (C) 1991-1995, 2004, 2006, 2007 Free Software Foundation, Inc.
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

(require 'hypb)

;;;
;;; Public variables
;;;

(defvar hui:menu-select "\C-m"
  "*Upper case char-string which selects the Hyperbole menu item at point.")
(defvar hui:menu-quit   "Q"
  "*Upper case char-string which quits selecting from a Hyperbole menu item.")
(defvar hui:menu-abort  "\C-g"
  "*Same function as 'hui:menu-quit'.")
(defvar hui:menu-top    "\C-t"
  "*Character which returns to top Hyperbole menu.")

(defvar hui:menu-p nil
  "Non-nil iff a current Hyperbole menu activation exists.")

(defvar hui:menus nil
  "Command menus for use with the default Hyperbole user interface.")
(setq
 hui:menus
 (delq nil
 (list (cons
	'hyperbole
	(append
	 (let ((version (if (= (aref hyperb:version 0) ?0)
			    (substring hyperb:version 1)
			  hyperb:version)))
	   (list (list (concat "Hy" version ">"))))
	 (delq nil
	       (list
		'("Act"         hui:hbut-act
		  "Activates button at point or prompts for explicit button.")
		'("Butfile/"    (menu . butfile)
		  "Quick access button files menus.")
		'("Doc/"        (menu . doc)
		  "Quick access to Hyperbole documentation.")
		'("Ebut/"       (menu . ebut)
		  "Explicit button commands.")
		'("Gbut/"       (menu . gbut)
		  "Global button commands.")
		'("Hist"        (hhist:remove current-prefix-arg)
		  "Jumps back to location prior to last Hyperbole button follow.")
		'("Ibut/"       (menu . ibut)
		  "Implicit button and button type commands.")
		'("Msg/"        (menu . msg)
		  "Mail and News messaging facilities.")
		(if hyperb:kotl-p
		    '("Otl/"        (menu . otl)
		      "Autonumbered outlining and hyper-node facilities."))
		'("Rolo/"       (menu . rolo)
		  "Hierarchical, multi-file rolodex lookup and edit commands.")
		'("Win/"       (menu . win)
		  "Window configuration management command.")
		))))
       '(butfile .
	 (("Butfile>")
	  ("DirFile"      (find-file hbmap:filename)
	   "Edits directory-specific button file.")
	  ("Info"
	   (id-info "(hyperbole.info)Button Files")
	   "Displays manual section on button files.") 
	  ("PersonalFile" (find-file
			    (expand-file-name hbmap:filename hbmap:dir-user))
	   "Edits user-specific button file.")
	  ))
       '(doc .
	 (("Doc>")
	  ("Demo"         (find-file-read-only
			    (expand-file-name "DEMO" hyperb:dir))
	   "Demonstrates Hyperbole features.")
	  ("Files"        (find-file-read-only
			    (expand-file-name "MANIFEST" hyperb:dir))
	   "Summarizes Hyperbole system files.  Click on an entry to view it.")
	  ("Glossary"
	   (id-info "(hyperbole.info)Glossary")
	   "Glossary of Hyperbole terms.")
	  ("HypbCopy"  (id-info "(hyperbole.info)Top")
	   "Displays general Hyperbole copyright and license details.")
	  ("Info"      (id-info "(hyperbole.info)Top")
	   "Online Info version of Hyperbole manual.")
	  ("MailLists" (id-info "(hyperbole.info)Mail Lists")
	   "Details on Hyperbole mail list subscriptions.")
	  ("New"          (progn
			    (hact 'link-to-regexp-match
				  "\\*[ \t]+What's New" 2
				  (expand-file-name "README" hyperb:dir))
			    (setq buffer-read-only nil)
			    (toggle-read-only))
	   "Recent changes to Hyperbole.")
	  ("SmartKy"      (find-file-read-only (hypb:mouse-help-file))
	   "Summarizes Smart Key mouse or keyboard handling.")
	  ("Types/"       (menu . types)
	   "Provides documentation on Hyperbole types.")
	 ))
       '(ebut .
	 (("EButton>")
	  ("Act"    hui:hbut-act
	    "Activates button at point or prompts for explicit button.")
	  ("Create" hui:ebut-create)
	  ("Delete" hui:ebut-delete)
	  ("Edit"   hui:ebut-modify "Modifies any desired button attributes.")
	  ("Help/"  (menu . ebut-help) "Summarizes button attributes.")
	  ("Info"
	   (id-info "(hyperbole.info)Explicit Buttons")
	   "Displays manual section on explicit buttons.")
	  ("Modify" hui:ebut-modify "Modifies any desired button attributes.")
	  ("Rename" hui:ebut-rename "Relabels an explicit button.")
	  ("Search" hui:ebut-search
	   "Locates and displays personally created buttons in context.")
	  ))
       '(ebut-help .
	 (("Help on>")
	  ("BufferButs"   (hui:hbut-report -1)
	   "Summarizes all explicit buttons in buffer.")
	  ("CurrentBut"   (hui:hbut-report)
	   "Summarizes only current button in buffer.")
	  ("OrderedButs"  (hui:hbut-report 1)
	   "Summarizes explicit buttons in lexicographically order.")
	  ))
       '(gbut .
	 (("GButton>")
	  ("Act"    gbut:act        "Activates global button by name.") 
	  ("Create" hui:gbut-create "Adds a global button to gbut:file.")
	  ("Edit"   hui:gbut-modify "Modifies global button attributes.")
	  ("Help"   gbut:help       "Reports on a global button by name.") 
	  ("Info"   (id-info "(hyperbole.info)Global Buttons")
	   "Displays manual section on global buttons.")
	  ("Modify" hui:gbut-modify "Modifies global button attributes.")
	  ))
       '(ibut .
	 (("IButton>")
	  ("Act"    hui:hbut-act    "Activates implicit button at point.") 
	  ("DeleteIButType"   (hui:htype-delete 'ibtypes)
	   "Deletes specified button type.")
	  ("Help"   hui:hbut-help   "Reports on button's attributes.")
	  ("Info"   (id-info "(hyperbole.info)Implicit Buttons")
	   "Displays manual section on implicit buttons.")
	  ("Types"  (hui:htype-help 'ibtypes 'no-sort)
	   "Displays documentation for one or all implicit button types.")
	  ))
       '(msg .
	 (("Msg>")
	  ("Compose-Hypb-Mail"
	   (hmail:compose "hyperbole-users@gnu.org" '(hact 'hyp-config))
	   "Send a message to the Hyperbole discussion list.")
	  ("Bug-Report"
	   (hmail:compose "bug-hyperbole@gnu.org" '(hact 'hyp-config))
	   "Send a bug report")
	  ("List-Admin" (menu . mailadm)
	   "Administer subscription to the mail lists.")
	  ))
       (if hyperb:kotl-p
	   '(otl
	     . (("Otl>")
		("All"       kotl-mode:show-all "Expand all collapsed cells.") 
		("Blanks"    kvspec:toggle-blank-lines
		 "Toggle blank lines between cells on or off.")
		("Create"    kfile:find   "Create or edit an outline file.")
		("Downto"    kotl-mode:hide-sublevels
		 "Hide all cells in outline deeper than a particular level.")
		("Examp"   (find-file-read-only
			      (expand-file-name
			       "EXAMPLE.kotl" (concat hyperb:dir "kotl/")))
		 "Display a self-descriptive example outline file.")
		("Hide"      (progn (kotl-mode:is-p)
				    (kotl-mode:hide-tree (kcell-view:label)))
		 "Collapse tree rooted at point.")
		("Info"
		 (id-info "(hyperbole.info)Outliner")
		 "Display manual section on Hyperbole outliner.")
		("Kill"      kotl-mode:kill-tree
		 "Kill ARG following trees starting from point.")
		("Link"      klink:create
		 "Create and insert an implicit link at point.")
		("Overvw"  kotl-mode:overview
		 "Show first line of each cell.")
		("Show"      (progn (kotl-mode:is-p)
				    (kotl-mode:show-tree (kcell-view:label)))
		 "Expand tree rooted at point.")
		("Top"       kotl-mode:top-cells
		 "Hide all but top-level cells.") 
		("Vspec"     kvspec:activate
		 "Prompt for and activate a view specifiction.")
		)))
       '(rolo .
	 (("Rolo>")
	  ("Add"              rolo-add	  "Add a new rolo entry.")
	  ("Display"          rolo-display-matches
	   "Display last found rolodex matches again.")
	  ("Edit"             rolo-edit   "Edit an existing rolo entry.")
	  ("Info"             (id-info "(hyperbole.info)Rolodex")
	   "Displays manual section on Hyperbole rolodex.")
	  ("Kill"             rolo-kill   "Kill an existing rolo entry.")
	  ("Mail"             rolo-mail-to "Mail to address following point.")
	  ("Order"            rolo-sort   "Order rolo entries in a file.")
	  ("RegexFind"        rolo-grep   "Find entries containing a regexp.")
	  ("StringFind"       rolo-fgrep  "Find entries containing a string.")
	  ("WordFind"         rolo-word   "Find entries containing words.")
	  ("Yank"             rolo-yank
	   "Find an entry containing a string and insert it at point.")
	  ))
       '(types .
	 (("Types>")
	  ("ActionTypes"      (hui:htype-help   'actypes)
	   "Displays documentation for one or all action types.")
	  ("IButTypes"        (hui:htype-help   'ibtypes 'no-sort)
	   "Displays documentation for one or all implicit button types.")
	  ))
       '(win .
	 (("WinConfig>")
	  ("AddName"        wconfig-add-by-name
	   "Name current window configuration.")
	  ("DeleteName"     wconfig-delete-by-name
	   "Delete named window configuration.")
	  ("RestoreName"    wconfig-restore-by-name
	   "Restore frame to window configuration given by name.")
	  ("PopRing"        (progn (wconfig-delete-pop)
				   (hyperbole 'win))
	   "Restores window configuration from ring and removes it from ring.")
	  ("SaveRing"       (wconfig-ring-save)
	   "Saves current window configuration to ring.")
	  ("YankRing"       (progn (call-interactively 'wconfig-yank-pop)
				   (hyperbole 'win))
	   "Restores next window configuration from ring.")
	  ))
       '(mailadm .
         (("MailAdmin>")
	  ("Hyperbole-Users-Adm"
	   (browse-url "http://lists.gnu.org/mailman/listinfo/hyperbole-users")
	   "Point web browser to administrative interface")
	  ("Bug-Hypberbole-Adm"
	   (browse-url "http://lists.gnu.org/mailman/listinfo/bug-hyperbole")
	   "Point web browser to administrative interface")
          ))
       )))

;;;
;;; Public functions
;;;

;;; Old name
(fset 'hui:menu 'hyperbole)

;;; Used as autoloaded main entry point to Hyperbole (but hsite.el) is the
;;; file that is autoloaded when this is invoked.
;;; It brings up a menu of commands. 
(defun hyperbole (&optional menu menu-list)
  "Invokes default Hyperbole menu user interface when not already active.
Suitable for binding to a key, e.g. {C-h h}.
Non-interactively, returns t if menu is actually invoked by call, else nil.

Two optional arguments may be given to invoke alternative menus.
MENU (a symbol) specifies the menu to invoke from MENU-LIST, (a
Hyperbole menu list structure).  MENU defaults to 'hyperbole and MENU-LIST
to `hui:menus'.  See `hui:menus' definition for the format of the menu list
structure."

  (interactive)
  (if (and hui:menu-p (> (minibuffer-depth) 0))
      (progn (beep) nil)
    (unwind-protect
	(progn
	  (require 'hsite) ;; Since "hui-mini" may be loaded without loading
			   ;; all of Hyperbole.
	  (if hyperbole-on-menubar (hyperb:init-menubar))
	  (setq hui:menu-p t)
	  (hui:menu-act (or menu 'hyperbole) menu-list)
	  t)
      (setq hui:menu-p nil))))

(defun hui:menu-act (menu &optional menu-list)
  "Prompts user with Hyperbole MENU (a symbol) and performs selected item.
Optional second argument MENU-LIST is a Hyperbole menu list structure from
which to extract MENU.  It defaults to `hui:menus'.  See its definition for
the menu list structure." 
  (let ((set-menu '(or (and menu (symbolp menu)
			    (setq menu-alist
				  (cdr (assq menu (or menu-list hui:menus)))))
		       (hypb:error "(menu-act): Invalid menu symbol arg: %s"
			      menu)))
	(show-menu t)
	(rtn)
	menu-alist act-form)
    (while (and show-menu (eval set-menu))
      (cond ((and (consp (setq act-form (hui:menu-select menu-alist)))
		  (cdr act-form)
		  (symbolp (cdr act-form)))
	     ;; Display another menu
	     (setq menu (cdr act-form)))
	    (act-form
	     (let ((prefix-arg current-prefix-arg))
	       (cond ((symbolp act-form)
		      (if (eq act-form t)
			  nil
			(setq show-menu nil
			      rtn (call-interactively act-form))))
		     ((stringp act-form)
		      (hui:menu-help act-form)
		      ;; Loop and show menu again.
		      )
		     (t (setq show-menu nil
			      rtn (eval act-form))))))
	    (t (setq show-menu nil))))
    rtn))

(defun hui:menu-enter (&optional char-str)
  "Uses CHAR-STR or last input character as minibuffer argument."
  (interactive)
  (let ((input (or char-str (aref (recent-keys) (1- (length (recent-keys)))))))
    (cond (hyperb:emacs19-p
	   (and (not (integerp input))
		(eventp input)
		(setq input (event-basic-type input))))
	  (hyperb:xemacs-p
	   (if (eventp input)
	       (setq input (event-to-character input)))))
    (if (or (symbolp input)
	    (and (integerp input)
		 (= input ?\r)))
	(setq input (hargs:at-p)))
    (erase-buffer)
    (or (symbolp input) (insert input)))
  (exit-minibuffer))

(defun hui:menu-help (help-str)
  "Displays HELP-STR in a small window.  HELP-STR must be a string."
  (let* ((window-min-height 2)
	 (owind (selected-window))
	 (buf-name (hypb:help-buf-name "Menu")))
    (unwind-protect
	(progn
	  (save-window-excursion
	    (hkey-help-show buf-name)) ;; Needed to save wconfig.
	  (if (eq (selected-window) (minibuffer-window))
	      (other-window 1))
	  (if (= (length (hypb:window-list 'no-mini)) 1)
	      (split-window-vertically nil))
	  (select-window (hui:bottom-window))
	  (switch-to-buffer (get-buffer-create buf-name))
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (insert "\n" help-str)
	  (set-buffer-modified-p nil)
	  (shrink-window
	   (- (window-height)
	      (+ 3 (length
		    (delq nil
			  (mapcar (function
				   (lambda (chr) (= chr ?\n)))
				  help-str)))))))
      (select-window owind))))

(defun hui:menu-xemacs (&optional menu menu-list)
  "Returns an XEmacs menu built from Hyperbole type menus.
Optional MENU (a symbol) specifies a specific submenu of optional MENU-LIST.
a Hyperbole menu list structure.  Otherwise, all menus are used.
MENU defaults to 'hyperbole and MENU-LIST to `hui:menus'.  See `hui:menus'
definition for the format of the menu list structure."
  (mapcar
   (function 
    (lambda (entry)
      (or (consp entry) 
	  (error "(hui:menu-xemacs): Invalid menu entry: %s" entry))
      (let ((label (car entry))
	    (content (car (cdr entry))))
	(cond ((null content) (hypb:replace-match-string ">$" label "" t))
	      ((and (consp content) (eq (car content) 'menu))
	       (hui:menu-xemacs (cdr content)))
	      (t (vector label content 't))))))
   (cdr (assq (or menu 'hyperbole) (or menu-list hui:menus)))))

(defun hui:menu-select (menu-alist)
  "Prompts user to choose the first character of any item from MENU-ALIST.
Case is not significant.  If chosen by direct selection with the Assist Key,
returns any help string for item, else returns the action form for the item."
  (let* ((menu-line (hui:menu-line menu-alist))
	 (set:equal-op 'eq)
	 (select-char (string-to-char hui:menu-select))
	 (quit-char (string-to-char hui:menu-quit))
	 (abort-char (string-to-char hui:menu-abort))
	 (top-char  (string-to-char hui:menu-top))
	 (item-keys (mapcar (function
			     (lambda (item) (aref item 0)))
			    (mapcar 'car (cdr menu-alist))))
	 (keys (apply 'list select-char quit-char abort-char
		      top-char item-keys))
	 (key 0)
	 (hargs:reading-p 'hmenu)
	 sublist)
    (while (not (memq (setq key (upcase
				 (string-to-char
				  (read-from-minibuffer
				   "" menu-line hui:menu-mode-map))))
		      keys))
      (beep)
      (setq hargs:reading-p 'hmenu)
      (discard-input))
    (cond ((eq key quit-char) nil)
	  ((eq key abort-char) (beep) nil)
	  ((eq key top-char) '(menu . hyperbole))
	  ((and (eq key select-char)
		(save-excursion
		  (if (search-backward " " nil t)
		      (progn (skip-chars-forward " ")
			     (setq key (following-char))
			     nil)  ;; Drop through.
		    t))))
	  (t (if (setq sublist (memq key item-keys))
		 (let* ((label-act-help-list
			 (nth (- (1+ (length item-keys)) (length sublist))
			      menu-alist))
			(act-form (car (cdr label-act-help-list))))
		   (if (eq hargs:reading-p 'hmenu-help)
		       (let ((help-str
			      (or (car (cdr (cdr label-act-help-list)))
				  "No help documentation for this item.")))
			 (concat (car label-act-help-list) "\n  "
				 help-str "\n    Action: "
				 (prin1-to-string act-form)))
		     act-form)))))))

;;;
;;; Private functions
;;;

(if (fboundp 'window-lowest-p)
    (defun hui:bottom-window ()
      "Return a window that is at the bottom of the selected frame."
      (let ((winds (hypb:window-list 'no-mini))
	    (window))
	(while (and (not window) winds)
	  (if (window-lowest-p (car winds))
	      (setq window (car winds))
	    (setq winds (cdr winds))))
	window))
  (defun hui:bottom-window ()
    "Return a window that is at the bottom of the selected frame."
    (let* ((winds (hypb:window-list 'no-mini))
	   (bot-list (mapcar
		      (function
		       (lambda (wind)
			 (nth 3 (window-edges wind))))
		      winds))
	   (bot (apply 'max bot-list)))
      (nth (- (length winds) (length (memq bot bot-list))) winds))))

(defun hui:menu-line (menu-alist)
  "Returns a menu line string built from MENU-ALIST."
  (let ((menu-prompt (concat (car (car menu-alist)) "  "))
	(menu-items (mapconcat 'car (cdr menu-alist) "  "))
	menu-line)
    (setq menu-line (concat menu-prompt menu-items))
    ;; Narrow menu by changing 2 spaces to 1 if too wide for current frame.
    (if (>= (length menu-line) (1- (frame-width)))
	(concat menu-prompt (mapconcat 'car (cdr menu-alist) " "))
      menu-line)))

;;;
;;; Private variables
;;;

;; Hyperbole menu mode is suitable only for specially formatted data.
(put 'hui:menu-mode 'mode-class 'special)

(defvar hui:menu-mode-map nil
  "Keymap containing hui:menu commands.")
(if hui:menu-mode-map
    nil
  (setq hui:menu-mode-map (make-keymap))
  (suppress-keymap hui:menu-mode-map)
  (define-key hui:menu-mode-map hui:menu-quit   'hui:menu-enter)
  (define-key hui:menu-mode-map hui:menu-abort  'hui:menu-enter)
  (define-key hui:menu-mode-map hui:menu-top    'hui:menu-enter)
  (define-key hui:menu-mode-map hui:menu-select 'hui:menu-enter)
  ;;
  ;; This next binding is necessary since the default button1 binding under
  ;; XEmacs, mouse-track, is broken under XEmacs V19.8.
  (and hyperb:xemacs-p window-system
       (define-key hui:menu-mode-map 'button1 'mouse-set-point))
  (let ((i 32))
    (while (<= i 126)
      (define-key hui:menu-mode-map (char-to-string i) 'hui:menu-enter)
      (setq i (1+ i)))))

(provide 'hui-mini)

;;; hui-mini.el ends here
