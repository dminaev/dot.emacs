;;; hyperbole.el --- Sets up Hyperbole for autoloading and use.

;; Copyright (C) 1992-1995, Free Software Foundation, Inc.
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
;;   See the "README" file for installation instructions.
;;
;;   There is no need to manually edit this file unless there are specific
;;   customizations you would like to make, such as whether the Hyperbole
;;   mouse buttons are placed on shifted or unshifted mouse buttons.
;;   (See the call of the function, hmouse-shift-buttons, below.)
;;
;;   Other site-specific customizations belong in "hsite.el" which is created
;;   from "hsite-ex.el" by the person who installs Hyperbole at your site.
;;   

;;; Code:

;;;
;;; Hyperbole directory setting
;;;

;; Defines hyperb:window-system, hyperb:kotl-p and
;; (hyperb:path-being-loaded), which are used below.
;; The Hyperbole distribution directory must either already be in
;; load-path or an explicit load of "hversion" must have been
;; done already or else the following line will fail to load hversion.
;; This is all documented in the Hyperbole installation instructions.
(require 'hversion)

(defgroup hyperbole nil
  "The Everyday Net-centric Information Manager."
  :link '(custom-manual "(hyperbole)Top")
  :group 'hypermedia)

;;;###autoload
(defun hyperb:customize ()
  (interactive)
  (customize-group "hyperbole"))

;; Reinitialize hyperb:dir on reload if initialization failed for any reason.
(and (boundp 'hyperb:dir) (null hyperb:dir) (makunbound 'hyperb:dir))

(defvar hyperb:dir (if (fboundp 'backtrace-frame) (hyperb:path-being-loaded))
  "Directory where the Hyperbole executable code is kept.
It must end with a directory separator character.")
(if (stringp hyperb:dir)
    (setq hyperb:dir (file-name-directory hyperb:dir))
  (error
   "(hyperbole.el): Failed to set hyperb:dir.  Try setting it manually."))

;;;
;;; Other required Elisp libraries
;;;

(require 'set (expand-file-name "set" hyperb:dir))

;; Add hyperb:dir and kotl subdirectory to load-path so other
;; Hyperbole libraries can be found.
(setq load-path (set:add hyperb:dir load-path))
(if hyperb:kotl-p
    (setq load-path (set:add (expand-file-name "kotl/" hyperb:dir) load-path)))

(require 'hvar)  ;; Defines var:append function.

;;;
;;; Public key bindings
;;;

;;; Setup so Hyperbole can be autoloaded from a key.
;;; Choose a key on which to place the Hyperbole menus.
;;; For most people this key binding will work and will be equivalent
;;; to {C-h h}.
;;;
(or (where-is-internal 'hyperbole)
    (where-is-internal 'hui:menu)
    (define-key help-map "h" 'hyperbole))

;;; Provides a site standard way of emulating most Hyperbole mouse drag
;;; commands from the keyboard.  This is most useful for rapidly creating
;;; Hyperbole link buttons from the keyboard without invoking the Hyperbole
;;; menu.  Only works if Emacs is run under a window system.
;;;
(or (not hyperb:window-system)
    (global-key-binding "\M-o")
    (where-is-internal 'hkey-operate)
    (global-set-key "\M-o" 'hkey-operate))

;;; Provides a site standard way of performing explicit button
;;; renames without invoking the Hyperbole menu.
;;;
(or (global-key-binding "\C-c\C-r")
    (where-is-internal 'hui:ebut-rename)
    (global-set-key "\C-c\C-r" 'hui:ebut-rename))

;;; The following operations are now available through the Hyperbole Win/
;;; menu.  In earlier versions of Hyperbole, each of these operations had its
;;; own keybindings.  Uncomment the following code lines if you still want
;;; to use those key bindings.
;;; Key bindings for window configuration save/restore ring, like kill-ring
;;; except holds the configuration of windows within a frame.
;;; {C-x 4 w} to save config; {C-x 4 y} to restore successive
;;; saves; {C-x 4 DEL} to delete successive saves.
;;;
;; (or (global-key-binding "\C-x4w")
;;     (global-set-key "\C-x4w" 'wconfig-ring-save))
;; (or (global-key-binding "\C-x4y")
;;     (global-set-key "\C-x4y" 'wconfig-yank-pop))
;; (or (global-key-binding "\C-x4\177")
;;     (global-set-key "\C-x4\177" 'wconfig-delete-pop))

;;; Provides a site standard way to easily switch between the Hyperbole mouse
;;; bindings and a set of personal mouse bindings.  You may instead show
;;; users how to bind this to a key via 'hyperb:init-hook' (see
;;; Hyperbole Manual).
;;;
(or (global-key-binding "\C-ct")
    (where-is-internal 'hmouse-toggle-bindings)
    (global-set-key "\C-ct" 'hmouse-toggle-bindings))

(defun hkey-either (arg)
  "Executes `action-key' or with non-nil ARG executes `assist-key'."
  (interactive "P")
  (if arg (assist-key) (action-key)))

;;; A value of t for 'hkey-init' below will cause the Hyperbole
;;; context-sensitive keys to be bound to keyboard keys, in addition to any
;;; mouse key bindings.  Comment it out or set it to nil if you don't want
;;; these bindings.  Or change the bindings in the succeeding lines.
;;;
(or (boundp 'hkey-init) (setq hkey-init t))
(and hkey-init
     (not (global-key-binding "\M-\C-m"))
     (global-set-key "\M-\C-m" 'hkey-either))
;;
;; Bind a key, {C-h A}, for Action Key help and {C-u C-h A} for Assist key
;; help.
(and hkey-init
     (not (where-is-internal 'hkey-help))
     (define-key help-map "A" 'hkey-help))

;;;
;;; Hyperbole key bindings for many non-edit modes.
;;; Set both to nil if unwanted.
;;;
(defvar action-key-read-only "\C-m"
  "Local Action Key binding for special read-only modes.")
(defvar assist-key-read-only "\M-\C-m"
  "Local Assist Key binding for special read-only modes.")

;;;
;;; Koutliner mode and file suffix importation settings.
;;;

;;;###autoload
(defvar kimport:mode-alist
  '((t . kimport:text)
    (outline-mode . kimport:star-outline))
  "Alist of (major-mode . importation-function) elements.
This determines the type of importation done on a file when `kimport:file' is
called if the major mode of the import file matches the car of an element in
this list.  If there is no match, then `kimport:suffix-alist' is checked.  If
that yields no match, the element in this list whose car is 't is used.  It
normally does an import of a koutline or text file.

Each importation-function must take two arguments, a buffer/file to import
and a buffer/file into which to insert the imported elements and a third
optional argument, CHILDREN-P, which when non-nil means insert imported cells
as the initial set of children of the current cell, if any.

   outline-mode  - imported as an Emacs outline whose entries begin with
                   asterisks; 
   .kot
   .kotl         - imported as a structured koutline

   all others    - imported as text.")

;;;###autoload
(defvar kimport:suffix-alist
  '(("\\.otl$". kimport:star-outline)
    ("\\.aug$" . kimport:aug-post-outline))
  "Alist of (buffer-name-suffix-regexp . importation-function) elements.
This determines the type of importation done on a file when `kimport:file' is
called.  Each importation-function must take two arguments, a buffer/file to
import and a buffer/file into which to insert the imported elements and a
third optional argument, CHILDREN-P, which when non-nil means insert imported
cells as the initial set of children of the current cell, if any.

   .otl  - imported as an Emacs outline whose entries begin with asterisks;
   .kot
   .kotl - imported as a structured koutline
   .aug  - imported as an Augment post-numbered outline.")

;;;
;;; You shouldn't need to modify anything below here.
;;;

(defun hkey-read-only-bindings ()
  "Binds Action and Assist Keys in many read-only modes.
Uses values of `action-key-read-only' and `assist-key-read-only'.  Does
nothing if either variable is nil."
  (if (not (and action-key-read-only assist-key-read-only))
      nil
    (if (and (boundp 'Buffer-menu-mode-map)
	     (keymapp Buffer-menu-mode-map))
	(progn
	  (define-key Buffer-menu-mode-map action-key-read-only 'action-key)
	  (define-key Buffer-menu-mode-map assist-key-read-only
	    'hkey-either)))
    (if (and (boundp 'calendar-mode-map)
	     (keymapp calendar-mode-map))
	(progn
	  (define-key calendar-mode-map action-key-read-only 'action-key)
	  (define-key calendar-mode-map assist-key-read-only
	    'hkey-either)))
    (if (and (boundp 'dired-mode-map)
	     (keymapp dired-mode-map))
	(progn
	  (define-key dired-mode-map action-key-read-only 'action-key)
	  (define-key dired-mode-map assist-key-read-only
	    'hkey-either)))
    (if (and (boundp 'gnus-group-mode-map)
	     (keymapp gnus-group-mode-map))
	(progn
	  (define-key gnus-group-mode-map action-key-read-only 'action-key)
	  (define-key gnus-group-mode-map assist-key-read-only
	    'hkey-either)))
    (if (and (boundp 'gnus-summary-mode-map)
	     (keymapp gnus-summary-mode-map))
	(progn
	  (define-key gnus-summary-mode-map action-key-read-only 'action-key)
	  (define-key gnus-summary-mode-map assist-key-read-only
	    'hkey-either)))
    (if (and (boundp 'Info-mode-map)
	     (keymapp Info-mode-map))
	(progn
	  (define-key Info-mode-map action-key-read-only 'action-key)
	  (define-key Info-mode-map assist-key-read-only
	    'hkey-either)))
    (if (and (boundp 'oo-browse-mode-map)
	     (keymapp oo-browse-mode-map))
	(progn 
	  (define-key oo-browse-mode-map action-key-read-only 'action-key)
	  (define-key oo-browse-mode-map assist-key-read-only
	    'hkey-either)))
    (if (and (boundp 'rmail-mode-map)
	     (keymapp rmail-mode-map))
	(progn
	  (define-key rmail-mode-map action-key-read-only 'action-key)
	  (define-key rmail-mode-map assist-key-read-only
	    'hkey-either)))
    (if (and (boundp 'rmail-summary-mode-map)
	     (keymapp rmail-summary-mode-map))
	(progn
	  (define-key rmail-summary-mode-map action-key-read-only 'action-key)
	  (define-key rmail-summary-mode-map assist-key-read-only
	    'hkey-either)))
    (if (and (boundp 'unix-apropos-map)
	     (keymapp unix-apropos-map))
	(progn
	  (define-key unix-apropos-map action-key-read-only 'action-key)
	  (define-key unix-apropos-map assist-key-read-only
	    'hkey-either)))
    ))

(hkey-read-only-bindings)

;;;
;;; Setup Hyperbole mouse bindings
;;;

(require 'hmouse-key)
;;; The following function call selects between shifted and unshifted Action
;;; and Assist mouse buttons.  With no argument or an argument of nil,
;;; shifted buttons are used, and under InfoDock, the middle button also acts
;;; as an Action Key.  With a positive number as an argument, use shifted
;;; buttons.  With any other integer, use unshifted buttons.
(hmouse-shift-buttons)

;;; Permits restore of the prior window configuration after any help buffer
;;; is shown by pressing either the Action or Assist Key at the end of the
;;; help buffer.  (Help buffer names end with "Help*".)
;;;
(setq temp-buffer-show-hook 'hkey-help-show
      temp-buffer-show-function temp-buffer-show-hook)

;;;
;;; Autoloads
;;;

;;; Menu items could call this function before Info is loaded.
(autoload 'Info-goto-node   "info"       "Jump to specific Info node."  t)

;;; Hyperbole user interface entry points that trigger loading of the full
;;; Hyperbole system.

;; Action type definitions.
(autoload 'defact           "hsite"
  "Creates an action TYPE (an unquoted symbol) with PARAMS, described by DOC."
  nil 'macro)
;; Implicit button type definitions.
(autoload 'defib            "hsite"
  "Creates implicit button TYPE (unquoted sym) with PARAMS, described by DOC."
  nil 'macro)

(autoload 'ebut:map          "hsite"      "Map over Hyperbole buffer buttons." nil)
(autoload 'hui:ebut-rename   "hsite"      "Rename a Hyperbole button."     t)
(autoload 'hyperbole         "hsite"      "Hyperbole info manager menus."  t)

(autoload 'action-key        "hsite"
  "Context-sensitive Action Key command."                                  t)
(autoload 'hkey-help         "hsite"
  "Display help for the Action Key command in current context.
With optional ASSIST-FLAG non-nil, display help for the Assist Key command.
Returns non-nil iff associated help documentation is found."               t)
(autoload 'assist-key-help   "hsite"
  "Display help for the Assist Key command in current context."            t)
(autoload 'hkey-help-hide    "hsite"
  "Restores frame to configuration prior to help buffer display."        nil)
(autoload 'hkey-help-show    "hsite"
  "Saves prior frame configuration if BUFFER displays help."             nil)
(autoload 'assist-key        "hsite"
  "Context-sensitive Assist Key command."                                  t)
(autoload 'action-mouse-key  "hsite"
  "Context-sensitive Action Mouse Key command."                            t)
(autoload 'assist-mouse-key  "hsite"
  "Context-sensitive Assist Mouse Key command."                            t)
(autoload 'hkey-operate      "hsite"      "Emulate Hyperbole mouse key drags." t)
(autoload 'symset:add        "hsite"      "Adds ELT to SYMBOL's PROP set." nil)
(autoload 'hact              "hsite"      "Performs action formed from rest of ARGS."
  nil)
(autoload 'actypes::exec-window-cmd "hsite"
	  "Executes an external window-based SHELL-CMD string asynchronously." nil)
(autoload 'hpath:absolute-to "hsite"
	  "Make PATH absolute from optional DEFAULT-DIRS." nil)
(autoload 'hpath:find        "hsite"
	  "Edit file FILENAME, possibly using a special command." t)
(autoload 'hpath:find-other-window "hsite"
	  "Edit file FILENAME in other window, possibly using a special command." t)

;;; Hyperbole entry points that trigger loading part of the system.

(autoload 'hypb:functionp    "hypb"       "Return t iff OBJ is a function." nil)

;;; Hyperbole msg reader autoloads.
(autoload 'Rmail-init "hrmail" "Initializes Hyperbole Rmail support." t)
(autoload 'Mh-init    "hmh"    "Initializes Hyperbole Mh support." t)
(autoload 'Vm-init    "hvm"    "Initializes Hyperbole Vm support." t)
(autoload 'Pm-init    "hpm"    "Initializes Hyperbole PIEmail support." t)
(autoload 'Gnus-init  "hgnus"  "Initializes Hyperbole Gnus support." t)

;;; Hyperbole msg composer autoloads.
(autoload 'hmail:compose    "hmail"
  "Compose mail with ADDRESS and evaluation of EXPR." t)
(autoload 'hmail:msg-narrow "hmail"
  "Narrows buffer to displayable part of current message.
Its displayable part begins at optional MSG-START and ends at or before MSG-END.")

;;; Hyperbole outliner main entry points.
(if (not hyperb:kotl-p)
    nil
  (autoload 'kfile:find           "kfile" "Edit an autonumbered outline." t)
  (autoload 'kfile:is-p           "kfile" "Is an unformatted outline?" nil)
  (autoload 'kfile:view           "kfile"
    "View an autonumbered outline in read-only mode." t)
  (autoload 'kotl-mode            "kfile" "Autonumbered outlining mode."  t)
  ;;
  ;; Entry points from Hyperbole Otl/ menu.
  (autoload 'klink:create         "klink" "Insert an implicit link at point." t)
  (autoload 'kotl-mode:is-p       "kfile" "Test if within a Hyperbole outline.")
  (autoload 'kotl-mode:hide-tree  "kfile" "Hide sublevels of current tree." t)
  (autoload 'kotl-mode:overview   "kfile"  "Show first line of each cell." t)
  (autoload 'kotl-mode:show-all   "kfile" "Expand all outline cells." t)
  (autoload 'kotl-mode:show-tree  "kfile" "Expand current tree cells." t)
  (autoload 'kotl-mode:top-cells  "kfile" "Hide all but top-level cells." t)
  ;;
  ;; Functions required from outline.el library.
  (autoload 'show-all             "outline" "Show all of the text in the buffer." t)
  ;;
  (autoload 'kimport:file         "kfile" "Import different file types." t)
  (autoload 'kimport:aug-post-outline "kfile" "Import Augment files." t)
  (autoload 'kimport:star-outline "kfile" "Import * outline files." t)
  (autoload 'kimport:text         "kfile" "Import text or koutline files." t)
  )

;;; Hyperbole rolodex main entry points.
(autoload 'rolo-add              "wrolo"      "Add an entry to rolodex"       t)
(autoload 'rolo-display-matches  "wrolo"      "Redisplay previous rolodex matches" t)
(autoload 'rolo-edit             "wrolo"      "Edit an existing rolodex entry" t)
(autoload 'rolo-fgrep            "wrolo"      "Rolodex string search"         t)
(autoload 'rolo-grep             "wrolo"      "Rolodex regexp search"         t)
(autoload 'rolo-kill             "wrolo"      "Delete an existing rolodex entry" t)
(autoload 'rolo-logic            "wrolo-logic" "Logical rolodex search filters." t)
(autoload 'rolo-sort             "wrolo"      "Sort rolodex entries" t)
(autoload 'rolo-word             "wrolo"      "Rolodex string search for a word" t)
(autoload 'rolo-yank             "wrolo"      "Insert a rolodex entry into current buffer" t)

;;; Hyperbole Key autoloads.
(autoload 'Info-handle-in-note "hmous-info"
          "Follows Info documentation references.")
(autoload 'smart-info "hmous-info" "Follows Info documentation references." t)
(autoload 'smart-info-assist "hmous-info"
          "Follows Info documentation references." t)
(autoload 'smart-asm-at-tag-p "hmouse-tag"
	  "Jumps to assembly identifier definitions.")
(autoload 'smart-c-at-tag-p "hmouse-tag" "Jumps to C identifier definitions.")
(autoload 'smart-lisp-mode-p "hmouse-tag"
	  "Jumps to Lisp identifier definitions.")
(autoload 'smart-c++ "hmouse-tag" "Jumps to C++ identifier definitions.")
;; Does nothing unless OO-Browser C++ support has been loaded.
(autoload 'smart-c++-oobr "hmouse-tag" "Jumps to C++ identifier definitions.")
(autoload 'smart-objc "hmouse-tag" "Jumps to Objective-C identifier definitions.")
;; Does nothing unless OO-Browser Objective-C support has been loaded.
(autoload 'smart-objc-oobr "hmouse-tag" "Jumps to Objective-C identifier definitions.")
(autoload 'smart-tags-file "hmouse-tag" "Determines nearest etags file.")
(autoload 'smart-tags-file-path "hmouse-tag" "Expands a filename from TAGS file.")

;;; Window configuration save and restore autoloads.
(autoload 'wconfig-add-by-name     "wconfig" "Save win config under name."  t)
(autoload 'wconfig-delete-by-name  "wconfig" "Delete win config under name." t)
(autoload 'wconfig-restore-by-name "wconfig" "Restore win config under name." t)
(autoload 'wconfig-ring-save  "wconfig"   "Save window-config to ring."  t)
(autoload 'wconfig-yank-pop   "wconfig"   "Pop window-config from ring." t)
(autoload 'wconfig-delete-pop "wconfig"   "Delete window-config from ring." t)

;;;
;;; Auto mode file suffixes 
;;;

;;; Invoke kotl-mode for files ending in ".kotl".  Also allow ".kot" for DOS
;;; and Windows users.
(if hyperb:kotl-p
    (setq auto-mode-alist (cons '("\\.kotl$\\|\\.kot$" . kotl-mode)
				auto-mode-alist)))

;;;
;;; MESSAGE SYSTEM SUPPORT CONFIGURATION
;;;

;;; Even if you don't need some of the following hook settings, you might
;;; as well leave them in so that if they ever become useful to you, you
;;; need not reconfigure Hyperbole.  These settings do nothing if the
;;; corresponding subsystems are never invoked.
;;;
;;; GNUS USENET news reader/poster support.
;;;
(var:append 'gnus-Startup-hook '(Gnus-init))
;;;
;;; Hyperbole mail reader support configuration.
;;;
;; Rmail
(var:append 'rmail-mode-hook    '(Rmail-init))
;; Mh-e
(var:append 'mh-inc-folder-hook '(Mh-init))
;;
;; VM support is based on V5.72 beta of VM.  If you have a version of VM
;; earlier than 5.70 beta, you should either upgrade or comment out the
;; following line so that Hyperbole support for VM is not enabled.
(var:append 'vm-mode-hook       '(Vm-init))
;;
;; PIEmail
(var:append 'pm-hook            '(Pm-init))
;;;
;;; Hyperbole mail composer support configuration.
;;;
(var:append 'mail-mode-hook      '((lambda () (require 'hsmail))))
(var:append 'mh-letter-mode-hook '((lambda () (require 'hsmail))))
(var:append 'vm-mail-mode-hook   '((lambda () (require 'hsmail))))

;;;
;;; Frame function aliases.
;;;
;; Create all needed 'frame-' aliases for all 'screen-' functions, e.g.
;; screen-width.
(if (fboundp 'selected-frame)
    nil
  (fset 'selected-frame 'selected-screen)
  (mapcar
   (function (lambda (func-name)
	       (or (fboundp (intern-soft (concat "frame" func-name)))
		   (fset (intern (concat "frame" func-name))
			 (intern-soft (concat "screen" func-name))))))
   '("-width" "-height")))

(provide 'hyperbole)

;;; hyperbole.el ends here
