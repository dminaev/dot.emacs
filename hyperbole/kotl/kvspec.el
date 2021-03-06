;;; kvspec.el --- Koutline view specification.

;; Copyright (C) 1995, 2006 Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;; Maintainer: Mats Lidell <matsl@contactor.se>
;; Keywords: outlines, wp

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
;;; Koutliner view specs
;; + means support code has been written already.
;;
;; +      all:     Show all lines of cells and all cells in the outline.
;; +      blank:   Blank lines are on.
;;          b - on
;; +      cutoff:  Show only NUM lines per cell, 0 = all
;;          c - set default cutoff lines
;;          cNUM - set cutoff lines to NUM
;;        descend: Only entries below this entry
;; +      elide:   Ellipses are on.
;;          e - ellipses on 
;;        filter:  Regexp or filter program to select entries for view,
;;                 off=select non-matching entries
;;        glue:    Freeze any group of entries selected to stay at top of
;;                 window, off=freeze those not-in-group.
;;        include: Include an entry referenced by a link.
;; +      level:   Some levels are hidden.
;;          l - set default level clipping
;;          lNUM - set level clipping to NUM
;;        name:    Display leading names within cells.
;;          m  -  show names
;; +      number:  Cell numbers are on
;;          n  - set default labels
;;          n0 - display idstamp labels
;;          n1 - display alpha labels
;;          n2 - display partial alpha labels
;;          n. - display legal labels
;;          n* - display star labels
;;          n~ - turn off labels
;;        rest:    Only following cells.
;;        synthesize: Use a named generator function to generate entries for
;;                    view. 
;;        view:    Turn koutliner view mode on.  Standard insertion keys then
;;                 can be used for browsing and view setting.
;;

;;; Code:

;;;
;;; Other required Elisp libraries
;;;

(require 'kview)

;;;
;;; Public variables
;;;

(defvar kvspec:current nil
  "String that represents the current view spec.
It is local to each koutline.  Nil value means it has not been set yet.")

;;;
;;; Public functions
;;;

(defun kvspec:activate (&optional view-spec)
  "Activate optional VIEW-SPEC or existing view spec in the current koutline.
VIEW-SPEC is a string or t, which means recompute the current view spec.  See
<${hyperb:dir}/kotl/EXAMPLE.kotl, 2b17=048> for details on valid view specs."
  (interactive (list (read-string "Set view spec: " kvspec:current)))
  (kotl-mode:is-p)
  (if (or (equal view-spec "") (equal view-spec kvspec:current))
      (setq view-spec nil))
  (kvspec:initialize)
  (kvspec:update view-spec)
  (kvspec:update-view))

(defun kvspec:initialize ()
  "Ensure that view spec settings will be local to the current buffer."
  (if (and (fboundp 'local-variable-p)
	   (local-variable-p 'kvspec:current (current-buffer)))
      nil
    (make-local-variable 'kvspec:current)
    (make-local-variable 'kvspec:string)))

(defun kvspec:levels-to-show (levels-to-keep)
  "Hide all cells in outline at levels deeper than LEVELS-TO-KEEP (a number).
Shows any hidden cells within LEVELS-TO-KEEP.  1 is the first level.  0 means
display all levels of cells."
  (if (null levels-to-keep)
      (setq levels-to-keep
	    (read-from-minibuffer "Show cells down to level (0 = show all levels): "
				  nil nil t)))
  (setq levels-to-keep (prefix-numeric-value levels-to-keep))
  (if (< levels-to-keep 0)
      (error "(kvspec:levels-to-show): Must display at least one level."))
  (kview:map-tree
   (function (lambda (kview) 
	       (if (/= (kcell-view:level) levels-to-keep)
		   (kotl-mode:show-tree)
		 (kotl-mode:hide-subtree)
		 ;; Move to last cell in hidden subtree, to skip further
		 ;; processing of these cells.
		 (if (kcell-view:next t)
		     (kcell-view:previous)
		   (goto-char (point-max))))))
   kview t)
  (kview:set-attr kview 'levels-to-show levels-to-keep))

(defun kvspec:show-lines-per-cell (num)
  "Show NUM lines per cell."
  (if (and (integerp num) (>= num 0))
      nil
    (error "(kvspec:show-lines-per-cell): Invalid lines per cell, '%d'" num))
  (kview:set-attr kview 'lines-to-show num)
  (let (start end count)
    (if (zerop num)
	;; Show all lines in cells.
	(kview:map-tree
	 (function
	  (lambda (kview)
	    ;; Use free variable label-sep-len bound in kview:map-tree for
	    ;; speed.
	    (setq start (goto-char (kcell-view:start nil label-sep-len))
		  end (kcell-view:end-contents))
	    ;; Show all lines in cell.
	    (subst-char-in-region start end ?\r ?\n t)))
	 kview t t)
      ;; Show NUM lines in cells.
      (kview:map-tree
       (function
	(lambda (kview)
	  ;; Use free variable label-sep-len bound in kview:map-tree for speed.
	  (setq start (goto-char (kcell-view:start nil label-sep-len))
		end (kcell-view:end-contents)
		count (1- num))
	  ;; Hide all lines in cell.
	  (subst-char-in-region start end ?\n ?\r t)
	  ;; Expand num - 1 newlines to show num lines.
	  (while (and (> count 0) (search-forward "\r" end t))
	    (replace-match "\n") (setq count (1- count)))))
       kview t t))))

(defun kvspec:toggle-blank-lines ()
  "Toggle blank lines between cells on or off."
  (interactive)
  (setq kvspec:current
	(if (string-match "b" kvspec:current)
	    (hypb:replace-match-string "b" kvspec:current "" t)
	  (concat "b" kvspec:current)))
  (kvspec:blank-lines)
  (kvspec:update-modeline))

(defun kvspec:update (view-spec)
  "Update current view spec according to VIEW-SPEC but don't change the view.
VIEW-SPEC is a string or t, which means recompute the current view spec.  See
<${hyperb:dir}/kotl/EXAMPLE.kotl, 2b17=048> for details on valid view specs."
  (cond ((stringp view-spec)
	 ;; Use given view-spec after removing extraneous characters.
	 (setq kvspec:current
	       (hypb:replace-match-string
		"[^.*~0-9abcdefgilnrsv]+" view-spec "" t)))
	((or (eq view-spec t) (null kvspec:current))
	 (setq kvspec:current (kvspec:compute))))
  ;; Update display using current specs.
  (kvspec:update-modeline))

;;;
;;; Private functions
;;;

(defun kvspec:blank-lines ()
  "Turn blank lines on or off according to 'kvspec:current'."
  (let ((modified-p (buffer-modified-p))
	(buffer-read-only))
      (if (string-match "b" kvspec:current)
	  ;; On
	  (progn (kview:set-attr kview 'blank-lines t)
		 (kproperty:remove (point-min) (point-max) '(invisible t)))
	;; Off
	(kview:set-attr kview 'blank-lines nil)
	(save-excursion
	  (goto-char (point-max))
	  (while (re-search-backward "[\n\r][\n\r]" nil t)
	    ;; Make blank lines invisible.
	    (kproperty:put (1+ (point)) (min (+ (point) 2) (point-max))
			   '(invisible t)))))
    (set-buffer-modified-p modified-p)))

(defun kvspec:compute ()
  "Compute and return current view spec string."
  (concat

   ;; a - Show all cells and cell lines.
   ;; Never compute this setting (use it only within links) since it will
   ;; expose all carefully hidden outline items if the user forgets to turn
   ;; it off when he resets the view specs.

   ;; b - blank separator lines
   (if (kview:get-attr kview 'blank-lines) "b")

   ;; c - cutoff lines per cell
   (let ((lines (kview:get-attr kview 'lines-to-show)))
     (if (zerop lines)
	 nil
       (concat "c" (int-to-string lines))))

   ;; e - ellipses on
   (if selective-display-ellipses "e")

   ;; l - hide some levels
   (let ((levels (kview:get-attr kview 'levels-to-show)))
     (if (zerop levels)
	 nil
       (concat "l" (int-to-string levels))))

   ;; n - numbering type
   (let ((type (kview:label-type kview)))
     (cond ((eq type 'no) nil)
	   ((eq type kview:default-label-type) "n")
	   (t (concat "n" (char-to-string
			   (car (rassq (kview:label-type kview)
				       kvspec:label-type-alist)))))))))

(defun kvspec:elide ()
  "Turn ellipses display following clipped cells on or off according to 'kvspec:current'."
  (setq selective-display-ellipses
	(if (string-match "e" kvspec:current) t)))

(defun kvspec:hide-levels ()
  "Show a set number of cell levels according to 'kvspec:current'."
  ;; "l" means use value of kview:default-levels-to-show.
  ;; "l0" means show all levels.
  (let (levels)
    (if (not (string-match "l\\([0-9]+\\)?" kvspec:current))
	;; Don't change the view if no view spec is given but note that
	;; all levels should be shown in the future.
	(kview:set-attr kview 'levels-to-show 0)
      (if (match-beginning 1)
	  (setq levels (string-to-int
			(substring kvspec:current (match-beginning 1)
				   (match-end 1))))
	(setq levels kview:default-levels-to-show))
      (kview:set-attr kview 'levels-to-show levels)
      (kvspec:levels-to-show levels))))

(defun kvspec:lines-to-show ()
  "Show a set number of lines per cell according to 'kvspec:current'."
  ;; "c" means use value of kview:default-lines-to-show.
  ;; "c0" means show all lines.
  (cond ((not (string-match "c\\([0-9]+\\)?" kvspec:current))
	 ;; Don't change the view if no view spec is given but note that all
	 ;; lines should be shown in the future.
	 (kview:set-attr kview 'lines-to-show 0))
	((match-beginning 1)
	 (kvspec:show-lines-per-cell
	  (string-to-int (substring kvspec:current (match-beginning 1)
				    (match-end 1)))))
	(t (kvspec:show-lines-per-cell kview:default-lines-to-show))))

(defun kvspec:numbering ()
  "Set the type of numbering (label) display according to 'kvspec:current'."
  (if (not (string-match "n\\([.*~0-2]\\)?" kvspec:current))
      nil
    ;; "n"  means use value of kview:default-label-type.
    ;; "n0" means display idstamps.
    ;; "n1" means display alpha labels.
    ;; "n2" means display partial alpha labels.
    ;; "n." means display legal labels.
    ;; "n*" means star labels.
    ;; "n~" means no labels.
    (let (spec type)
      (if (match-beginning 1)
	  (setq spec (string-to-char
		      (substring kvspec:current
				 (match-beginning 1) (match-end 1)))
		type (cdr (assq spec kvspec:label-type-alist)))
	(setq type kview:default-label-type))
      (kview:set-label-type kview type))))

(defun kvspec:update-modeline ()
  "Setup or update display of the current kview spec in the modeline."
  (if (stringp kvspec:current)
      (setq kvspec:string (format kvspec:string-format kvspec:current)))
  (if (memq 'kvspec:string mode-line-format)
      nil
    (setq mode-line-format (copy-sequence mode-line-format))
    (let ((elt (or (memq 'mode-line-buffer-identification mode-line-format)
		   (memq 'modeline-buffer-identification mode-line-format))))
      (if elt
	  (setcdr elt (cons 'kvspec:string (cdr elt)))
	(if hyperb:xemacs-p
	    (let ((mf modeline-format) 
		  elt)
	      (while mf
		(setq elt (car mf))
		(if (and (consp elt) (eq (cdr elt) 'modeline-buffer-identification))
		    (progn (setcdr mf (cons 'kvspec:string (cdr mf)))
			   (setq mf nil)))
		(setq mf (cdr mf)))))))))

(defun kvspec:update-view ()
  "Update view according to current setting of local 'kvspec:current' variable."
  (let ((modified-p (buffer-modified-p))
	(buffer-read-only))
    (save-excursion

      (if (string-match "a" kvspec:current)
	  (kotl-mode:show-all))

      (kvspec:blank-lines) ;; b

      ;; This must come before kvspec:lines-to-show or else it could show
      ;; lines that should be hidden.
      (kvspec:hide-levels) ;; l

      (kvspec:lines-to-show) ;; c

      (if (string-match "d" kvspec:current)
	  nil)

      (kvspec:elide) ;; e

      (if (string-match "f" kvspec:current)
	  nil)

      (if (string-match "g" kvspec:current)
	  nil)

      (if (string-match "i" kvspec:current)
	  nil)

      (if (string-match "r" kvspec:current)
	  nil)

      (if (string-match "s" kvspec:current)
	  nil)

      ;; Do this last since it can trigger an error if partial alpha is
      ;; selected.
      (kvspec:numbering) ;; n

      )
    (set-buffer-modified-p modified-p)))

;;;
;;; Private variables
;;;

(defvar kvspec:label-type-alist
  '((?0 . idstamp) (?1 . alpha) (?2 . partial-alpha)
    (?. . legal) (?* . star) (?~ . no))
  "Alist of (view-spec-character . label-type) pairs.")

(defvar kvspec:string ""
  "String displayed in koutline modelines to reflect the current view spec.
It is local to each koutline.  Set this to nil to disable modeline display of
the view spec settings.")

(defvar kvspec:string-format " <|%s>"
  "Format of the kview spec modeline display.
It must contain a '%s' which is replaced with the current set of view spec
characters at run-time.")

(provide 'kvspec)

;;; kvspec.el ends here
