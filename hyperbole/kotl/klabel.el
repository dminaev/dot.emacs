;;; klabel.el --- Display label handling for koutlines.

;; Copyright (C)  Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;;         Kellie Clark
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

;;; Code:

;;;
;;; Public variables
;;;

(defvar klabel-type:changing-flag nil
  "Non-nil only while the label type in the current view is being changed.")

;;;
;;; Public functions
;;;

;;;
;;; klabel - koutline display labels
;;;

(defun klabel:child (label)
  "Return LABEL's child cell label."
  (funcall (kview:get-attr kview 'label-child) label))

(defun klabel:increment (label)
  "Return LABEL's sibling label."
  (funcall (kview:get-attr kview 'label-increment) label))

(defun klabel:level (label)
  "Return outline level of LABEL using current kview label type."
  (let ((label-type (kview:label-type kview)))
    (cond ((memq label-type '(alpha legal))
	   (funcall (intern-soft (concat "klabel:level-"
					 (symbol-name label-type)))
		    label))
	  ((eq label-type 'no) 1)
	  ((eq label-type 'star) (length label))
	  ((eq label-type 'id)
	   (error
	    "(klabel:level): Can't compute the level of an idstamp label"))
	  ((eq label-type 'partial-alpha)
	   (error
	    "(klabel:level): Can't compute the level of a partial-alpha label"))
	  (t (error "(klabel:level): Invalid label type setting: '%s'"
		    label-type)))))

(defun klabel:parent (label)
  "Return LABEL's parent label."
  (funcall (kview:get-attr kview 'label-parent) label))

(defun klabel-type:child (label-type)
  "Return function which computes child cell label of LABEL-TYPE."
  (cond ((memq label-type '(alpha legal partial-alpha))
	 (intern-soft (concat "klabel:child-"
			      (symbol-name label-type))))
	((eq label-type 'no)
	 (function (lambda (label) "")))
	((eq label-type 'star)
	 (function (lambda (label) (concat label "*"))))
	((eq label-type 'id)
	 (function
	  (lambda (label)
	    (error
	     "(klabel:child-id): Can't compute child of idstamp label"))))
	(t (error
	    "(klabel-type:child): Invalid label type setting: '%s'"
	    label-type))))

(defun klabel-type:increment (label-type)
  "Return function which computes sibling cell label of LABEL-TYPE."
  (cond ((memq label-type '(alpha legal partial-alpha))
	 (intern-soft (concat "klabel:increment-"
			      (symbol-name label-type))))
	((eq label-type 'no)
	 (function
	  (lambda (label)
	    (if (equal label "0")
		(error "(klabel:increment-no): 0 cell cannot have a sibling")
	      ""))))
	((eq label-type 'star)
	 (function
	  (lambda (label)
	    (if (string-equal label "0")
		(error "(klabel:increment-star): 0 cell cannot have a sibling")
	      label))))
	((eq label-type 'id)
	 (function
	  (lambda (label)
	    (if (string-equal label "0")
		(error "(klabel:increment-no): 0 cell cannot have a sibling")
	      (error "(klabel:increment-id): Can't compute sibling of idstamp label")))))
	(t (error
	    "(klabel:increment): Invalid label type setting: '%s'"
	    label-type))))

(defun klabel-type:parent (label-type)
  "Return function which computes parent cell label of LABEL-TYPE."
  (cond ((memq label-type '(alpha legal partial-alpha))
	 (intern-soft (concat "klabel:parent-"
			      (symbol-name label-type))))
	((eq label-type 'no)
	 (function
	  (lambda (label)
	    (if (equal label "0")
		(error "(klabel:parent-no): 0 cell cannot have a parent")
	      ""))))
	((eq label-type 'star)
	 (function
	  (lambda (label)
	    (if (string-equal label "0")
		(error "(klabel:parent-star): 0 cell cannot have a parent")
	      (substring label 0 (1- (length label)))))))
	((eq label-type 'partial-alpha)
	 (function
	  (lambda (label)
	    (error
	     "(klabel:parent-partial-alpha): Can't compute parent of partial alpha label"))))
	((eq label-type 'id)
	 (function
	  (lambda (label)
	    (error
	     "(klabel:parent-id): Can't compute parent of idstamp label"))))
	(t (error
	    "(klabel-type:parent): Invalid label type setting: '%s'"
	    label-type))))

;;;
;;; alpha klabels
;;;

(defun klabel:child-alpha (label)
  "Return label for first child of alpha LABEL."
  (if (or (string-equal label "0")
	  (string-equal label ""))
      "1"
    (concat label (if (< (aref label (1- (length label))) ?a)
		      "a" "1"))))

(defun klabel:increment-alpha (alpha-label)
  "Increment full ALPHA-LABEL by one and return."
  (if (string-equal alpha-label "0")
      (error "(klabel:increment-alpha): 0 cell cannot have a sibling")
    (let ((kotl-label (klabel:to-kotl-label alpha-label)))
      (concat (substring alpha-label 0 (- (length kotl-label)))
	      (kotl-label:increment kotl-label 1)))))

(defun klabel:level-alpha (label)
  "Return outline level as an integer of alpha-style (Augment-style) LABEL.
First visible outline cell is level 1."
  (if (string-equal label "0")
      0
    (let ((i 0)
	  (level 0)
	  (len (length label))
	  (digit-p nil)
	  chr)
      (while (< i len)
	(if (and (>= (setq chr (aref label i)) ?0)
		 (<= chr ?9))
	    (or digit-p (setq level (1+ level)
			      digit-p t))
	  ;; assume chr is alpha
	  (if digit-p (setq level (1+ level)
			    digit-p nil)))
	(setq i (1+ i)))
      level)))

(defun klabel:parent-alpha (label)
  "Return parent label of full alpha LABEL."
  (cond ((or (string-equal label "0")
	     (string-equal label ""))
	 (error "(klabel:parent-alpha): 0 cell cannot have a parent"))
	((kotl-label:integer-p label)  ;; level 1 label
	 "0")
	(t (substring label 0 (- (length (klabel:to-kotl-label label)))))))

;;;
;;; partial-alpha klabels
;;;

(fset 'klabel:child-partial-alpha 'kotl-label:child)

(defun klabel:increment-partial-alpha (label)
  "Increment partial alpha LABEL by one and return."
  (if (string-equal label "0")
      (error "(klabel:increment-partial-alpha): 0 cell cannot have a sibling")
    (kotl-label:increment label 1)))

;;;
;;; legal klabels
;;;

(defun klabel:child-legal (label)
  "Return label for first child of legal LABEL."
  (if (or (string-equal label "0")
	  (string-equal label ""))
      "1"
    (concat label ".1")))

(defun klabel:increment-legal (label)
  "Increment full legal LABEL by one and return."
  (cond ((string-equal label "0")
	 (error "(klabel:increment-legal): 0 cell cannot have a sibling"))
	((string-match "[0-9]+$" label)
	 (concat (substring label 0 (match-beginning 0))
		 (int-to-string
		  (1+ (string-to-int (substring label (match-beginning 0)))))))
	(t (error "(klabel:increment-legal): Invalid label, '%s'" label))))

(defun klabel:level-legal (label)
  "Return outline level as an integer of legal-style LABEL.
First visible outline cell is level 1."
  (if (string-equal label "0")
      0
    (let ((i 0)
	  (level 1)
	  (len (length label)))
      (while (< i len)
	(if (= (aref label i) ?.)
	    (setq level (1+ level)))
	(setq i (1+ i)))
      level)))

(defun klabel:parent-legal (label)
  "Return parent label of full legal LABEL."
  (cond ((or (string-equal label "0")
	     (string-equal label ""))
	 (error "(klabel:parent-legal): 0 cell cannot have a parent"))
	((kotl-label:integer-p label)  ;; level 1 label
	 "0")
	(t (substring label 0 (string-match "\\.[0-9]+$" label)))))

;;;
;;; klabel-type - Sets display label format and converts among formats
;;;
;; Default label-type to use for new views.
;; It must be one of the following symbols:
;;   no              for no labels,
;;   id              for permanent idstamp labels, e.g. 001, 002, etc.
;;   alpha           for '1a2' full alphanumeric labels
;;   legal           for '1.1.2' labels
;;   partial-alpha   for partial alphanumeric labels, e.g. '2' for node '1a2'
;;   star            for multi-star labeling, e.g. '***'.

;;
;; Functions to compute sibling and child labels for particular label types.
;;
(defun klabel-type:function (&optional label-type)
  "Return function which will return display label for current cell.
Label format is optional LABEL-TYPE or the default label type for the current view.

Function signature is: (func prev-label &optional child-p), where prev-label
is the display label of the cell preceding the current one and child-p is
non-nil if cell is to be the child of the preceding cell."
  (or label-type (setq label-type (kview:label-type kview)))
  (cond ((eq label-type 'no)
	 (function (lambda (prev-label &optional child-p)
		     "")))
	((eq label-type 'partial-alpha)
	 (function (lambda (prev-label &optional child-p)
		     (if child-p
			 (if (kotl-label:integer-p prev-label)
			     "a" "1")
		       (kotl-label:increment prev-label 1)))))
	((eq label-type 'id)
	 (function (lambda (prev-label &optional child-p)
		     (format "0%d" (kcell-view:idstamp)))))
	(t (intern-soft (concat "klabel-type:"
				(symbol-name label-type) "-label")))))

(defun klabel-type:alpha-label (prev-label &optional child-p)
  "Return full alphanumeric label, e.g. 1a2, for cell following PREV-LABEL's cell.
With optional CHILD-P, return label for first child cell of PREV-LABEL cell."
  (if child-p
      (klabel:child prev-label)
    (klabel:increment prev-label)))

(defun klabel-type:legal-label (prev-label &optional child-p)
  "Return full legal label, e.g. 1.1.2, for cell following PREV-LABEL's cell.
With optional CHILD-P, return label for first child cell of PREV-LABEL cell."
  (if child-p
      (if (string-equal prev-label "0")
	  "1"
	(concat prev-label ".1"))
    (let* ((last-part (string-match "[0-9]+$" prev-label))
	   (partial-legal (substring prev-label last-part))
	   (next (kotl-label:create (1+ (string-to-int partial-legal)))))
      (if (equal last-part prev-label)
	  next
	(concat (substring prev-label 0 last-part) next)))))

(defun klabel-type:to-label-end (&optional label-type)
  "Return function which will search backward to a the end of a cell's label.
Label format is optional LABEL-TYPE or the default label type for the current view.

Function signature is: ().  It takes no arguments and begins the search from point."
  (or label-type (setq label-type (kview:label-type kview)))
  (or (cdr (assq label-type
		 (list
		  (cons
		   'alpha
		   (function
		    (lambda ()
		      (if (re-search-backward
			   "\\(\\`\\|[\n\r][\n\r]\\)[ \t]*[1-9][0-9a-zA-Z]*"
			   nil t)
			  (goto-char (match-end 0))))))
		  (cons
		   'legal
		   (function
		    (lambda ()
		      (if (re-search-backward
			   "\\(\\`\\|[\n\r][\n\r]\\)[ \t]*[0-9]+\\(\\.[0-9]+\\)*"
			   nil t)
			  (goto-char (match-end 0))))))
		    (cons
		     'star
		     (function
		      (lambda ()
			(if (re-search-backward
			     "\\(\\`\\|[\n\r][\n\r]\\)[ \t]*\\*+" nil t)
			    (goto-char (match-end 0))))))
		    (cons
		     'no
		     (function
		      (lambda ()
			(goto-char
			 (if (and (not hyperb:xemacs-p)
				  (string-lessp emacs-version "19.22"))
			     (kproperty:previous-single-change (point) 'kcell)
			   ;; (GNU Emacs V19.22 / Lucid Emacs V19.9) or greater
			   (- (kproperty:previous-single-change
			       (point) 'kcell) 1))))))
		    (cons
		     'partial-alpha
		     (function
		      (lambda ()
			(if (re-search-backward
			     "\\(\\`\\|[\n\r][\n\r]\\)[ \t]*[0-9]+\\|[a-zA-Z]+"
			     nil t)
			    (goto-char (match-end 0))))))
		    (cons
		     'id
		     (function
		      (lambda ()
			(if (re-search-backward
			     "\\(\\`\\|[\n\r][\n\r]\\)[ \t]*0[0-9]+" nil t)
			    (goto-char (match-end 0)))))))))
	(error "(kview:to-label-end): Invalid label type: '%s'" label-type)))

(defun klabel-type:star-label (prev-label &optional child-p)
  "Return full star label, e.g. ***, for cell following PREV-LABEL's cell.
With optional CHILD-P, return label for first child cell of PREV-LABEL cell."
  (if child-p
      (concat prev-label "*")
    prev-label))

;;
;; Functions to compute labels for cells following point and for all cells in
;; a view.
;;

(defun klabel-type:set-labels (label-type)
  "Replace labels of all cells in current view with those of LABEL-TYPE (a symbol)."
  (let (first-label)
    (save-excursion
      (goto-char (point-min))
      (goto-char (kcell-view:start))
      (setq first-label
	    (cond ((memq label-type '(alpha legal partial-alpha))
		   "1")
		  ((eq label-type 'id) (kcell-view:idstamp))
		  ((eq label-type 'no) "")
		  ((eq label-type 'star) "*")
		  (t (error
		      "(klabel-type:set-labels): Invalid label type: '%s'"
		      label-type))))
      (let ((klabel-type:changing-flag t))
	(klabel-type:update-labels-from-point label-type first-label)))))

(defun klabel-type:set-alpha (current-cell-label label-sep-len current-indent
			      per-level-indent &optional current-tree-only)
  "Set the labels of current cell, its following siblings and their subtrees.
CURRENT-CELL-LABEL is the label to display for the current cell.
LABEL-SEP-LEN is the length of the separation between a cell's label
and the start of its contents." 
  (let (label-prefix label-suffix suffix-val suffix-function opoint)
    (if current-cell-label
	(setq label-suffix (klabel:to-kotl-label current-cell-label)
	      label-prefix (substring current-cell-label
				      0 (- (length label-suffix)))
	      suffix-function (if (kotl-label:integer-p label-suffix)
				  (progn (setq suffix-val
					       (string-to-int label-suffix))
					 'int-to-string)
				(setq suffix-val
				      (kotl-label:alpha-to-int label-suffix))
				'kotl-label:int-to-alpha)))
    (while current-cell-label
      ;; Set current cell's label.
      (klabel:set current-cell-label label-sep-len)
      ;; Process any subtrees of current cell.
      (if (kcell-view:child nil label-sep-len)
	  ;; Recurse over subtree.
	  (klabel-type:set-alpha
	   (klabel:child-alpha current-cell-label)
	   label-sep-len
	   (+ current-indent per-level-indent)
	   per-level-indent))
      ;; Process next sibling of current cell if any.
      (setq opoint (point))
      (if (and (not current-tree-only)
	       (kcell-view:next nil label-sep-len)
	       (= current-indent (kcell-view:indent nil label-sep-len)))
	  (setq suffix-val (1+ suffix-val)
		label-suffix (funcall suffix-function suffix-val)
		current-cell-label (concat label-prefix label-suffix))
	(goto-char opoint)
	(setq current-cell-label nil)))))

(defun klabel-type:set-id (current-cell-label label-sep-len &rest ignore)
  "Set the labels of current cell, its following siblings and their subtrees.
CURRENT-CELL-LABEL is the label to display for the current cell."
  ;; Only need to do this when switching from one label type to another,
  ;; i.e. when every cell label will be updated.  So if not starting with the
  ;; first cell, do nothing.
  (if (kotl-mode:first-cell-p)
      (while (and (klabel:set (kcell-view:idstamp) label-sep-len)
		  (kcell-view:next nil label-sep-len)))))

(defun klabel-type:set-legal (current-cell-label label-sep-len current-indent
			      per-level-indent &optional current-tree-only)
  "Set the labels of current cell, its following siblings and their subtrees.
CURRENT-CELL-LABEL is the label to display for the current cell.
LABEL-SEP-LEN is the length of the separation between a cell's label
and the start of its contents." 
  (let (label-prefix label-suffix suffix-val opoint)
    (if current-cell-label
	(setq label-suffix (klabel:to-kotl-label current-cell-label)
	      label-prefix (substring current-cell-label
				      0 (- (length label-suffix)))
	      suffix-val (string-to-int label-suffix)))
    (while current-cell-label
      ;; Set current cell's label.
      (klabel:set current-cell-label label-sep-len)
      ;; Process any subtrees of current cell.
      (if (kcell-view:child nil label-sep-len)
	  ;; Recurse over subtree.
	  (klabel-type:set-legal
	   (klabel:child-legal current-cell-label)
	   label-sep-len
	   (+ current-indent per-level-indent)
	   per-level-indent))
      ;; Process next sibling of current cell if any.
      (setq opoint (point))
      (if (and (not current-tree-only)
	       (kcell-view:next nil label-sep-len)
	       (= current-indent (kcell-view:indent nil label-sep-len)))
	  (setq suffix-val (1+ suffix-val)
		label-suffix (int-to-string suffix-val)
		current-cell-label (concat label-prefix label-suffix))
	(goto-char opoint)
	(setq current-cell-label nil)))))

(defun klabel-type:set-no (current-cell-label label-sep-len &rest ignore)
  "Set the labels of current cell, its following siblings and their subtrees.
CURRENT-CELL-LABEL is the label to display for the current cell."
  ;; Only need to do this when switching from one label type to another,
  ;; i.e. when every cell label will be updated.  So if not starting with the
  ;; first cell, do nothing.
  (if (kotl-mode:first-cell-p)
      (while (and (klabel:set "" label-sep-len)
		  (kcell-view:next nil label-sep-len)))))

(defun klabel-type:set-partial-alpha (current-cell-label label-sep-len
                                      current-indent per-level-indent
				      &optional current-tree-only)
  "Set the labels of current cell, its following siblings and their subtrees.
CURRENT-CELL-LABEL is the label to display for the current cell.
LABEL-SEP-LEN is the length of the separation between a cell's label
and the start of its contents."
  (let (label-suffix suffix-val suffix-function opoint)
    (if current-cell-label
	(setq label-suffix current-cell-label
	      suffix-function (if (kotl-label:integer-p label-suffix)
				  (progn (setq suffix-val
					       (string-to-int label-suffix))
					 'int-to-string)
				(setq suffix-val
				      (kotl-label:alpha-to-int label-suffix))
				'kotl-label:int-to-alpha)))
    (while current-cell-label
      ;; Set current cell's label.
      (klabel:set current-cell-label label-sep-len)
      ;; Process any subtrees of current cell.
      (if (kcell-view:child nil label-sep-len)
	  ;; Recurse over subtree.
	  (klabel-type:set-partial-alpha
	   (klabel:child-partial-alpha current-cell-label)
	   label-sep-len
	   (+ current-indent per-level-indent)
	   per-level-indent))
      ;; Process next sibling of current cell if any.
      (setq opoint (point))
      (if (and (not current-tree-only)
	       (kcell-view:next nil label-sep-len)
	       (= current-indent (kcell-view:indent nil label-sep-len)))
	  (setq suffix-val (1+ suffix-val)
		label-suffix (funcall suffix-function suffix-val)
		current-cell-label label-suffix)
	(goto-char opoint)
	(setq current-cell-label nil)))))

(defun klabel-type:set-star (current-cell-label label-sep-len &rest ignore)
  "Set the labels of current cell, its following siblings and their subtrees.
CURRENT-CELL-LABEL is the label to display for the current cell.
LABEL-SEP-LEN is the length of the separation between a cell's label
and the start of its contents." 
  ;; Only need to do this when switching from one label type to another,
  ;; i.e. when every cell label will be updated.  So if not starting with the
  ;; first cell, do nothing.
  (if (kotl-mode:first-cell-p)
      (while (and (klabel:set (make-string
			       (kcell-view:level nil label-sep-len) ?*)
			      label-sep-len)
		  (kcell-view:next nil label-sep-len)))))

(defun klabel-type:update-labels (current-cell-label)
  "Update the labels of current cell, its following siblings and their subtrees.
CURRENT-CELL-LABEL is the label to display for the current cell.
If, however, it is \"0\", then all cell labels are updated."
  (let ((label-type (kview:label-type kview)))
    (if (string-equal current-cell-label "0")
	;; Update all cells in view.
	(klabel-type:set-labels label-type)
      ;; Update current tree and its siblings only.
      (klabel-type:update-labels-from-point
       label-type current-cell-label))))

(defun klabel-type:update-tree-labels (current-cell-label)
  "Update the labels of current cell and its subtree.
CURRENT-CELL-LABEL is the label to display for the current cell.
Use '(klabel-type:update-labels "0")' to update all cells in an outline."
  (let ((label-type (kview:label-type kview))
	(label-sep-len (kview:label-separator-length kview)))
    (save-excursion
      (funcall (intern-soft (concat "klabel-type:set-"
				    (symbol-name label-type)))
	       first-label label-sep-len
	       (kcell-view:indent nil label-sep-len)
	       (kview:level-indent kview)
	       ;; Update current tree only.
	       t))))

;;;
;;; kotl-label--the part of a full label which represents a
;;;             kcell's relative position in the koutline hierarchy,
;;;             e.g. the full label "1a2" has kotl-label "2".
;;;
(defun kotl-label:alpha-to-int (alpha-label)
  "Return integer value of ALPHA-LABEL, e.g. `b' returns 2.
Assumes ALPHA-LABEL is alphabetic."
  (let ((power (length alpha-label))
	(digit 0)
	(min (1- ?a)))
    (apply '+ (mapcar
	       (function (lambda (chr)
			   (setq digit (- chr min)
				 power (1- power))
			   (* (apply '* (make-list power 26)) digit)
			   ))
	       alpha-label))))

(defun kotl-label:alpha-p (label)
  "Return LABEL if LABEL is composed of all alphabetic characters, else return nil."
  (if (string-match "\\`[a-zA-Z]+\\'" label) label))

(defun kotl-label:child (label)
  "Return child label of partial alpha LABEL."
  (cond ((or (string-equal label "0")
	     (string-equal label ""))
	 "1")
	((kotl-label:integer-p label) "a")
	(t "1")))

(defun kotl-label:create (int-or-string)
  "Return new kcell label from INT-OR-STRING."
  (cond ((integerp int-or-string) (int-to-string int-or-string))
	((equal int-or-string "") "0")
	(t int-or-string)))

(defun kotl-label:increment (label n)
  "Return LABEL incremented by N.
For example, if N were 1, 2 would become 3, z would become aa, and aa would
become bb.  If N were -2, 4 would become 2, etc.
LABEL must be >= 1 or >= a.  If LABEL is decremented below 1 or a, an error
is signaled."
  (if (not (kotl-label:is-p label))
      (error
       "(kotl-label:increment): First arg, '%s', must be a kotl-label."
       label))
  (let ((int-p) (val 0))
    (if (or (setq int-p (kotl-label:integer-p label))
	    (kotl-label:alpha-p label))
	;; Test if trying to decrement below 1 or a.
	(if int-p
	    (progn (setq int-p (string-to-int label))
		   (if (> (setq val (+ int-p n)) 0)
		       (kotl-label:create val)
		     (error "(kotl-label:increment): Decrement of '%s' by '%d' is less than 1." label n)))
	  ;; alpha-p
	  (if (<= 0 (setq val (+ n (kotl-label:alpha-to-int label))))
	      (kotl-label:create
	       (kotl-label:int-to-alpha val))
	    (error "(kotl-label:increment): Decrement of '%s' by '%d' is illegal." label n)))
      (error "(kotl-label:increment): label, '%s', must be all digits or alpha characters" label))))

(defun kotl-label:increment-alpha (label)
  "Return alphabetic LABEL incremented by 1.
For example, z would become aa, and aa would become bb.  LABEL must be >= a." 
  (kotl-label:int-to-alpha
   (1+ (kotl-label:alpha-to-int label))))

(defun kotl-label:increment-int (int-string)
  "Return INT-STRING label incremented by 1.
For example, \"14\" would become \"15\"."
  (int-to-string (1+ (string-to-int int-string))))

(defun kotl-label:integer-p (label)
  "Return LABEL iff LABEL is composed of all digits, else return nil."
  (if (string-match "\\`[0-9]+\\'" label) label))

;; This handles partial alphabetic labels with a maximum single level
;; sequence of 17575 items, which = (1- (expt 26 3)), after which it gives
;; invalid results.  This should be large enough for any practical cases.

(defun kotl-label:int-to-alpha (n)
  "Return alphabetic representation of N as a string.
N may be an integer or a string containing an integer."
  (if (stringp n) (setq n (string-to-int n)))
  (let ((lbl "") pow26 exp26 quotient remainder)
    (if (= n 0)
	""
      (setq pow26 (floor (kotl-label:log26
			  (if (= (mod (1- n) 26) 0) n (1- n)))))
      (while (>= pow26 0)
	(setq exp26 (expt 26 pow26)
	      quotient (floor (/ n exp26))
	      remainder (mod n exp26))
	(if (= remainder 0)
	    (setq quotient (- quotient (1+ pow26))
		  n 26)
	  (setq n remainder
		quotient (max 0 (1- quotient))))
	(setq lbl (concat lbl (char-to-string (+ quotient ?a)))
	      pow26 (1- pow26)))
      lbl)))

(defun kotl-label:is-p (object)
  "Return non-nil if OBJECT is a KOTL-LABEL."
  (stringp object))



;;;
;;; Private functions
;;;

(defun klabel:set (new-label &optional label-sep-len)
  "Replace label displayed in cell at point with NEW-LABEL, which may be a different label type.
Return NEW-LABEL string."
  (let ((modified (buffer-modified-p))
	(buffer-read-only)
	(thru-label (- (kcell-view:indent nil label-sep-len)
		       (or label-sep-len
			   (kview:label-separator-length kview)))))
    (save-excursion
      (kcell-view:to-label-end)
      ;; delete backwards thru label
      (delete-backward-char thru-label)
      ;; replace with new label, right justified
      (insert (format (format "%%%ds" thru-label) new-label)))
    (set-buffer-modified-p modified)
    new-label))

(defun klabel:to-kotl-label (label)
  "Given full alpha or legal LABEL, return rightmost part, called a kotl-label.
For example, the full label \"1a2\" has kotl-label \"2\", as does \"1.1.2\"."
  (if (string-match "[0-9]+$\\|[a-zA-Z]+$" label)
      (substring label (match-beginning 0))
    (error "(klabel:to-kotl-label): Invalid label, '%s'" label)))

(defun klabel-type:update-labels-from-point (label-type first-label)
  (let ((label-sep-len (kview:label-separator-length kview)))
    (save-excursion
      (funcall (intern-soft (concat "klabel-type:set-"
				    (symbol-name label-type)))
	       first-label label-sep-len
	       (kcell-view:indent nil label-sep-len)
	       (kview:level-indent kview)))))

(defun kotl-label:log26 (n)
  "Return log base 26 of integer N."
  (/ (log10 n)
     ;; Next line = (log10 26.514147167125703)
     1.423477662509912))

(provide 'klabel)

;;; klabel.el ends here
