;;; klink.el --- Implicit reference to a kcell action type, for use in koutlines.

;; Copyright (C) 1993, 1995 Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;;         Kellie Clark
;; Maintainer: Mats Lidell <matsl@contactor.se>
;; Keywords: extensions, hypermedia, outlines, wp

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
;;; link =
;;    < pathname [, cell-ref] [, position] >
;;    < @ cell-ref >  ;; In same buffer
;;    < journal-name, journal-item-number [, cell-ref] [, position] >
;;
;;; pathname =
;;    path   ;; display path in Emacs buffer
;;    !path  ;; execute pathname within a shell
;;    &path  ;; execute path as a windowed program
;;    -path  ;; Load as an Emacs Lisp program
;;
;;; cell-ref =
;;    cell - 1a, 012, 1.2, 1a=012 (both relative and absolute ids separated
;;                                 by an equal sign)
;;    range - 1a-5c, 1a-+3 (include 3 cells past 1a)  (not yet implemented)
;;    tree  - 1a+  (not yet implemented)
;;
;;   optionally followed by a period and 1 or more relative position specs
;;   (not yet implemented):
;;
;;    previous-cell - .b
;;    down-a-level - .d
;;    end-of-branch - .e
;;    follow-next-link - .l
;;    return-to-prev-location - .r
;;    return-to-prev-buffer - .rf
;;    sibling - .s, .2s for 2 siblings forward
;;    tail-of-tree - .t
;;    up-a-level - .u
;;    last char of cell - .f
;;
;;   and then optionally followed by any amount of whitespace, a pipe `|'
;;   character and then one or more view specification characters.  (Augment
;;   viewspec characters may be given instead, preceded by a colon.  They are
;;   ignored for now.)
;;
;;; position (relative to cell start) = (not yet implemented)
;;    char-pos, e.g. 28 or C28
;;    word-num, e.g. W5
;;    line-num, e.g. L2
;;    paragraph-num, e.g. P3
;;    regexp-match, e.g. "regexp"
;;

;;; Code:

;;;
;;; Public functions
;;;

;;;###autoload
(defun klink:create (reference)
  "Insert at point an implicit link to REFERENCE.
REFERENCE should be a cell-ref or a string containing \"filename, cell-ref\".
See documentation for 'kcell:ref-to-id' for valid cell-ref formats."
  (interactive
   ;; Don't change the name or delete default-dir used here.  It is referenced
   ;; in "hargs.el" for argument getting.
   (let ((default-dir default-directory))
     (barf-if-buffer-read-only)
     (hargs:iform-read
      (list 'interactive "*+LInsert link to <[file,] cell-id [|vspecs]>: "))))
  (barf-if-buffer-read-only)
  ;; Reference generally is a string.  It may be a list as a string, e.g.
  ;; "(\"file\" \"cell\")", in which case, we remove the unneeded internal
  ;; double quotes and then parse it with pattern matching.
  (and (stringp reference) (> (length reference) 0)
       (= (aref reference 0) ?\()
       (setq reference (hypb:replace-match-string "\\\"" reference "" t)))
  (let ((default-dir default-directory)
	file-ref cell-ref)
    (setq reference (klink:parse reference)
	  file-ref  (car reference)
	  cell-ref  (car (cdr reference)))
    ;; Don't need filename if link is to a cell in current buffer.
    (if (and file-ref (equal buffer-file-name
			     (expand-file-name file-ref default-directory)))
	(setq file-ref nil))
    (cond (file-ref
	   (setq file-ref (hpath:relative-to file-ref))
		 ;; "./" prefix, if any.
	   (if (string-match "^\\./" file-ref)
	       (setq file-ref (substring file-ref (match-end 0))))
	   (insert "<" file-ref)
	   (if cell-ref (insert ", " cell-ref))
	   (insert ">"))
	  (cell-ref (insert "<@ " cell-ref ">"))
	  (t  (error "(klink:create) Invalid reference, '%s'" reference)))))

(defun klink:at-p ()
  "Return non-nil iff point is within a klink.
See documentation for the `link-to-kotl' function for valid klink formats.
Value returned is a list of: link-label, link-start-position, and
link-end-position, (including delimiters)."
  (let (bol klink referent)
    (if (and
	 ;; If this is an OO-Browser listing buffer, ignore anything that
	 ;; looks like a klink, e.g. a C++ <template> class.
	 (if (fboundp 'br-browser-buffer-p)
	     (not (br-browser-buffer-p))
	   t)
	 ;; Don't match to C/C++ lines like:  #include < path >
	 (save-excursion
	   (beginning-of-line)
	   (setq bol (point))
	   (require 'hmouse-tag)
	   (not (looking-at smart-c-include-regexp)))
	 (save-excursion
	   ;; Don't match Elisp print objects such as #<buffer>
	   (and (search-backward "<" bol t)
		(/= (preceding-char) ?#)
		;; Don't match to \<(explicit)> Hyperbole buttons
		(/= (char-after (1+ (point))) ?\()))
	 (setq klink (hbut:label-p t "<" ">" t))
	 (stringp (setq referent (car klink)))
	 ;; Eliminate matches to e-mail address like, <user@domain>.
	 (not (string-match "[^<> \t\n][!&@]" referent)))
	klink)))

;;;
;;; Hyperbole type definitions
;;;

(defib klink ()
  "Follows a link delimited by <> to a koutline cell.
See documentation for the `link-to-kotl' function for valid klink formats."
  (let* ((link-and-pos (klink:at-p))
	 (link (car link-and-pos))
	 (start-pos (car (cdr link-and-pos))))
    (if link
	(progn (ibut:label-set link-and-pos)
	       (hact 'klink:act link start-pos)))))

(defact link-to-kotl (link)
  "Displays at the top of another window the referent pointed to by LINK.
LINK may be of any of the following forms, with or without delimiters:
  < pathname [, cell-ref] >
  < [-!&] pathname >
  < @ cell-ref >

See documentation for 'kcell:ref-to-id' for valid cell-ref formats."

  (interactive "sKotl link specifier: ")
  (or (stringp link) (error "(link-to-kotl): Non-string link argument, %s"
			    link))
  (cond
   ((or (string-match (format "\\`<?\\s *@\\s *\\(%s\\)\\s *>?\\'"
			      klink:cell-ref-regexp) link)
	(string-match (format "\\`<?\\s *\\([|:]%s\\)\\s *>?\\'"
			      klink:cell-ref-regexp) link))
    ;; < @ cell-ref > or < |viewspec > or < :augment-viewspec >
    (hact 'link-to-kcell
	  nil
	  (kcell:ref-to-id
	   (substring link (match-beginning 1) (match-end 1)))))
   ((string-match
     (format "\\`<?\\s *\\([^ \t\n,<>]+\\)\\s *\\(,\\s *\\(%s\\)\\)?\\s *>?\\'"
	     klink:cell-ref-regexp)
     link)
    ;; < pathname [, cell-ref] >
    (hact 'link-to-kcell
	  (substring link (match-beginning 1) (match-end 1))
	  (if (match-end 3)
	      (kcell:ref-to-id
	       (substring link (match-beginning 3) (match-end 3))))))
   ((string-match
     "\\`<?\\s *\\(\\([-!&]\\)?\\s *[^ \t\n,<>]+\\)\\s *>?\\'" link)
    ;; < [-!&] pathname >
    (hpath:find-other-window
     (substring link (match-beginning 1) (match-end 1))))
   (t (error "(link-to-kotl): Invalid link specifier, %s" link))))

;;;
;;; Private functions
;;;

(defun klink:act (link start-pos)
  (let ((obuf (current-buffer)))
    ;; Perform klink's action which is to jump to link referent.
    (hact 'link-to-kotl link)
    ;; Update klink label if need be, which might be in a different buffer
    ;; than the current one.
    (klink:update-label link start-pos obuf)))

(defun klink:parse (reference)
  "Returns (file-ref cell-ref) list parsed from REFERENCE string.
Either element of the list may be nil if REFERENCE does not contain that
element.  REFERENCE should be one of the following forms (and may include an
optional pair of <> delimiters:
  (pathname, cell-ref)
  pathname, cell-ref
  cell-ref
  |viewspec
  :augment-viewspec (ignored for now)

See documentation for 'kcell:ref-to-id' for valid cell-ref formats."

  (or (stringp reference)
      (error "(klink:parse): Non-string reference argument, %s"
	     reference))
  (cond
   ((string-match
     (format
      "\\`\\s *[<\(]?\\s *\\([^|: \t\n\r,<>][^ \t\n\r,<>]*\\)\\s *,\\s *\\(%s\\)\\s *[\)>]?\\s *\\'"
      klink:cell-ref-regexp)
     reference)
    ;; pathname cell-ref
    (list (substring reference (match-beginning 1) (match-end 1))
	  (substring reference (match-beginning 2) (match-end 2))))
   ((string-match (format "\\`\\s *<?\\s *\\(%s\\)\\s *>?\\s *\\'"
			  klink:cell-ref-regexp)
		  reference)
    ;; cell-ref
    (list nil (substring reference (match-beginning 1) (match-end 1))))
   (t (error "(klink:parse): Invalid reference specifier, %s" reference))))

(defun klink:replace-label (klink link-buf start new-label)
  "Replace out of date relative id in a link reference of the form, relid=idstamp."
  (save-excursion
    (set-buffer link-buf)
    (if buffer-read-only
	(message "Relative label should be `%s' in klink <%s>."
		 new-label klink)
      (goto-char start)
      (cond ((or (looking-at "<\\s *@\\s *")
		 (looking-at "[^,]+,\\s *"))
	     (goto-char (match-end 0))
	     (zap-to-char 1 ?=)
	     (insert new-label ?=))
	    (t nil)))))

(defun klink:update-label (klink start link-buf)
  "Update label of KLINK if its relative cell id has changed.
Assume point is in klink referent buffer, where the klink points."
  (if (and (stringp klink)
	   (string-match
	    "[@,]\\s *\\([*0-9][*.0-9a-zA-Z]*\\)\\s *=\\s *0[0-9]*"
	    klink))
      ;; Then klink has both relative and permanent ids.
      (let* ((label (substring klink (match-beginning 1) (match-end 1)))
	     (new-label (kcell-view:label)))
	  (if (and new-label (not (equal label new-label)))
	      (klink:replace-label klink link-buf start new-label)))))

;;;
;;; Private variables.
;;;

(defvar klink:cell-ref-regexp
  "[|:0-9a-zA-Z][|:.*~=0-9a-zA-Z \t\n\r]*"
  "Regexp matching a cell reference including relative and view specs.
Contains no groupings.")

(provide 'klink)

;;; klink.el ends here
