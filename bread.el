;;; bread.el --- book reader for Emacs

(defconst bread-version "0.1" "bread version.")

;; Author: Dmitri Minaev <minaev@gmail.com>
;; Version: 0.01
;; Keywords: fictionbook2 ebooks reading library
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; bread   includes   two   parts:   ebooks   cataloging   application
;; (bread-library) and a reader for FictionBook2 files.

(defvar bread-version "0.2"
  "Bread version string.")


(provide 'bread)
(require 'org)
(require 'bread-library)
(require 'xml)
(defun bread-mode()
  (interactive)
  (sgml-mode)
  (sgml-tags-invisible 0)
  (longlines-mode)
  (view-mode))

