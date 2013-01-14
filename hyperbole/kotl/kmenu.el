;;; kmenu.el --- Pulldown and popup menus for kotl-mode, the Hyperbole Outliner.

;; Copyright (C) 1994, 1995, 2006, 2007 Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;; Maintainer: Mats Lidell <matsl@contactor.se>
;; Keywords: mouse, outlines, wp

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

(defconst kmenu:menu
  '("Koutline"
    ["All-Cells-Attributes" (kotl-modecell-help nil -1)  t]
    ["Help"                describe-mode                  t]
    ["Manual"              (id-info "(hyperbole.info)Outliner") t]
    "----"
    ("Edit"
     ["Set-Cell-Attribute"  kotl-mode:set-cell-attribute   t]
     "----"
     ["Add-Child"           kotl-mode:add-child            t]
     ["Add-Cell"            kotl-mode:add-cell             t]
     ["Add-Parent"          kotl-mode:add-parent           t]
     ["Append-Cell"         kotl-mode:append-cell          t]
     ["Split-Cell"          kotl-mode:split-cell           t]
     "----"
     ["Kill-to-Cell-End"    kotl-mode:kill-contents        t]
     ["Kill-Tree"           kotl-mode:kill-tree            t]
     ["Yank"                kotl-mode:yank                 t]
     "----"
     ["Copy-After-Cell"     kotl-mode:copy-after           t]
     ["Copy-Before-Cell"    kotl-mode:copy-before          t]
     ["Move-After-Cell"     kotl-mode:move-after           t]
     ["Move-Before-Cell"    kotl-mode:move-before          t]
     "----"
     ["Fill"                kotl-mode:fill-cell            t]
     ["Fill-Paragraph"      kotl-mode:fill-paragraph       t]
     ["Set-Fill-Prefix"     kotl-mode:set-fill-prefix      t]
     )
    ("Jump-to"
     ["Cell"                kotl-mode:goto-cell            t]
     "----"
     ["Cell-Beginning"      kotl-mode:beginning-of-cell    t]
     ["Cell-End"            kotl-mode:end-of-cell          t]
     "----"
     ["Child"               kotl-mode:down-level           t]
     ["Parent"              kotl-mode:up-level             t]
     "----"
     ["Next-Cell"           kotl-mode:next-cell            t]
     ["Prev-Cell"           kotl-mode:previous-cell        t]
     "----"
     ["Next-Same-Level"     kotl-mode:forward-cell         t]
     ["Prev-Same-Level"     kotl-mode:backward-cell        t]
     "----"
     ["First-Sibling"       kotl-mode:first-sibling        t]
     ["Last-Sibling"        kotl-mode:last-sibling         t]
     "----"
     ["Beginning-of-Tree"   kotl-mode:beginning-of-tree    t]
     ["End-of-Tree"         kotl-mode:end-of-tree          t]
     "----"
     ["First-Cell"          kotl-mode:beginning-of-buffer  t]
     ["Last-Cell"           kotl-mode:end-of-buffer        t]
     )
    ("Label-Type"
     ["Alphanumeric (Default)"  (kview:set-label-type kview 'alpha)  t]
     ["Legal"                   (kview:set-label-type kview 'legal)  t]
     ["None"                    (kview:set-label-type kview 'no)     t]
     ["Partial-Alpha"           (kview:set-label-type kview 'partial-alpha) t]
     ["Permanent-Idstamp"       (kview:set-label-type kview 'id)     t]
     ["Stars"                   (kview:set-label-type kview 'star) t]
     )
    ("Link"
     ["Add-at-Point"        klink:create                   t]
     )
    ("Tree"
     ["Copy-to-Buffer"      kotl-mode:copy-to-buffer       t]
     ["Demote"              kotl-mode:demote-tree          t]
     ["Kill"                kotl-mode:kill-tree            t]
     ["Mail"                kotl-mode:mail-tree            t]
     ["Promote"             kotl-mode:promote-tree         t]
     ["Show-Attributes"     (kotl-modecell-help nil 2)   t]
     "----"
     ["Copy-After-Cell"     kotl-mode:copy-after           t]
     ["Copy-Before-Cell"    kotl-mode:copy-before          t]
     ["Move-After-Cell"     kotl-mode:move-after           t]
     ["Move-Before-Cell"    kotl-mode:move-before          t]
     )
    ("View"
     ["Set-View-Spec"       kvspec:activate                t]
     ["Toggle-Blank-Lines"  kvspec:toggle-blank-lines      t]
     "----"
     ["Set-Cell-Attribute"   kotl-mode:set-cell-attribute  t]
     ["Show-Cell-Attributes" (kotl-modecell-help)        t]
     ["All-Cells-Attributes" (kotl-modecell-help nil -1) t]
     ["Show-Tree-Attributes" (kotl-modecell-help nil 2)  t]
     "----"
     ["Hide (Collapse)"     kotl-mode:hide-tree            t]
     ["Hide-Levels"         kotl-mode:hide-sublevels       t]
     ["Hide-Subtree"        kotl-mode:hide-subtree         t]
     ["Overview"            kotl-mode:overview             t]
     "----"
     ["Show (Expand)"       kotl-mode:show-tree            t]
     ["Show-All"            kotl-mode:show-all             t]
     ["Show-Subtree"        kotl-mode:show-subtree         t]
     ["Show-Top-Level-Only" kotl-mode:top-cells            t]
     )
    "----"
    ["Find (Open)"         find-file                      t]
    ["Find-Read-Only"      find-file-read-only            t]
    ["Save"                save-buffer                    t]
    ["Toggle-Read-Only"    toggle-read-only               t]
    ["Write (Save as)"     kfile:write                    t]
    "----"
    ["Quit"                (id-tool-quit '(kill-buffer nil))  t]
    ))

;;;
;;; Public functions
;;;

(provide 'kmenu)

;;; kmenu.el ends here
