;;; hui-ep-but.el --- Support for highlighting/flashing buttons under Epoch.

;; Copyright (C) 1990-1995, Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.

;; Author: Bob Weiner, Brown U.
;; Maintainer: Mats Lidell <matsl@contactor.se>
;; Keywords: faces, hypermedia

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
;;   This is truly prototype code.
;;
;;   Can't use epoch read-only buttons here because then outline-mode
;;   becomes unusable; when it tries to do a 'subst-char-in-region'
;;   epoch triggers a read-only error.
;;

;;; Code:

(if (and (boundp 'epoch::version) (stringp epoch::version)
	 (or noninteractive (string-lessp epoch::version "Epoch 4")))
    nil
  (error "(hui-ep-but.el): Load only under Epoch version 3 or lower."))

;;;
;;; Other required Elisp libraries
;;;

(require 'hbut)

;;;
;;; Public variables
;;;

;;;
;;; Public functions
;;;

(fset 'hproperty:but-add 'epoch::add-button)

(defun hproperty:but-color ()
  "Return current color of buffer's buttons."
  (if hproperty:color-ptr
      (car hproperty:color-ptr)
    (epoch::foreground)))

(defun hproperty:but-create-all (&optional start-delim end-delim regexp-match)
  "Mark all hyper-buttons in buffer as Epoch buttons, for later highlighting.
Will use optional strings START-DELIM and END-DELIM instead of default values.
If END-DELIM is a symbol, e.g. t, then START-DELIM is taken as a regular
expression which matches an entire button string.
If REGEXP-MATCH is non-nil, only buttons matching this argument are
highlighted."
  (ebut:map (function (lambda (lbl start end)
			(hproperty:but-add start end hproperty:but)))
	    start-delim end-delim regexp-match 'include-delims))
	       
(defun hproperty:but-delete (&optional pos)
  (epoch::delete-button-at (or pos (point))))

;;;
;;; Private functions
;;;

(defmacro hproperty:list-cycle (list-ptr list)
  "Move LIST-PTR to next element in LIST or when at end to first element."
  (` (or (and (, list-ptr)
	      (setq (, list-ptr) (cdr (, list-ptr))))
	 (setq (, list-ptr) (, list)))))

;;;
;;; Private variables
;;;

(defconst hproperty:color-list '( "red" "blue" "paleturquoise4" "mediumpurple2"
"lightskyblue3" "springgreen2" "salmon" "yellowgreen" "darkorchid2"
"aquamarine4" "slateblue4" "slateblue1" "olivedrab1" "goldenrod4"
"goldenrod3" "cadetblue2" "burlywood1" "slategrey" "mistyrose"
"limegreen" "lightcyan" "goldenrod" "gainsboro" "skyblue1" "honeydew"
"yellow2" "tomato3" "skyblue" "purple4" "orange3" "bisque3" "bisque2"
"grey34" "gray99" "gray63" "gray44" "gray37" "gray33" "gray26" "azure1"
"snow4" "peru" "red" "lightgoldenrod4" "mediumseagreen" "blush"
"mediumorchid2" "lightskyblue1" "darkslateblue" "midnightblue"
"lightsalmon1" "lemonchiffon" "yellow" "lightsalmon" "coral"
"dodgerblue3" "darkorange4" "blue" "royalblue4" "red" "green" "cyan"
"darkviolet" "darksalmon" "darkorange" "blue" "pink" "magenta2"
"sienna4" "khaki2" "grey75" "grey74" "grey73" "grey69" "grey68" "grey35"
"grey13" "gray90" "gray81" "gray55" "gray51" "gray31" "snow2" "pink3"
"grey7" "gray1" "red4" "red3" "tan" "red" "yellow" "mediumvioletred"
"lightslategrey" "lavenderblush4" "turquoise" "darkturquoise"
"darkslategrey" "lightskyblue" "lightsalmon4" "lightsalmon3"
"forestgreen" "dodgerblue4" "orchid" "rosybrown4" "brown" "peachpuff3"
"palegreen3" "orangered2" "rose" "lightcyan4" "indianred4" "indianred3"
"seagreen2" "indianred" "deeppink1" "navyblue" "lavender" "grey"
"deeppink" "salmon4" "salmon3" "oldlace" "grey78" "grey77" "grey54"
"grey45" "grey21" "gray97" "gray96" "gray95" "gray88" "gray87" "gray86"
"gray70" "gray57" "gray38" "gray12" "gray11" "plum3" "linen" "gray9"
"gray8" "blue4" "beige" "turquoise" "blue" "lemonchiffon4"
"darkseagreen1" "antiquewhite3" "mediumorchid" "springgreen"
"turquoise4" "steelblue3" "mistyrose2" "lightcyan2" "red" "firebrick2"
"royalblue" "cadetblue" "skyblue3" "yellow3" "salmon1" "orange4"
"hotpink" "grey90" "gray56" "gray39" "gray18" "gray14" "plum4" "grey6"
"gray6" "gold3" "gold1" "blue2" "tan2" "cyan" "mediumspringgreen"
"darkolivegreen2" "goldenrod" "lightsteelblue" "brown" "whip"
"chartreuse3" "violetred4" "royalblue2" "royalblue1" "papayawhip"
"mistyrose3" "lightcyan1" "aquamarine" "skyblue4" "hotpink4" "hotpink3"
"hotpink2" "dimgray" "tomato" "grey66" "grey65" "grey64" "grey33"
"grey27" "gray76" "gray69" "gray68" "grey0" "azure" "green"
"darkgoldenrod4" "darkgoldenrod3" "darkgoldenrod2" "darkgoldenrod"
"brown" "lightsalmon2" "deepskyblue4" "deepskyblue3" "deepskyblue2"
"deepskyblue" "darkorange1" "violetred3" "violetred2" "violetred1"
"slateblue3" "slateblue2" "drab" "indianred1" "firebrick1" "cadetblue4"
"violetred" "rosybrown" "blue" "firebrick" "grey100" "wheat4" "grey79"
"grey76" "grey61" "gray93" "gray84" "gray65" "gray36" "gray32" "gray13"
"gray10" "azure3" "snow1" "tan1" "gray" "darkolivegreen1" "blue"
"almond" "lavenderblush3" "lavenderblush2" "lavenderblush1"
"darkolivegreen" "lavenderblush" "aquamarine2" "red" "olivedrab2"
"mistyrose4" "mistyrose1" "lightcyan3" "lightcoral" "chartreuse"
"peachpuff" "palegreen" "mintcream" "skyblue2" "moccasin" "tomato1"
"orchid3" "maroon3" "salmon" "grey81" "grey62" "grey39" "grey38"
"grey37" "gray92" "gray83" "gray66" "gray54" "gray50" "gray30" "gray19"
"gray15" "azure4" "grey3" "tan3" "pink" "gray" "blue" "lightsteelblue2"
"lightsteelblue1" "green" "lightslategray" "lemonchiffon2"
"springgreen1" "greenyellow" "chartreuse2" "grey" "royalblue3"
"powderblue" "peachpuff2" "palegreen2" "cream" "slateblue" "seashell2"
"deeppink2" "darkkhaki" "maroon4" "sienna" "grey71" "grey67" "grey18"
"gray59" "gray43" "gray25" "bisque" "red1" "mediumslateblue"
"lightgoldenrod1" "goldenrod" "paleturquoise3" "lightskyblue4" "green"
"yellow" "smoke" "blue" "white" "steelblue4" "rosybrown3" "peachpuff1"
"palegreen1" "blueviolet" "seashell4" "sienna3" "grey40" "gray91"
"gray82" "gray5" "cyan2" "cyan1" "blue1" "snow" "lightgoldenrod2"
"lightslateblue" "mediumorchid3" "darkseagreen4" "springgreen3" "green"
"slategray4" "slategray3" "slategray2" "blue" "peachpuff4" "palegreen4"
"green" "orangered3" "goldenrod1" "ghostwhite" "firebrick4" "firebrick3"
"cadetblue3" "slategray" "seashell3" "honeydew3" "cornsilk4" "cornsilk2"
"purple1" "dimgrey" "khaki1" "ivory3" "grey70" "grey60" "grey32"
"grey22" "grey12" "gray98" "gray89" "gray71" "gray64" "gray60" "gray49"
"azure2" "gray3" "paleturquoise1" "mediumpurple1" "purple"
"lemonchiffon1" "blue" "navajowhite3" "darkorchid1" "orange"
"goldenrod2" "khaki" "chocolate2" "burlywood2" "honeydew1" "darkgreen"
"thistle3" "thistle2" "thistle1" "thistle" "maroon2" "maroon1" "grey53"
"grey44" "grey25" "gray74" "gray45" "gray41" "gray35" "gray27" "gray23"
"gray16" "brown4" "wheat" "coral" "tan4" "lightgoldenrodyellow" "blue"
"green" "gray" "palevioletred3" "mediumpurple4" "mediumpurple3"
"saddlebrown" "blue" "darkorchid4" "darkorchid3" "puff" "olivedrab4"
"lightblue4" "lightpink" "lightgray" "honeydew2" "cornsilk1" "lace"
"sienna1" "bisque4" "orchid" "khaki3" "grey84" "grey83" "grey82"
"grey72" "grey52" "grey43" "grey26" "grey14" "grey10" "gray75" "gray53"
"gray21" "gray20" "brown3" "grey8" "red2" "navy" "grey" "gold"
"mediumaquamarine" "lightgoldenrod" "darkslategray4" "darkseagreen3"
"darkseagreen2" "antiquewhite4" "white" "springgreen4" "lightyellow4"
"white" "aquamarine1" "turquoise3" "steelblue2" "rosybrown2" "pink"
"gray" "indianred2" "dodgerblue" "green" "seagreen1" "deeppink4"
"aliceblue" "magenta1" "pink" "sienna2" "orchid1" "gray100" "grey97"
"grey94" "grey87" "grey86" "grey51" "grey42" "grey19" "gray94" "gray85"
"gray61" "brown2" "khaki" "grey1" "gold4" "blue" "green" "grey"
"turquoise" "paleturquoise" "mediumorchid4" "antiquewhite2"
"lightyellow2" "violet" "salmon" "chartreuse1" "turquoise1" "sandybrown"
"orangered1" "lightpink1" "lightblue2" "lightblue1" "grey" "seagreen4"
"seagreen3" "lightblue" "deeppink3" "burlywood" "seashell" "hotpink1"
"gray" "yellow4" "yellow" "purple" "orange" "ivory4" "grey99" "grey89"
"grey63" "grey58" "grey49" "grey31" "grey24" "grey20" "green4" "green1"
"gray73" "gray67" "coral3" "coral2" "plum2" "pink4" "ivory" "gray4"
"gray2" "gold2" "aquamarine" "grey" "lightgoldenrod3" "darkolivegreen3"
"darkgoldenrod1" "goldenrod" "orchid" "chiffon" "navajowhite4"
"deepskyblue1" "lightyellow" "floralwhite" "blue" "mediumblue"
"chocolate4" "chocolate3" "burlywood4" "turquoise" "steelblue" "green"
"lawngreen" "honeydew4" "seagreen" "orchid4" "wheat1" "violet" "ivory1"
"grey88" "grey85" "grey57" "grey56" "grey55" "grey48" "grey47" "grey46"
"grey30" "grey17" "gray47" "gray29" "pink2" "grey5" "grey4" "green"
"gray0" "brown" "lightsteelblue4" "darkolivegreen4" "palevioletred4"
"blue" "darkslategray3" "darkslategray2" "darkslategray1"
"blanchedalmond" "palegoldenrod" "blue" "lightseagreen" "lemonchiffon3"
"darkslategray" "green" "darkseagreen" "antiquewhite" "darkorange2"
"chartreuse4" "blue" "rosybrown1" "olivedrab3" "lightpink2" "orangered"
"thistle4" "blue" "cornsilk" "salmon2" "orchid2" "ivory2" "grey93"
"grey92" "grey91" "grey36" "grey29" "grey28" "grey16" "gray79" "gray78"
"gray77" "gray48" "gray17" "coral4" "coral1" "plum1" "pink1" "grey9"
"grey2" "gray7" "cyan4" "blue3" "plum" "cornflowerblue" "lightskyblue2"
"antiquewhite1" "navajowhite2" "navajowhite1" "lightyellow3"
"navajowhite" "darkorange3" "whitesmoke" "turquoise2" "steelblue1"
"lightpink4" "lightblue3" "green" "chocolate1" "blue" "olivedrab"
"lightgrey" "chocolate" "magenta4" "magenta3" "yellow1" "purple3"
"purple2" "orange2" "orange1" "magenta" "bisque1" "wheat2" "maroon"
"khaki4" "grey96" "grey95" "grey80" "grey50" "grey41" "grey15" "grey11"
"gray80" "gray58" "gray40" "gray34" "gray22" "brown1" "snow3"
"mediumturquoise" "lightsteelblue3" "palevioletred2" "palevioletred1"
"paleturquoise2" "green" "palevioletred" "mediumorchid1" "white"
"mediumpurple" "lightyellow1" "dodgerblue2" "dodgerblue1" "violet"
"aquamarine3" "slategray1" "gray" "orangered4" "lightpink3" "blue"
"darkorchid" "cadetblue1" "burlywood3" "seashell1" "cornsilk3" "tomato4"
"tomato2" "wheat3" "grey98" "grey59" "grey23" "green3" "green2" "gray72"
"gray62" "gray52" "gray46" "gray42" "gray28" "gray24" "white" "cyan3"
"black" ))

(defvar hproperty:color-ptr nil
  "Pointer to current color name table to use for Hyperbole buttons in Epoch.")

(defconst hproperty:good-colors
  '(
    "medium violet red" "indianred4" "firebrick1" "DarkGoldenrod" "NavyBlue"
    "darkorchid" "tomato3" "mediumseagreen" "deeppink" "forestgreen"
    "mistyrose4" "slategrey" "purple4" "dodgerblue3" "mediumvioletred"
    "lightsalmon3" "orangered2" "turquoise4" "Gray55"
    )
  "Good colors for contrast against wheat background and black foreground.")

(provide 'hui-ep-but)


;;; hui-ep-but.el ends here
