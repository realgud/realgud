;; Copyright (C) 2016 Free Software Foundation, Inc

;; Authors: ClÃ©ment Pit--Claudel, Nick Roberts <nickrob@gnu.org>,
;; Rocky Bernstein

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; Run â€˜cask exec emacs -Q -L . -l etc/screenshot.elâ€™ from the project's root to
;; build a screenshot.

(require 'load-relative)

(defconst realgud-bp-xpm-data
        "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"10 10 2 1\",
\"  c red\",
\"+ c None\",
/* pixels */
\"+++    +++\",
\"++      ++\",
\"+        +\",
\"          \",
\"          \",
\"          \",
\"          \",
\"+        +\",
\"++      ++\",
\"+++    +++\",
};"
  "XPM data used for breakpoint enable and disable icon.")

;; For seeing what above icon looks like:
(if nil
    (progn
      (let ((image
	     (find-image `((:type xpm :data
				  ,realgud-bp-xpm-data
				  :ascent 100 :pointer hand)))))
	(insert-image image))   ;; eval-last-sexp after previous ))
      (let ((image
	     (find-image `((:type xpm :data
				  ,realgud-bp-xpm-data
				  :conversion disabled
				  :ascent 100 :pointer hand)))))
	(insert-image image))   ;; eval-last-sexp after previous ))
      )
  )

(defconst realgud-bp-enabled-pbm-data
  "P1
10 10\",
0 0 0 0 1 1 1 1 0 0 0 0
0 0 0 1 1 1 1 1 1 0 0 0
0 0 1 1 1 1 1 1 1 1 0 0
0 1 1 1 1 1 1 1 1 1 1 0
0 1 1 1 1 1 1 1 1 1 1 0
0 1 1 1 1 1 1 1 1 1 1 0
0 1 1 1 1 1 1 1 1 1 1 0
0 0 1 1 1 1 1 1 1 1 0 0
0 0 0 1 1 1 1 1 1 0 0 0
0 0 0 0 1 1 1 1 0 0 0 0"
  "PBM data used for enabled breakpoint icon.")

;; For seeing what above icon looks like:
(if nil
    (let ((image
	  (find-image `((:type pbm :data
			       ,realgud-bp-enabled-pbm-data
			       :ascent 100 :pointer hand)))))
      (insert-image image))   ;; eval-last-sexp after previous ))
  )

(defconst realgud-bp-disabled-pbm-data
  "P1
10 10\",
1 1 1 1 0 0 0 0 1 1 1 1
1 1 1 0 0 0 0 0 0 1 1 1
1 1 0 0 0 0 0 0 0 0 1 1
1 0 0 0 0 0 0 0 0 0 0 1
1 0 0 0 0 0 0 0 0 0 0 1
1 0 0 0 0 0 0 0 0 0 0 1
1 0 0 0 0 0 0 0 0 0 0 1
1 1 0 0 0 0 0 0 0 0 1 1
1 1 1 0 0 0 0 0 0 1 1 1
1 1 1 1 0 0 0 0 1 1 1 1"
  "PBM data used for disabled breakpoint icon.")

;; For seeing what above icon looks like:
(if nil
    (let ((image
	  (find-image `((:type pbm :data
			       ,realgud-bp-disabled-pbm-data
			       :ascent 100 :pointer hand)))))
      (insert-image image))   ;; eval-last-sexp after previous ))
  )



(defconst realgud-bp-enabled-svg-data
"<?xml version='1.0' standalone='no'?>
<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 20010904//EN'
  'http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd'>
<svg width='10' height='10'>
  <circle cx='0' cy='0' r='1' fill='none'/>
  <circle cx='1' cy='0' r='1' fill='none'/>
  <circle cx='2' cy='0' r='1' fill='none'/>
  <circle cx='3' cy='0' r='1' fill='red'/>
  <circle cx='4' cy='0' r='1' fill='red'/>
  <circle cx='5' cy='0' r='1' fill='red'/>
  <circle cx='6' cy='0' r='1' fill='red'/>
  <circle cx='7' cy='0' r='1' fill='none'/>
  <circle cx='8' cy='0' r='1' fill='none'/>
  <circle cx='9' cy='0' r='1' fill='none'/>
  <circle cx='0' cy='1' r='1' fill='none'/>
  <circle cx='1' cy='1' r='1' fill='none'/>
  <circle cx='2' cy='1' r='1' fill='red'/>
  <circle cx='3' cy='1' r='1' fill='red'/>
  <circle cx='4' cy='1' r='1' fill='red'/>
  <circle cx='5' cy='1' r='1' fill='red'/>
  <circle cx='6' cy='1' r='1' fill='red'/>
  <circle cx='7' cy='1' r='1' fill='red'/>
  <circle cx='8' cy='1' r='1' fill='none'/>
  <circle cx='9' cy='1' r='1' fill='none'/>
  <circle cx='0' cy='2' r='1' fill='none'/>
  <circle cx='1' cy='2' r='1' fill='red'/>
  <circle cx='2' cy='2' r='1' fill='red'/>
  <circle cx='3' cy='2' r='1' fill='red'/>
  <circle cx='4' cy='2' r='1' fill='red'/>
  <circle cx='5' cy='2' r='1' fill='red'/>
  <circle cx='6' cy='2' r='1' fill='red'/>
  <circle cx='7' cy='2' r='1' fill='red'/>
  <circle cx='8' cy='2' r='1' fill='red'/>
  <circle cx='9' cy='2' r='1' fill='none'/>
  <circle cx='0' cy='3' r='1' fill='red'/>
  <circle cx='1' cy='3' r='1' fill='red'/>
  <circle cx='2' cy='3' r='1' fill='red'/>
  <circle cx='3' cy='3' r='1' fill='red'/>
  <circle cx='4' cy='3' r='1' fill='red'/>
  <circle cx='5' cy='3' r='1' fill='red'/>
  <circle cx='6' cy='3' r='1' fill='red'/>
  <circle cx='7' cy='3' r='1' fill='red'/>
  <circle cx='8' cy='3' r='1' fill='red'/>
  <circle cx='9' cy='3' r='1' fill='red'/>
  <circle cx='0' cy='4' r='1' fill='red'/>
  <circle cx='1' cy='4' r='1' fill='red'/>
  <circle cx='2' cy='4' r='1' fill='red'/>
  <circle cx='3' cy='4' r='1' fill='red'/>
  <circle cx='4' cy='4' r='1' fill='red'/>
  <circle cx='5' cy='4' r='1' fill='red'/>
  <circle cx='6' cy='4' r='1' fill='red'/>
  <circle cx='7' cy='4' r='1' fill='red'/>
  <circle cx='8' cy='4' r='1' fill='red'/>
  <circle cx='9' cy='4' r='1' fill='red'/>
  <circle cx='0' cy='5' r='1' fill='red'/>
  <circle cx='1' cy='5' r='1' fill='red'/>
  <circle cx='2' cy='5' r='1' fill='red'/>
  <circle cx='3' cy='5' r='1' fill='red'/>
  <circle cx='4' cy='5' r='1' fill='red'/>
  <circle cx='5' cy='5' r='1' fill='red'/>
  <circle cx='6' cy='5' r='1' fill='red'/>
  <circle cx='7' cy='5' r='1' fill='red'/>
  <circle cx='8' cy='5' r='1' fill='red'/>
  <circle cx='9' cy='5' r='1' fill='red'/>
  <circle cx='0' cy='6' r='1' fill='red'/>
  <circle cx='1' cy='6' r='1' fill='red'/>
  <circle cx='2' cy='6' r='1' fill='red'/>
  <circle cx='3' cy='6' r='1' fill='red'/>
  <circle cx='4' cy='6' r='1' fill='red'/>
  <circle cx='5' cy='6' r='1' fill='red'/>
  <circle cx='6' cy='6' r='1' fill='red'/>
  <circle cx='7' cy='6' r='1' fill='red'/>
  <circle cx='8' cy='6' r='1' fill='red'/>
  <circle cx='9' cy='6' r='1' fill='red'/>
  <circle cx='0' cy='7' r='1' fill='none'/>
  <circle cx='1' cy='7' r='1' fill='red'/>
  <circle cx='2' cy='7' r='1' fill='red'/>
  <circle cx='3' cy='7' r='1' fill='red'/>
  <circle cx='4' cy='7' r='1' fill='red'/>
  <circle cx='5' cy='7' r='1' fill='red'/>
  <circle cx='6' cy='7' r='1' fill='red'/>
  <circle cx='7' cy='7' r='1' fill='red'/>
  <circle cx='8' cy='7' r='1' fill='red'/>
  <circle cx='9' cy='7' r='1' fill='none'/>
  <circle cx='0' cy='8' r='1' fill='none'/>
  <circle cx='1' cy='8' r='1' fill='none'/>
  <circle cx='2' cy='8' r='1' fill='red'/>
  <circle cx='3' cy='8' r='1' fill='red'/>
  <circle cx='4' cy='8' r='1' fill='red'/>
  <circle cx='5' cy='8' r='1' fill='red'/>
  <circle cx='6' cy='8' r='1' fill='red'/>
  <circle cx='7' cy='8' r='1' fill='red'/>
  <circle cx='8' cy='8' r='1' fill='none'/>
  <circle cx='9' cy='8' r='1' fill='none'/>
  <circle cx='0' cy='9' r='1' fill='none'/>
  <circle cx='1' cy='9' r='1' fill='none'/>
  <circle cx='2' cy='9' r='1' fill='none'/>
  <circle cx='3' cy='9' r='1' fill='red'/>
  <circle cx='4' cy='9' r='1' fill='red'/>
  <circle cx='5' cy='9' r='1' fill='red'/>
  <circle cx='6' cy='9' r='1' fill='red'/>
  <circle cx='7' cy='9' r='1' fill='none'/>
  <circle cx='8' cy='9' r='1' fill='none'/>
  <circle cx='9' cy='9' r='1' fill='none'/>
</svg>")

;; For seeing what above icon looks like:
(if nil
    (let ((image
	  (find-image `((:type svg :data
			       ,realgud-bp-enabled-svg-data
			       :ascent 100 :pointer hand)))))
      (insert-image image))   ;; eval-last-sexp after previous ))
  )

(defconst realgud-bp-disabled-svg-data
"<?xml version='1.0' standalone='no'?>
<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 20010904//EN'
  'http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd'>
<svg width='10' height='10'>
  <circle cx='0' cy='0' r='1' fill='none'/>
  <circle cx='1' cy='0' r='1' fill='none'/>
  <circle cx='2' cy='0' r='1' fill='none'/>
  <circle cx='3' cy='0' r='1' fill='gray'/>
  <circle cx='4' cy='0' r='1' fill='gray'/>
  <circle cx='5' cy='0' r='1' fill='gray'/>
  <circle cx='6' cy='0' r='1' fill='gray'/>
  <circle cx='7' cy='0' r='1' fill='none'/>
  <circle cx='8' cy='0' r='1' fill='none'/>
  <circle cx='9' cy='0' r='1' fill='none'/>
  <circle cx='0' cy='1' r='1' fill='none'/>
  <circle cx='1' cy='1' r='1' fill='none'/>
  <circle cx='2' cy='1' r='1' fill='gray'/>
  <circle cx='3' cy='1' r='1' fill='gray'/>
  <circle cx='4' cy='1' r='1' fill='gray'/>
  <circle cx='5' cy='1' r='1' fill='gray'/>
  <circle cx='6' cy='1' r='1' fill='gray'/>
  <circle cx='7' cy='1' r='1' fill='gray'/>
  <circle cx='8' cy='1' r='1' fill='none'/>
  <circle cx='9' cy='1' r='1' fill='none'/>
  <circle cx='0' cy='2' r='1' fill='none'/>
  <circle cx='1' cy='2' r='1' fill='gray'/>
  <circle cx='2' cy='2' r='1' fill='gray'/>
  <circle cx='3' cy='2' r='1' fill='gray'/>
  <circle cx='4' cy='2' r='1' fill='gray'/>
  <circle cx='5' cy='2' r='1' fill='gray'/>
  <circle cx='6' cy='2' r='1' fill='gray'/>
  <circle cx='7' cy='2' r='1' fill='gray'/>
  <circle cx='8' cy='2' r='1' fill='gray'/>
  <circle cx='9' cy='2' r='1' fill='none'/>
  <circle cx='0' cy='3' r='1' fill='gray'/>
  <circle cx='1' cy='3' r='1' fill='gray'/>
  <circle cx='2' cy='3' r='1' fill='gray'/>
  <circle cx='3' cy='3' r='1' fill='gray'/>
  <circle cx='4' cy='3' r='1' fill='gray'/>
  <circle cx='5' cy='3' r='1' fill='gray'/>
  <circle cx='6' cy='3' r='1' fill='gray'/>
  <circle cx='7' cy='3' r='1' fill='gray'/>
  <circle cx='8' cy='3' r='1' fill='gray'/>
  <circle cx='9' cy='3' r='1' fill='gray'/>
  <circle cx='0' cy='4' r='1' fill='gray'/>
  <circle cx='1' cy='4' r='1' fill='gray'/>
  <circle cx='2' cy='4' r='1' fill='gray'/>
  <circle cx='3' cy='4' r='1' fill='gray'/>
  <circle cx='4' cy='4' r='1' fill='gray'/>
  <circle cx='5' cy='4' r='1' fill='gray'/>
  <circle cx='6' cy='4' r='1' fill='gray'/>
  <circle cx='7' cy='4' r='1' fill='gray'/>
  <circle cx='8' cy='4' r='1' fill='gray'/>
  <circle cx='9' cy='4' r='1' fill='gray'/>
  <circle cx='0' cy='5' r='1' fill='gray'/>
  <circle cx='1' cy='5' r='1' fill='gray'/>
  <circle cx='2' cy='5' r='1' fill='gray'/>
  <circle cx='3' cy='5' r='1' fill='gray'/>
  <circle cx='4' cy='5' r='1' fill='gray'/>
  <circle cx='5' cy='5' r='1' fill='gray'/>
  <circle cx='6' cy='5' r='1' fill='gray'/>
  <circle cx='7' cy='5' r='1' fill='gray'/>
  <circle cx='8' cy='5' r='1' fill='gray'/>
  <circle cx='9' cy='5' r='1' fill='gray'/>
  <circle cx='0' cy='6' r='1' fill='gray'/>
  <circle cx='1' cy='6' r='1' fill='gray'/>
  <circle cx='2' cy='6' r='1' fill='gray'/>
  <circle cx='3' cy='6' r='1' fill='gray'/>
  <circle cx='4' cy='6' r='1' fill='gray'/>
  <circle cx='5' cy='6' r='1' fill='gray'/>
  <circle cx='6' cy='6' r='1' fill='gray'/>
  <circle cx='7' cy='6' r='1' fill='gray'/>
  <circle cx='8' cy='6' r='1' fill='gray'/>
  <circle cx='9' cy='6' r='1' fill='gray'/>
  <circle cx='0' cy='7' r='1' fill='none'/>
  <circle cx='1' cy='7' r='1' fill='gray'/>
  <circle cx='2' cy='7' r='1' fill='gray'/>
  <circle cx='3' cy='7' r='1' fill='gray'/>
  <circle cx='4' cy='7' r='1' fill='gray'/>
  <circle cx='5' cy='7' r='1' fill='gray'/>
  <circle cx='6' cy='7' r='1' fill='gray'/>
  <circle cx='7' cy='7' r='1' fill='gray'/>
  <circle cx='8' cy='7' r='1' fill='gray'/>
  <circle cx='9' cy='7' r='1' fill='none'/>
  <circle cx='0' cy='8' r='1' fill='none'/>
  <circle cx='1' cy='8' r='1' fill='none'/>
  <circle cx='2' cy='8' r='1' fill='gray'/>
  <circle cx='3' cy='8' r='1' fill='gray'/>
  <circle cx='4' cy='8' r='1' fill='gray'/>
  <circle cx='5' cy='8' r='1' fill='gray'/>
  <circle cx='6' cy='8' r='1' fill='gray'/>
  <circle cx='7' cy='8' r='1' fill='gray'/>
  <circle cx='8' cy='8' r='1' fill='none'/>
  <circle cx='9' cy='8' r='1' fill='none'/>
  <circle cx='0' cy='9' r='1' fill='none'/>
  <circle cx='1' cy='9' r='1' fill='none'/>
  <circle cx='2' cy='9' r='1' fill='none'/>
  <circle cx='3' cy='9' r='1' fill='gray'/>
  <circle cx='4' cy='9' r='1' fill='gray'/>
  <circle cx='5' cy='9' r='1' fill='gray'/>
  <circle cx='6' cy='9' r='1' fill='gray'/>
  <circle cx='7' cy='9' r='1' fill='none'/>
  <circle cx='8' cy='9' r='1' fill='none'/>
  <circle cx='9' cy='9' r='1' fill='none'/>
</svg>")

;; For seeing what above icon looks like:
(if nil
    (let ((image
	  (find-image `((:type svg :data
			       ,realgud-bp-disabled-svg-data
			       :ascent 100 :pointer hand)))))
      (insert-image image))   ;; eval-last-sexp after previous ))
  )

(defconst realgud-bp-enabled-tiff-data
"II* (  ÿÿÿÿÿÿÿÿþÿ@@@@ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿBBBBÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿþÿ@@@@ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿBBBBÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿBBBBÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿþÿ>>>>ÿÿþÿ<<<<ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿþÿ>>>>ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿþÿ<<<<ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿþÿ>>>>ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿþÿ<<<<ÿÿÿÿÿÿÿÿÿÿÿÿÿÿþÿ>>>>ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ      
       
                       
           &                           f              4      <         (       )       >    D  ?    T  R               /tmp/en.tiff     H      H   àF   @ V   @€Âõ(   @`¸   @@33   @€ff&   @ ™™	   @<
×   @"
  "TIFF data used for breakpoint enabled icon.")

;; For seeing what above icon looks like:
(if nil
    (let ((image
	  (find-image `((:type tiff :data
			       ,realgud-bp-enabled-tiff-data
			       :ascent 100 :pointer hand)))))
      (insert-image image))   ;; eval-last-sexp after previous ))
  )

(defconst realgud-bp-disabled-tiff-data
"II* ˜  ÿÿÿÿ  ÿÿ‚‚ÿÿ€ÿÿ€ÿÿ€ÿÿ‚‚ÿÿ ¡ÿÿÿÿÿÿÿÿÿÿ  ÿÿ‚‚ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ‚‚ÿÿ ¡ÿÿÿÿÿÿ‚‚ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ‚‚ÿÿ ¡ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ‡ˆÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ‡ˆÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ‡ˆÿÿ‚‚ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ‚‚ÿÿŸŸÿÿžžÿÿ‚‚ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ€ÿÿ‚‚ÿÿŸŸÿÿÿÿÿÿÿÿÿÿžžÿÿ‚‚ÿÿ€ÿÿ€ÿÿ€ÿÿ‚‚ÿÿŸŸÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿžžÿÿ‡ˆÿÿ‡ˆÿÿ‡ˆÿÿŸŸÿÿÿÿÿÿÿÿÿÿÿÿÿÿ      
       
                       
           Ž                           Ì             œ      ¤         (       )       >    ¬  ?    ¼  R           /tmp/dis.tiff    H      H   àF   @ V   @€Âõ(   @`¸   @@33   @€ff&   @ ™™	   @<
×   @"
  "TIFF data used for breakpoint disabled icon.")

;; For seeing what above icon looks like:
(if nil
    (let ((image
	  (find-image `((:type tiff :data
			       ,realgud-bp-disabled-tiff-data
			       :ascent 100 :pointer hand)))))
      (insert-image image))   ;; eval-last-sexp after previous ))
  )


(provide-me "realgud-")
