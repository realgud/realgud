;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(load-file "../realgud/common/buffer/source.el")
(load-file "../realgud/common/loc.el")

(declare-function __FILE__                    'load-relative)
(declare-function make-realgud-loc            'realgud-loc)
(declare-function realgud-loc-cmd-marker      'realgud-loc)
(declare-function realgud-loc-line-number     'realgud-loc)
(declare-function realgud-loc-marker          'realgud-loc)
(declare-function realgud-loc-marker=         'realgud-loc)

(test-simple-start)

(save-current-buffer

  ;; Below, we need to make sure current-buffer has an associated
  ;; file with it.
  (find-file (symbol-file 'test-simple))

  (note "location field extraction")
  (let* ((buff (current-buffer))
	 (filename (buffer-file-name buff))
	 (source-marker (point-marker))
	 (cmd-marker (point-marker))
	 (good-loc (make-realgud-loc
		    :filename filename
		    :line-number 5
		    :marker source-marker
		    :cmd-marker cmd-marker
		    ))
	 (good-loc2 (make-realgud-loc
		     :filename filename
		     :line-number 6
		     :marker source-marker
		     :cmd-marker cmd-marker
		     ))
	 ;; (good-loc3 (realgud-loc-current buff cmd-marker))
	 )

    (assert-equal 5 (realgud-loc-line-number good-loc) "line-number extraction")

    (assert-equal source-marker (realgud-loc-marker good-loc)
		  "source code marker extraction")


    (assert-equal cmd-marker (realgud-loc-cmd-marker good-loc)
		  "command process marker extraction")


    (realgud-loc-marker= good-loc2 source-marker)
    (assert-equal source-marker (realgud-loc-marker good-loc2)
		  "marker set")

    ))

(end-tests)

; TODO: add test for debug-loc-goto, e.g.
;(realgud-loc-goto (realgud-loc-new "/tmp/bashdb.diff" 8))
;(realgud-loc-goto (realgud-loc-new "/tmp/bashdb.diff" 8) 'other-window 1)
