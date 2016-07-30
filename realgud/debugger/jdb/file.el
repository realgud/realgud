;; Copyright (C) 2016 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; Association list of fully qualified class names (package + class name)
;; and their source files.
(defvar gud-jdb-class-source-alist nil
  "Association list of fully qualified class names and source files.")

;; This is used to hold a source file during analysis.
(defvar gud-jdb-analysis-buffer nil)

(defvar gud-jdb-classpath-string nil
  "Holds temporary classpath values.")

(defun gud-jdb-build-source-files-list (path extn)
  "Return a list of java source files (absolute paths).
PATH gives the directories in which to search for files with
extension EXTN.  Normally EXTN is given as the regular expression
 \"\\.java$\" ."
  (apply 'nconc (mapcar (lambda (d)
			  (when (file-directory-p d)
			    (directory-files d t extn nil)))
			path)))

;; Move point past whitespace.
(defun gud-jdb-skip-whitespace ()
  (skip-chars-forward " \n\r\t\014"))

;; Move point past a "// <eol>" type of comment.
(defun gud-jdb-skip-single-line-comment ()
  (end-of-line))

;; Move point past a "/* */" or "/** */" type of comment.
(defun gud-jdb-skip-traditional-or-documentation-comment ()
  (forward-char 2)
  (catch 'break
    (while (not (eobp))
      (if (eq (following-char) ?*)
	  (progn
	    (forward-char)
	    (if (not (eobp))
		(if (eq (following-char) ?/)
		    (progn
		      (forward-char)
		      (throw 'break nil)))))
	(forward-char)))))

;; Move point past any number of consecutive whitespace chars and/or comments.
(defun gud-jdb-skip-whitespace-and-comments ()
  (gud-jdb-skip-whitespace)
  (catch 'done
    (while t
      (cond
       ((looking-at "//")
	(gud-jdb-skip-single-line-comment)
	(gud-jdb-skip-whitespace))
       ((looking-at "/\\*")
	(gud-jdb-skip-traditional-or-documentation-comment)
	(gud-jdb-skip-whitespace))
       (t (throw 'done nil))))))

;; Move point past things that are id-like.  The intent is to skip regular
;; id's, such as class or interface names as well as package and interface
;; names.
(defun gud-jdb-skip-id-ish-thing ()
  (skip-chars-forward "^ /\n\r\t\014,;{"))

;; Move point past a string literal.
(defun gud-jdb-skip-string-literal ()
  (forward-char)
  (while (not (cond
	       ((eq (following-char) ?\\)
		(forward-char))
	       ((eq (following-char) ?\042))))
    (forward-char))
  (forward-char))

;; Move point past a character literal.
(defun gud-jdb-skip-character-literal ()
  (forward-char)
  (while
      (progn
	(if (eq (following-char) ?\\)
	    (forward-char 2))
	(not (eq (following-char) ?\')))
    (forward-char))
  (forward-char))

;; Move point past the following block.  There may be (legal) cruft before
;; the block's opening brace.  There must be a block or it's the end of life
;; in petticoat junction.
(defun gud-jdb-skip-block ()

  ;; Find the beginning of the block.
  (while
      (not (eq (following-char) ?{))

    ;; Skip any constructs that can harbor literal block delimiter
    ;; characters and/or the delimiters for the constructs themselves.
    (cond
     ((looking-at "//")
      (gud-jdb-skip-single-line-comment))
     ((looking-at "/\\*")
      (gud-jdb-skip-traditional-or-documentation-comment))
     ((eq (following-char) ?\042)
      (gud-jdb-skip-string-literal))
     ((eq (following-char) ?\')
      (gud-jdb-skip-character-literal))
     (t (forward-char))))

  ;; Now at the beginning of the block.
  (forward-char)

  ;; Skip over the body of the block as well as the final brace.
  (let ((open-level 1))
    (while (not (eq open-level 0))
      (cond
       ((looking-at "//")
	(gud-jdb-skip-single-line-comment))
       ((looking-at "/\\*")
	(gud-jdb-skip-traditional-or-documentation-comment))
       ((eq (following-char) ?\042)
	(gud-jdb-skip-string-literal))
       ((eq (following-char) ?\')
	(gud-jdb-skip-character-literal))
       ((eq (following-char) ?{)
	(setq open-level (+ open-level 1))
	(forward-char))
       ((eq (following-char) ?})
	(setq open-level (- open-level 1))
	(forward-char))
       (t (forward-char))))))

;; Find the package and class definitions in Java source file FILE.  Assumes
;; that FILE contains a legal Java program.  BUF is a scratch buffer used
;; to hold the source during analysis.
(defun gud-jdb-analyze-source (buf file)
  (let ((l nil))
    (set-buffer buf)
    (insert-file-contents file nil nil nil t)
    (goto-char 0)
    (catch 'abort
      (let ((p ""))
	(while (progn
		 (gud-jdb-skip-whitespace)
		 (not (eobp)))
	  (cond

	   ;; Any number of semi's following a block is legal.  Move point
	   ;; past them.  Note that comments and whitespace may be
	   ;; interspersed as well.
	   ((eq (following-char) ?\073)
	    (forward-char))

	   ;; Move point past a single line comment.
	   ((looking-at "//")
	    (gud-jdb-skip-single-line-comment))

	   ;; Move point past a traditional or documentation comment.
	   ((looking-at "/\\*")
	    (gud-jdb-skip-traditional-or-documentation-comment))

	   ;; Move point past a package statement, but save the PackageName.
	   ((looking-at "package")
	    (forward-char 7)
	    (gud-jdb-skip-whitespace-and-comments)
	    (let ((s (point)))
	      (gud-jdb-skip-id-ish-thing)
	      (setq p (concat (buffer-substring s (point)) "."))
	      (gud-jdb-skip-whitespace-and-comments)
	      (if (eq (following-char) ?\073)
		  (forward-char))))

	   ;; Move point past an import statement.
	   ((looking-at "import")
	    (forward-char 6)
	    (gud-jdb-skip-whitespace-and-comments)
	    (gud-jdb-skip-id-ish-thing)
	    (gud-jdb-skip-whitespace-and-comments)
	    (if (eq (following-char) ?\073)
		(forward-char)))

	   ;; Move point past the various kinds of ClassModifiers.
	   ((looking-at "public")
	    (forward-char 6))
	   ((looking-at "abstract")
	    (forward-char 8))
	   ((looking-at "final")
	    (forward-char 5))

	   ;; Move point past a ClassDeclaration, but save the class
	   ;; Identifier.
	   ((looking-at "class")
	    (forward-char 5)
	    (gud-jdb-skip-whitespace-and-comments)
	    (let ((s (point)))
	      (gud-jdb-skip-id-ish-thing)
	      (setq
	       l (nconc l (list (concat p (buffer-substring s (point)))))))
	    (gud-jdb-skip-block))

	   ;; Move point past an interface statement.
	   ((looking-at "interface")
	    (forward-char 9)
	    (gud-jdb-skip-block))

	   ;; Anything else means the input is invalid.
	   (t
	    (message "Error parsing file %s." file)
	    (throw 'abort nil))))))
    l))

(defun gud-jdb-build-class-source-alist-for-file (file)
  (mapcar
   (lambda (c)
     (cons c file))
   (gud-jdb-analyze-source gud-jdb-analysis-buffer file)))

;; Return an alist of fully qualified classes and the source files
;; holding their definitions.  SOURCES holds a list of all the source
;; files to examine.
(defun gud-jdb-build-class-source-alist (sources)
  (setq gud-jdb-analysis-buffer (get-buffer-create " *gud-jdb-scratch*"))
  (prog1
      (apply
       'nconc
       (mapcar
	'gud-jdb-build-class-source-alist-for-file
	sources))
    (kill-buffer gud-jdb-analysis-buffer)
    (setq gud-jdb-analysis-buffer nil)))
