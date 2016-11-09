;;;; -*- mode: lisp -*-
;;;;
;;;; NOT FOR DISTRIBUTION OUTSIDE THE SCONE GROUP.
;;;;
;;;; This is a package designed to read a lisp file and to
;;;; automatically produce various kinds of useful documentation in
;;;; the form of HTML files. This is not meant to be a general
;;;; facility.  It has been specialized for the needs of the Scone
;;;; code files and documents.
;;;;
;;;; Inspired to some degree by Ben Lambert's adaptation of a more
;;;; general package by Sven Van Caekenberghe.
;;;;
;;;; Modifed by Ben Lambert, 9-6-07
;;;;

(defun section (&rest rest) (declare (ignore rest))) 
(defun subsection (&rest rest) (declare (ignore rest)))
(defun subsubsection (&rest rest) (declare (ignore rest)))
(defun to-do (&rest rest) (declare (ignore rest)))
(defun todo (&rest rest) (declare (ignore rest)))



(declaim (optimize (speed 1) (debug 3) (space 1) (safety 1)))

(defvar *default-doc-string* "<font color='red'>NO DOCUMENTATION STRING</font>")

(defstruct (item)
  "One of these is created for each function or variable we find."
  type
  name
  arglist
  (docstring *default-doc-string*)
  ;; ITEM-TAG is a label for links to this entry.
  tag
  ;; ITEM-SECTION is a generated section/subsection number.
  section
  (class-members nil)
  default-value
)

(defvar *tag-counter* 0)
(defvar *section* 0)
(defvar *subsection* 0)
(defvar *subsubsection* 0)

(defmacro gen-tag ()
  '(progn
    (incf *tag-counter*)
    (format nil "TAG-~D" *tag-counter*)))    

(defun extract-items  (pathname)
  "Read through the file indicated by PATHNAME.  For each def-form,
   create a item structure and push it onto ITEM-LIST.  For
   certain comment-forms, also create an item.  Return the list."
  ;; Open the input file.
  (with-open-file (s pathname)
    (let ((item-list nil)
	  (item nil))
      ;; Initialize counters.
      (setq *tag-counter* 0)
      (setq *section* 0)
      (setq *subsection* 0)
      (setq *subsubsection* 0)
      ;; Read each top-level form in the file, skipping comments.
      (do ((form (read s nil 'eof) (read s nil 'eof)))
	  ((eq form 'eof) (nreverse item-list))
	;; If the top-level form is an EVAL-WHEN, check the subforms.
	(if (eq (car form) 'eval-when)
	    (dolist (subform (cddr form))
	      (and (setq item (note-item subform))
		   (push item  item-list)))
	    ;; Otherwise, just see if the form itself produces an
	    ;; item.
	    (and (setq item (note-item form))
		 (push item  item-list)))))))

(defun note-item (form)
  "Given one form that may be a definition of some kind, maybe create
   and return an item for it."
  (when (listp form)
    (case (car form)
      (defun
	  (make-item
	   :type :function
	   :name (second form)
	   :arglist (third form)
	   :docstring (if (stringp (fourth form))
			  (fourth form)
			  *default-doc-string*)
	   :tag (gen-tag)))
      (defmacro
	  (make-item
	   :type :macro
	   :name (second form)
	   :arglist (third form)
	   :docstring (if (stringp (fourth form))
			  (fourth form)
			  *default-doc-string*)
	   :tag (gen-tag)))
      (defclass
	  (make-item
	   :type :class
	   :name (second form)
	   :arglist (third form)
	   :docstring (if (stringp (second (fifth form)))
			  (second (fifth form))
			  *default-doc-string*)
	   :class-members (fourth form)
	   :tag (gen-tag)))
      (defgeneric
	  (make-item
	   :type :generic
	   :name (second form)
	   :arglist (third form)
	   :docstring (if (stringp (fourth form))
			  (fourth form)
			  *default-doc-string*)
	   :tag (gen-tag)))
      (defmethod
	  (make-item
	   :type :method
	   :name (second form)
	   :arglist (third form)
	   :docstring (if (stringp (fourth form))
			  (fourth form)
			  nil)
	   :tag (gen-tag)))
      ((defvar defparameter)
       (make-item
	:type :variable
	:name (second form)
	:docstring (if (stringp (fourth form))
		       (fourth form)
		       *default-doc-string*)
	:default-value (third form)
	:tag (gen-tag)))
      (defconstant
	  (make-item
	   :type :constant
	   :name (second form)
	   :docstring (if (stringp (fourth form))
			  (fourth form)
			  *default-doc-string*)
	   :default-value (third form)
	   :tag (gen-tag)))
      ;; Syntax: (SECTION section-name &optional docstring)
      ;; Section numbers start at 1.
      (section
       (incf *section*)
       (setq *subsection* -1)
       (setq *subsubsection* -1)
       (make-item
	:type :section
	:name (second form)
	:docstring (if (stringp (third form))
		       (third form)
		       nil)
	:tag (gen-tag)
	:section (format nil "~D" *section*)))
      ;; Subsection numbers start at 0 within each section.
      (subsection
       (incf *subsection*)
       (setq *subsubsection* -1)
       (make-item
	:type :subsection
	:name (second form)
	:docstring (if (stringp (third form))
		       (third form)
		       nil)
	:tag (gen-tag)
	:section (format nil "~D.~D"
			 *section* *subsection*)))
      ;; Subsubsection numbers start at 0 within each
      ;; subsection.
      (subsubsection
       (incf *subsubsection*)
       (make-item
	:type :subsubsection
	:name (second form)
	:docstring (if (stringp (third form))
		       (third form)
		       nil)
	:tag (gen-tag)
	:section (format nil "~D.~D.~D"
			 *section* *subsection* *subsubsection*)))
      ;; Syntax (TO-DO string)
      ((to-do todo)
       (when (stringp (second form))
	 (make-item
	  :type :to-do
	  :docstring (second form)))))))


(defun lispdoc (filename)
  "Given a FILENAME, create a matching document file in the same
   directory in HTML format."
  (let* ((input-pathname
	  (merge-pathnames 
	   (pathname filename)
	   (pathname "foo.lisp")))
	 (title
	  (format nil "Documentation For ~S File"
		  (pathname-name input-pathname)))
	 (output-pathname
	  (merge-pathnames
	   (pathname
	    (concatenate 'string
			 (pathname-name input-pathname)
			 "-doc.html"))
	   input-pathname))
	 (items (extract-items input-pathname)))
	(with-open-file (out output-pathname
			     :direction :output
			     :if-does-not-exist :create
			     :if-exists :supersede)
	  (format out "<html><head><title>~a</title></head><body>" title)
	  (format out "<p><p><head><H1>Body of File</b></H1><p><p>")
	  (render-body items out)
	  (format out "<p><p><head><H1>Crib Sheet</b></H1><p><p>")
	  (render-crib items out)
	  (format out "<p><p><head><H1>Index</b></H1><p><p>")
	  (render-index items out)
	  (format out "<p><H1>Lisp Table of Contents</H1>")
	  (render-lisp-toc items out)
	  (format out "</body></html>"))))

(defun render-body (items s)
  "Given an ITEMS list, as created by EXTRACT-ITEMS, produce the
   appropriate HTML output to stream S."
  (dolist (item items)
    (ecase (item-type item)
      ((:function :macro :generic :method)
       (format
	s
	"<p><a name=~A>~A</a> <i>[~A]</i>"
	(item-tag item)
	(call-format
	 (item-name item)
	 (item-arglist item))
	(symbol-name (item-type item)))
       (when (item-docstring item)
	 (format s "<blockquote>~A</blockquote>" (item-docstring item))))
      ((:variable :constant)
       (format
	s
	"<p><a name=~A><b>~A</b></a> <i>[~A]</i>"
	(item-tag item)
	(string-downcase (item-name item))
	(symbol-name (item-type item)))
       (when (item-docstring item)
	 (format s "<blockquote>~A</blockquote>" (item-docstring item)))
       (when (item-default-value item)
	 (format s "<blockquote><i>Default value:</i> ~A</blockquote>" (item-default-value item)))
)
      (:class
       (format
	s
	"<p><a name=~A><b>~A</b></a> <i>[~A]</i>"
	(item-tag item)
	(string-downcase (item-name item))
	(symbol-name (item-type item)))
       (when (item-docstring item)
	 (format s "<blockquote>~A</blockquote>" (item-docstring item)))

       (when (item-class-members item)
	 ;(format s "<blockquote>")
	 (dolist (member (item-class-members item))
	   (format s "<blockquote><i>~A</i> - ~A</blockquote>" (string-downcase (first member)) (getf (cdr member) :documentation)))
	 ;(format s "</blockquote>")
       ))
      ((:section)
       (format
	s
	"<BR><p><a name=~A><h2>~A: ~A</h2></a>"
	(item-tag item)
	(item-section item)
	(item-name item))
       (when (item-docstring item)
	 (format s "<i>~A</i>" (item-docstring item))))
      ((:subsection)
       (format
	s
	"<p><a name=~A><h3>~A: ~A</h3></a>"
	(item-tag item)
	(item-section item)
	(item-name item))
       (when (item-docstring item)
	 (format s "<i>~A</i>" (item-docstring item))))
      ((:subsubsection)
       (format
	s
	"<p><a name=~A><h4>~A: ~A</h4></a>"
	(item-tag item)
	(item-section item)
	(item-name item))
       (when (item-docstring item)
	 (format s "<i>~A</i>" (item-docstring item))))
      (:to-do
       (format s "<p><font color=red>TODO:</font> ~A"
       (item-docstring item))))))

(defun render-crib (items s)
  "Given an ITEMS list, as created by EXTRACT-ITEMS, produce a
   cribsheet showing all the functions in call format, with
   links back to the body forms.  Output to stream S."
  (dolist (item items)
    (case (item-type item)
      ((:function :macro :generic)
       (format
	s
	"<br><a href=\"#~A\">~A</a>"
	(item-tag item)
	(call-format
	 (item-name item)
	 (item-arglist item))))
      ((:variable :constant)
       (format
	s
	"<br><a href=\"#~A\"><b>~A</b></a>"
	(item-tag item)
	(string-downcase (item-name item))))
      ((:section :subsection :subsubsection)
       (format
	s
	"<p><b>~A</b>"
	(item-name item))))))

(defun render-index (items s)
  "Given an ITEMS list, as created by EXTRACT-ITEMS, produce an
   unsorted index showing all the function and variable names,
   with links back to the body form."
  (dolist (item items)
    (case (item-type item)
      ((:function :macro :generic :variable :constant)
       (format
	s
	"<p><a href=\"#~A\"><b>~A</b></a>"
	(item-tag item)
	(item-name item))))))

(defun render-lisp-toc (items s)
  "Given an ITEMS list, produce a table of comments in HTML
   formatsuitable for pasting into a Lisp file, sending the output to
   stream S."
  (format s "<PRE><P>")
  (dolist (item items)
    (case (item-type item)
      (:section
       (format
	s
	"<br><b>;;; ~A</b>"
	(item-name item)))
      (:subsection
       (format
	s
	"<br><b>;;;    ~A</b>"
	(item-name item)))
      (:subsubsection
       (format
	s
	"<br><b>;;;       ~A</b>"
	(item-name item)))))
  (format s "</PRE>"))

(defun call-format (fn arglist)
  "Create a fragment of HTML showing the function and its arglist."
  (let ((fn-string (string-downcase (symbol-name fn))))
    (cond (arglist
	   ;; Process the arglist and merge it with the fn.
	   (let ((argstring (argtrim arglist)))
	     (setf (aref argstring 0) #\space)
	     (format nil "(<b>~A</b>~A" fn-string argstring)))
	  ;; Arglist is nil, so just use (fn).
	  (t (format nil "(<b>~A</b>)" fn-string)))))

(defun argtrim (arglist)
  "Strip out the default values and other stuff in an arglist, leaving
   only the symbols.  For keyword arguments, turn the vanilla symbols
   into keywords.  Downcase the string and return."
  (let ((in-keywords nil)
	(outlist nil))
    ;; For each member of ARGLIST...
    (dolist (a arglist)
      ;; If it's a list, just keep the CAR.
      (when (listp a)
	(setq a (car a)))
      (cond
	;; After &KEY, turn vars into keywords.
	((eq a '&key)
	 (setq in-keywords t)
	 (push a outlist))
	;; For any other &-tokens, just save them.
	((member a '(&optional &rest &aux))
	 (push a outlist))
	;; If we've seen &KEY, turn vars to keywords.
	((and in-keywords (not (keywordp a)))
	     (push (intern (symbol-name a) "KEYWORD") outlist))
	;; Otherwise, just add the symbol to the list.
	(t (push a outlist))))
    ;; Return the list, lower case.
    (string-downcase (format nil "~A" (nreverse outlist)))))

