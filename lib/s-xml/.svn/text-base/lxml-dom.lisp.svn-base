;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: lxml-dom.lisp,v 1.1 2006/10/11 14:08:46 benlambert Exp $
;;;;
;;;; LXML implementation of the generic DOM parser and printer.
;;;;
;;;; Copyright (C) 2002, 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-xml)

;;; the lxml hooks to generate lxml

(defun lxml-new-element-hook (name attributes seed)
  (declare (ignore name attributes seed))
  '())

(defun lxml-finish-element-hook (name attributes parent-seed seed)
  (let ((xml-element
	 (cond ((and (null seed) (null attributes))
		name)
	       (attributes
		`((,name ,@(let (list)
			     (dolist (attribute attributes list)
			       (push (cdr attribute) list)
			       (push (car attribute) list))))
		  ,@(nreverse seed)))
	       (t
		`(,name ,@(nreverse seed))))))
    (cons xml-element parent-seed)))

(defun lxml-text-hook (string seed)
  (cons string seed))

;;; standard DOM interfaces

(defmethod parse-xml-dom (stream (output-type (eql :lxml)))
  (car (start-parse-xml stream
			(make-instance 'xml-parser-state
				       :new-element-hook #'lxml-new-element-hook
				       :finish-element-hook #'lxml-finish-element-hook
				       :text-hook #'lxml-text-hook))))

(defmethod print-xml-dom (dom (input-type (eql :lxml)) stream pretty level)
  (cond ((symbolp dom) (format stream "<~a/>" dom))
	((stringp dom) (print-string-xml dom stream))
	((consp dom)
	 (let (tag attributes)
	   (cond ((symbolp (car dom)) (setf tag (car dom)))
		 ((consp (car dom)) (setf tag (caar dom) attributes (cdar dom)))
		 (t (error "Input not recognized as LXML ~s" dom)))
	   (format stream "<~a" tag)
	   (labels ((print-attributes (attributes)
				      (unless (null attributes)
					(format stream " ~a=\"" (car attributes))
					(print-string-xml (cadr attributes) stream)
					(format stream "\"")
					(print-attributes (cddr attributes)))))
	     (when attributes (print-attributes attributes)))
	   (if (cdr dom)
	       (let ((children (cdr dom)))
		 (format stream ">")
		 (if (and (= (length children) 1) (stringp (first children)))
		     (print-string-xml (first children) stream)
		   (progn
		     (dolist (child children)
		       (when pretty
			 (terpri stream)
			 (dotimes (i (* 2 level)) (write-char #\space stream)))
		       (if (stringp child)
			   (print-string-xml child stream)
			 (print-xml-dom child input-type stream pretty (1+ level))))
		     (when pretty
		       (terpri stream)
		       (dotimes (i (* 2 (1- level))) (write-char #\space stream)))))
		 (format stream "</~a>" tag))
	     (format stream "/>" tag))))
	(t (error "Input not recognized as LXML ~s" dom))))
  
;;;; eof