;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: sxml-dom.lisp,v 1.1 2006/10/11 14:08:46 benlambert Exp $
;;;;
;;;; LXML implementation of the generic DOM parser and printer.
;;;;
;;;; Copyright (C) 2003, 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-xml)

;;; the sxml hooks to generate sxml

(defun sxml-new-element-hook (name attributes seed)
  (declare (ignore name attributes seed))
  '())

(defun sxml-finish-element-hook (name attributes parent-seed seed)
  (let ((xml-element (append (list name)
			     (when attributes
			       (list (let (list)
				       (dolist (attribute attributes (cons :@ list))
					 (push (list (car attribute) (cdr attribute)) list)))))
			     (nreverse seed))))
    (cons xml-element parent-seed)))

(defun sxml-text-hook (string seed)
  (cons string seed))

;;; the standard DOM interfaces

(defmethod parse-xml-dom (stream (output-type (eql :sxml)))
  (car (start-parse-xml stream
			(make-instance 'xml-parser-state
				       :new-element-hook #'sxml-new-element-hook
				       :finish-element-hook #'sxml-finish-element-hook
				       :text-hook #'sxml-text-hook))))

(defmethod print-xml-dom (dom (input-type (eql :sxml)) stream pretty level)
  (cond ((stringp dom) (print-string-xml dom stream))
	((consp dom)
	 (let ((tag (car dom))
	       attributes
	       children)
	   (if (and (consp (cadr dom)) (eq (caadr dom) :@))
	       (setf attributes (cdadr dom)
		     children (cddr dom))
	     (setf children (cdr dom)))
	   (format stream "<~a" tag)
	   (dolist (pair attributes)
	     (format stream " ~a=\"" (car pair))
	     (print-string-xml (cadr pair) stream)
	     (format stream "\""))
	   (if children
	       (progn
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
	(t (error "Input not recognized as SXML ~s" dom))))

;;;; eof
