;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: xml-struct-dom.lisp,v 1.1 2006/10/11 14:08:46 benlambert Exp $
;;;;
;;;; XML-STRUCT implementation of the generic DOM parser and printer.
;;;;
;;;; Copyright (C) 2002, 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-xml)

;;; xml-element struct datastructure and API

(defstruct xml-element
  name        ; :tag-name
  attributes  ; a assoc list of (:attribute-name . "attribute-value")
  children    ; a list of children/content either text strings or xml-elements
  )

(setf (documentation 'xml-element-p 'function)
      "Return T when the argument is an xml-element struct"
      (documentation 'xml-element-attributes 'function)
      "Return the alist of attribute names and values dotted pairs from an xml-element struct"
      (documentation 'xml-element-children 'function)
      "Return the list of children from an xml-element struct"
      (documentation 'xml-element-name 'function)
      "Return the name from an xml-element struct"
      (documentation 'make-xml-element 'function)
      "Make and return a new xml-element struct")

(defun xml-element-attribute (xml-element key)
  "Return the string value of the attribute with name the keyword :key
  of xml-element if any, return null if not found"
  (let ((pair (assoc key (xml-element-attributes xml-element) :test #'eq)))
    (when pair (cdr pair))))

(defun (setf xml-element-attribute) (value xml-element key)
  "Set the string value of the attribute with name the keyword :key of
  xml-element, creating a new attribute if necessary or overwriting an
  existing one, returning the value"
  (let ((attributes (xml-element-attributes xml-element)))
    (if (null attributes)
	(push (cons key value) (xml-element-attributes xml-element))
      (let ((pair (assoc key attributes :test #'eq)))
	(if pair
	    (setf (cdr pair) value)
	  (push (cons key value) (xml-element-attributes xml-element)))))
    value))

(defun new-xml-element (name &rest children)
  "Make a new xml-element with name and children"
  (make-xml-element :name name :children children))

(defun first-xml-element-child (xml-element)
  "Get the first child of an xml-element"
  (first (xml-element-children xml-element)))

(defun xml-equal (xml-1 xml-2)
  (and (xml-element-p xml-1)
       (xml-element-p xml-2)
       (eq (xml-element-name xml-1)
	   (xml-element-name xml-2))
       (equal (xml-element-attributes xml-1)
	      (xml-element-attributes xml-2))
       (reduce #'(lambda (&optional (x t) (y t)) (and x y))
	       (mapcar #'(lambda (x y)
			   (or (and (stringp x) (stringp y) (string= x y))
			       (xml-equal x y)))
		       (xml-element-children xml-1)
		       (xml-element-children xml-2)))))

;;; printing xml structures

(defmethod print-xml-dom (xml-element (input-type (eql :xml-struct)) stream pretty level)
  (format stream "<~a" (xml-element-name xml-element))
  (dolist (attribute (xml-element-attributes xml-element))
    (format stream " ~a=\"" (car attribute))
    (print-string-xml (cdr attribute) stream)
    (format stream "\""))
  (let ((children (xml-element-children xml-element))) 
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
	  (format stream "</~a>" (xml-element-name xml-element)))
      (format stream "/>"))))

;;; the standard hooks to generate xml-element structs

(defun standard-new-element-hook (name attributes seed)
  (declare (ignore name attributes seed))
  '())

(defun standard-finish-element-hook (name attributes parent-seed seed)
  (let ((xml-element (make-xml-element :name name
				       :attributes attributes
				       :children (nreverse seed))))
    (cons xml-element parent-seed)))

(defun standard-text-hook (string seed)
  (cons string seed))

;;; top level standard parser interfaces

(defmethod parse-xml-dom (stream (output-type (eql :xml-struct)))
  (car (start-parse-xml stream
			(make-instance 'xml-parser-state
				       :new-element-hook #'standard-new-element-hook
				       :finish-element-hook #'standard-finish-element-hook
				       :text-hook #'standard-text-hook))))

;;;; eof
