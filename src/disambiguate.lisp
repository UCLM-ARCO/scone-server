;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; lookup-element-predicate and disambiguate functions from 
;;; the preview (v7) version of Scone Engine for the Scone Knowledge Representation System
;;;
;;; AUTHOR: Scott E. Fahlman <sef@cs.cmu.edu>
;;; ***************************************************************************

;;; Copyright (C) 2003-2006, Carnegie Mellon University.

;;; The Scone software is made available to the public under the CPL 1.0
;;; Open Source license.  A copy of this license is distributed with the
;;; software.  The license can also be found at URL
;;; <http://www.opensource.org/licenses/cpl.php>.

;;; By using, modifying, reproducing, or distibuting the Scone software,
;;; you agree to be bound by the terms and conditions set forth in the CPL
;;; 1.0 Open Source License.  If you do not agree to these terms and
;;; conditions, or if they are not legally applicable in the jurisdiction
;;; where such use takes place, then you may not use the Scone software.

;;; Scone incoporates some parts of the NETL2 system, developed by Scott E.
;;; Fahlman for IBM Coporation between June 2001 and May 2003.  IBM holds
;;; the copyright on NETL2 and has made that software available to the
;;; author under the CPL 1.0 Open Source license.

;;; Development of Scone has been supported in part by the Defense Advanced
;;; Research Projects Agency (DARPA) under contract number NBCHD030010.
;;; Any opinions, findings and conclusions or recommendations expressed in
;;; this material are those of the author(s) and do not necessarily reflect
;;; the views of DARPA or the Department of Interior-National Business
;;; Center (DOI-NBC).

;;; ***************************************************************************
;;; GENERAL STYLE NOTE: We want the runtime marker-scanning operations to
;;; be as fast as possible and to be non-consing.  So that code is
;;; optimized for performance first and for clarity second.  Code that
;;; builds or modifies the KB, or that does higher-level setup for marker
;;; ops, can be slower, so I have tried to optimize that for clarity and
;;; ease of maintenance..
;;; ***************************************************************************


(defvar *disambiguate-policy* :heuristic
  "This global var controls what happens when an ambiguous element name
   is encountered during input.  :ERROR says to signal an error.  :ASK
   says to ask the user.  :HEURISTIC says that we should try to choose
   the best definition based on the :HINTS or :SYNTAX-TAGS arguments
   to DISAMBIGUATE (if any), or just pick a definition at random.")

;; NEW
;; We need a version of (lookup-element) that doesn't give
;; ambiguous results
(defun lookup-element-predicate (name &key syntax-tags hints)
  "Given an element name, look up and return the corresponding element.  If
   the element doesn't yet exist, return NIL.  If we are given a string,
   we look this up as an English name.  :SYNTAX-TAGS, if present, is a list
   of tags that we will consider in this lookup.  If the lookup still
   returns multiple candidates, pass the set of candidates along with
   with :SYNTAX-TAGS and :HINTS to DISAMBIGUATE.  If we look up an
   English name and return an element, return its syntax tag as the second
   return value."
  (typecase name
    ;; If it's already an element, return it.
    (element name)
    ;; If it's an iname-structure, look up the element.
    (element-iname
     (let* ((string (element-iname-string name))
	    (colon-pos)
	    (namespace-name)
	    (namespace)
	    (iname-string))
       (if (setq colon-pos  (position #\: string))
	   ;; It's an iname with an explicit namespace, followed by a
	   ;; colon.  Pick it apart and look it up. Return NIL on failure.
	   (progn 
	     (setq namespace-name  (subseq string 0 colon-pos))
	     (setq namespace
		   (gethash (string-upcase namespace-name) *namespaces*))
	     (when namespace
	       (setq iname-string (subseq string (+ colon-pos 1) nil))
	       ;; Return the iname if it exists in the specified
	       ;; namespace.
	       (lookup-string-in-namespace iname-string namespace)))
	   ;; It's a vanilla iname.  Look it up in default namespace.
	   (lookup-string-in-namespace string *namespace*))))
    ;; If it's a string, look it up as an English word or phrase.
    (string
     (let ((defs (lookup-definitions name syntax-tags)))
       (cond ((null defs) nil)
	     ((null (cdr defs))
	      (values (car (car defs))
		      (cdr (car defs))))
	     (t (disambiguate name defs
			      syntax-tags hints)))))
    ;; If it's a number or function, create and return 
    ;; the element.
    (number (new-number name))
    (function (new-function name))
    (structure-object (new-struct name))))



(defun disambiguate (name definition-list
			  &optional syntax-tags hints)
  "Given a list of possible definitions (element.tag pairs) for an
   English word, try to disambiguate it based on the value of
   *disambiguate-policy*.  If :ERROR, just signal an error.  If
   :ASK, ask the user.  If :HEURISTIC, use HINTS and SYNTAX-TAGS
   to choose the best definition, or if that fails just pick one.

   HINTS is a list of elements.  We consider these one by one, and
   return any definition element that is either above or below the
   hint-element in the is-a hierarchy.  If we do not choose a winner
   based on HINTS, assume that the SYNTAX-TAGS argument is a list
   of tags in descending order of preference, and return the definition
   that has the most favored tag."
  (ecase *disambiguate-policy*
    ;; If :ERROR, just signal an error.
    (:error "The English word ~S with syntax ~S is ambiguous."
	    name syntax-tags)
    ;; If :ASK, print out the options, with the parent of each
    ;; candidate element, then ask the user to choose one by number.
    (:ask
     ;; Print a header.
     (if syntax-tags
	 (format t "~%The English word ~S with syntax ~S is ambiguous.  ~
		 Possible meanings:~2%"
		 name syntax-tags)
	 (format t "~%The English word ~S is ambiguous.  ~
		 Possible meanings:~2%"
		 name))
     ;; Print each of the definitions, preceded by a numerical index.
     (do ((defs definition-list (cdr defs))
	  (n 0 (1+ n)))
	 ((null defs))
       (let ((def (car defs)))
	 (format t "~D: ~S, of element-type ~S, syntax ~S~%"
		 n (car def) (element-type-label (car def)) (cdr def))
	 (format t "    Parent: ~A~2%" (parent-wire (car def)))))
     ;; Now ask the users to choose.
     (format t "Type the number of the meaning you want: ")
     (force-output t)
     (let ((n (read t)))
       (terpri t)
       (values (car (nth n definition-list))
	       (cdr (nth n definition-list)))))
    (:heuristic
     ;; First process the hints, in order, if there are any.
     (when hints
       (with-temp-markers (m1 m2)
	 (dolist (hint hints)
	   ;; Mark elements above and below the hint element.
	   (upscan hint m1)
	   (downscan hint m2)
	   (dolist (def definition-list)
	     ;; Return any definition that is so marked.
	     (when (or (fast-marker-on? (car def) m1)
		       (fast-marker-on? (car def) m2))
	       (return-from disambiguate 
		 (values (car def) (cdr def))))))))
     ;; Now use the syntax-tags list.  Look at each
     ;; syntax tag in order of preference, and return the first
     ;; candidate definition of that type.
     (when syntax-tags
       (dolist (pref syntax-tags)
	 (unless (eq pref :other)
	   (dolist (def definition-list)
	     (when (eq (cdr def) pref)
	       (return-from disambiguate
		 (values (car def) (cdr def))))))))
     ;; No winners, just return the first def in the list.
     (values (car (car definition-list))
	     (cdr (car definition-list))))))
