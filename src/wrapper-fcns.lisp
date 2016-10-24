;;; ***************************************************************************
;;; 
;;;  wrapper-fcns.lisp

;;;  This is where to put wrapper functions for the Scone server...
;;; 
;;;  Author: Ben Lambert (benlambert@cmu.edu)

;;; ***************************************************************************


(defun scone-annotation->xml (annot)
  "Convert a Scone annotation '({iname} offet length) to XML."
  (format nil "<scone-element value='~A' offset='~A' length='~A'/>" (make-element-xml-friendly (first annot)) (cadr annot) (caddr annot)))

(defun scone-annotations->xml (annot-list)
  "Convert a list of SCONE annotations to an XML <response>."
  (reduce                                          ;folds all the elements of a list into a single value (using a specified function)
   #'(lambda (x y) (concatenate 'string x y))      ;this is the function reduce uses
   (concatenate 'list                              ;this creates a list of strings which we want to return as a single string
		'("<response>")
		'("<scone-elements>")
		(map 'list #'scone-annotation->xml annot-list)
		'("</scone-elements>")
		'("</response>"))))

(defun get-scone-annotations-xml (args)
  (scone-annotations->xml                     ;convert the output to XML
   (get-scone-annotations                    ;call Alicia's function
    (get-tagname (get-first-child args)))))  ;this gets the the sole argument to the function from the DOM object
  
(defun scone-o-tator-xml (string)
  (scone-annotations->xml                     ;convert the output to XML
   (get-scone-annotations                    ;call Alicia's function
    string)))


(defun get-describe-graph-xml (args)
  (get-describe-graph
   (if (get-first-child args :|text|)
       (get-attribute (get-first-child args :|text|) :|value|)
       (read-from-string (get-attribute (get-first-child args :|scone-element|) :|value|)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  SCONE API FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun safe-lookup (name) 
  (if (> (length (lookup-definitions name)) 1)
      (error "AMBIGUOUS SCONE NAME: ~A~%" name)
      (lookup-element-pred name)))

(defun safe-lookup-event (name) 
  (if (= (length (lookup-definitions name)) 1)
      (lookup-element-pred name)
      (let ((event nil))
	(dolist (e (lookup-definitions name))
	  (if (is-x-a-y? (car e) {event})
	      (setf event (car e))))
	event)))


(defmacro new-indv-xml (&rest args)
  `(response-wrapper (element->xml (new-indv ,@args))))

(defmacro new-statement-xml (&rest args)
  `(response-wrapper (element->xml (new-statement ,@args))))

(defun string->concepts-xml (string)
  (response-wrapper (elements->xml (map 'list #'car (lookup-definitions string)))))

(defun get-english-names-xml (e)
  (let* ((elt (lookup-element e))
	(ename-list (map 'list #'car (get-english-names elt)))
	(iname (element-name elt)))
    (reduce 
     #'(lambda (x y) (concatenate 'string x y)) 
     (map 'list  
	  #'(lambda (x) (format nil "<scone-element value='~A' text='~A'/>" iname x ))
	  ename-list))))

(defun concept->strings-xml (string)
  (response-wrapper 
   (get-english-names-xml string)))

(defun english-names-xml (e name)
    (let* ((elt (lookup-element e))
	(ename-list (map 'list #'car (get-english-names elt)))
	(iname (element-name elt)))
    (reduce 
     #'(lambda (x y) (concatenate 'string x y)) 
     (map 'list  
	  #'(lambda (x) (format nil "<scone-element value='~A' text='~A'/>" iname x ))
	  ename-list))))

(defun is-x-a-y?-xml (x y)
  (response-wrapper 
   (if (is-x-a-y? x y)
       (format nil "<true/>")
       (format nil "<false/>"))))

(defun is-statement-true?-xml (x y z)
  (response-wrapper 
   (if (is-statement-true? x y z)
       (format nil "<true/>")
       (format nil "<false/>"))))

(defun lookup-element-xml (string)
  (let ((elt (lookup-element-pred string)))
    (response-wrapper
     (if (null elt)
	 ""
	 (element->xml elt)))))

(defun get-mined-facts-xml ()
  (response-wrapper
   (statements->xml (get-mined-facts))))

(defun get-all-statements-xml (rel)
  (response-wrapper
   (statements->xml (list-instances rel))))


(defun list-rel-xml (a rel)
  (response-wrapper (elements->xml (list-rel a rel))))  

(defun list-rel-inverse-xml (rel b)
  (response-wrapper (elements->xml (list-rel-inverse rel b))))

(defun english-xml (a b)
  (response-wrapper (elements->xml (english a b))))

;;; NEW function for adding temporary english names (akt)
(defun temp-english (e name)
  "Register NAME as an english-name of E, but also put NAME on a 
   list of temporary lexicon items that will be removed when the
   SconeEdit demo is reset for the next user."
  (multiple-value-bind (value exists) (intern name)
		       (when exists
			 (push (cons e name) *temp-dictionary*)))
  (english e name))

;;;Adding this one for Javelin support -BEL
(defun get-synonyms (args)
  (let ((iname (safe-lookup
		(if (get-first-child args :|text|)
		    (get-attribute (get-first-child args :|text|) :|value|)
		    (read-from-string (get-attribute (get-first-child args :|scone-element|) :|value|))))))
    (unless (null iname)
      (let ((ename-list (map 'list #'car (get-english-names iname)))
	    (xml ""))
	(dolist (ename ename-list)
	  (setq xml (concatenate 'string xml
				 (format nil "<scone-element value=\"~A\" text=\"~A\" />" iname ename))))
	(format nil "<scone-elements>~A</scone-elements>~%" xml)))))


;;;Adding this one for Javelin support -BEL
(defun get-all-subclasses (args)
  (let ((iname (safe-lookup
		(if (get-first-child args :|text|)
		    (get-attribute (get-first-child args :|text|) :|value|)
		    (read-from-string (get-attribute (get-first-child args :|scone-element|) :|value|))))))
    (unless (null iname)
      (let ((m1 (get-marker)))
	(downscan iname m1)
	(concatenate 'string "<response>" (elements->xml (list-marked m1)) "</response>")
))))


;	    (xml ""))
;	(dolist (ename ename-list)
;	  (setq xml (concatenate 'string xml
;				 (format nil "<scone-element value=\"~A\" text=\"~A\" />" iname ename))))
;	(format nil "<scone-elements>~A</scone-elements>~%" xml)))))




;(defun get-transitive-closure (node relation)
;  (let ((set (list-rel node relation)
;  (response-wrapper (elements->xml )))

#|
(defun get-transitive-closure (node relation depth)
  (with-temp-marker (m)
    (get-transitive-closure-helper node relation m depth)))


(defun get-transitive-closure-helper (node relation m depth)
  (pprint node)
  (if (<= depth 0)
      '()
      (let* ((lst (if (atom node) 
		      (list node)
		      node))
	     (expanded (reduce #'append (map 'list #'(lambda (x) (list-rel x relation)) lst)))
	     (filtered (remove-if #'(lambda (x) (marker-on? x m)) expanded)))
	(dolist (e filtered) (mark e m))
	(append filtered (get-transitive-closure-helper filtered relation m (- depth 1))))))
  
;  (let ((set (list-rel node relation)
;	  (response-wrapper (elements->xml )))

|#

(defun get-event-preconditions (event)
  (unless (is-x-a-y? event {event})
    (format t "Not an event")
    (return-from get-event-preconditions))
  (response-wrapper (statements->xml (list-before event))))

(defun get-precondition-predicates (event)
  (let* ((preconds (list-before event))
	 (precond-stmts (remove-if-not #'(lambda (x) (statement? x)) preconds)))
    (remove-duplicates (map 'list #'parent-wire precond-stmts))))

(defun events-caused-by (stmt)
  (let ((events '())
	(switch-list '()))
    (dolist (event (list-inferiors {event}))
      (dolist (precond-stmt (list-before event))
	(when (and (statement? precond-stmt)
		   (eq (parent-wire stmt) (parent-wire precond-stmt)))
	  ;; we have an event
	  (push 1 switch-list)
	  (push 1 switch-list)
	  (format t "A wire: ~A~%" (a-wire precond-stmt))
	  (format t "B wire: ~A~%" (b-wire precond-stmt))
	  (push event events)
	  ;;(setf events (append (list event) events))
	  (format t "Stmt type: ~A~%" (parent-wire stmt))
	  (format t "Prec type: ~A~%" (parent-wire precond-stmt)))))
    events))
    
#|
(defun expand-event (event)
  (let* ((previous-context *context*)
	 (temp-context (in-context (new-indv (gen-iname "tmp-context") previous-context))) ;; get into a new context
	 (event-indv (new-indv (gen-iname (element-name (lookup-element event))) event ) ))
    (new-eq previous-context temp-context)  ;; set them equal, so everything seems the same..
    (dolist (role '({arg0} {arg1} {arg2} {arg3}))
      (the-x-of-y-is-z role event-indv (new-indv (gen-iname (extract-name (element-name (lookup-element role)))) (parent-wire (lookup-element role)))))
    
    (dolist (arg '({arg0} {arg1} {arg2} {arg3}))
      (let ((role (get-the-x-of-y arg event-indv)))
	(in-context (get-the-x-role-of-y {before context} event-indv))
	(dolist (predicate (get-precondition-predicates event))
	  (dolist (e(list-rel arg predicate))
	    (pprint (list role predicate e))))
	(in-context temp-context)))

    ;; destroy the context we created and go back to the previous context
    (in-context previous-context)
    (destroy-context temp-context)  ;; hopefully this doesn't destroy everything..
    event-indv))
|#



(defun destroy-context (context)
  (with-temp-marker (m1)
    (mark-context-contents context m1)
    (dolist (x (list-marked m1))
      (remove-element x))))


(defun get-transitive-closure (args)
  (response-wrapper 
   (let ((relation (get-attribute (get-child args :|relation| ) :|value|))
	 (predicate (get-attribute (get-child (get-child args :|node| ) :|text|) :|value|)))
     (cond
       ((string-equal relation "precondition-of")
	(statements->xml (list-before (safe-lookup-event predicate))))
       ((string-equal relation "postcondition-of")
	(statements->xml (list-after (safe-lookup-event predicate))))
       ((string-equal relation "inverse-of")
	(inverted-events->xml (list-rel (safe-lookup-event predicate) {inverse event})))
       (t
	'())))))









