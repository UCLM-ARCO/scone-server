;;; ***************************************************************************
;;; 
;;;  xml-helper-fcns.lisp

;;;  XML helper functions, primarily for manipulating a parsed XML DOM structure.
;;; 
;;;  Author: Ben Lambert (benlambert@cmu.edu)

;;; ***************************************************************************



;; HELPER FUNCTIONS

;;; These functions are some basic functions for accessing the low-level structure 
;;; of the DOM tree.  These functions are specfic to the S-XML parser's default
;;; output format dubbed ":LXML".



(defun make-string-xml-friendly (string)
  "Substitute the 5 predefined entity references, in an UNTAGGED string can be used in an XML file."
  (setf string (cl-ppcre:regex-replace-all "&" string "&amp;")) ;; You have to do the ampersand first!
  (setf string (cl-ppcre:regex-replace-all "<" string "&lt;"))
  (setf string (cl-ppcre:regex-replace-all ">" string "&gt;"))
  (setf string (cl-ppcre:regex-replace-all "'" string "&apos;"))
  (setf string (cl-ppcre:regex-replace-all "\"" string "&quot;")))

(defun make-element-xml-friendly (element)
  (make-string-xml-friendly (format nil "~A" element)))

(defun has-attributes? (dom)
  "Does the top level element have any attributes?"
  (if (atom dom)    ;if it's an atom, then it doesn't have attributes or children
      nil
      (not (atom (car dom)))))  ;other check if the car is atomic or a list

(defun get-attributes (dom)
  "Get a p-list of attributes for the toplevel node."
  (if (has-attributes? dom)
      (rest (first dom))))

(defun get-attribute (dom attr)
  "Get the value for the specified attribute name from the toplevel node in the DOM tree."
  (if (has-attributes? dom)
      (getf (get-attributes dom) attr)
      nil))

(defun get-children (dom)
  "Get a list of the children of the topmost node in the DOM tree."
  (if (atom dom)
      nil
      (rest dom))) ;The children are every other than the first

(defun get-tagname (dom)
  "Get the tagname of the topmost node in the DOM tree."
  (if (atom dom)
      dom
      (if (atom (first dom))
	  (first dom)
	  (caar dom))))

(defun get-child (dom child-name)
  "Get a list of all child nodes where the tagname of the child matches that specified by the parameter child-name.  This returns a list since you could have an element with two children with the same name (e.g <a><b>text1</b><b>text2</b></a>).  If there is only a single matching child this returns just that child for convenience."
  (let ((return-list))
    (dolist (child (get-children dom)) ;; for each child
      (if (eql (get-tagname child) child-name)
	  (setf return-list (append return-list (list child)))))
    (if (= (length return-list) 1) ;if there's only one child, strip the outermost list off of it for convenience(?)
	(car return-list)
	return-list
	)))

(defun get-first-child (dom &optional (child-name nil child-name-supplied-p))
  "Get a list of all child nodes where the tagname of the child matches that specified by the parameter child-name.  This returns a list since you could have an element with two children with the same name (e.g <a><b>text1</b><b>text2</b></a>).  If there is only a single matching child this returns just that child for convenience."
  (let ((children (if child-name-supplied-p
		      (get-child dom child-name)
		      (get-children dom))))
    (if (= (length children) 1)
	children
	(car children))))

(defun element->xml (element)
  "Convert a SCONE element into an XML <node>."
  (if (lookup-element-predicate element)
      (concatenate 'string "<scone-element value='" (element-name element) "' text='" (extract-name (element-name element)) "'/>")
      (concatenate 'string "<scone-element value='null' text=''/>")))

(defun elements->xml (elt-list)
  (reduce 
   #'(lambda (x y) (concatenate 'string x y)) 
   (concatenate 'list 
		'("<scone-elements>")
		(map 'list #'element->xml elt-list)
		'("</scone-elements>"))))
	


;(defun eval-lisp (x)
;  (pprint (eval (read-from-string (cadr x))) *output-to-client* )  ;; a hack, until I know how to convert S-Expr to a string
;  ""
;)

(defun eval-lisp (x)
  (format nil "~A" (eval (read-from-string (cadr x)))))

;; OUTPUT FUNCTIONS


(defun statement->xml (stmt)
  "Convert a SCONE statement into XML."
  (if (or (statement? stmt) (not-statement? stmt))
      (concatenate 'string
		   "<scone-relation "
		   "qualifier='"
		   (if (not-statement? stmt) "false")
		   "' "
		   "arity='" (if (c-wire stmt) "3" "2") "' "
		   "context-id='" "'"
		   ">"
		   "<type>"
		   (element->xml (parent-wire stmt))
		   "</type>"
		   "<slot n='0'>"
		   (element->xml (a-wire stmt))
		   ;(element->xml (get-srl-arg-label (a-wire stmt)))
		   "</slot>"
		   "<slot n='1'>"
		   (element->xml (b-wire stmt))
		   ;(element->xml (get-srl-arg-label (b-wire stmt)))
		   "</slot>"
		   (if (c-wire stmt)
		       (concatenate 'string
				    "<slot n='3'>"
				    (element->xml (c-wire stmt))
				    "</slot>"))
		   "</scone-relation>"
		   )
      ""))
(defun statements->xml (stmt-list)
  "Convert a list of SCONE statements to an XML <response>."
  (let ((xml-string-list (map 'list #'statement->xml stmt-list)))
    (concatenate 'string "<scone-relations>"
    (cond 
      ((> (length xml-string-list) 1)
       (reduce 
	#'(lambda (x y) (concatenate 'string x y)) 
	(map 'list #'statement->xml stmt-list)))
      ((= (length xml-string-list) 1)
       (car xml-string-list))
      ((= (length xml-string-list) 0)
       ""))
    "</scone-relations>")))


(defun event->xml (event &optional arg-subst-plist)
  "Convert a SCONE event into XML."
  (if (is-x-a-y? event {event})
      (let ((roles-to-print '()))
	(dolist (arg '({arg0} {arg1} {arg2} {arg3}))

	  (if (find-the-x-role-of-y arg event)
	      (push arg roles-to-print)))
	
	(concatenate 'string
		     "<scone-relation qualifier='" "' "
		     "arity='" (format nil "~A" (length roles-to-print)) "'>"
		     "<type>"
		     (element->xml (lookup-element event))
		     "</type>"
		     (reduce 
		      #'(lambda (x y) (concatenate 'string x y))
		      (map 'list 
			   #'(lambda (x) (concatenate 
					  'string 
					  "<slot n='" 
					  (let ((arg
						 (cond
						   ((eq (lookup-element x) (lookup-element {arg0})) 0)
						   ((eq (lookup-element x) (lookup-element {arg1})) 1)
						   ((eq (lookup-element x) (lookup-element {arg2})) 2)
						   ((eq (lookup-element x) (lookup-element {arg3})) 3)
						   ((eq (lookup-element x) (lookup-element {arg4})) 4))))
					    (if (getf arg-subst-plist arg)
						(format nil "~A" (getf arg-subst-plist arg))
						(format nil "~A" arg)))

					  "'>" 
					  ;;(element->xml (get-the-x-role-of-y x event))
					  (element->xml (lookup-element x))
					  "</slot>")) 
			   roles-to-print ))
		     "</scone-relation>"
		     
		     ))))
	
(defun print-inverted-event (event)
  (event->xml event (read-from-string (caar (get-english-names (get-the-x-of-y {inverted event alternation} event))))))

(defun inverted-events->xml (stmt-list)
  "Convert a list of SCONE events to an XML <response>."
  (let ((xml-string-list (map 'list #'print-inverted-event stmt-list)))
    (concatenate 'string "<scone-relations>"
    (cond 
      ((> (length xml-string-list) 1)
       (reduce 
	#'(lambda (x y) (concatenate 'string x y))
	xml-string-list))
      ((= (length xml-string-list) 1)
       (car xml-string-list))
      ((= (length xml-string-list) 0)
       ""))
    "</scone-relations>")))

(defun x-is-the-what-of-y-wrt-z (x y z)
  (dolist (role (list-roles z))
	   (if (and (is-x-a-y-of-z? x role y)
		    (not (is-x-eq-y? role (lookup-element {involved element}))))
	       (return role))))

(defun get-srl-arg-label (role)
  (x-is-the-what-of-y-wrt-z role (context-wire (lookup-element role)) {action}))


(defun response-wrapper (string)
  "Wrap a string response with <response> tags"
  (format nil "<response>~A</response>" string))

(defun display-element-connections (e)
  "This function prints what is connected to each wire of a SCONE element to stdout."
  (format t "Full iname: ~a~%" (element-name e))
  (format t "A wire: ~a~%" (a-wire e))
  (format t "B wire: ~a~%" (b-wire e))
  (format t "C wire: ~a~%" (c-wire e))
  (format t "Parent wire: ~a~%" (parent-wire e))
  (format t "Context wire: ~a~%" (context-wire e))
  (format t "Split wires: ~a~%" (split-wires e))
  (format t "Role list(?): ~a~%" (list-roles e))
  (format t "Incoming A wires: ~a~%" (incoming-a-wires e))
  (format t "Incoming B wires: ~a~%" (incoming-b-wires e))
  (format t "Incoming C wires: ~a~%" (incoming-c-wires e))
  (format t "Incoming parent wires: ~a~%" (incoming-parent-wires e))
  (format t "Incoming context wires: ~a~%" (incoming-context-wires e))
  (format t "Incoming split wires: ~a~%" (incoming-split-wires e))
  (format t "~%~%")
)

