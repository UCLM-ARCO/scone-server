;(defparameter *focus-search-types* '("links" "all-wires" "wires-out" "wires-in"))

(defparameter *focus-search-types* '("user-level" "developer-level"))

(defparameter *max-elements-to-return* 200)

(defun get-focus-search-types ()
  (format *output-to-client* "~A" 
	  (reduce 
	   (lambda (x y) (concatenate 'string x " " y)) 
	   *focus-search-types*))
  (format nil ""))

;; If you change the name of this function you have to change the string on the second to last line.

(defun focus-search-developer-level (args)
  (format *output-to-client* 
	  (build-focus-element
	   ;; If this came from the search window of the Text View, it is already a string
	   (if (get-first-child args :|text|)
	       (get-attribute (get-first-child args :|text|) :|value|)
	       ;; If this came from the KB View (i.e. clicking on a node), read it into
	       ;; a string before processing it
	       (read-from-string (get-attribute (get-first-child args :|scone-element|) :|value|)))
	   #'format-element-all-wires-nbr-lists
	   "developer-level"))
  (format nil ""))

;; If you change the name of this function you have to change the string on the second to last line.

(defun focus-search-user-level (args)
  (format *output-to-client* 
	  (build-focus-element
	   (if (get-first-child args :|text|)
	       (get-attribute (get-first-child args :|text|) :|value|)
	       (read-from-string (get-attribute (get-first-child args :|scone-element|) :|value|)))
	   #'format-element-link-nbr-lists
	   "user-level"))
  (format nil ""))

(defun build-focus-element (e format-element-fn search-type)
  (format nil "<scone-focus-element search-type=\"~A\">~A</scone-focus-element>"
	  search-type
	  (funcall format-element-fn e)))

(defun build-nbr-list (e nbr-type display-info get-nbr-elements-fn format-nbr-element-fn 
			 &key (conn-name nbr-type) (conn-direction-out t) (nbr-list nil nbr-list-supplied-p))
  "Creates a nbr-list xml description for a given element e.  The list of neighbor elements to be included should be returned when get-nbr-elements-fn is applied to e.  This function is superceded by the nbr-list argument when it is supplied.  The format-nbr-element-fn should create a closed xml description when passed a neighboring element."  
  (format nil "<nbr-list nbr-type=\"~A\" conn-name=\"~A\" conn-dir=\"~A\" display-info=\"~A\">~A</nbr-list>"
	  nbr-type
	  conn-name
	  (if conn-direction-out "out" "in")
	  display-info
	  (reduce
	   #'(lambda (x y) (concatenate 'string x (funcall format-nbr-element-fn y)))
	   (if nbr-list-supplied-p
	       ;; If there are more than max, crop the list... -Ben
	       (if (> (length nbr-list) *max-elements-to-return*)
		   (subseq nbr-list 0 *max-elements-to-return*)
		   nbr-list)
	       (funcall get-nbr-elements-fn e))
	   :initial-value "")))

(defun format-element-all-wires-nbr-lists (e)
  (setq e (lookup-element-predicate e ))
  (if (not (null e))
      (format nil "~A~A~A</element>"
	      (element-tag-xml e nil)
	      (build-wires-out-nbr-lists e)
	      (build-wires-in-nbr-lists e))
      "NO_RESULTS"))
  


(defun build-wires-out-nbr-lists (e)
  (format nil "~A~A~A~A~A~A"
	  (if (a-wire e)
	      (build-nbr-list e 
			      "a-wire" 
			      "iname" 
			      nil 
			      #'element-tag-xml-closed 
			      :nbr-list (list (a-wire e)))
	      "")
	  (if (b-wire e)
	      (build-nbr-list e
			      "b-wire" 
			      "iname" 
			      nil #'element-tag-xml-closed 
			      :nbr-list (list (b-wire e)))
	      "")
	  (if (c-wire e)
	      (build-nbr-list e
			      "c-wire" 
			      "iname" 
			      nil 
			      #'element-tag-xml-closed 
			      :nbr-list (list (c-wire e)))
	      "")
	  (if (parent-wire e)
	      (build-nbr-list e
			      "parent-wire" 
			      "iname" 
			      nil 
			      #'element-tag-xml-closed 
			      :nbr-list (list (parent-wire e)))
	      "")
	  (if (context-wire e)
	      (build-nbr-list e 
			      "context-wire" 
			      "iname" 
			      nil 
			      #'element-tag-xml-closed 
			      :nbr-list (list (context-wire e)))
	      "")
	  (if (split-wires e)
	      (build-nbr-list e 
			      "split-wires" 
			      "iname" 
			      nil 
			      #'element-tag-xml-closed 
			      :nbr-list (split-wires e)
			      :conn-name "split-wire")
	      "")))


(defun build-wires-in-nbr-lists (e)
  (let ((l nil))
    (format nil "~A~A~A~A~A~A" 
	    (if (setq l (incoming-a-wires e))
		(build-nbr-list e 
				"incoming-a-wires" 
				"iname" 
				nil 
				#'element-tag-xml-closed 
				:nbr-list l
				:conn-name "a-wire"
				:conn-direction-out nil) 
		"")
	    (if (setq l (incoming-b-wires e))
		(build-nbr-list e
				"incoming-b-wires" 
				"iname" 
				nil #'element-tag-xml-closed 
				:nbr-list l
				:conn-name "b-wire"
				:conn-direction-out nil)
		"")
	    (if (setq l (incoming-c-wires e))
		(build-nbr-list e
				"incoming-c-wires" 
				"iname" 
				nil 
				#'element-tag-xml-closed 
				:nbr-list l
				:conn-name "c-wire"
				:conn-direction-out nil)
		"")
	    (if (setq l (incoming-parent-wires e))
		(build-nbr-list e
				"incoming-parent-wires" 
				"iname" 
				nil 
				#'element-tag-xml-closed 
				:nbr-list l
				:conn-name "parent-wire"
				:conn-direction-out nil)
		"")
	    (if (setq l (incoming-context-wires e))
		(build-nbr-list e 
				"incoming-context-wires" 
				"iname" 
				nil 
				#'element-tag-xml-closed 
				:nbr-list l
				:conn-name "context-wire"
				:conn-direction-out nil)
		"")
	    (if (setq l (incoming-split-wires e))
		(build-nbr-list e 
				"incoming-split-wires" 
				"iname" 
				nil 
				#'element-tag-xml-closed 
				:nbr-list l
				:conn-name "split-wire"
				:conn-direction-out nil)
		""))))
  
(defun format-element-link-nbr-lists (e)
  ;; NEW
  ;; E is a string here.  We don't want to trigger
  ;; user queries for disambiguation in the case of an
  ;; ambiguous string. So we'll call a version of lookup-element
  ;; that doesn't prompt, namely lookup-element-predicate
  (setq e (lookup-element-predicate e ))
  (if (not (null e))
      (format nil "~A~A</element>"
	      (element-tag-xml e nil)
	      (build-link-nbr-lists e))
      "NO_RESULTS"))



(defun build-link-nbr-lists (e)
  (let ((parents nil)
	(not-parents nil)
	(children nil)
	(not-children nil)
	(equal nil)
	(not-equal nil)
	(maps-filler-of nil)
	(maps-role-of nil)
	(maps-owner-of nil)
	(splits nil)
	;(complete-splits "")
	(relations nil)
	(statements nil)
	(contexts nil)
	(nodes-split nil)
	(nodes-contextualized nil)
	(roles nil)
	(unknown nil)
	(target nil))
    (when (setq target (parent-wire e))
      (push target parents))
    (when (setq target (context-wire e))
      (push target contexts))
    (when (setq target (split-wires e))
      (setq nodes-split target))
    (when (setq target (incoming-parent-wires e))
      (setq children target))
    (when (setq target (incoming-context-wires e))
      (setq nodes-contextualized target))
    (when (setq target (incoming-split-wires e))
      (setq splits target))
    (when (setq target (list-roles e))
      (setq roles target))
    (when (setq target (incoming-a-wires e))
      (dolist (nbr target)
	(cond
	  ((is-a-link? nbr)(push (b-wire nbr) parents))
	  ((is-not-a-link? nbr)(push (b-wire nbr) not-parents))
	  ((eq-link? nbr)(push (b-wire nbr) equal))
	  ((not-eq-link? nbr)(push (b-wire nbr) not-equal))
          ;((map? nbr)(push nbr maps-filler-of))
	  ((relation? nbr)(push nbr relations))
	  ((statement? nbr)(push nbr statements)))))
	  ;;(t (push nbr unknown)))))  ;; DEPRECATED (akt)
    (when (setq target (incoming-b-wires e))
      (dolist (nbr target)
	(cond
	  ((is-a-link? nbr)(push (a-wire nbr) children))
	  ((is-not-a-link? nbr)(push (a-wire nbr) not-children))
	  ((eq-link? nbr)(push (a-wire nbr) equal))
	  ((not-eq-link? nbr)(push (a-wire nbr) not-equal))
	  ;((map? nbr)(push nbr maps-role-of))
	  ((relation? nbr)(push nbr relations))
	  ((statement? nbr)(push nbr statements)))))
	  ;;(t (push nbr unknown))))) ;; DEPRECATED (akt)
    (when (setq target (incoming-c-wires e)) 
      (dolist (nbr target)
	(cond
	  ;((map? nbr)(push nbr maps-owner-of))
	  ((relation? nbr)(push nbr relations))
	  ((statement? nbr)(push nbr statements)))))
	  ;; (t (push nbr unknown))))) ;; DEPRECATED (akt)

    (concatenate 'string
		 ;; This more or less adds "roles" to SconeEdit!! -Ben
		 (when roles		
		     (build-nbr-list e
				     "Roles"     ;title of the pane in the list view
				     "iname"	 ;??
				     nil
				     #'element-tag-xml-closed
				     :nbr-list roles
				     :conn-name "role"			;;name on the edge in the graph
				     :conn-direction-out t))		;; direction of the edge in the graph
		 (when parents
		     (build-nbr-list e
				     "Parents (IS A)"
				     "iname"
				     nil
				     #'element-tag-xml-closed
				     :nbr-list parents
				     :conn-name "is-a"
				     :conn-direction-out t))
		 (when not-parents
		     (build-nbr-list e
				     "Not-Parents (IS A)"
				     "iname"
				     nil
				     #'element-tag-xml-closed
				     :nbr-list not-parents
				     :conn-name "is-not-a"
				     :conn-direction-out t))
		 (when children
		     (build-nbr-list e
				     "Children (IS A)"
				     "iname"
				     nil
				     #'element-tag-xml-closed
				     :nbr-list children
				     :conn-name "is-a"
				     :conn-direction-out nil))
		 (when not-children
		     (build-nbr-list e
				     "Not Children (IS A)"
				     "iname"
				     nil
				     #'element-tag-xml-closed
				     :nbr-list not-children
				     :conn-name "is-not-a"
				     :conn-direction-out nil))
		 (when equal
		     (build-nbr-list e
				     "Equal"
				     "iname"
				     nil
				     #'element-tag-xml-closed
				     :nbr-list equal
				     :conn-name "equal"
				     :conn-direction-out t))
		 (when not-equal
		     (build-nbr-list e
				     "Not Equal"
				     "iname"
				     nil
				     #'element-tag-xml-closed
				     :nbr-list not-equal
				     :conn-name "not-equal"
				     :conn-direction-out t))
		 (when maps-filler-of
		     (build-nbr-list e
				     "Maps Filler Of"
				     "iname"
				     nil
				     #'element-tag-xml-closed
				     :nbr-list maps-filler-of
				     :conn-name "filler"
				     :conn-direction-out nil))
		 (when maps-role-of
		     (build-nbr-list e
				     "Maps Role Of"
				     "iname"
				     nil
				     #'element-tag-xml-closed
				     :nbr-list maps-role-of
				     :conn-name "role"
				     :conn-direction-out nil))
		 (when maps-owner-of
		     (build-nbr-list e
				     "Maps Owner Of"
				     "iname"
				     nil
				     #'element-tag-xml-closed
				     :nbr-list maps-filler-of
				     :conn-name "owner"
				     :conn-direction-out nil))
		 (when splits
		     (build-nbr-list e
				     "Splits"
				     "iname"
				     nil
				     #'element-tag-xml-closed
				     :nbr-list splits
				     :conn-name "split"
				     :conn-direction-out nil))
		 (when relations
		     (build-nbr-list e
				     "Relations"
				     "iname"
				     nil
				     #'element-tag-xml-closed
				     :nbr-list relations
				     :conn-name "relation"
				     :conn-direction-out nil))
		 (when statements
		     (build-nbr-list e
				     "Statements"
				     "iname"
				     nil
				     #'element-tag-xml-closed
				     :nbr-list statements
				     :conn-name "statement"
				     :conn-direction-out nil))
		  (when contexts
		      (build-nbr-list e
				      "Contexts"
				      "iname"
				      nil
				      #'element-tag-xml-closed
				      :nbr-list contexts
				      :conn-name "context"
				      :conn-direction-out t))
		  (when nodes-split
		      (build-nbr-list e
				      "Nodes Split"
				      "iname"
				      nil
				      #'element-tag-xml-closed
				      :nbr-list nodes-split
				      :conn-name "split"
				      :conn-direction-out t))
		  (when nodes-contextualized
		      ;; Nodes-contextualized have to be interpreted differently
		      ;; depending on their type.  If E is a type-node, then they are
		      ;; mapped roles.  If E is a context, then they are just nodes 
		      ;; existing in that context
		      ;;
		      (cond ((is-x-a-y? e {context})
			     (build-nbr-list e
					     "Nodes specific to this Context" 
					     "iname"
					     nil
					     #'element-tag-xml-closed
					     :nbr-list nodes-contextualized
					     :conn-name "in-context"
					     :conn-direction-out nil))
			    ((type-node? e)
			     (build-nbr-list e
					     "Mapped Roles" 
					     "iname"
					     nil
					     #'element-tag-xml-closed
					     :nbr-list nodes-contextualized
					     :conn-name "Map"
					     :conn-direction-out nil))))
		 #| (if unknown
		      (build-nbr-list e
				      "Other related knowledge"
				      "iname"
				      nil
				      #'element-tag-xml-closed
				      :nbr-list unknown
				      :conn-name "unknown"
				      :conn-direction-out t)
		      "") |#
	)))

(defun element-tag-xml-closed (e)
  (element-tag-xml e t))

(defun element-tag-xml (e close-tag)
  "Returns an xml tag of type <element> describing the given element e 
(specifying iname and type).  If close-tag is true, adds a final '/' to the end
of the tag so a closing </element> is not required."

  (setq e (lookup-element e))
(when (not (null e))
  (format nil "<element name=\"~A\" type=\"~A\"~A>"
	  (element-name e)
           ;; Look here: I think there's a better way to get these labels.
           ;; why would this return an element of type 'UNKNOWN'??!
	  (element-type-label e)
	  (if close-tag "/" ""))))



