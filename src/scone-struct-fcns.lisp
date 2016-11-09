(defun get-neighborhood-sconestruct (args)
  (format *output-to-client*
	  (get-neighborhood-sconestruct-xml
	   (if (get-first-child args :|text|)
	       (get-attribute (get-first-child args :|text|) :|value|)
	       (read-from-string (get-attribute (get-first-child args :|scone-element|) :|value|)))))
  (format nil ""))

(defun get-neighborhood-sconestruct-xml (e)
  " "
  (setq e (lookup-element e))
  ;; Include e in its own neighborhood. 
  (let ((neighborhood (format nil "<focus>~A</focus>~%" (format-element-xml e))))
    (dolist (z (get-links-internal e))
      (setq neighborhood 
	    (concatenate 'string neighborhood (format-element-xml z))))
    (format nil "<sconestruct>~%~A</sconestruct>~%" neighborhood)))

(defun get-links-internal (e)
  "Returns all the nodes connected to (with wires to) this element."
  (setq e (lookup-element e))
  (append (incoming-a-wires e)
	  (incoming-b-wires e)
	  (incoming-c-wires e)
	  (incoming-parent-wires e)
	  (incoming-context-wires e)
	  (incoming-split-wires e)))

(defun format-element-xml (e)
  "Returns a full specification of an element in xml including iname, type and wire connections."
  (setq e (lookup-element e))
  (let ((a "")
	(b "")
	(c "")
	(parent "")
	(context "")
	(split "")
	(target nil))
    (when (setq target (a-wire e))
      (setq a (format nil "<a-wire>~A</a-wire>~%" (element-tag-xml target t))))
    (when (setq target (b-wire e))
      (setq b (format nil "<b-wire>~A</b-wire>~%" (element-tag-xml target t))))
    (when (setq target (c-wire e))
      (setq c (format nil "<c-wire>~A</c-wire>~%" (element-tag-xml target t))))
    (when (setq target (parent-wire e))
      (setq parent (format nil "<parent-wire>~A</parent-wire>~%" (element-tag-xml target t))))
    (when (setq target (context-wire e))
      (setq context (format nil "<context-wire>~A</context-wire>~%" (element-tag-xml target t))))
    (when (setq target (split-wires e))
      (dolist (z target)
	(setq split (concatenate 'string split 
				 (format nil "<split-wire>~A</split-wire>~%" 
					 (element-tag-xml target t))))))
    (format nil "~A~A~A~A~A~A~A</element>"
	    (element-tag-xml e nil)
	    a
	    b
	    c
	    parent
	    context
	    split)))

(defun element-tag-xml (e close-tag)
  "Returns an xml tag of type <element> describing the given element e (specifying iname and type).  If close-tag is true, adds a final '/' to the end of the tag so a closing </element> is not required."
  (setq e (lookup-element e))
  (format nil "<element iname=\"~A\" type=\"~A\"~A>" 
	  (element-name e) 
	  (element-type-label e)
	  (if (close-tag) "/" "")))
