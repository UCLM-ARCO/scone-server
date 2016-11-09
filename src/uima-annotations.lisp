;;
;; tokenize.lisp
;;
;;
;; Tokenization for Scone analysis of email texts;
;; lookup Scone elements and keep track of text position (offset, length)
;; for each element found.
;;
;; CHANGES
;;
;; 20 May
;; -Adding quotes around the tag name.  old: <tag name={thing}   new: <tag name="{thing}"
;; -Adding lookup for the role of people in Radar: {speaker} {person-in-charge}, etc.
;; 
;;
;;

;;----------------------------------------------------------------------
;; TOKENIZATION
;;
;; Define a few of token types
(defvar *string-token*  0)  
(defvar *list-sep*      0)  
(defvar *iname-token*   1)  
(defvar *iname-token-end*  2)
(defvar *whspace-token* 3)   
(defvar *english-name*  4)   
(defvar *english-name-end* 5)
(defvar *pos-tag*       6)   
(defvar *pos-tag-end*   7)   
(defvar *integer-token* 8)   
(defvar *var-token*     9)

;; Punctuation 
;;
(defvar *punctuation* (make-hash-table :test #'equal))
(setf (gethash "'" *punctuation*) "*apostrophe*")
(setf (gethash "." *punctuation*) "*period*")
(setf (gethash "?" *punctuation*) "*quest*")
(setf (gethash "," *punctuation*) "*comma*")
(setf (gethash "(" *punctuation*) "*oparen*")
(setf (gethash ")" *punctuation*) "*cparen*")


;; Tokens are structured with an id, type, and offset: (id . type)
;;
(defun make-token (id type offset)
  (declare (ignore type))
  (list (list (string-right-trim '(#\Newline) id) offset (length id))))
(defun token-id (tok) (first tok))
(defun token-type (tok) (second tok))
(defun token-p (tok) 
  (let ((id (token-id tok)) (type (token-type tok)))
    (and (or (stringp id) 
	     (symbolp id) 
	     (numberp id))
	 (numberp type))))


;; Scone tokens have an id, an offset, and a length.  They 
;; must correspond to Scone elements, otherwise make-scone-token
;; returns NIL rather than tokens.
;;
;; NEW: keyword arg ALLOW-DUPLICATES controls whether multiple meanings
;;  are returned for ambiguous words.  
;;  :allow-duplicates :none  -- returns NIL if there are multiple meanings.
;;  :allow-duplicates nil    -- returns the first only if there are multiple meanings.
;;  :allow-duplicates t      -- returns all if there are multiple meanings.
;;
(defun make-scone-token (id type offset &key (allow-duplicates nil))
  "Rather than returning a token type, return any Scone elements 
   associated with ID along with the offset and length of this 
   piece of text."
  (declare (ignore type))
  (let ((defs (lookup-definitions id))
        (len  (length id))
	(return-value nil))
    (when defs
      (cond ((null allow-duplicates)    ;; ALLOW-DUPLICATES is NIL.
             (setf defs (first defs))
             (setf return-value 
                   (list (list (first defs) offset len))))
            ((eq allow-duplicates ':none)     ;; ALLOW-DUPLICATES is :none                 
             (unless (> (length defs) 1)
               (setf return-value 
                     (list (list (first (first defs)) offset len)))))
            (t                               ;; ALLOW-DUPLICATES is 't                  
             (dolist (def defs)
               (push (list (first def) offset len) return-value)))))
    (return-from make-scone-token (nreverse return-value)))) ;; this shouldn't be "return-from"???


(defun token-offset (tok) (third tok))
(defun scone-token-p (tok) 
  (let ((id (token-id tok)) (type (token-type tok)) (offset (token-offset tok)))
    (and (or (stringp id) 
	     (symbolp id) 
	     (numberp id))
	 (numberp type)
	 (numberp offset))))


;; Repeated from makedict.lisp, to prevent errors when loading separately.
;;
(defun unmark-frequent-ancestors (m)
  "Examine the nodes marked with M, and remove top-level
   nodes that are uninformative.  Specifically: 
    If {person} is marked, unmark all ancestors of {person}.
    If {building} is marked, unmark all ancestors of {building}.
    If {room} is marked, unmark all ancestors of {room}.
    If {event} is marked, unmark all ancestors of {event}.
  "
  (with-temp-markers (m1)
                     (when (marker-on? {person} m)
                       (upscan {person} m1)
                       (unmark {person} m1)
                       (do-marked (e m1) (if (marker-on? e m) (unmark e m))))
                     (when (marker-on? {room} m)
                       (upscan {person} m1)
                       (unmark {person} m1)
                       (do-marked (e m1) (if (marker-on? e m) (unmark e m))))
                     (when (marker-on? {building} m)
                       (upscan {person} m1)
                       (unmark {person} m1)
                       (do-marked (e m1) (if (marker-on? e m) (unmark e m))))
                     (when (marker-on? {event} m)
                       (upscan {person} m1)
                       (unmark {person} m1)
                       (do-marked (e m1) (if (marker-on? e m) (unmark e m))))))



;; Scone tokens have an id, an offset, and a length.  They 
;; must correspond to Scone elements, otherwise make-scone-token
;; returns NIL rather than tokens.
;;
(defun make-scone-token-bak (id type offset)
  "Rather than returning a token type, return any Scone elements 
   associated with ID along with the offset and length of this 
   piece of text."
  (declare (ignore type))
  (let ((defs (lookup-definitions id))
	(len  (length id))
	(return-value nil))
    (cond (defs
	    (dolist (def defs)
	      (setf return-value (append return-value (list (first def) offset len))))
	    (setf return-value (list return-value))))
     return-value))






;;
;;
(defun split-by-one-space (string)
  "Returns a list of substrings of string 
   divided by ONE space each. 
   Note: Two consecutive spaces will be seen as
   if there were an empty string between them."
  (loop for i = 0 then (1+ j)
    as j = (position #\Space string :start i)
    collect (subseq string i j)
    while j))



;; tokenize-recursive
;;
;;(defun tokenize-recursive (nextchar currentword rem state offset 
;;                                    &key (allow-duplicates nil))
(defun tokenize-recursive (nextchar currentword rem state offset &key (allow-duplicates nil))
  "Convert a string into a list of tokens recursively.  
   NEXTCHAR is the lookahead character, 
   CURRENTWORD is the token we're currently building,
   REM is the remainder of the string we're tokenizing, after the current word
   STATE determines what to do with NEXTCHAR and CURRENTWORD.
   OFFSET is the current offset for the beginning of this token in the string.
  "
  ;;(format t "TOKENIZE-RECURSIVE \"~A\" \"~A\" \"~A\" ~A ~A~%" nextchar currentword rem state offset)
  (cond ((equal rem "")
	 ;; 
	 ;; We completed a token.
	 (cond ((equal currentword "") nil)
	       (t
		(if (or (eq state *english-name-end*) 
			(eq state *pos-tag-end*) 
			(eq state *iname-token-end*)) 
		    (setq state (- state 1)))
		(cond ((or (equal nextchar "}") (equal nextchar " ") (equal nextchar "]"))
		       (make-scone-token currentword 
					 state 
					 (- offset (length currentword))
                                         :allow-duplicates allow-duplicates))
		      (t (make-scone-token (format nil "~a~a" currentword nextchar) 
					   state 
					   (- offset (length currentword))
                                           :allow-duplicates allow-duplicates))))))
	(t
	 (cond 
	  ;; Starting a new element iname: state = 1 and continue
	  ((equal nextchar "{")
	   (tokenize-recursive (subseq rem 0 1)
			  ""
			  (subseq rem 1)
			  *iname-token*
			  (+ 1 offset)
                          :allow-duplicates allow-duplicates))
	  ;; Ending an element iname: change state to reflect that.
	  ((equal nextchar "}")
	   (tokenize-recursive (subseq rem 0 1) 
			  currentword
			  (subseq rem 1)
			  *iname-token-end*
			  (+ 1 offset)
                          :allow-duplicates allow-duplicates))
	  ;; Starting an english name
	  ((equal nextchar "[")
	   (tokenize-recursive (subseq rem 0 1)
			  ""
			  (subseq rem 1)
			  *english-name*
			  (+ 1 offset)
                          :allow-duplicates allow-duplicates))
	  ;; Ending an english name
	  ((equal nextchar "]")
	   (cond ((eq state *english-name*)
		  (tokenize-recursive (subseq rem 0 1) 
				 currentword
				 (subseq rem 1)
				 *english-name-end*
				 (+ 1 offset)
                                 :allow-duplicates allow-duplicates))
		 ((eq state *pos-tag*)
		  (tokenize-recursive (subseq rem 0 1) 
				 currentword
				 (subseq rem 1)
				 *pos-tag-end*
				 (+ 1 offset)
                                 :allow-duplicates allow-duplicates))))
	  ;; Starting a POS tag
	  ((equal nextchar ":")
	   (tokenize-recursive (subseq rem 0 1)
			  (format nil "~a~a" currentword nextchar)
			  (subseq rem 1)
			  *pos-tag*
			  (+ 1 offset)
                          :allow-duplicates allow-duplicates))
	  ;; Commas are list separators and they must end a token 
	  ((gethash nextchar *punctuation*)
	   (when (or (eq state *english-name-end*)
		     (eq state *iname-token-end*)
		     (eq state *pos-tag-end*)) 
	     (setf state (- state 1)))
	   (append (append (make-scone-token currentword 
					     state 
					     (- offset (length currentword))
                                             :allow-duplicates allow-duplicates)
			   (make-scone-token (gethash nextchar *punctuation*)
					     *list-sep* 
					     offset
                                             :allow-duplicates allow-duplicates))
		   (tokenize-recursive (subseq rem 0 1) 
				  "" 
				  (subseq rem 1) 
				  *string-token* 
				  (+ 1 offset)
                                  :allow-duplicates allow-duplicates)))
	  ;; If we're at the end of a word
	  ((equal nextchar " ")
	   (cond 
	    ((or (eq state *pos-tag-end*) (eq state *string-token*) 
		 (eq state *iname-token-end*) (eq state *english-name-end*))
	     ;;
	     ;;  Just completed a token. Return it and keep going.
	     (unless (eq state *string-token*) (setf state (- state 1)))
	     (cond ((equal currentword "")
		    (tokenize-recursive (subseq rem 0 1) 
                                        "" 
                                        (subseq rem 1) 
                                        *string-token* 
                                        (+ 1 offset)
                                        :allow-duplicates allow-duplicates))
		   (t
		    (append (make-scone-token currentword 
                                              state 
                                              (- offset (length currentword))
                                              :allow-duplicates allow-duplicates)
			    (tokenize-recursive (subseq rem 0 1) 
                                                "" 
                                                (subseq rem 1) 
                                                *string-token* 
                                                (+ 1 offset)
                                                :allow-duplicates allow-duplicates)))))
	    ((or (eq state *iname-token*) (eq state *english-name*))
	     ;; In the middle of an iname
	     (tokenize-recursive (subseq rem 0 1)
                                 (format nil "~a~a" currentword nextchar)
                                 (subseq rem 1)
                                 state
                                 offset
                                 :allow-duplicates allow-duplicates))))
	  ;; Otherwise we're in the middle of a word, keep building it
	  (t
	   (tokenize-recursive (subseq rem 0 1) 
                               (format nil "~a~a" currentword nextchar)
                               (subseq rem 1)
                               state
                               (+ 1 offset)
                               :allow-duplicates allow-duplicates))))))

;; tokenize
;;
(defun tokenize (string &key (offset -1) (allow-duplicates nil))
  "Convert a string into a stream of tokens: inames or words.
   Calls tokenize-recursive with proper args. 
   Assume that the starting offset is 0 unless OFFSET is given.
  "
  (tokenize-recursive "" "" string *string-token* offset :allow-duplicates allow-duplicates))


;; Parse-body-line
;; New: 
;; If ANCESTORS is :FILTERED, print all ancestors that pass through
;; the filter (unmark-frequent-ancestors).
;; 20 May 
;; when we print out a {person}, also print that person's position, 
;; if possible:  {speaker}, {person-in-charge}, {radar-user}
;;
;; If ANCESTORS is :immediate, print the parent of each node.
;; If ANCESTORS is :filtered, print ancestors subject to a filter for 
;; non-informative supertypes.
;; If ANCESTORS is t, print all ancestors of each node.
;; If ANCESTORS is nil, don't print any ancestors.  
;; 
;; Return a list of the tags found in this line.
;;
(defun parse-body-line (line &key 
                             (start-position 0)
                             (ancestors nil)
                             (nest-structure t)
                             (allow-duplicates nil)
                             (unique-attributes nil))
  "Parse the string LINE and store any resulting annotations to TAG-LIST.
   Return the tag list.  Used to return the offset, but this can
   easily be calculated in the function that calls parse-body-line.
  "
  ;; Ignore nesting parameter for now
  (declare (ignore nest-structure))
  ;;(declare (ignore unique-attributes))

  ;; For each line in the file, there may be a list of scone tags
  ;;
  (let ((tag-name nil) (tag-offset nil)	(tag-length nil) (scone-type nil) (tag-list nil))
    (dolist (scone-tag (tokenize line :allow-duplicates allow-duplicates))
      (setf tag-name (first scone-tag))
      (setf tag-offset (+ start-position (second scone-tag)))
      (setf tag-length (third scone-tag))
      (setf scone-type "scone-element")
      ;;
      ;; Format each one with XML: name, offset, length
      (push (format nil "<tag name=\"~A\" offset=\"~A\" length=\"~A\" "
                    tag-name
                    tag-offset
                    tag-length)
            tag-list)
      ;; Get the element type
      (if (is-x-a-y? (lookup-element tag-name) {action})
	  (setf scone-type "scone-action"))
      (push (format nil "type=\"~A\">~%" scone-type) tag-list)
      ;; Print the element iname as a tag attribute
      (if unique-attributes
          (push (format nil
                        "     <tag-attr name=\"element-iname ~A\" value=\"True\" />~%"
                        tag-name)
                tag-list)
        (push (format nil
                      "     <tag-attr name=\"element-iname\" value=\"~A\" />~%"
                      tag-name)
              tag-list))
      
      ;; ANCESTORS
      (when ancestors
        (if (eq ancestors ':immediate)
            ;; If ANCESTORS is :immediate, 
            ;; only print the parent of the element as a tag attribute
            (if unique-attributes
                (push (format nil 
                              "     <tag-attr name=\"parent-iname ~A\" value=\"True\" />~%"
                              (parent-wire (lookup-element tag-name)))
                      tag-list)
              (push (format nil 
                            "     <tag-attr name=\"parent-iname\" value=\"~A\" />~%"
                            (parent-wire (lookup-element tag-name)))
                    tag-list))
          ;; Otherwise upscan, possibly filter.
          (with-temp-markers (m1)
                             (upscan tag-name m1)
                             (unmark tag-name m1)
                             (when (eq ancestors ':filtered)
                               (unmark-frequent-ancestors m1))
                             (do-marked (e m1)
                                        (if unique-attributes
                                            (push (format nil
                                                          "     <tag-attr name=\"ancestor-iname ~A\" value=\"True\" />~%"
                                                          e)
                                                  tag-list)
                                          (push (format nil
                                                        "     <tag-attr name=\"ancestor-iname\" value=\"~A\" />~%"
                                                        e)
                                                tag-list))))))
      ;; If the element is a {person}, print the radar-role of the person:
      ;; {speaker}, {person-in-charge}
      (when (is-x-a-y? tag-name {person})
        (if (is-x-a-y? tag-name {speaker})
            (push (format nil
                          "     <tag-attr name=\"conference-speaker\" value=\"True\" />~%")
                  tag-list))
        (if (is-x-a-y? tag-name {person in charge})
            (push (format nil
                          "     <tag-attr name=\"conference-organizer\" value=\"True\" />~%") 
                  tag-list)))
      ;; Print the end tag
      (push (format nil "</tag>~%") tag-list))
    ;; Return the tag list
    (nreverse tag-list)))

;; Tokenize-email-file
;; 
(defun tokenize-email-file (file &key (ancestors :immediate) 
                                      (allow-duplicates nil)
                                      (print-to-file nil)
                                      (unique-attributes nil))
  "Read in the example email file line by line.
   For each line in the body, find any Scone tags and create a 
   line of annotation for the tag.  After reading an entire body, 
   print out the Scone tags as xml annotations.  
   If ANCESTORS is non-nil, print all the ancestors for any 
   Scone concept found.  Otherwise, print parents only.
   NEW: if ANCESTORS is :FILTERED, print all ancestors that 
   pass through the filter (unmark-frequent-ancestors).
  
   Results are printed to \"FILE.scone-annotated\"

   NOTE: this is *not* a real XML parser, so if the file format
   changes then this code won't necessarily work.  Just a patch-up
   for labeling the example emails in example-emails.xml
  "
  (with-open-file 
   (ofile 
    (if print-to-file (format nil "~A" print-to-file)
      (format nil "~A.scone-annotated" file))
    :direction :output)
   (with-open-file (ifile file :direction :input)
		   ;; Keep track of where we are in the file.  
		   ;; SCONE-TAGS keeps the list of tags for the 
		   ;; current email body.
		   (let ((id nil)
			 (body-start nil)
			 (body-end nil)
			 (in-body nil)
			 (pos 0)
			 (scone-tags nil)
			 (ln 0))
		     ;;
		     ;; Process the next line
		     (do ((line (read-line ifile nil '%eof%)))
			 ((eq line '%eof%))
		       (setf id (search "<email id=" line))
		       (setf body-start (search "<body><![CDATA[" line))
		       (setf body-end (search "</body>" line))
		       (setf ln (length line))
		       ;;
		       ;; Check for lines that indicate a new email number.
		       (cond (id
			      (format ofile "~A~%" line)
			      (setf id 
				    (string-trim "<email id=" 
						 (string-trim ">" (subseq line id)))))
			     ;;
			     ;; Lines that indicate a new email body.
			     (body-start
			      (format ofile "~A~%" line)
			      (setf line (subseq line body-start))
			      (setf in-body t)
			      (setf pos 0))
			     ;;
			     ;; Lines that indicate an email body end.
			     (body-end 
			      (setf line (subseq line 0 body-end))
			      (format ofile "~A</body>~%" line)
			      (let ((tags (parse-body-line line 
                                                           :start-position pos 
                                                           :ancestors ancestors
                                                           :allow-duplicates allow-duplicates
                                                           :unique-attributes unique-attributes)))
				(if tags (setf scone-tags (append tags scone-tags))))
                              ;;
			      ;; Print out the current tags for this body to the scone-annotated file.
			      (format ofile "<annotations>~%")
			      (dolist (tag scone-tags)
				(format ofile tag))
			      (format ofile "</annotations>~%</email>~%")
			      (setf scone-tags nil)
			      (setf in-body nil))
			     ;; All other lines
			     (t
			      (format ofile "~A~%" line)))
		       ;;
		       ;; If this is a line from the body of an email, tokenize it
		       (cond (in-body
			      (setf pos (+ ln pos))
			      (let ((tags (parse-body-line line 
                                                           :start-position pos 
                                                           :ancestors ancestors
                                                           :allow-duplicates allow-duplicates
                                                           :unique-attributes unique-attributes)))
				(if tags (setf scone-tags (append scone-tags tags))))
                              (setf line (read-line ifile nil '%eof%)))
			     (t
			      (setf line (read-line ifile nil '%eof%)))))))))


;; Tokenize-file
;; 
(defun tokenize-file (file &key (ancestors nil) (allow-duplicates nil))
  "Read in a text file (NOT XML) line by line and return the Scone elements
   from each line.  This is a testing function for development of tokenize-email-file.
  "
  (let ((pos 0)
        (tags nil))
    (with-open-file 
     (ofile (format nil "~A.scone-annotated" file) :direction :output)
     (with-open-file (ifile file :direction :input)
                     (do ((line (read-line ifile nil '%eof%)))
                         ((eq line '%eof%))
                       (setf tags (append tags (parse-body-line line 
                                                                :start-position pos 
                                                                :ancestors ancestors
                                                                :allow-duplicates allow-duplicates)))
                       ;; Print the line itself
                       (format ofile "<line>~A</line>~%" line)
                       ;; Print out the current tags for this line.
                       (format ofile "<annotations>~%")
                       (dolist (tag tags) (format ofile tag))
                       (format ofile "</annotations>~%~%")
                       (setf tags nil)
                       (setf line (read-line ifile nil '%eof%)))))))



(defun tokenize-run-all-parameters (infile &key(outfile nil))
  "Run (tokenize-email-file) with several paremeter variations.
   Used to generate files for experiments with the classifier."
  (unless outfile (setf outfile infile))
  (dolist (ancestors '(nil t :filtered :immediate))
    (dolist (allow-duplicates '(nil t :none))
      (let ((filename 
             (format nil "~A.ancestors-~A.duplicates-~A.xml"
                     outfile ancestors allow-duplicates)))
        (format t "Generating ~A~%" filename)
        (tokenize-email-file infile 
                             :ancestors ancestors 
                             :allow-duplicates allow-duplicates
                             :print-to-file filename
                             :unique-attributes t)))))
                              

(defclass annotation ()
  ((name
    :initarg :name
    :initform (error "Name required for an annotation")
    :reader annot-name
    :documentation "The iname of the annotation.")
   (offset
    :initarg :offset
    :initform (error "Offset required for an annotation")
    :reader annot-offset
    :documentation "The offset from the beginning of the given string that this annotation occurs.")
   (length
    :initarg :length
    :initform  (error "Length required for an annotation")
    :reader annot-length
    :documentation "The length of the span that this annotation covers.")
   (type
    :initarg :type
    :initform NIL
    :reader annot-type
    :documentation "The annotation type, this may be null.")))

(defgeneric print-annotation (annotation))

(defmethod print-annotation (annotation)
  (format t "Name:   ~A~%" (annot-name annotation))
  (format t "Offset: ~A~%" (annot-offset annotation))
  (format t "Length: ~A~%" (annot-length annotation))
  (format t "Type:   ~A~%" (annot-type annotation)))

(defun unmark-linguistic-concepts (m)
  ;(when (lookup-element-pred {LingThing})
  (when (lookup-element-pred {EnglishWord})
    (with-temp-markers (m1)
      (downscan {EnglishWord} m1)
      (unmark-boolean m (list m1) '()))))


(defun get-pruned-ancestors (e)
  "Use Alicia's algorithm to get a pruned set of ancestors."
    (with-temp-markers (m1)
      (upscan e m1)
      (unmark e m1)
      (unmark-frequent-ancestors m1)
      (unmark-linguistic-concepts m1)
      (list-marked m1)))

(defun uima-annot->xml (annot)
  "Convert a UIMA annotation into an XML string."
  (format nil "<scone-element value='~A' offset='~A' length='~A' type='~A'/>"
	  (annot-name annot)
	  (annot-offset annot)
	  (annot-length annot)
	  (annot-type annot)))

(defun uima-annots->xml (annot-list)
  (reduce 
   #'(lambda (x y) (concatenate 'string x y)) 
   (concatenate 'list 
		'("<annotations>")
		(map 'list #'uima-annot->xml annot-list)
		'("</annotations>"))))

(defun get-uima-annotations (line &key 
				  (start-position 0)
				  (allow-duplicates nil)
				  (annotation-types nil))
  "A modified version of Alicia's original (Parse-body-line)
   This function returns a list of elements found in the string as well as
   some of the elements that are related to those elements, such as
   the element's parents.  This doesn't fully emulate (parse-body-line) yet.
   The list returned contains elements that look like this:
   '({element iame} offset length type) where type could be 'iname' or 'parent'."

  ;; For each line in the file, there may be a list of scone tags
  (let ((tag-name nil)
	(tag-offset nil)
	(tag-length nil)
	;(scone-type nil)
	(tag-list nil))
    (dolist (scone-tag (tokenize line :allow-duplicates allow-duplicates))
      (setf tag-name (first scone-tag))
      (setf tag-offset (+ start-position (second scone-tag)))
      (setf tag-length (third scone-tag))
     
      ;; put an annotation for the element itself on the list
      (if (or (not annotation-types) (find 'INAME annotation-types))
      (push
       (make-instance 'annotation :name tag-name :offset tag-offset :length tag-length :type "iname")
       tag-list))
      
      ;; Next, we need to get related elements

      ;;first, let's get all the immediate parents
      (if (or (not annotation-types) (find 'PARENT annotation-types))
      (dolist (parent (list-parents tag-name))
	(push
	 (make-instance 'annotation :name parent :offset tag-offset :length tag-length :type "parent")
	 tag-list)))

      ;; Next, we would want to add other related elements.  We probably want to have a list of relations as input to this function.
      
      (if (or (not  annotation-types) (find 'ANCESTOR annotation-types))
      (dolist (e (get-pruned-ancestors tag-name))
	(push
	 (make-instance 'annotation :name e :offset tag-offset :length tag-length :type "ancestor")
	 tag-list)))

      (when (is-x-a-y? tag-name {person})
        (if (is-x-a-y? tag-name {speaker})
	    (if (or (not annotation-types) (find 'CONFERENCE-SPEAKER annotation-types))
		(push
		 (make-instance 'annotation :name tag-name :offset tag-offset :length tag-length :type "conference-speaker")
		 tag-list)))
        (if (is-x-a-y? tag-name {person in charge})
	    (if (or (not annotation-types) (find 'CONFERENCE-ORGANIZER annotation-types))
	    (push
	     (make-instance 'annotation :name tag-name :offset tag-offset :length tag-length :type "conference-organizer")
	     tag-list)))))
    tag-list))



(defun get-uima-annotations-xml (line)
  "Get the XML annotations from a string."
  (uima-annots->xml (get-uima-annotations line)))

(defun get-uima-annotations-from-xml (xml)
  "Get the XML annotations from a parse XML structure."
  
  (let* ((type-string (first (get-children (get-child xml :|types|))))
	 (types (if type-string
		    (read-from-string type-string)
		    NIL))
	 (line (first (get-children (get-child (first (get-children (get-child xml :|sentences|))) :|text|))))
	 (tokens-struct (get-children (get-child (first (get-children (get-child xml :|sentences|))) :|tokens|)))
	 (non-pos-annot (if types 
			    (get-uima-annotations line :annotation-types types)
			    (get-uima-annotations line)))
	 (pos-annot '()))

#|
    (dolist (token tokens-struct)
      
      ;;for some reason using let* breaks this.
      (let ((token (car (get-children (get-child token :|text|))))
	    (lemma (car (get-children (get-child token :|lemma|))))
	    (pos (car (get-children (get-child token :|POS|))))
	    (offset (car (get-children (get-child token :|offset|))))
	    (length (car (get-children (get-child token :|length|)))))
        (let ((pos-definitions (lookup-definitions token (list (read-from-string (format nil ":~A" pos))))))
	  ;; not using lemmas yet.
					;(lemma-definitions (lookup-definitions lemma (list (read-from-string (format nil ":~A" pos))))))
          
          (format t "token: ~A, lemma: ~A, pos: ~A, offset: ~A, length: ~A~%" token lemma pos offset length)
	  
	  (if (> (length pos-definitions) 0)
	      (push (make-instance 'annotation :name (caar pos-definitions) :offset offset :length length :type "pos-iname") pos-ann
		    ot))
	  
	  (if (> (length pos-definitions) 1)
	      (format *error-output* "Arbitrarily choosing the first word sense~%"))
	  
	  ))) 
|#
    (uima-annots->xml (append non-pos-annot pos-annot))))
