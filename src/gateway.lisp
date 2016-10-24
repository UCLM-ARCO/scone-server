;;; ***************************************************************************
;;; 
;;;  gateway.lisp
;;; 
;;; 
;;;  Functions that direct what is sent to the Scone server before the request
;;;  is acted on.  This file excludes functions for TCP/IP.  This file includes
;;;  functions for authenication, XML processing, public function definitions,
;;;  and some basic error handling and recovery.  This file also has the 
;;;  function definitions for creating users, user contexts, passwords, etc.
;;; 
;;;  Author: Ben Lambert (benlambert@cmu.edu)

;;; ***************************************************************************


(defvar *user-table* (make-hash-table :test 'equal)
  "A mapping from usernames to passwords")

(defvar *user-context* (new-type {user-context} {general})
  "A parent class for user contexts.")


(defun new-user (username password)
  "Create a new user with the provided password.  This function also creates a Scone context for that user."
  (if (gethash username *user-table*)
      (format t "User already exists~%")
      (let ((new-context (new-indv (read-from-string (format nil "{~A-context}" username)) {user-context})))
	(setf (gethash username *user-table*) (cons password new-context)))))
	
(defun authenticate (username password)
  "Check if the username matches the password."
  (if (or (not username) (not password))
      nil
      (equal (car (gethash username *user-table*)) password)))

(defun get-session-id (dom)
  "Get the session ID from a request to Scone."
  (get-attribute dom :|session|))

(defun get-password (dom)
  "Get the password from the a Request to Scone."
  (get-attribute dom :|password|))

(defun get-function (dom)
  "Get the function to be called.  This function can be called with either the 'apply' or the 'funcall' functions."
  (coerce (read-from-string (symbol-name (get-tagname (get-children dom)))) 'function))

(defun get-function-args (dom)
  "Get the function args to be passed to a function."
  (first (get-children dom))) ; get the children of the first child
  
(defun apply-function (dom)
  "An example function of how the Gateway server will call functions given the parsed XML DOM tree."
  (funcall
   (get-function dom)
   (get-function-args dom)))

(defmacro with-context (context &body body)
  "Switch into the given context, execute the body, then return to the original context."
  `(let ((this-context *context*))
    (in-context ,context)
    (unwind-protect (progn ,@body)
      (in-context this-context))))

(defmacro with-namespace (namespace &body body)
  "Switch into the given namespace, execute the body, then return to the original namespace."
  `(let ((this-namespace *namespace*))
    (in-namespace ,namespace :include (namespace-name this-namespace))
    (unwind-protect (progn ,@body)
      (setf *namespace* this-namespace))))


(defvar *scone-public-functions*
  ())
#|

;; Right now, we're not checking if it's a legal function, so we don't need this yet.

;; This returns something nasty if the function list is a list of the actual functions...
(defun list-public-functions ()
  "Returns a list of all the functions in the public function list."
  (format nil "~A" *scone-public-functions*))

     ;;NOTE: if eval-lisp is public, then *public-functions-only* could be redefined...
;; built-in server functions
;;  "A list of function names that can be called by a client connecting to the SCONE server."
;(defvar *scone-public-functions*
;  (list 
;   #'echo #'disconnect #'kill-server #'list-public-functions #'eval-lisp))

;; Add to the built-in functions, the public event semantics functions and the SCONE viewer functions
;; (setq *scone-public-functions* (concatenate 'list *scone-public-functions*  *scone-viewer-public-functions*))

|#


(defun handle-scone-lisp-request (line)
  "Read the string and send it to the REPL."
  (eval (read-from-string line)))

(defun handle-scone-xml-request (line)
  "Handle each incoming XML command.  This includes: checking if it's valid, evaluating, returning a string, and catching exceptions."
  
  (handler-case  ;;try to catch any error that occurs in this function
      (let* ((xml-dom (s-xml:parse-xml-string line))      ;first parse the XML, then extract the important parts
	     (session-id (get-session-id xml-dom))
	     (password (get-password xml-dom))
	     (the-function (get-function xml-dom))
	     (function-args (get-function-args xml-dom)))

	;; Print what we received to the log file
	(when (< *output-verbosity* 2)
	  (format *log-stream* "Received: ~A~%" line)
	  (finish-output *log-stream*))

	;; Check if we have a valid user
	(unless (authenticate session-id password)
	  (format *log-stream* "Could not authenticate user~%")
	  (format *log-stream* "Received: ~A~%" line)
	  (finish-output *log-stream*)
	  ;;this print can cause an error condition, the function exits at the return-from
	  (format *output-to-client* "<response>Could not authenticate user</response>~%")
	  (disconnect)
	  (return-from handle-scone-xml-request ""))
	
	;; Then switch context and namespace and call the function if it's legal.
	(when (authenticate session-id password)
	  (format *log-stream* "Authenticated user ~a~%" session-id)
	  (finish-output *log-stream*)
       
       ;; for right now, we won't auto-switch into a new context/namespace
       ;(with-context (cdr (gethash session-id *user-table*))
	 ;(with-namespace session-id 
	  (if (or (not *public-functions-only*)
		  (and *public-functions-only* (member the-function *scone-public-functions*)))
	      (if (atom function-args)
		  (funcall the-function)
		  (funcall the-function function-args))
	      (if (and *public-functions-only* (not (member the-function *scone-public-functions*)))
		  (format *output-to-client* "<response>Function not a public function</response>")))
	     
	   ;))
       )
     )
    ;if we catch an error condition, print it, and return
    (error (e) 
	   (progn 
	     (format *log-stream* "~A: Error in (handle-scone-xml-request): ~A~%" (current-time) e)
	     (format *output-to-client* "*****SCONE ERROR*****. CAUGHT EXCEPTION: ~A~%" e)
	     "")))
)


#-(and true-threading sbcl)
(defun handle-scone-request (string)
  "Direct a request to the REPL or the XML parser depending on the variable *use-xml*"
  (if *use-xml*
      (handle-scone-xml-request string)
      (handle-scone-lisp-request string)))

#+(and true-threading sbcl)
(defun handle-scone-request (string)
  "Direct a request to the REPL or the XML parser depending on the variable *use-xml*, but first wait until a lock can be acquired since this may be multithreaded."
  (sb-thread:with-mutex (*scone-lock*)
    (if *use-xml*
	(handle-scone-xml-request string)
	(handle-scone-lisp-request string))))


(defun echo-xml (x)
  "Echo the DOM argument as an XML string."
  (s-xml:print-xml-string x :pretty :t))



;; the default user, at least in all the examples...
(new-user "ABC" "XYZ")

;; Create a bunch of dummy users for testing.
(new-user "a" "a")
(new-user "b" "b")
(new-user "c" "c")
(new-user "d" "d")
(new-user "e" "e")
(new-user "f" "f")
(new-user "g" "g")
(new-user "h" "h")
(new-user "i" "i")
(new-user "j" "j")
(new-user "k" "k")
(new-user "l" "l")




;; Moved from load.lisp


(defvar *demo-last-preloaded-element* nil
  "The last element loaded for the SconeEdit demo setup.  We always roll the
   KB back to this element on demo-reset.")


;;; Reset function for the SconeEdit demo.  When this function is called, 
;;  roll back the KB to the last pre-loaded element (i.e. remove all 
;;  user-added elements) and unregister all the english names from 
;;  *temp-dictionary*
(defun demo-reset ()
  (remove-elements-after *demo-last-preloaded-element*)
  (dolist (entry *temp-dictionary*)
    (let ((english (car entry))
	  (elt     (cdr entry)))
      (unregister-definition elt english)))
  (setq *temp-dictionary* nil)
  (return-from demo-reset t))


(defun reset-demo () ;; An alias 
  (demo-reset))

