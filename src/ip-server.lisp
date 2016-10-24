
;;; ***************************************************************************
;;; ip-server.lisp
;;; ***************************************************************************
;;; Author: Ben Lambert
;;;
;;;
;;; 
#|
This code handles TCP/IP communication between Scone and remote clients.
This currently works on CMUCL, SBCL, ACL, however, multi-threading support
is only available on SBCL (where :sb-threads has specifically been compiled in.
|#
;;;
;;;
;;; TABLE OF CONTENTS
;;;
;;;
;;; ***************************************************************************

;;; Use normal compilation policy, where safety counts as much as speed.
(declaim (optimize (speed 1) (space 1) (safety 1)))

;;??
#|
(setq cl::*debugger-hook*
      (lambda (condition previous)
	(declare (ignore previous))
	(format *error-output* "~%Error condition encountered: ~A~%" condition)
	(invoke-restart (first (last (compute-restarts))))))
|#

;;; ========================================================================
;;; Special variables needed by the server

;;; These include things like the current port number, and the current
;;; number of connected clients.


#+sbcl
(defvar *error-output-ref* nil
  "A reference to *error-output* since that gets redefined when we make a new thread.")
#+sbcl
(defvar *standard-output-ref* nil
  "A reference to *standard-output* since that gets redefined when we make a new thread.")
#+sbcl
(defvar *trace-output-ref* nil
  "A reference to *trace-output* (??) since that gets redefined when we make a new thread(??).")

(defvar *client-count* 0
  "This is the number of clients that are currently connected.")

(defvar *connection-id* 0
  "Keep a running counter and use this to identify threads") 

(defvar *this-socket* nil
  "This is the actual socket we're using.  However, we wrap it in a file stream so it's easier to use.")

(defvar *server-stream* nil
  "Character-stream communication to/from the server socket ends up looking
   like stream I/O within the Lisp.  This is the stream.")

;; Although *server-stream* will be a bidirectional stream, for the sake of redirecting
;; inputs and output to log files and to the terminal, we will use one input stream for
;; receiving from the client and another output stream for sending to the client. 
(defvar *output-to-client* nil)
(defvar *input-from-client* nil)

;; In this version, we're just going to send the log output to stdout,
;; the script that starts this will direct that into a log file.
(defvar *log-stream* *standard-output*
  "Use this stream to record server logging messages.")

;; Use this stream to report errors.  By sending it to stdout, the error will appear in the log,
;; also send it to stderr in case the use is monitoring the shell where the process was started.
;;(setf *error-output* (make-broadcast-stream system:*stderr* *log-stream*))

;; Even with detachtty, we got an error writing to stderr.  Try not using it at all
(setf *error-output*  *log-stream*)

;; NOTE: We have to make sure that the Lisp doesn't crash after the terminal window is closed, so send the error stream to the log file.

;; Global variables used to close a connection and to kill the server.
(defvar *terminate-server* nil
  "Set to t to stop accepting new connections after the next time through the inner loop.  If a connections is active, *disconnect* must be set to t also.")

(defvar *disconnect* nil
  "Set to t to disconnect the current connection and start listening for another.")

(defvar *prompt* "[PROMPT]~%")


#+sbcl
(require :sb-bsd-sockets)


#+sbcl
(defparameter *server-address* nil )


;;could this have been related to the "too many open files" error 24 "accept" problem??  
(defparameter *default-server-backlog* 1024            
  "The default number of simultaneous (incomplete?) connections to the server. (see tcp_max_syn_backlog)")

#+(and true-threading sbcl)
(defvar *scone-lock* (sb-thread:make-mutex :name "scone lock")
  "A lock to prevent two threads from using Scone at the same time.")

;; The Lisp process receives a SIGPIPE signal when it tries to read from a closed socket, we are supposed to ignore the signal and wait until we read an EOF from the stream.
#+sbcl
(sb-sys:ignore-interrupt sb-unix:SIGPIPE)

#+cmu
(system:ignore-interrupt unix:SIGPIPE)


;;; ========================================================================
;;; General, but necessary, functions needed by the server

(defun current-time ()
  "Create a human-readable string representing the current time"
  (multiple-value-bind (sec min hr d m y dow dst zone)
		       (get-decoded-time)
    (declare (ignore dow dst zone))
    (format nil
	    "~A/~A/~A ~A:~A:~A"
	    m d y
	    hr min sec)))

(defun kill-server ()
  "Disconnect any current connection and stop listening for new connections."
  (setf *disconnect* t)
  (setf *terminate-server* t)
  "")

(defun disconnect ()
  "Disconnect the current connection and listen for a new connection."
  (setf *disconnect* t)
  "")

;;; Just to suppress a spurious warning.
(defun handle-scone-request (request)
  (declare (ignore request))
  (error "(handle-scone-request) not defined")
  nil)

;;; ========================================================================
;;; Connection setup routines for CMUCL, and ACL

;;; These are different in each of ACL, SBCL, and CMUCL

#+allegro
(defun open-socket (port)
  ;; Start listening on the specified port.  If another process is using this port we get a socket-error.
  ;; TODO - catch this error, print something, and quit
  (setf *this-socket* (acl-socket:make-socket :local-port port :connect :passive))

  ;; Wait until a client connects, then encapsulate the socket with a bidirectional stream
  (setf *server-stream*
    (acl-socket:accept-connection *this-socket*)))

#+cmu
(defun open-socket (port)

  (trace close close-socket) (untrace) ;; this is necessary for some reason
  
  ;; Start listening on the specified port.  If another process is using this port we'll get a socket-error.
  (handler-case
   (setf *this-socket* (create-inet-listener port :stream :reuse-address t :backlog *default-server-backlog*))
   (extensions::socket-error (e)
			     (progn
			       (format *log-stream* "~A: Error encountered.  There may be another process using the specified port~%~A" (current-time) e)
			       (finish-output *log-stream*)
			       (quit))))

  ;; Wait until a client connects, then encapsulate the socket with a bidirectional stream
  (handler-case
   (setf *server-stream*
	 (when (sys:wait-until-fd-usable *this-socket* :input)
	   (sys:make-fd-stream (accept-tcp-connection *this-socket*)
			       :buffering :full ;;vs. line or none
			       :input t
			       :output t
			       :element-type 'character )))
   (error (e)
	  (progn
	    (format *log-stream* "~A: Error occurred while attempting to open a stream with the client: ~A~%" (current-time) e)
	    (finish-output *log-stream*)
	    (quit)))))

#+(or cmu allegro)
(defun setup-connection (port)
  "Wait for a client to connect and set up relevant streams."

  (setf *prompt* "[PROMPT]~%")
  
  ;; open up the socket, this function is platform-dependent
  (open-socket port)

  ;; If *output-verbosity* is set to 2, send all input and output to stdout and thus the log.
  (cond
    ((>= *output-verbosity* 2)
     (setf *output-to-client* (make-broadcast-stream *server-stream* *log-stream*))
     (setf *input-from-client* (make-echo-stream *server-stream* *log-stream*)))
    (t
     (setf *output-to-client* *server-stream*)
     (setf *input-from-client* *server-stream*)))

  ;; TODO - move these elsewhere???

  ;; If the client wants to see the output from show- functions, we need to direct this stream to them -- v6 only !?
  (setf *show-stream* *output-to-client*)
  
  ;; Not sure if the client wants to see this.  Send it to them for now.
  (setf *commentary-stream* *output-to-client*)
)

;;; ========================================================================
;;; Inner session loop

;;; This function implements the basic protocol.  All Lisp implementations
;;; share this function.  

(defun inner-session-loop ()
  (loop while (not *disconnect*) do
	;; Create a scope with a couple variables just to make this part easier to read
	(let ((request nil)
	      (response nil))
	  
	  ;; Print the prompt to the client so they know we're listening
	  (handler-case
	      (progn
		(unless (or *terminate-server* *disconnect*)
		  (format *output-to-client* *prompt*)
		  (force-output *output-to-client*)))
	    (error (e)  ;; in case we can't send the string thru the socket
		   (progn
		     (format *log-stream* "~A: Error writing prompt to socket. Disconnecting. Error:~A~%" (current-time) e)
		     (disconnect))))
	  
	  ;; Try to read a line from the client
	  (handler-case 
	      (setf request (read-line *input-from-client*))
	    (end-of-file (e)  ;; we get EOF when the connection is closed.
			 (declare (ignore e))
			 (progn
			   (format *log-stream* "~A: Read EOF from client stream. Attempting to disconnect and continue.~%" (current-time))
			   (force-output *log-stream*)
			   (disconnect)));; call (disconnect) to get out of the inner loop

	    #+sbcl
	    (sb-int:simple-stream-error (e)
					(declare (ignore e))
					(progn  ;; This error happens a lot, and isn't an error, so don't bother printing a message.
					  ;(format *log-stream* "Simple stream error.  This usually means the client has closed the socket without calling disconnect, or closed it before we could.  Disconnecting.~%")
					  (disconnect)))

	    (error (e)  ;; if another error happens reading from the socket, catch it!
		   (progn
		     (format *log-stream* "~A: Unknown error occurred reading from the client stream.  Trying to continue.  If the server fails to continue accepting connections, please report this error: ~A~%" (current-time) e)
		     (finish-output *log-stream*))))
	  
	  ;; if the request is null, then 
	  (when (not request)
	    (format *log-stream* "~A: Request is null.  Disconnecting.~%" (current-time))
	    (disconnect))

	  ;; Now try to handle whatever was sent to us.
	  ;; If we catch any errors, send them back to the client
	  (unless *disconnect*
	    (progn
	      (handler-case
		  (progn
		    (finish-output *log-stream*)
		    (setf response (handle-scone-request request)) ;; this is the call into the rest of the system (gateway.lisp)
		    (finish-output *log-stream*)) 
		(error (e) ;; this may be a syntax error in what was sent to us
		       (progn
			 (format *log-stream* "~A: *****SCONE-ERROR***** Received:~A. Error: ~A~%" (current-time) request e) (finish-output *log-stream*)
			 (handler-case  ;; since we're reporting this over the socket, need to catch any errors.
			     (format *output-to-client* "*****SCONE-ERROR***** Received:~A.  Error:~A~%" request e)
			   (error (e) (format *log-stream* "~A: ERROR - Unable to send last error message to client: ~A~%" (current-time) e)))
			 (finish-output *log-stream*))))
	      
	      ;; Make a checkpoint file every time....???
	      ;(if *log-kb*
	;	  (log-kb))
	      ))
	  	  
	  ;; Now send our response back to the client		     
	  (unless (or *terminate-server* *disconnect*)
	    (handler-case
		(progn
		  (format *output-to-client* "~A~%" response)
		  (force-output *output-to-client*))
	      (error (e)  ;; if we have trouble writing back through the socket, catch the error
		     (progn
		       (format *log-stream* "~A: Error sending response back to client.  Ignoring error and continuing. Error: ~A~%" (current-time) e)
		       (finish-output *log-stream*))))))))


;;; ========================================================================
;;; "Outer" server loops.

;;; These functions help to set up new connections as they come in, and
;;; loop over connections rather than over individual requests.

;; maybe this should be called "handle-client" to be consistent?
#+(or cmu allegro)
(defun server-loop (&optional (port *default-port*))

  (setf *terminate-server* nil)
  (setf *disconnect* nil)
  
  ;; While *terminate-server* is false, start listening on the socket after the connection is closed
  (loop while (not *terminate-server*) do 
	(progn
	  
	  ;; Setup the socket, listen for a connection, then encapsulate the socket in a stream.
	  (setup-connection port)
	  (format *log-stream* "~A Connected on port ~A~%" (current-time) port)	  
	  (inner-session-loop)
	  (format *log-stream* "~A Disconnecting~%" (current-time))
 
	  ;; if we get to this point, the client either:
	  ;; 1.  called disconnect (and the inner loop stopped looping since *disconnect* = false)
	  ;; 2.  closed the connection and we read EOF, then called (disconnect) for them
	  ;; in either case we can close the server stream and the socket
	  
	  ;; Wrap each of these with a handler-case, just in case they can't be closed
	  ;; Does the order we do these matter?
	  (handler-case
	    #+cmu
	    (close-socket *this-socket*)
	    #+allegro
	    (close *this-socket*)
	    (error (e)
		   (format *log-stream* "~A: Error attempting to close *this-socket*: ~A~%" (current-time) e)))
	  (handler-case
	   (close *server-stream*)
	   (error (e)
		  ;; if this fails, how do we get rid of the file handle!!!???
		  (progn
		    (format *log-stream* "~A: Error attempting to close *server-stream*: ~A~%" (current-time) e)
		    ;;if we got an error closing the stream, we need to nuke it with the :abort option or we have an FD descriptor leak!
		    (close *server-stream* :abort t))))


	  ;; At the point we have closed the socket and server stream
	  ;; We can safely start listening for new connections again 
	  ;; or let the function return if we're done accepting connections
	  
	  ;; Reset the variable *disconnect* for the next client
	  (if (not *terminate-server*)
	      (setf *disconnect* nil))
	  
	  )) ;; end "while not *terminate-server*"
  
  (format *log-stream* "~A: Server closing~%" (current-time)))


#+cmu
(defun log-kb ()
  (handler-case
      (checkpoint-kb (unix:unix-maybe-prepend-current-directory *persistent-kb*))
    (error (e)
	   (progn
	     (format *log-stream* "~A: Error writing checkpoint file.  Error:~A~%" (current-time) e)
	     (handler-case  ;; since we're reporting this over the socket, need to catch any errors.
		 (format *output-to-client* "Error writing checkpoint file.  Error:~A~%" e)
	       (error (e) (format *log-stream* "~A: Error sending error to client: ~A~%" (current-time) e)))
	     (finish-output *log-stream*)))))

#+(or allegro sbcl)
(defun log-kb ()
  (error "Operation not supported on this platform."))


;;; ========================================================================
;;; "Outer" server loops for SBCL.

;;; These deserve their own section because SBCL is complicated by
;;; multi-threading capabilities.


#+sbcl
(defgeneric handle-client (sb-bsd-sockets:inet-socket))

#+sbcl
(defmethod handle-client ((socket sb-bsd-sockets:inet-socket))
  "Handle a client request."
  (incf *client-count*)
  (unwind-protect
       (progn
	 (handler-case
	     (let* ((client-stream (sb-bsd-sockets:socket-make-stream socket 
								      :input t 
								      :output t 
								      :element-type 'character 
								      :buffering :full 
								      :external-format :us-ascii))
		    (s (make-array 4096 :fill-pointer 0 :adjustable t :element-type 'character))
		    (*input-from-client* client-stream)    ;;DYNAMICALLY REBIND THESE VARIABLES TO WORK IN A THREADED SITUATION...  THIS IS A CONFUSING SOLUTION!!!
		    (*output-to-client* client-stream)
		    (*show-stream* *output-to-client*)
		    (*disconnect* nil))
	       (handler-case
		   (progn
		     #+(and true-threading sbcl)
		     (progn
		       (format *log-stream* "~A: Preparing for new client (multi-threading enabled)...~%" (current-time))
		       (setf *error-output* *error-output-ref*)
		       (setf *standard-output* *standard-output-ref*)
		       (setf *trace-output* *trace-output-ref*))

		     (cond
		       ((>= *output-verbosity* 2)
			(setf *output-to-client* (make-broadcast-stream client-stream *log-stream*))
			(setf *input-from-client* (make-echo-stream client-stream *log-stream*)))
		       (t
			(setf *output-to-client* client-stream)
			(setf *input-from-client* client-stream)))

		     (setf *show-stream* *output-to-client*)
		     (setf *commentatary-stream* *output-to-client*)

		     ;(let ((*show-stream* *output-to-client*))
		       (inner-session-loop)
		     ;  )
		     
		     (setf (fill-pointer s) 0)   ; Reset our character array 
		     (close client-stream)
		     
		     (format *log-stream* "~A: Closing client connection.~%" (current-time))
		     (finish-output *log-stream*)
		     (sb-bsd-sockets:socket-close socket)
		     (format *log-stream* "~A: Socket closed.~%" (current-time))
		     (finish-output *log-stream*))
		 (error (e)
			(progn
			  (format *log-stream* "~A: Error in handle client. Disconnecting. Error: ~S~%" (current-time) e)
			  (close client-stream :abort t)
			  (sb-bsd-sockets:socket-close socket)))))
	   (error (e)
		  (progn
		    (format *log-stream* "~A: Error in handle client [2]. Disconnecting. Error: ~S~%" (current-time) e)
		    (sb-bsd-sockets:socket-close socket)))))
    (decf *client-count*)))

#+sbcl
(defmacro with-socket (socket &body body)            
  "Create and close a socket around the body."       
  `(let ((,socket (make-instance 'sb-bsd-sockets:inet-socket        
				 :type :stream       
				 :protocol :tcp)))   
    (unwind-protect (progn ,@body)
      (sb-bsd-sockets:socket-close ,socket))))
      

(defun convert-address (address)
  "Convert IP address to string."
  (etypecase address
     (string address)
     (vector (format nil "~{~d.~^~}" (map 'list #'identity address)))))


#-(and true-threading sbcl)
(defun handle-client-helper (client-socket)
  (handle-client client-socket))

#+(and true-threading sbcl)
(defun handle-client-helper (client-socket)
  (incf *connection-id*)
  (sb-thread:make-thread (lambda () (handle-client client-socket)) :name (format nil "ThreadForClient-~A-~A" (convert-address (sb-bsd-sockets:socket-peername client-socket)) *connection-id* ) ))

#+sbcl
;(defmethod run (&key (port *default-port*))
(defun run (&key (port *default-port*))
  "Run the server, listening on the specified port and dispatching client requests."
  (handler-case
      (progn
	(format *log-stream* "~A: Starting server on port ~A.~%" (current-time) port)
	(finish-output *log-stream*)
	(with-socket server-socket
	  (setf (sb-bsd-sockets:sockopt-reuse-address server-socket) t)  
	  (sb-bsd-sockets:socket-bind server-socket *server-address* port) 
	  (sb-bsd-sockets:socket-listen server-socket *default-server-backlog*)
	  (format *log-stream* "~A: Listening for new connections...~%"(current-time) ) (finish-output *log-stream*)

	  ;; We were getting an error from 'accept' about there being too many files open before.
	  (do ((client-socket (sb-bsd-sockets:socket-accept server-socket) (sb-bsd-sockets:socket-accept server-socket)))  ; ignore peer value           
	      (nil)  ; infinite loop 

	    #+true-threading
	    (progn
	      (format *log-stream* "~A: New client connecting (current connections: ~A) (thread count: ~A)~%" (current-time) *client-count* (length (sb-thread:list-all-threads))) 
	      (finish-output *log-stream*)
	      (if (>= *output-verbosity* 5)
		  (pprint (sb-thread:list-all-threads))))
	    
	    (format *log-stream* "~A: Received connection from ~a~%" (current-time) (convert-address (sb-bsd-sockets:socket-peername client-socket)))
	    (let ((client-socket client-socket))
	      (handle-client-helper client-socket)))))
    (error (e)
	   (format *log-stream* "~A: Error unhandled in main server loop.  The server cannot recover from this error.  This is likely a bug in the server please report the following error.  Caught exception main server loop: ~A~%" (current-time) e)))) 


#-(and true-threading sbcl)
(defun start (&key (port *default-port*))
  "Start a Scone server without threading support)"
  (run :port port))

#+(and true-threading sbcl)
(defun start (&key (port *default-port*) (xml nil))
  "Start a Scone server with threading support"
  (setf *error-output-ref* *error-output*)
  (setf *standard-output-ref* *standard-output*)
  (setf *trace-output-ref* *trace-output*)
  (setf myServer-thread (sb-thread:make-thread (lambda () (run :port port)) :name "SconeServer"))
  ;;I'm not sure why, but when this function returns this and the other threads die...
  ;;For now, sleeping indefinitely seems to fix the problem
  (do ((x (sleep 10000)(sleep 10000)))
      (nil)))

#+(and true-threading sbcl)
(defun stop ()
  (let ((st myServer-thread))
    (cond
      ((thread-alive-p st)
       (write-line "Server stopped.")
       (terminate-thread st))
      (t
       (write-line "Server is not running.")
       nil))))

