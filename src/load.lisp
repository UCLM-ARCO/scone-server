;;; ***************************************************************************
;;;
;;;  load.lisp

;;;  This file loads all the Lisp files that are needed for a SCONE server.
;;;
;;;  Author: Ben Lambert (benlambert@cmu.edu)

;;;  Modified: 12-13-05 TI
;;;    This file loads only those files necessary for the SCONE server.  In
;;;    particular the event representation functions are not loaded nor is
;;;    Alicia's version of the CNS database (these were all loaded in the
;;;    version that Ben built for Javelin).

;;;  Modified: 12-17-05 TI
;;;    Added load of "scone-struct-fcns.lisp" to support new search functions.

;;; ***************************************************************************

(format t "Beginning server load process...")

;; The :sb-thread "feature" doesn't really work...
;; we have to check manually...
#+sbcl
(handler-case
    (progn
      (sb-thread:make-thread (lambda () (+ 2 2)))
      (push :true-threading *features*)
      (format t "Threading support detected~%"))
  (error (e) (declare (ignore e)) (format t "No threading support detected~%")))


#+sbcl
(require :sb-bsd-sockets)

;; Additional parameters, not set in the start/load server function:
(defparameter *public-functions-only* nil
  "This parameter determines whether only functions in the list *public-functions* can be called.")
(defvar *output-verbosity* 5
  "Set this to 1 for basic logging of connections and errors.  Set to 2 to log everything that comes in and goes out through the socket stream.")
(defvar *log-kb* nil
   "Log each change to the KB, so it can be reconstructed later -- not available currently")
(defvar *persistent-kb* nil
  "Name of the file in which we store the persistent KB -- enable with *log-kb*  -- also not currently available")


;;; Variables to hold the ultimate values of each parameter
(defvar *scone-path* "/usr0/sef/scone/"
  "The location where the Scone executable is located.  Doesn't include the version number - must have a trailing slash")
(defvar *use-xml* nil
  "This parameter toggles between accepting XML inputs and Lisp inputs.")
(defvar *this-port* nil
  "Keep track of the actual port we decided to run on.")

;;; Some defaults to use, just in case they don't get set somewhere else:

(defvar *default-scone-path* nil)
(defvar *default-scone-version* "0.7b")
(defvar *default-persistent-kb* nil)
(defvar *default-use-xml* nil)
(defvar *default-port* 6517)
(defvar *default-server-address* "0.0.0.0")

;;; This sets up some local stuff for Scone.

(defvar *version*)
(defvar *default-kb-pathname*)

;; Just to suppress a spurious warning.
(defun load-kb (filename &key (verbose nil))
  (declare (ignore filename verbose))
  nil)


(defun scone (&optional (version "current"))
  (setq *version* version)
  (setq *default-kb-pathname*
    (format nil "~A~A/kb/anonymous.lisp"  *scone-path* *version*))
  (load (format nil "~A~A/engine"  *scone-path* *version*))
  (load-kb "bootstrap")
  (provide "scone")
  (format t "~&;;; Scone KB engine loaded and ready.~%")
  (values))

;;the next line is the new asdf setup
(require "asdf")
(push #P"./lib" asdf:*central-registry*)
(push #P"../lib" asdf:*central-registry*)

(setf asdf:*central-registry*
  '(*default-pathname-defaults*
    #P"lib"))


(defun load-asdf-lib (lib)
  (asdf:operate 'asdf:load-op lib))


;; Always load the TCP/IP stuff up front, we're going to need it no matter what.
(format t "Loading ip-server.lisp...~%")
(load "src/ip-server.lisp")




(defun load-scone-server (&key
			  (scone-path *default-scone-path*)
			  (scone-version *default-scone-version*)
			  ;(persistent-kb *default-persistent-kb*)
			  )


  ;(load-lib 'scone)

  (setf *scone-path* scone-path)
  (scone scone-version)


  ;; We're going to need these no matter what, so let's load them up front
  ;(load-asdf-lib 'parenscript)
  ;(load-asdf-lib 'cl-json)
  ;(load-asdf-lib 'cl-ppcre)
  ;(load-asdf-lib 's-xml)


  (load "lib/cl-ppcre/load.lisp")
  (load "lib/s-xml/package.lisp")


  (format t "Loading xml-helper-fcns.lisp...~%")
  (load "src/xml-helper-fcns.lisp")

  (format t "Loading wrapper-fcns.lisp...~%")
  (load "src/wrapper-fcns.lisp")

  (format t "Loading gateway.lisp...~%")
  (load "src/gateway.lisp")                  ;; fcns to enforce the "gateway"


  ;; have to load the radar KB first!
  ;;
  ;; TO DO: remove (tokenize) functions from Radar here, or
  ;; load the radar KB's by hand instead of calling the top-level
  ;; function.  (tokenize) is re-defined in get-scone-annotations.lisp
  (load-kb "core")

  ;(load "src/core-kb-no-time.lisp")
  ;(load "src/time.lisp")

  ;(load "src/core-kb-cinar.lisp")

  ;(load-kb "radar")
  ;(load-kb "radar")
  ;(load-kb "radar")

  ;(load-kb "cns-kb")
  ;(load-kb "rtw-kb")
  ;(load-kb "dblp-2k-kb.lisp")
  ;(load-kb "dblp-150-kb.lisp")

  ;; File needed for SconeEdit

  ;(format t "Loading SconeEdit support...~%")
  ;(load "src/get-scone-annotations.lisp")
  ;(load "src/scone-focus-search-fcns.lisp")
  ;(setq *demo-last-preloaded-element* *last-element*)


  ;; PATRICK extensions from Dan
  ;(format t "Loading Patrick extensions...~%")
  ;(load "src/patrick-extensions.lisp")

  ;; UIMA annotator support
  ;(format t "Loading UIMA support...~%")
  ;(load "src/uima-annotations.lisp")


  ;(format t "Loading learning demo support...~%")
  ;(compile-file "../scone-learner/scone-learner.lisp")
  ;(load "../scone-learner/scone-learner")
  ;(load "../scone-learner/kb/radar-demo-kb.lisp")


  ;(setf *no-checking* t)
  ;(load "kb/travis-kb-autogen-small-kb.lisp" :external-format :ascii)
  ;(load "kb/travis-kb-aux.lisp" :external-format :ascii)
  ;(load "kb/travis-kb-autogen-small-fcn.lisp" :external-format :ascii)
  ;(load "kb/travis-kb-autogen-small.lisp" :external-format :ascii)

  ;(load "kb/travis-kb-autogen-medium-kb.lisp" :external-format :ascii)
  ;(load "kb/travis-kb-aux.lisp" :external-format :ascii)
  ;(load "kb/travis-kb-autogen-medium-fcn.lisp" :external-format :ascii)
  ;(load "kb/travis-kb-autogen-medium.lisp" :external-format :ascii)


  ;(load "kb/travis-kb-autogen.lisp" :external-format :ascii)
  ;(setf *no-checking* nil)

  ;(load "const-engine/c-engine-loader.lisp")

  ;(compute-cooccurance-counts :type (lookup-element "publication"))
  ;(compute-cooccurance-counts :type (lookup-element "radar event"))
  ;(setf *ready-to-predict* t)

  ;(load "src/travis-constructions.lisp")

)

#+(or cmucl cmu allegro)
(defun start-scone-server (&key
			   (xml *default-use-xml*)
			   (port *default-port*)
			   (server-address *default-server-address*))

  (format t "Starting server...~%")
  (format t "Use XML: ~a~%" xml)
  (format t "Server address: ~A~%" server-address)
  (format t "Port number: ~A~%" port)
  (finish-output t)

  ;(setq *server-address* (sb-bsd-sockets:make-inet-address server-address))
  (setq *use-xml* xml)
  (server-loop port))

#+sbcl
(defun start-scone-server (&key
			   (xml *default-use-xml*)
			   (port *default-port*)
			   (server-address *default-server-address*))
  (format t "Starting server...~%")
  (format t "Use XML: ~a~%" xml)
  (format t "Server address: ~A~%" server-address)
  (format t "Port number: ~A~%" port)
  (finish-output t)

  (setq *server-address* (sb-bsd-sockets:make-inet-address server-address))
  (setq *use-xml* xml)
  ;(start :port port :xml xml)
  (start :port port))

(defun load-and-start-scone-server (&key
			  (xml *default-use-xml*)
			  (scone-path *default-scone-path*)
			  (scone-version *default-scone-version*)
			  ;(persistent-kb *default-persistent-kb*)
			  (port *default-port*)
			  (server-address *default-server-address*))
  ;(load-scone-server :scone-path scone-path :scone-version scone-version :persistent-kb persistent-kb)
  (load-scone-server :scone-path scone-path :scone-version scone-version)
  (start-scone-server :xml xml :port port :server-address server-address ))




;; Any code to deal with turning on/off logging and loading an older file should go in this file

#+cmu
(defun setup-logging ()
  "If there's a log file, loaded it, otherwise load core kb?"
  (if (probe-file *persistent-kb*)
      (load-kb (unix:unix-maybe-prepend-current-directory *persistent-kb*))
      (load-kb "core")))

#+allegro
(defun setup-logging ()
   (load-kb "core")
   (error "Operation not supported on this platform."))

