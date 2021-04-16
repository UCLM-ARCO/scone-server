(load "src/load.lisp")

(load-and-start-scone-server
  :scone-path "/usr/share/scone/"
  :scone-version "1.0.0"
  :xml nil
  :server-address "0.0.0.0")
  :port 6517
  (quit)
)
