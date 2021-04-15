(load "src/load.lisp")

(load-and-start-scone-server
  :port 6517
  :scone-path "/usr/share/scone/"
  :scone-version "1.0.0"
  :xml t
  :server-address "0.0.0.0")
  (quit)
)
