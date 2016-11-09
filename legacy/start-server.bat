SETLOCAL

# Allegro could be installed anywhere (NOTE: you have to trick DOS when there's a space in the pathname)
SET ALLEGRO=C:/Progra~1/acl80/alisp

# We need to say where Scone is and what version to use. (NOTE: This should contain single forward slashes and have a trailing forward slash (e.g. C:/dir1/scone/ )
SET PATH_TO_SCONE=L:/scone/
SET SCONE_VERSION=v7

# Parameters to give to Scone
SET LOG_FILENAME=SCONE-SERVER.LOG
SET PORT=6517
SET PERSISTENT_KB=

# Finally start the server.  The parameter "+cx -L" specify whether to have a splash screen, reside in the taskbar, etc.
start %ALLEGRO% +cx -L "src/load.lisp" -e '(load-and-start-scone-server :xml t :port %PORT% :scone-path "%PATH_TO_SCONE%" :scone-version "%SCONE_VERSION%")' -d %LOG_FILENAME%
