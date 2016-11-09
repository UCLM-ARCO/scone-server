#!/bin/bash

### CONFIGURATION VARIABLES ###

# LISP and LISP location

# Specify which Lisp to use
LISP=sbcl
#LISP=cmucl

#Specify the location of the Lisp binary
LISP_LOC=/usr/bin/sbcl

HEAP_SIZE=512                    #only for CMUCL

# Scone and Scone location
PATH_TO_SCONE="/usr/share/scone/"    # need trailing slash
SCONE_VERSION="1.0.0"

LOG_FILENAME="SCONE-SERVER.LOG"
SERVER_ADDRESS="0.0.0.0"


### DON'T EDIT AFTER THIS LINE ###

# Use this to make pathnames absolute
# WHERE=/opt/scone/scone-server-1.0
WHERE="$PWD"

# Scone server port number, from the command line.
PORT=${1:-6517}

### PROCESS THE COMMAND LINE ARGS ###

# If no port number is supplied, exit and print usage
if [ "$PORT" = "" ]; then
    echo "Usage: ./start-server PORT_NUMBER { -noxml | -xml } [SERVER-ADDRESS] "
    exit
fi

if [ "$2" = "-xml" ]; then
    XML=t
else
    XML=nil  # not use XML by default
fi

if [ -n "$3" ]; then #if we get a third command line option
    SERVER_ADDRESS="$3"
fi

echo -e "Config: port: $PORT, xml: $XML, host=$SERVER_ADDRESS\n"

# Now we have everything we need.  Let's get ready to start the server.

SERVER_PID=$WHERE/scone-server.pid

# Make sure there isn't already a PID file here.

if [ -f $SERVER_PID ]; then echo "Error: $WHERE/scone-server.pid file detected.  If you believe the server is still running, find the PID and kill that process.  If the server is not running please delete the file scone-server.pid."; exit; fi


# This is the form that we're going to have Lisp evaluate
EVAL_STRING="(progn (load-and-start-scone-server :port $PORT :scone-path \"$PATH_TO_SCONE\" :scone-version \"$SCONE_VERSION\" :xml $XML :server-address \"$SERVER_ADDRESS\" ) (quit))"
# This is the form that we're going to have Lisp evaluate


# Make sure we haven't been given an unsupported set of options.
if [ "$LISP" = "cmucl" ]; then
    if [ "$SERVER_ADDRESS" != "0.0.0.0" ]; then echo "Using an alternate interface is only available on SBCL.  Please use 0.0.0.0."; exit;  fi
fi
if [ "$LISP" = "acl" ]; then
    if [ "$SERVER_ADDRESS" != "0.0.0.0" ]; then echo "Using an alternate interface is only available on SBCL.  Please use 0.0.0.0."; exit; fi
fi

# Now, go ahead and start the server, sending stdout and stderr to the specified log file.

echo "All error and log messages will be printed to \"$LOG_FILENAME\")..."
echo "Server started. Press C-c to stop"

# Start server with CMUCL
if [ "$LISP" = "cmucl" ]; then
    setsid $LISP_LOC -dynamic-space-size $HEAP_SIZE -quiet -load $WHERE/src/load.lisp -eval "$EVAL_STRING" > $LOG_FILENAME 2>&1 &
fi

# Start server with SBCL
if [ "$LISP" = "sbcl" ]; then
    setsid $LISP_LOC --noinform --load $WHERE/src/load.lisp --eval "$EVAL_STRING" > $LOG_FILENAME 2>&1 &
fi

echo $! > $SERVER_PID

trap ctrl-c INT

function ctrl-c {
    kill -9 $(cat $SERVER_PID)
    rm -f $SERVER_PID
    echo -e "\n[end]"
    exit
}

while true; do
    read
done
