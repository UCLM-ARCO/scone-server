#!/bin/bash

LISP=/usr/bin/sbcl

WORKDIR=${1:-"$PWD/.scone"}
mkdir -p $WORKDIR

LOG="$WORKDIR/server.log"
SERVER_PID="$WORKDIR/server.pid"

# Make sure there isn't already a PID file here.
if [ -f "$SERVER_PID" ]; then echo "Error: $SERVER_PID file detected.  If you believe the server is still running, find the PID and kill that process.  If the server is not running please delete the file server.pid."; exit; fi

echo "All error and log messages will be printed to \"$LOG\"..."
echo "Server started. Press C-c to stop"

setsid $LISP --noinform --load src/server.lisp > "$LOG" 2>&1 &

server_pid=$!
echo $server_pid > "$SERVER_PID"

sleep 1

if ! kill -0 $server_pid > /dev/null 2>&1; then
    echo "ERROR: scone server did not start"
    rm -f "$SERVER_PID"
    exit 1
fi

echo "[ready] scone-server pid:$server_pid port:6517"

trap ctrl-c INT QUIT

function ctrl-c {
    echo -e "\nstopping scone-server..."

    kill -SIGABRT $server_pid > /dev/null 2>&1
    sleep 1

    while kill -0 $server_pid > /dev/null 2>&1; do
	    echo "killing scone-server pid:$server_pid"
	    kill -SIGKILL $server_pid > /dev/null 2>&1
        sleep 1
    done

    rm -f "$SERVER_PID"
    echo -e "[end] scone-server"
    exit
}

while true; do
    read
done
