#!/bin/bash

LISP=/usr/bin/sbcl

DOTSCONE=${1:-"$PWD/.scone"}
mkdir -p $DOTSCONE

LOG="$DOTSCONE/server.log"
SERVER_PID="$DOTSCONE/server.pid"

function scone_slive {
    kill -0 $server_pid > /dev/null 2>&1
    return $?
}

function stop-server {
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

# Make sure there isn't already a PID file here.
if [ -f "$SERVER_PID" ]; then echo "Error: $SERVER_PID file detected.  If you believe the server is still running, find the PID and kill that process.  If the server is not running please delete the file server.pid."; exit; fi

setsid $LISP --noinform --load src/server.lisp > "$LOG" 2>&1 &

server_pid=$!
echo $server_pid > "$SERVER_PID"

echo "waiting server to start..."
tini=$(date +%s)
while ! ss -lptn 2> /dev/null | grep ":6517" > /dev/null; do
    now=$(date +%s)
    delta=$((now - tini))
    sleep 1
    if [ $delta -gt 4 ]; then
        echo "ERROR: scone server did not start after 5 seconds"
        rm -f "$SERVER_PID"
        exit 1
    fi
done

if [ -d scone-knowledge.d ]; then
    knowledge_dir=$(realpath scone-knowledge.d)

    for f in $(find $knowledge_dir -type f -print | sort); do
        echo "Loading knowledge: $f"
        echo \(load-kb \"$f\"\) \; | ncat localhost 6517 >> $LOG 2>&1
    done

    if grep --text "^*****SCONE-ERROR" $LOG; then
        echo "ERROR loading knowledge, aborting..."
        stop-server
    fi
fi

echo "log is \"$LOG\""
echo "[ready] scone-server pid:$server_pid port:6517"
echo "press C-c to stop"

trap stop-server INT QUIT

while true; do
    read
done
