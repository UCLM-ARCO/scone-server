# SCONE Server #

## Install

 
* Add [arco repository](https://uclm-arco.github.io/debian/)
* sudo apt install scone-server

## Running

    $ scone-server 
    Waiting server to start...
    log is "/home/john/my-prj/.scone/server.log"
    [ready] scone-server pid:307842 port:6517
    Press C-c to stop


See [README.legacy](https://github.com/UCLM-ARCO/scone-server/blob/master/README.legacy) for a description.


## Stopping the server

The termination of the service is done using the combination Ctrl+C. However, if the server does not properly finish its execution, you will not be able to start it up again. In order to overcome this problem, simply delte `.scone/scone-server.pid`. This will solve the problem.

## Local knowledge

Different knowledge bases can be load automatically during the scone-wrapper startup. This is very convenient if you don't want to mess around with core-kb files or if you are simply doing some tests.

In order to do that, you just have to keep all your .lisp files in a directory called `scone-knowledge.d`. Then, the `scone-server` will have to be launched in the directoy where `scone-knowledge.d` directory is. Please, note that all ".lisp" files contained in the`scone-knowledge.d` directory and its subdirectories are loaded in alphabetic order.

Start `scone-server` from parent:

    $ tree
    .
    └── scone-knowledge.d
        └── monkeys.lisp

    $ cat scone-knowledge.d/monkeys.lisp
    (new-indv {Martin} {monkey})
    (new-indv {Felix}  {monkey})

    $ scone-server
    Waiting server to start...
    Loading knowledge:
    - /home/john/my-prj/scone-knowledge.d/monkeys.lisp
    log is "/home/john//my-prj/.scone/server.log"
    [ready] scone-server pid:308677 port:6517
    Press C-c to stop

You may check knowledge is loaded just with:

    $ echo "(is-x-a-y? {Felix} {monkey})" | ncat localhost 6517
    [PROMPT]
    YES
    [PROMPT]
  
  
## Run server from the source directory (this repository)

To start scone-wrapper from repository contents run:

    scone-server$ ./server.sh


## Python client

To invoke the scone-server from python, install [scone-client](https://github.com/UCLM-ARCO/scone-client) module.
