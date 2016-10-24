all: clean compile

clean:
	find -name "*.fasl" | xargs rm -f
	find -name "*~" | xargs rm -f

compile:
	cd lib && ./compile-s-xml.sh

start:
	./start-server 5000 -noxml

stop:
	./stop-server

superclean: stop clean
	-rm SCONE-SERVER.LOG
