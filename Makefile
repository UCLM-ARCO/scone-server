
all: clean compile

clean:
	-rm lib/s-xml.fasl
	-rm lib/s-xml/*.fasl
	-rm lib/cl-ppcre/*.fasl
	-rm *~
	-rm src/*~
	cd lib && ./clean.sh && cd ..

compile:
	cd lib && ./compile-s-xml.sh

start:
	./start-server 5000 -noxml

superclean: clean
	-rm SCONE-SERVER.LOG
