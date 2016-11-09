# -*- mode: makefile-gmake; coding: utf-8 -*-

DESTDIR?=~
BASE=$(DESTDIR)/usr/share/scone-server

all: clean compile

compile:
	cd lib && ./compile-s-xml.sh

install:
	install -d $(DESTDIR)/usr/bin
	install -m 644 server.sh $(DESTDIR)/usr/bin/scone-server
	install -d -m 766 $(BASE)

PHONY: clean
clean:
	find -name "*.fasl" | xargs rm -f
	find -name "*~" | xargs rm -f

superclean: clean
	-rm -f SCONE-SERVER.LOG
