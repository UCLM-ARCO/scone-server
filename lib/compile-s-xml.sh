#!/bin/sh

#$LISP_LOC
sbcl --eval "(progn (compile-file \"s-xml/package.lisp\") (compile-file \"s-xml/dom.lisp\") (compile-file \"s-xml/lxml-dom.lisp\")(compile-file \"s-xml/sxml-dom.lisp\")(compile-file \"s-xml/xml.lisp\")(compile-file \"s-xml/xml-struct-dom.lisp\")(quit))" 

cd s-xml

cat  package.fasl dom.fasl lxml-dom.fasl sxml-dom.fasl xml.fasl xml-struct-dom.fasl > ../s-xml.fasl 
cd ..
