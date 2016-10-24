SETLOCAL

SET LOG_FILENAME=COMPILE.LOG

start C:/acl80/alisp +cx -e '(progn (compile-file "s-xml/package.lisp") (compile-file"s-xml/dom.lisp") (compile-file "s-xml/lxml-dom.lisp")(compile-file "s-xml/sxml-dom.lisp")(compile-file "s-xml/xml.lisp")(compile-file "s-xml/xml-struct-dom.lisp")(exit))' -d %LOG_FILENAME% cd s-xml copy package.fasl+dom.fasl+lxml-dom.fasl+sxml-dom.fasl+xml.fasl+xml-struct-dom.fasl s-xml.fasl move s-xml.fasl .. cd ..