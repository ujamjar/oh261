all: lib_release

lib_release:
	ocamlbuild -use-ocamlfind oh261.cma oh261.cmxa

test: 
	ocamlbuild -use-ocamlfind \
		oh261d.byte oh261e.byte \
		oh261d.native oh261e.native

debug: 
	ocamlbuild -use-ocamlfind \
		oh261d.d.byte oh261e.d.byte 

profile: 
	ocamlbuild -use-ocamlfind \
		oh261d.p.native oh261e.p.native

jsdec: 
	ocamlbuild -use-ocamlfind oh261djs.byte 
	js_of_ocaml -opt 3 oh261djs.byte \
		-I bits -file cifakiyo128.p64:/ \
		-o lib_test/oh261djs.js

clean:
	ocamlbuild -clean
	-find . -name "*~" | xargs rm -f



