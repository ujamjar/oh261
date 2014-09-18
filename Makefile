all:
	ocamlbuild -use-ocamlfind oh261.cma oh261.cmxa

test: all
	ocamlbuild -use-ocamlfind oh261d.byte oh261e.byte

clean:
	ocamlbuild -clean
	-find . -name "*~" | xargs rm -f
