all:
	ocamlbuild -use-ocamlfind oh261.cma oh261.cmxa

# TODO
test: all
	ocamlbuild -use-ocamlfind oh261_decode.byte

clean:
	ocamlbuild -clean
	-find . -name "*~" | xargs rm -f
