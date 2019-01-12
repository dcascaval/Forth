finth: main.native
	mv main.native finth

clean:
	ocamlbuild -clean

%:
	ocamlbuild -use-ocamlfind $*

