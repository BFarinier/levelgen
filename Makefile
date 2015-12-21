all:
	ocamlbuild -package ocamlgraph -package graphics -package gg main.native

clean:
	ocamlbuild -clean
