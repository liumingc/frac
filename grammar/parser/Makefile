a.out:
	ocamlyacc parse.mly
	ocamllex lex.mll
	ocamlc -c helper.ml
	ocamlc -c parse.mli
	ocamlc -c parse.ml
	ocamlc -c lex.ml
	ocamlc -c main.ml
	ocamlc -o $@ lex.cmo parse.cmo main.cmo helper.cmo

clean:
	rm -f *.cmo *.cmi
	rm -f parse.mli parse.ml lex.ml
	rm -f a.out
