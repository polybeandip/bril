.PHONY: clean

lvn: util.ml env.cmo lvn.ml
	ocamlfind ocamlc -package bril -linkpkg -o $@ $^

env.cmo: util.ml env.ml env.cmi
	ocamlfind ocamlc -package bril -linkpkg -c util.ml env.ml

%.cmi: %.mli
	ocamlfind ocamlc -package bril -linkpkg -c $<

clean:
	rm -rf lvn *.cmi *.cmo
