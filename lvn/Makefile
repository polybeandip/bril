.PHONY: clean

lvn: lvn.ml
	ocamlfind ocamlc -package bril -linkpkg -o $@ $^

clean:
	rm -rf lvn *.cmi *.cmo
