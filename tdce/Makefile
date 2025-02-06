.PHONY: clean

tdce: tdce.ml
	ocamlfind ocamlc -package bril -linkpkg -o $@ $^

clean:
	rm -rf tdce *.cmi *.cmo
