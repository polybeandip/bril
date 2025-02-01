.PHONY: clean

reorder: reorder.ml
	ocamlfind ocamlc -package bril -linkpkg -o reorder reorder.ml

clean:
	rm -rf reorder *.cmi *.cmo
