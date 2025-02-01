.PHONY: clean

cfg: cfg.ml
	ocamlfind ocamlc -package bril -linkpkg -o cfg cfg.ml

clean:
	rm -rf cfg *.cmi *.cmo
