all:
	ocamlc -c graphics.cma images.mli
	ocamlc -c graphics.cma images.ml
	ocamlfind ocamlc -c -package batteries basics.ml
	ocamlfind ocamlc -c -package batteries myparsing.ml
	ocamlfind ocamlc -package batteries graphics.cma basics.cmo images.cmo myparsing.cmo make_images.ml -linkpkg -o make_images
clean:
	rm -f *.cmo *.cmi
	rm -f make_images
