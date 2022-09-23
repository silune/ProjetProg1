prog: name.cmo main.ml
	ocamlc -o prog name.cmo main.ml

name.cmo: name.ml name.cmi
	ocamlc -c name.ml

name.cmi: name.mli
	ocamlc -c name.mli

clean:
	rm -rf prog *.cmi *.cmo *~

