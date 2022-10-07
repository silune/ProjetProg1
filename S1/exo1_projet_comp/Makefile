all: test_my_list rapport

test_my_list: my_list.cmo test_my_list.ml
	ocamlc -o test_my_list my_list.cmo test_my_list.ml

my_list.cmi : my_list.mli
	ocamlc -c my_list.mli

my_list.cmo : my_list.cmi my_list.ml
	ocamlc -c my_list.ml

rapport: rapport.tex
	pdflatex rapport.tex

clean:
	rm -rf test_my_list *.cmi *.cmo *~ rapport.aux rapport.log rapport.pdf
