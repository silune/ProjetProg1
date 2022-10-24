test:
	ocamlc -c x86_64.mli
	ocamlc -c x86_64.ml
	ocamlc -c lexer.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.mli
	ocamlc -c lexer.cmo parser.ml
	ocamlc parser.cmo x86_64.cmo compiler.ml -o test
	./test
	gcc -no-pie test.s -o test2

clean:
	rm -rf *.cmi *.cmo test test.s test2
