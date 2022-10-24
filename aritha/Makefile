aritha: x86_64 lexer parser compiler
	ocamlc x86_64.cmo lexer.cmo parser.cmo compiler.cmo main.ml -o aritha

x86_64:
	ocamlc -c x86_64.mli
	ocamlc -c x86_64.ml

lexer:
	ocamlc -c lexer.mli
	ocamlc -c lexer.ml

parser: lexer
	ocamlc -c lexer.cmo parser.mli
	ocamlc -c lexer.cmo parser.ml

compiler: x86_64 parser
	ocamlc -c x86_64.cmo parser.cmo compiler.mli
	ocamlc -c x86_64.cmo parser.cmo compiler.ml

clean:
	rm -rf *.cmi *.cmo *.s aritha
