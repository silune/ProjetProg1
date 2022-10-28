all:	aritha rapport

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

rapport:
	pdflatex rapport.tex

test: aritha
	./aritha examples/testInteger.exp
	gcc -no-pie examples/testInteger.s -o examples/testInteger
	./aritha examples/testFloat.exp
	gcc -no-pie examples/testFloat.s -o examples/testFloat
	./aritha examples/testBonus.exp
	gcc -no-pie examples/testBonus.s -o examples/testBonus

clean:	clean_examples
	rm -rf *.cmi *.cmo *.s *.log *.aux *.toc *.pdf *.exp aritha

clean_examples:
	rm -rf examples/*.s testInteger testFloat testBonus
