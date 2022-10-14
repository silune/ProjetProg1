all: q1 q2 q3 q4

q1:
	gcc -no-pie -o q1 q1.s

q2:
	gcc -no-pie -o q2 q2.s

q3:
	gcc -no-pie -o q3 q3.s

q4:
	ocamlc -o q4 q4.ml

clean:
	rm -rf q1 q2 q3 q4 q4.cmi q4.cmo addFromCaml.s
