SRC    = dep.ml main.ml
RESULT = ocamlinit
build:
	ocamlc -o $(RESULT) -pp camlp4of unix.cma $(SRC)

clean:
	rm -f *.cmo *.cmi

clean-all: clean
	rm -f $(RESULT)
