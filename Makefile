
PROGRAMS_ML=$(wildcard src/*.ml)
PROGRAMS=$(patsubst %.ml,%.native,$(PROGRAMS_ML))

all: $(PROGRAMS)

clean:
	ocamlbuild -clean

%.native: %.ml
	ocamlbuild -use-ocamlfind $@

install: $(PROGRAMS)
	echo ocamlfind install syncasist META $(wildcard _build/*.cm[xioa]) $(wildcard _build/*.cmxa) $(wildcard *.o) $(wildcard _build/*.a) $(wildcard *.ml*)
