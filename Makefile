
OCB = ocamlbuild -use-ocamlfind -use-menhir -I src

all:native byte

native:
	$(OCB) main.native

byte:
	$(OCB) main.byte

clean:
	$(OCB) -clean
