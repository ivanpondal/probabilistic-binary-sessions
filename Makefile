.PHONY: default build install uninstall clean utop

default: build

build:
	dune build

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

utop:
	dune utop . -- -rectypes
