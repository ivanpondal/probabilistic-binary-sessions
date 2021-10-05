.PHONY: default build install uninstall test clean utop

default: build

build:
	dune build

install:
	dune install

uninstall:
	dune uninstall

test:
	dune runtest

clean:
	dune clean

utop:
	dune utop . -- -rectypes