.PHONY: default build install debug uninstall test clean

export OCAMLRUNPARAM = b

default: build

debug:
	dune bui

build:
	dune bui

clean:
	dune clean
	git clean -dfXq
