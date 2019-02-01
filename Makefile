.PHONY: default all clean

default: all

all:
	dune build @install
	@test -L bin || ln -s _build/install/default/bin .

clean:
	dune clean
