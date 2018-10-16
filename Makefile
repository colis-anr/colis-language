.PHONY: build test clean extract-why3 clean-why3

build: extract-why3
	dune build @install
	ln -sf _build/install/default/bin .

test: build
	dune runtest

doc: build
	dune build @doc
	[ -e doc ] || ln -s _build/default/_doc/_html doc

clean: clean-why3
	dune clean
	rm -f bin doc

extract-why3:
	mkdir -p src/why3
	rm -f src/why3/*
	why3 extract --modular --recursive \
		-D ocaml64 \
		-D src/language/driver.drv -D src/concrete/driver.drv \
		-L src/language -L src/concrete \
		-o src/why3 \
		map.Const \
		interpreter.Interpreter

clean-why3:
	rm -rf src/why3

install:
	dune install

uninstall:
	dune uninstall
