.PHONY: build test replay-proofs doc clean extract-why3 clean-why3 install uninstall

build: extract-why3
	dune build @install
	ln -sf _build/install/default/bin .

clean: clean-why3
	dune clean
	rm -f bin doc

install:
	dune install

uninstall:
	dune uninstall

doc: build
	dune build @doc
	[ -e doc ] || ln -s _build/default/_doc/_html doc

test: build
	dune runtest

replay-proofs: $(patsubst %, replay-proofs-concrete-%, lemmas semantics interpreter)

replay-proofs-concrete-%: src/concrete/%.mlw src/concrete/%/why3session.xml
	why3 replay --use-steps \
		-L src/language -L src/concrete \
		src/concrete/$*

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
