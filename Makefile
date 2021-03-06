# Targets to replay proofs for the concrete interpreter
replay-concrete-proofs=$(patsubst %, replay-concrete-proof-%, auxiliaries semantics interpreter)
replay-symbolic-proofs=$(patsubst %, replay-symbolic-proof-%, collection symbolicInterpreter)

.PHONY: build test doc clean install uninstall \
  extract-why3 clean-why3 replay-proofs \
  $(replay-concrete-proofs) $(replay-symbolic-proofs)

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

## Extract Why3 to OCaml

extract-why3:
	mkdir -p src/why3
	rm -f src/why3/*
	why3 extract --modular --recursive \
		-D ocaml64 \
		-D src/language/driver.drv \
    -D src/concrete/driver.drv \
    -D src/symbolic/driver.drv \
		-L src/language \
    -L src/concrete \
    -L src/symbolic \
		-o src/why3 \
		interpreter.Interpreter \
    symbolicInterpreter.Interpreter

clean-why3:
	rm -rf src/why3

## Replay Why3 proofs

replay-proofs: $(replay-concrete-proofs) $(replay-symbolic-proofs)

$(replay-concrete-proofs): replay-concrete-proof-%: src/concrete/%.mlw src/concrete/%/why3session.xml
	why3 replay --use-steps --quiet \
		-L src/language -L src/concrete \
		src/concrete/$*

$(replay-symbolic-proofs): replay-symbolic-proof-%: src/symbolic/%.mlw src/symbolic/%/why3session.xml
	why3 replay --use-steps --quiet \
		-L src/language -L src/concrete -L src/symbolic \
		src/symbolic/$*
