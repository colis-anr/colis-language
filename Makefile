.PHONY: why3-extraction

why3-extraction:
	mkdir -p src/why3-extraction # Not sure where to extract to
	why3 extract --modular --recursive \
		-D ocaml64 \
		-D src/language/driver.drv -D src/concrete/driver.drv \
		-L src/language -L src/concrete \
		-o src/why3-extraction/ \
		map.Const \
		interpreter.Interpreter
