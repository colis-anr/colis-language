.PHONY: build clean

MAIN=main

build:
	if which latexmk; then \
		latexmk -pdf $(MAIN) ; \
	else \
		pdflatex $(MAIN) ; \
	fi

watch:
	latexmk -pdf -pvc $(MAIN)

clean:
	if which latexmk; then \
		latexmk -C ; \
	fi
