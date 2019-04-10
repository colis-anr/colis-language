MAIN=standalone

default: $(MAIN)

contents.cmi: contents.mli
	ocamlc $<

contents.cmo: contents.ml contents.cmi
	ocamlc -c $<

$(MAIN).cmo: $(MAIN).ml contents.cmi
	ocamlc -c $<

$(MAIN): contents.cmo $(MAIN).cmo
	ocamlc -o $@ $^

clean:
	rm -f *.cmi *.cmo $(MAIN)

.PHONY: clean
