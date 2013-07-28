OCAML := ocaml
OCAMLC := ocamlc -g
OCAMLOPT := ocamlopt -g
OCAMLDOC := ocamldoc
OCAMLFIND := ocamlfind
VERSION := $(shell date --iso)

OCAML_DIR := $(shell $(OCAMLC) -where)
DOC_DIR := doc
MKDIR := mkdir -p

.PHONY: all opt
all: iEase.cma  iTimed.cma
opt: iEase.cmxa iTimed.cmxa

.SECONDARY:

%.cmi: %.mli
	$(OCAMLC) -c -o $@ $<

%.cmo: %.ml %.cmi
	$(OCAMLC) -c $<
%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) -c $<
%.cmx: %.ml
	$(OCAMLOPT) -c $<

%.cma: %.cmi %.cmo
	$(OCAMLC) -a -o $@ $(lastword $^)

%.cmxa: %.cmi %.cmx
	$(OCAMLOPT) -a -o $@ $(lastword $^)

%.cmxs: %.cmxa
	$(OCAMLOPT) -shared -linkall -o $@ $<

META: META.in
	sed -e 's/@VERSION@/$(VERSION)/g' $< > $@

.PHONY: findinstall
findinstall: META iEase.cma
	$(OCAMLFIND) install iease META *.cm[iax] *.cmx[as] *.mli *.a

.PHONY: doc
doc:
	$(MKDIR) $(DOC_DIR)
	$(OCAMLDOC) -d $(DOC_DIR) -html iEase.mli iTimed.mli

.PHONY: cleandoc
cleandoc:
	$(RM) -r doc

.PHONY: clean
clean:
	$(RM) *.cm[ioxa] *.[oa] *.cmx[as] *.opt *.exe *.byte

