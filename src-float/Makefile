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
all: fEase.cma  fTimed.cma  fEaseBz.cma
opt: fEase.cmxa fTimed.cmxa fEaseBz.cmxa

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
findinstall: META fEase.cma
	$(OCAMLFIND) install fease META *.cm[iax] *.cmx[as] *.mli *.a

.PHONY: doc
doc:
	$(MKDIR) $(DOC_DIR)
	$(OCAMLDOC) -d $(DOC_DIR) -html fEase.mli fTimed.mli fEaseBz.mli

.PHONY: cleandoc
cleandoc:
	$(RM) -r doc

.PHONY: clean
clean:
	$(RM) *.cm[ioxa] *.[oa] *.cmx[as] *.opt *.exe *.byte
