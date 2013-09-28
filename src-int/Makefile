LIBDIR = iease
OCAML = ocaml
OCAMLC = ocamlc -g
OCAMLOPT = ocamlopt -g
OCAMLDOC = ocamldoc
OCAMLMKLIB = ocamlmklib
OCAMLFIND = ocamlfind
OCAMLDIR = $(shell $(OCAMLC) -where)
VERSION = $(shell date --iso)
DESTDIR = $(OCAMLDIR)/$(LIBDIR)
DOC_DIR = doc
MKDIR = mkdir -p

INSTALL_CMD = install
INSTALL_FILES = $(INSTALL_CMD) -m 0644
INSTALL_EXE = $(INSTALL_CMD) -m 0755
INSTALL_DIR = $(INSTALL_CMD) -d

.PHONY: all lib opt
all: lib opt
lib: iEase.cma  iTimed.cma
opt: iEase.cmxa iTimed.cmxa

.SECONDARY:

%.cmi: %.mli
	$(OCAMLC) -c $<

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
	$(OCAMLFIND) install $(LIBDIR) META *.a *.cm[iatx] *.cmx[as] *.mli

.PHONY: manualinstall
manualinstall: META iEase.cma
	$(INSTALL_DIR) $(DESTDIR)
	$(INSTALL_FILES) META *.a *.cm[iatx] *.cmx[a] *.mli $(DESTDIR)
	$(INSTALL_EXE) *.cmx[s] $(DESTDIR)

.PHONY: doc
doc:
	$(MKDIR) $(DOC_DIR)
	$(OCAMLDOC) -d $(DOC_DIR) -html iEase.mli iTimed.mli

.PHONY: cleandoc
cleandoc:
	$(RM) -r $(DOC_DIR)

.PHONY: clean
clean:
	$(RM) *.cm[iotxa] *.[oa] *.cmx[as] *.opt *.exe *.byte

