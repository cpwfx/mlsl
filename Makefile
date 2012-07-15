SRCDIR=src
OBJDIR=obj
BINDIR=bin
OCAMLFILES=errors mlslAst parserMisc mlslParser mlslLexer parser topDef \
	typeCheck
.PHONY: all mlslYacc clean

all: $(OBJDIR) $(BINDIR) \
	$(foreach file, $(OCAMLFILES), $(OBJDIR)/$(file).cmi) $(BINDIR)/mlsl

$(OBJDIR):
	mkdir $(OBJDIR)

$(BINDIR):
	mkdir $(BINDIR)

$(BINDIR)/mlsl: $(foreach file, $(OCAMLFILES), $(OBJDIR)/$(file).cmx) \
	$(OBJDIR)/mlsl.cmx
	ocamlopt -o $@ $^

$(SRCDIR)/mlslLexer.ml: $(SRCDIR)/mlslLexer.mll
	ocamllex $<

mlslYacc: $(SRCDIR)/mlslParser.mly
	ocamlyacc $<

$(SRCDIR)/mlslParser.mli: mlslYacc
	@

$(SRCDIR)/mlslParser.ml: mlslYacc
	@

$(OBJDIR)/mlsl.cmx: $(SRCDIR)/mlsl.ml
	ocamlopt -I $(OBJDIR) -o $@ -c $<

$(OBJDIR)/%.cmi: $(SRCDIR)/%.mli
	ocamlc -I $(OBJDIR) -o $@ -c $<

$(OBJDIR)/%.cmx: $(SRCDIR)/%.ml $(OBJDIR)/%.cmi
	ocamlopt -I $(OBJDIR) -o $@ -c $<

clean:
	rm -f $(SRCDIR)/mlslParser.mli $(SRCDIR)/mlslParser.ml \
		$(SRCDIR)/mlslLexer.ml $(OBJDIR)/* $(BINDIR)/mlsl
