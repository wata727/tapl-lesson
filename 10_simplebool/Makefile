OBJS = support.cmo core.cmo parser.cmo lexer.cmo main.cmo

all: $(OBJS) f

include .depend

f: $(OBJS)
	@echo Linking $@
	ocamlc -o $@ $(OBJS)

%.cmi: %.mli
	ocamlc -c $<

%.cmo: %.ml
	ocamlc -c $<

parser.ml parser.mli: parser.mly
	ocamlyacc -v parser.mly

%.ml %.mli: %.mll
	ocamllex $<

clean:
	rm -rf lexer.ml parser.ml parser.mli *.o *.cmo *.cmi parser.output f

depend:
	ocamldep $(INCLUDE) *.mli *.ml > .depend
