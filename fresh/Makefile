OCAMLC=ocamlc 
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=	#
OCAMLFLAGS=$(INCLUDES)	#
OCAMLOPTFLAGS=$(INCLUDES)
# prog1 should be compiled to bytecode, and is composed of three # units: mod1, mod2 and mod3.
# The list of object files for prog1 
PROG1_OBJS=lexer.cmo parser.cmo ast.cmo main.cmo

parse: $(PROG1_OBJS) 
	$(OCAMLC) -o parse $(OCAMLFLAGS) $(PROG1_OBJS)

# Common rules 

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.ml: parser.mly
	ocamlyacc parser.mly

parser.mli: parser.mly
	ocamlyacc parser.mly

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<
.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<
.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<
# Clean up 
clean:
	rm -f *.cm[iox]
# Dependencies 
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend 
include .depend

