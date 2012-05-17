OC=ocamlc
OL=ocamllex
OY=ocamlyacc
ODEP=ocamldep
OBJS=lexer.cmo parser.cmo syntax.cmo calc.cmo 

run: calc
	./calc < test

calc: $(OBJS)
	$(OC) -o calc $(OBJS)

parser.ml: parser.mly
	$(OY) parser.mly

parser.mli: parser.mly
	$(OY) parser.mly

lexer.ml: lexer.mll
	$(OL) lexer.mll

%.cmo : %.ml
	$(OC) -c $<

# prog1 should be compiled to bytecode, and is composed of three # units: mod1, mod2 and mod3.
# The list of object files for prog1 PROG1_OBJS=mod1.cmo mod2.cmo mod3.cmo
#
# prog2 should be compiled to native-code, and is composed of two # units: mod4 and mod5.
# The list of object files for prog2 PROG2_OBJS=mod4.cmx mod5.cmx
# Common rules 
.SUFFIXES: .ml .mli .cmo .cmi 
.ml.cmo: $(OC) -c $<
.mli.cmi: $(OC) -c $<
# Clean up 
# Dependencies 
depend:
	$(ODEP) *.mli *.ml > .depend 

include .depend
