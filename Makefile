OCAMLC=ocamlc 
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=	#
OCAMLFLAGS=$(INCLUDES)	-annot #
OCAMLOPTFLAGS=$(INCLUDES)
PROG1_OBJS=lexer.cmo ast.cmo parser.cmo main.cmo 
TEST_OBJS=lexer.cmo ast.cmo parser.cmo test.cmo
# #ast.cmo:
# 	#ocamlc -c -pp "camlp4o -I `ocamlfind query type-conv` -I `ocamlfind query sexplib` pa_type_conv.cma pa_sexp_conv.cma" unix.cma bigarray.cma nums.cma -I `ocamlfind query sexplib` sexplib.cma str.cma ast.ml 

main: $(PROG1_OBJS) 
	$(OCAMLC) -o main $(OCAMLFLAGS) $(PROG1_OBJS) 

run: main
	./main

verify: test
	./test

inter: $(PROG1_OBJS) 
	ocaml $(PROG1_OBJS) 

#-I `ocamlfind query sexplib` unix.cma bigarray.cma nums.cma sexplib.cma
test: $(TEST_OBJS)
	$(OCAMLC) -o test $(OCAMLFLAGS) $(TEST_OBJS)

print: main
	./main < printer

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.ml: parser.mly
	ocamlyacc parser.mly

parser.mli: parser.mly
	ocamlyacc parser.mly

lexer.cmo: lexer.ml parser.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c lexer.ml

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
	rm -f lexer.ml parser.ml parser.mli
# Dependencies 
.depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend 

undepend:
	rm .depend

include .depend

