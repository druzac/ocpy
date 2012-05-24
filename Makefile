OCAMLC=ocamlc 
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=	#
OCAMLFLAGS=$(INCLUDES)	#
OCAMLOPTFLAGS=$(INCLUDES)
PROG1_OBJS=lexer.cmo parser.cmo ast.cmo main.cmo 

#ast.cmo:
	#ocamlc -c -pp "camlp4o -I `ocamlfind query type-conv` -I `ocamlfind query sexplib` pa_type_conv.cma pa_sexp_conv.cma" unix.cma bigarray.cma nums.cma -I `ocamlfind query sexplib` sexplib.cma str.cma ast.ml 

main: $(PROG1_OBJS) 
	$(OCAMLC) -o main $(OCAMLFLAGS) $(PROG1_OBJS) 

#-I `ocamlfind query sexplib` unix.cma bigarray.cma nums.cma sexplib.cma
test: main
	./main < test.txt

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
# Dependencies 
.depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend 

undepend:
	rm .depend

include .depend

