ocamllex lexer.mll
echo "lexed"
ocamlyacc parser.mly
echo "parsed"
ocamlc -c syntax.ml
echo "syntaxed"
ocamlc -c parser.mli
echo "parse interface"
ocamlc -c lexer.ml
echo "lexer ml"
ocamlc -c parser.ml
echo "parse ml"
ocamlc -c calc.ml
echo "calc ml"
ocamlc -o calc lexer.cmo parser.cmo syntax.cmo calc.cmo 
