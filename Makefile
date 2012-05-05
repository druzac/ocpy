test1 : first_pass.ml
	ocaml first_pass.ml < basic.py

test2 : tokenize.ml
	ocaml tokenize.ml < indent.py

first_pass.ml : first_pass.mll
	ocamllex first_pass.mll

tokenize.ml : tokenize.mll
	ocamllex tokenize.mll

run:
	ocaml tokenize.ml

keywords: tokenize.ml
	ocaml tokenize.ml < keywords.py

punct: tokenize.ml
	ocaml tokenize.ml punct.py

num: tokenize.ml
	ocaml tokenize.ml num.py

string: tokenize.ml
	ocaml tokenize.ml string.py
