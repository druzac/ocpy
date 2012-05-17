/*  
 *  Assume we have a file syntax.mli/ml defined, from where we get the syntax.
 *  We're going to implement a tiny little calculator here...
 *  it will return a concrete AST
 *  
 */

%{
    open Syntax
%}

%token PLUS MINUS STAR SLASH EOF
%token NEWLINE
%token <int> NUMBER

%start toplevel
%type < Syntax.tree> toplevel
%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

toplevel : 
exprs EOF {T (List.rev $1)}

exprs:  { [] }
| exprs expr { $2::$1}

expr:
    expr PLUS factor { SUM($1, $3) }
    | expr MINUS factor { DIFF ($1, $3) }
    | factor            {E_F $1}


factor:
    result  { F_R $1}
    | factor STAR result { PROD ($1, $3)}

result:
    NUMBER { R $1}
