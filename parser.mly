
%{
(*open Printf*)
%}

/* put all the base tokens for the lexer here */
/* the grammar rules go in the rules section */

/* whitespace/control symbols */
%token NEWLINE INDENT DEDENT ENDMARKER
/* special symbol for only the initial lexer */
%token <int> INDENT_SPACE

/* keywords */
%token FALSE CLASS FINALLY IS RETURN NONE CONTINUE FOR LAMBDA TRY TRUE DEF FROM NONLOCAL WHILE
%token AND DEL GLOBAL NOT WITH AS ELIF IF OR YIELD ASSERT ELSE IMPORT PASS BREAK EXCEPT IN RAISE
/* end keywords */

/* punctuation */
%token PLUS MINUS STAR DSTAR SLASH DSLASH PERCENT DBCHEVRON DFCHEVRON AMP
%token PIPE CARET TILDE BCHEVRON FCHEVRON LTEQ GTEQ EQEQ EXEQ
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token COMMA COLON DOT SEMICOLON ATSYM EQ PLUSEQ MINEQ STAREQ SLASHEQ DSLASHEQ
%token PERCENTEQ AMPEQ PIPEEQ CARETEQ DFCHEVRONEQ DBCHEVRONEQ DSTAREQ ELLIPSIS
/* end punctuation */

/* literals */
%token TRUE FALSE
%token <string> STRING
%token <string> NAME
%token <int> NUMBER
/* end literals */


%start start
%type <Ast.program> start

%%  

/* we need to get a run just to see our status... */
start:  atoms ENDMARKER  { Ast.Program (List.rev $1) }
     |  ENDMARKER             { Ast.Program [] }

/*
stmts:
    stmt_item stmts { $1::$2}
    | stmt_item {$1}

    */
    atoms:  atom { [$1] }
    | atoms COMMA atom  { $3::$1}


atom: 
    /*LPAREN RPAREN
| LPAREN arglist RPAREN
| LBRACKET RBRACKET
| LBRACKET testlist RBRACKET
| LBRACE RBRACE
| LBRACE dictorsetmaker RBRACE
*/

 NAME          {Ast.Name $1}
 
| NUMBER        { Ast.Number $1}
| strings      { Ast.String $1} 
| ELLIPSIS      { Ast.Ellipsis}
| NONE          {Ast.None}
| TRUE          { Ast.True}
| FALSE         { Ast.False}

strings: strings STRING { $1 ^ $2 }
| STRING  { $1 }

/*
testlist:   leave blank 

arglist:  leave blank for now 
*/

/*
finputs: finputs finput { if $2 = NEWLINE then $1 else $2 :: $1}
| finput              { $1 }     

finput: NEWLINE     { $1 }
| expr               { Exp (List.rev $1)}

expr: expr PIPE xor_expr  { }
    | xor_expr     {    }

    xor_expr: xor_expr CARET and_expr { }
| and_expr   { And_exp $1}

and_expr: and_expr AMP shift_expr {}
| shift_expr { Shift ($1, [])}

shift_expr: shift_expr DFCHEVRON arith_expr { }
| shift_expr DBCHEVRON arith_expr { }
| arith_expr 

arith_expr: arith_expr arith_op term  
| term

arith_op: PLUS
| MINUS

term: term term_op factor
| factor

term_op: STAR
| SLASH
| PERCENT
| FSLASH

*/
/*
factor: factor_op factor
| power

factor_op: UPLUS | UMINUS | TILDE

indexed:  indexed trailer   
atom  { IndAtom ($1, []) }


trailer: LPAREN RPAREN
| LPAREN arglist RPAREN
| LBRACKET RBRACKET
| LBRACKET tuple_or_test RBRACKET
| DOT NAME


power: indexed  { }
| indexed DSTAR factor {}
*/
