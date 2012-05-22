
%{
(*open Printf*)
open Ast
exception Parse_error of string

(*let arith_op_map = function
    | PLUS -> Plus
    | MINUS -> Minus
    | _ -> raise Parse_error ("Unexpected character")
    *)

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
%token PLUS MINUS STAR DSTAR SLASH DSLASH PERCENT DLT DGT AMP
%token PIPE CARET TILDE LT GT LTEQ GTEQ DEQ NOTEQ
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token COMMA COLON DOT SEMICOLON ATSYM EQ PLUSEQ MINUSEQ STAREQ SLASHEQ DSLASHEQ
%token PERCENTEQ AMPEQ PIPEEQ CARETEQ DGTEQ DLTEQ DSTAREQ ELLIPSIS
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

start:  
arith_exprs NEWLINE ENDMARKER          { Ast.Program (List.rev $1) }
|  ENDMARKER           { Ast.Program [] } 

arith_exprs: 
/*arith_exprs arith_expr {$2 :: $1}
| arith_exprs NEWLINE { $1 }
*/
arith_expr  { let Arith(t, l) = $1 in [Arith(t, List.rev l)] }

arith_expr: 
arith_expr PLUS term           { let Arith(t, l) = $1 in Arith(t, (Plus,$3)::l)}
| arith_expr MINUS term        { let Arith(t, l) = $1 in Arith(t, (Minus,$3)::l)}
| term                       {let Term(f, l) = $1 in Arith(Term(f, List.rev l), [])}

/*arith_op: PLUS { Plus }
| MINUS        { Minus }
*/
term: term term_op factor { let Term(f, l) = $1 in Term(f, ($2,$3)::l)}
| factor  { Term($1, [])}

term_op: STAR  {Star}
| SLASH         {Fslash}
| PERCENT       {Percent}
| DSLASH        {Dfslash}  

factor: factor_op factor { Uapp ($1, $2) }
| power                  { Power $1}

factor_op: PLUS { Uplus }
| MINUS  { Uminus }
| TILDE  { Utilde}

indexed:
/* indexed trailer   
| atom  { IndAtom ($1, []) } */
atom { IndAtom ($1, [])}


/*
trailer: LPAREN RPAREN
| LPAREN arglist RPAREN
| LBRACKET RBRACKET
| LBRACKET tuple_or_test RBRACKET
| DOT NAME
*/


power: indexed  {Pow_index $1 }
| indexed DSTAR factor { Pow_factor ($1, $3)}

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

finputs: finputs finput { if $2 = NEWLINE then $1 else $2 :: $1}
| finput              { $1 }     

finput: NEWLINE     { $1 }
| expr               { Exp (List.rev $1)}
*/

/*
stmts:
    stmt_item stmts {$1::$2}
    | stmt_item {$1}

    */
/*
exprs: exprs expr { $2 :: $1}
 | expr       { 

expr: expr PIPE xor_expr  { }
    | xor_expr     {    }

    xor_expr: xor_expr CARET and_expr { }
| and_expr   { And_exp $1}

and_expr: and_expr AMP shift_expr {}
| shift_expr { Shift ($1, [])}

shift_expr: shift_expr DFCHEVRON arith_expr { }
| shift_expr DBCHEVRON arith_expr { }
| arith_expr 

*/
