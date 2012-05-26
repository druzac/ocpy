
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
stmts ENDMARKER          { Ast.Program (List.rev $1) }
|  ENDMARKER           { Ast.Program [] } 

stmts: stmts stmt  { $2 :: $1 }
| stmt {[$1]}

stmt: simple_stmt_wrapper { Smpl_stmt $1}

simple_stmt_wrapper: simple_stmt SEMICOLON NEWLINE { $1 }
| simple_stmt NEWLINE                              {$1}

simple_stmt: small_stmt { Single $1 }

small_stmt: expr_stmt { Expr_stmt $1 }


/* the second rule is supposed to be a tuple_or_test, but those look
exactly like testlists to me */

expr_stmt: testlist assign_op tuple_or_test {Assignment ($2, $1, $3)}
| tuple_or_test                        {Expr $1}

tuple_or_test: t_o_t COMMA { $1 }
| t_o_t                    { $1}

t_o_t: t_o_t COMMA test  { match $1 with Test t -> Tuple ($3 ::[t]) | Tuple l ->Tuple ($3 :: l)}
| test                   { Test $1}

assign_op: PLUSEQ       { Pluseq}
| MINUSEQ      {Minuseq}
| STAREQ      {Stareq}
| SLASHEQ      {Slasheq}
| PERCENTEQ      {Percenteq}
| AMPEQ      {Ampeq}
| PIPEEQ      {Pipeeq}
| CARETEQ      {Careteq}
| DLTEQ      {Dlteq}
| DGTEQ      {Dgteq}
| DSTAREQ      {Dstareq}
| DSLASHEQ      {Dslasheq}
| EQ            {Eq}

testlist: testseq COMMA { $1 }
| testseq {$1}

testseq: testseq COMMA test { $3 :: $1 }
| test                      { [$1] }

test: or_test IF or_test ELSE test { If_test ($1, $3, $5)}
| or_test                          { Or_test $1}
/* add lambdadef here */

or_test: or_test OR and_test { let Or(l) = $1 in Or($3::l) }
| and_test      { Or[$1] }

and_test: and_test AND not_test {let And(l) = $1 in And($3::l)}
| not_test {And([$1])}

not_test: NOT not_test        {Not $2}
| comparison                    {Comp $1}

comparison: comparison comp_op star_expr  { let Cmp_cmp(stexp, l) = $1 in Cmp_cmp(stexp, ($2, $3)::l)}
| star_expr                               {Cmp_cmp($1, [])}

/* python doesn't have <> ? */
comp_op:
LT          {Lt}
| GT        {Gt}
| DEQ       {Eqeq}
| GTEQ      {Gteq}
| LTEQ      {Lteq}
| NOTEQ     {Noteq}
| IN        {In}
| NOT IN    {Notin}
| IS        {Is}
| IS NOT    {Isnot}

star_expr: STAR expr   {Sexp_sexp $2}
| expr                {Sexp_exp $1}

expr: expr PIPE xor_expr    {let Exp(l) = $1 in Exp($3::l)}
|xor_expr                   {Exp([$1]) }

xor_expr: xor_expr CARET and_expr   { let Xor_exp(l) = $1 in Xor_exp($3::l)}
| and_expr                          { Xor_exp([$1])}

and_expr: and_expr AMP shift_expr { let And_exp(l) = $1 in And_exp($3::l)}
| shift_expr                      {And_exp([$1])}

shift_expr: shift_expr shift_op arith_expr  {let Shift(a, l) = $1 in Shift(a, ($2,$3)::l)}
| arith_expr                                {Shift($1, [])}

shift_op: DLT       {Dlt}
| DGT               {Dgt}

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
| power                  { F_Pow $1}

factor_op: PLUS { Uplus }
| MINUS  { Uminus }
| TILDE  { Utilde}

indexed:
indexed trailer             {match $1 with Atom a -> Power (a, $2) | Power (a, l) -> (a, $2::l)}
| atom                      {Atom $1}

trailer: LPAREN RPAREN      { Called []}
| LPAREN arglist RPAREN     {Called $2}
| LBRACKET RBRACKET         {Subscript (Tuple [])}
| LBRACKET tuple_or_test RBRACKET   {Subscript $2} 
| DOT NAME                          {Dot $2}

power: indexed  {Pow_index $1 }
| indexed DSTAR factor { Pow_factor ($1, $3)}

atom: 
LPAREN RPAREN                   {A_tot (Tuple [])}
| LPAREN arglist RPAREN         {A_tot $2}
| LBRACKET RBRACKET             {A_testlist []}
| LBRACKET testlist RBRACKET    {A_testlist $2}
| LBRACE RBRACE                 {A_d
| LBRACE dictorsetmaker RBRACE
| NAME          {Ast.Name $1}
| NUMBER        { Ast.Number $1}
| strings      { Ast.String $1} 
| ELLIPSIS      { Ast.Ellipsis}
| NONE          {Ast.None}
| TRUE          { Ast.True}
| FALSE         { Ast.False}

strings: strings STRING { $1 ^ $2 }
| STRING  { $1 }
