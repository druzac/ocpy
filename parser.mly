
%{
(*open Printf*)
exception Parse_error of string
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

funcdef: DEF NAME parameters COLON suite  { Ast.Def ($2, $3, $5)}

parameters: LPAREN params RPAREN    { List.rev $2}
| LPAREN RPAREN                        { []}

params: paramlist COMMA             {$1}
| paramlist                         {$1}

paramlist: paramlist COMMA NAME     {$3::$1}
| NAME                              {[$1]}

/* -> stmt list */
stmts: stmts stmt  { $2 :: $1 }
| stmt {[$1]}

/* -> stmt */
stmt: simple_stmts_wrapper { Ast.Simple $1}
| compound_stmt           {$1}

/* simple_stmt */
simple_stmts_wrapper: simple_stmts SEMICOLON NEWLINE { Ast.smpl_stmt_fin $1 }
| simple_stmts NEWLINE                               {Ast.smpl_stmt_fin $1}

/* small_stmt -> simple_stmt (begin or single)*/
/* doesn't use the first bit! we drop everything but the last statement */
simple_stmts: simple_stmts SEMICOLON small_stmt { Ast.smpl_stmt_add $3 $1}
| small_stmt {Ast.Single $1}

/* -> small_stmt */
small_stmt: expr_stmt { $1 }
| del_stmt  {$1}
| pass_stmt {$1}
| flow_stmt{$1}
| global_stmt{$1}
| nonlocal_stmt{$1}
| assert_stmt{$1}

expr_stmt: testlist assign_op testlist {Ast.Assignment ($2, List.rev $1, (Ast.testlist_to_tot $3))}
| testlist                        {Ast.Expr (Ast.testlist_to_tot $1)}

assign_op: PLUSEQ       { Ast.Pluseq}
| MINUSEQ      {Ast.Minuseq}
| STAREQ      {Ast.Stareq}
| SLASHEQ      {Ast.Slasheq}
| PERCENTEQ      {Ast.Percenteq}
| AMPEQ      {Ast.Ampeq}
| PIPEEQ      {Ast.Pipeeq}
| CARETEQ      {Ast.Careteq}
| DLTEQ      {Ast.Dlteq}
| DGTEQ      {Ast.Dgteq}
| DSTAREQ      {Ast.Dstareq}
| DSLASHEQ      {Ast.Dslasheq}
| EQ            {Ast.Eq}

del_stmt: DEL star_expr  {Ast.Del $2}

pass_stmt: PASS   {Ast.Pass}

/* -> small_stmt */
flow_stmt:
break_stmt{$1}
| continue_stmt{$1}
| return_stmt{$1}
| raise_stmt{$1}

break_stmt: BREAK  {Ast.Break}

continue_stmt: CONTINUE {Ast.Continue}

return_stmt: RETURN     { Ast.Return []}
| RETURN testlist       {Ast.Return (List.rev $2)}

raise_stmt: RAISE       { Ast.Raise (None, None)}
| RAISE test            { Ast.Raise (Some $2, None)}
| RAISE test FROM test  { Ast.Raise (Some $2, Some $4)}

global_stmt: GLOBAL name_list {Ast.Global (List.rev $2)}

nonlocal_stmt: NONLOCAL name_list {Ast.Nonlocal (List.rev $2)}

name_list: name_list COMMA NAME { $3 :: $1}
| NAME                          {[$1]}

assert_stmt: ASSERT testlist   {Ast.Assert (List.rev $2)}

/* -> stmt */
compound_stmt: if_stmt  {$1}
| while_stmt {$1}
| for_stmt {$1}
| try_stmt {$1}
| funcdef {$1}

/* This is all dummy stuff, needs to be finished */
if_stmt: ifs_elifs ELSE COLON suite  {Ast.Cond (List.rev $1, Some $4)}
| ifs_elifs {Ast.Cond (List.rev $1, None)}

ifs_elifs: ifs_elifs ELIF test COLON suite  {($3,$5) :: $1}
| IF test COLON suite                       { [($2,$4)] }

while_stmt: while_core ELSE COLON suite     {Ast.while_fin $1 (Some $4)}
| while_core                                {Ast.while_fin $1 None}

while_core: WHILE test COLON suite          { $2,$4}

for_stmt: for_core ELSE COLON suite         {Ast.for_fin $1 (Some $4)}
| for_core                                  {Ast.for_fin $1 None}

for_core: FOR NAME IN test COLON suite      { ($2,$4,$6)}


/* for now, just return the suite part */
try_stmt: try_core exc_el_finally           {Ast.try_fin $1 $2}
| try_core finally                          {Ast.try_fin $1 ([],None,Some $2)}

finally: FINALLY COLON suite                {$3}

/* returns the suite part of the try block */
try_core: TRY COLON suite                   {$3}

/* except clauses returns the (catch * suite) list part,
  el_finally returns the (suite option) * (suite option) */
exc_el_finally: except_clauses el_finally   {Ast.excepts_elfin $1 $2}
| except_clauses                            {Ast.excepts_elfin $1 (None, None)}


except_clauses: except_clauses except_clause_w_suite  {$2::$1}
| except_clause_w_suite                     {[$1]}

except_clause_w_suite: 
except_clause COLON suite                   {$1, $3}

/* -> catch */
except_clause: EXCEPT                        {Ast.Except None}
| EXCEPT test                                {Ast.Except (Some ($2, None))}
| EXCEPT test AS NAME                        {Ast.Except (Some ($2, Some $4))}

el_finally: else_in_try fin_in_try           {Some $1, Some $2}
| fin_in_try                                 {None, Some $1}
| else_in_try                                {Some $1, None}

else_in_try: ELSE COLON suite                {$3}

fin_in_try: FINALLY COLON suite              {$3}

/* since a suite must be followed by a... wait, must it?
* I don't remember. Maybe it's fine */
suite: simple_stmts_wrapper  {Ast.Suite_single $1}
| NEWLINE INDENT stmts DEDENT {Ast.Suite (List.rev $3)}

/* the second rule is supposed to be a tuple_or_test, but those look
exactly like testlists to me */

test: or_test IF or_test ELSE test {Ast.If_test (Ast.lor_fin $1, Ast.lor_fin $3, $5)}
| or_test                          {Ast.Or_test (Ast.lor_fin $1)}
| lambdef                          {$1}

lambdef: LAMBDA paramlist COLON test {Ast.Lambda ((List.rev $2), $4)}
| LAMBDA COLON test                  {Ast.Lambda ([], $3)}

or_test: or_test OR and_test {Ast.lor_add $3 $1}
| and_test      { Ast.lor_new $1}


and_test: and_test AND not_test {Ast.land_add $3 $1}
| not_test {Ast.land_new $1}

/* -> not_test */
not_test: NOT not_test        {Ast.Not $2}
| comparison                    {Ast.comp_fin $1}

/* -> not_test */
comparison: comparison comp_op star_expr  {Ast.comp_add $2 $3 $1}
| star_expr                               {Ast.comp_new $1}

/* python doesn't have <> ? */
comp_op:
LT          {Ast.Lt}
| GT        {Ast.Gt}
| DEQ       {Ast.Eqeq}
| GTEQ      {Ast.Gteq}
| LTEQ      {Ast.Lteq}
| NOTEQ     {Ast.Noteq}
| IN        {Ast.In}
| NOT IN    {Ast.Notin}
| IS        {Ast.Is}
| IS NOT    {Ast.Isnot}

star_expr: STAR expr   {Ast.Star_sexp (Ast.e_fin $2)}
| expr                {Ast.Star_exp (Ast.e_fin $1)}

expr: expr PIPE xor_expr    {Ast.e_add $3 $1}
|xor_expr                   {Ast.e_new $1}

xor_expr: xor_expr CARET and_expr   {Ast.xor_add $3 $1}
| and_expr                          { Ast.xor_new $1}

and_expr: and_expr AMP shift_expr { Ast.and_add $3 $1}
| shift_expr                      {Ast.and_new $1}

shift_expr: shift_expr shift_op arith_expr  {Ast.shift_add $2 $3 $1}
| arith_expr                                {Ast.shift_new $1}

shift_op: DLT       {Ast.Dlt}
| DGT               {Ast.Dgt}

arith_expr: 
arith_expr arith_op term     { Ast.arith_add $2 $3 $1}
| term                       {Ast.arith_new $1}

arith_op:
PLUS                     {Ast.Plus}
| MINUS                  {Ast.Minus}

term: term term_op factor { Ast.term_add $2 $3 $1}
| factor  { Ast.Term($1, [])}

term_op: STAR  {Ast.Star}
| SLASH         {Ast.Fslash}
| PERCENT       {Ast.Percent}
| DSLASH        {Ast.Dfslash}  

factor: factor_op factor { Ast.Uapp ($1, $2) }
| power                  { Ast.Fact_single $1}

factor_op: PLUS { Ast.Uplus }
| MINUS  { Ast.Uminus }
| TILDE  { Ast.Utilde}

indexed:
indexed trailer             {Ast.indexed_add $2 $1}
| atom                      {Ast.Atom $1}

power: indexed  {Ast.Pow_single (Ast.indexed_fin $1) }
| indexed DSTAR factor { Ast.Power (Ast.indexed_fin $1, $3)}

/* -> atom */
atom: 
LPAREN RPAREN                   {Ast.Empty_tuple}
| LPAREN testlist RPAREN   {Ast.Tot (Ast.testlist_to_tot $2)}
| LBRACKET RBRACKET             {Ast.List []}
| LBRACKET testlist RBRACKET    {Ast.List (List.rev $2)}
| LBRACE RBRACE                 {Ast.Dict []}
| LBRACE dictorsetmaker RBRACE  {$2}
| NAME          {Ast.Name $1}
| NUMBER        { Ast.Number $1}
| strings      { Ast.String $1} 
| ELLIPSIS      { Ast.Ellipsis}
| NONE          {Ast.None}
| TRUE          { Ast.True}
| FALSE         { Ast.False}

trailer: LPAREN RPAREN      { Ast.Called []}
| LPAREN testlist RPAREN     {Ast.Called (List.rev $2)}
| LBRACKET RBRACKET         {Ast.Subscript (Ast.Tuple [])}
| LBRACKET testlist RBRACKET   {Ast.Subscript (Ast.testlist_to_tot $2)} 
| DOT NAME                          {Ast.Dot $2}

testlist: tests COMMA { $1 }
| tests {$1}

tests: tests COMMA test { $3 :: $1 }
| test                      { [$1] }

/*tuple_or_test: tuple_or_test_core COMMA { $1 }
| tuple_or_test_core                    { $1}

tuple_or_test_core: tuple_or_test_core COMMA test  { Ast.tot_add $3 $1}
| test                   { Ast.Test $1}
*/

dictorsetmaker: dict      { Ast.Dict (List.rev $1)}
| set                     {Ast.Set (List.rev $1)}

dict: dict_core COMMA     {$1}
| dict_core               {$1}

dict_core: dict_core COMMA test COLON test   {Ast.dict_add $3 $5 $1}
| test COLON test                            {Ast.dict_add $1 $3 []}

set: testlist                    {$1}


  /* very inefficient for long string sequences */
strings: strings STRING { $1 ^ $2 }
| STRING  { $1 }

