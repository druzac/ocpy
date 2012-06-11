
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

/* precedences: from low to high */

%left OR
%left AND
%nonassoc NOT
%left LT GT LTEQ GTEQ DEQ NOTEQ IN NOTIN IS ISNOT

%left PIPE
%left CARET
%left AMP
%left DGT DLT
%left PLUS MINUS
%left STAR SLASH PERCENT DSLASH

%start start
%type <Ast.program> start

%%  

start:  
stmts ENDMARKER					{ Ast.Prog (List.rev $1) }
|  ENDMARKER					{ Ast.Prog [] } 

/*
funcdef: DEF NAME parameters COLON suite	{ Ast.Def ($2, $3, $5)}
*/
parameters: LPAREN params RPAREN		{$2}
| LPAREN RPAREN					{[]}

params: paramlist COMMA				{List.rev $1}
| paramlist					{List.rev $1}

paramlist: paramlist COMMA NAME			{$3::$1}
| NAME						{[$1]}

/* -> stmt list */
stmts: stmts stmt				{ $2 :: $1 }
| stmt						{[$1]}

/* -> stmt */
stmt: simple_stmts_wrapper			{match $1 with 
                                                        Ast.Begin l -> Ast.Begin (List.rev l)
                                                                | _ -> $1}
/*
| compound_stmt					{$1}
*/

/* simple_stmt */
simple_stmts_wrapper: simple_stmts SEMICOLON NEWLINE {$1}
| simple_stmts NEWLINE                               {$1}

/* small_stmt -> simple_stmt (begin or single)*/
simple_stmts: simple_stmts SEMICOLON small_stmt {match $1 with Ast.Begin l -> Ast.Begin ($3::l)
                                                                    | _ -> Ast.Begin ($3::[$1])}
| small_stmt						{$1}

/* -> small_stmt */
small_stmt: 
testlist						{Ast.Expr (Ast.exprs2expr $1)}
| testlist assign_op testlist				{ Ast.Assign ($2, 
							              Ast.exprs2exprl $1,
								      Ast.exprs2exprl $3)}
  /*
| del_stmt						{$1}
| pass_stmt						{$1}
| flow_stmt						{$1}
| global_stmt						{$1}
| nonlocal_stmt						{$1}
| assert_stmt						{$1}
*/

assign_op: 
/*PLUSEQ						{ Ast.Pluseq}
| MINUSEQ						{Ast.Minuseq}
| STAREQ						{Ast.Stareq}
| SLASHEQ						{Ast.Slasheq}
| PERCENTEQ						{Ast.Percenteq}
| AMPEQ							{Ast.Ampeq}
| PIPEEQ						{Ast.Pipeeq}
| CARETEQ						{Ast.Careteq}
| DLTEQ							{Ast.Dlteq}
| DGTEQ							{Ast.Dgteq}
| DSTAREQ						{Ast.Dstareq}
| DSLASHEQ						{Ast.Dslasheq}
*/
| EQ							{Ast.Aeq}
/*
del_stmt: DEL star_expr					{Ast.Del $2}

pass_stmt: PASS						{Ast.Pass}

flow_stmt:
break_stmt						{$1}
| continue_stmt						{$1}
| return_stmt						{$1}
| raise_stmt						{$1}

break_stmt: BREAK					{Ast.Break}

continue_stmt: CONTINUE					{Ast.Continue}

return_stmt: RETURN					{ Ast.Return []}
| RETURN testlist					{Ast.Return (List.rev $2)}

raise_stmt: RAISE					{ Ast.Raise (None, None)}
| RAISE test						{ Ast.Raise (Some $2, None)}
| RAISE test FROM test					{ Ast.Raise (Some $2, Some $4)}

global_stmt: GLOBAL name_list				{Ast.Global (List.rev $2)}

nonlocal_stmt: NONLOCAL name_list			{Ast.Nonlocal (List.rev $2)}

name_list: name_list COMMA NAME				{ $3 :: $1}
| NAME							{[$1]}

assert_stmt: ASSERT testlist				{Ast.Assert (List.rev $2)}

compound_stmt: if_stmt					{$1}
| while_stmt						{$1}
| for_stmt						{$1}
| try_stmt						{$1}
| funcdef						{$1}

if_stmt: ifs_elifs ELSE COLON suite			{Ast.Cond (List.rev $1, Some $4)}
| ifs_elifs						{Ast.Cond (List.rev $1, None)}

ifs_elifs: ifs_elifs ELIF test COLON suite		{($3,$5) :: $1}
| IF test COLON suite					{ [($2,$4)] }

while_stmt: while_core ELSE COLON suite			{Ast.while_fin $1 (Some $4)}
| while_core						{Ast.while_fin $1 None}

while_core: WHILE test COLON suite			{ $2,$4}

for_stmt: for_core ELSE COLON suite			{Ast.for_fin $1 (Some $4)}
| for_core						{Ast.for_fin $1 None}

for_core: FOR NAME IN test COLON suite			{ ($2,$4,$6)}

try_stmt: try_core exc_el_finally			{Ast.try_fin $1 $2}
| try_core finally					{Ast.try_fin $1 ([],None,Some $2)}

finally: FINALLY COLON suite				{$3}

try_core: TRY COLON suite				{$3}

exc_el_finally: except_clauses el_finally		{Ast.excepts_elfin $1 $2}
| except_clauses					{Ast.excepts_elfin $1 (None, None)}

except_clauses: except_clauses except_clause_w_suite	{$2::$1}
| except_clause_w_suite					{[$1]}

except_clause_w_suite: 
except_clause COLON suite				{$1, $3}

except_clause: EXCEPT					{Ast.Except None}
| EXCEPT test						{Ast.Except (Some ($2, None))}
| EXCEPT test AS NAME					{Ast.Except (Some ($2, Some $4))}

el_finally: else_in_try fin_in_try			{Some $1, Some $2}
| fin_in_try						{None, Some $1}
| else_in_try						{Some $1, None}

else_in_try: ELSE COLON suite				{$3}

fin_in_try: FINALLY COLON suite				{$3}

suite: simple_stmts_wrapper				{Ast.Suite_single $1}
| NEWLINE INDENT stmts DEDENT				{Ast.Suite (List.rev $3)}
*/
    
/* everything below here returns an expr
except testlist, which returns a list of expressions */

test:
| bool_test IF bool_test ELSE test			{Ast.If ($1, $3, $5)}
| bool_test						{$1}
| lambdef						{$1}

lambdef: LAMBDA params COLON test			{Ast.Lambda ($2, $4)}
| LAMBDA COLON test					{Ast.Lambda ([], $3)}

bool_test:
| bool_test AND bool_test {Ast.Bapp (Ast.Band, $1, $3)}
| bool_test OR bool_test {Ast.Bapp(Ast.Bor, $1, $3)}
| NOT bool_test {Ast.Uapp (Ast.Unot, $2)}
| comparison {$1}

comparison: 
| comparison comp_op star_expr		{Ast.Bapp ($2, $1, $3)}
| star_expr						{$1}

comp_op:
| LT							{Ast.Blt}
| GT							{Ast.Bgt}
| DEQ							{Ast.Bdeq}
| GTEQ							{Ast.Bgteq}
| LTEQ							{Ast.Blteq}
| NOTEQ							{Ast.Bnoteq}
| IN							{Ast.Bin}
| NOT IN						{Ast.Bnotin}
| IS							{Ast.Bis}
| IS NOT						{Ast.Bisnot}

star_expr: STAR expr					{Ast.Star $2}
| expr							{$1}

expr:
| expr PIPE expr					{Ast.Bapp (Ast.Bpipe, $1, $3)}
| expr CARET expr				{Ast.Bapp (Ast.Bcaret, $1, $3)}
| expr AMP expr				{Ast.Bapp (Ast.Bamp, $1, $3)}
| expr DLT expr				{Ast.Bapp (Ast.Bdlt, $1, $3)}
| expr DGT expr				{Ast.Bapp (Ast.Bdgt, $1, $3)}
| expr PLUS expr					{ Ast.Bapp (Ast.Bplus, $1, $3)}
| expr MINUS expr					{ Ast.Bapp (Ast.Bminus, $1, $3)}
| expr STAR expr					{ Ast.Bapp (Ast.Bstar, $1, $3)}
| expr SLASH expr					{ Ast.Bapp (Ast.Bslash, $1, $3)}
| expr PERCENT expr					{ Ast.Bapp (Ast.Bpercent, $1, $3)}
| expr DSLASH expr					{ Ast.Bapp (Ast.Bdslash, $1, $3)}
| factor						{$1}

factor: factor_op factor				{Ast.Uapp ($1, $2) }
| power							{$1}

factor_op: PLUS						{ Ast.Uplus }
| MINUS							{ Ast.Uminus }
| TILDE							{ Ast.Utilde}

power: indexed						{$1}
| indexed DSTAR factor					{Ast.Bapp(Ast.Bdstar, $1, $3)}

indexed:
  indexed LPAREN RPAREN					{ Ast.App ($1, [])}
| indexed LPAREN testlist RPAREN			{Ast.App ($1, Ast.exprs2exprl $3)}
| indexed LBRACKET testlist RBRACKET                    {Ast.Subscript ($1, (Ast.exprs2expr $3))}
| indexed DOT NAME					{Ast.Dot ($1, $3)}
| atom							{$1}

atom: 
LPAREN RPAREN						{Ast.Tuple []}
| LPAREN testlist RPAREN				{Ast.exprs2expr $2}
| LBRACKET RBRACKET					{Ast.List []}
| LBRACKET testlist RBRACKET				{Ast.List (Ast.exprs2exprl $2)}
| LBRACE RBRACE						{Ast.Dict []}
| LBRACE dictorsetmaker RBRACE			{$2}
| NAME							{Ast.Name $1}
| NUMBER						{ Ast.Number $1}
| strings						{ Ast.String $1} 
| ELLIPSIS						{ Ast.Ellipsis}
| NONE							{Ast.None}
| TRUE							{ Ast.True}
| FALSE							{ Ast.False}

/* -> [expr list] (* in proper order *) */
/* trailing comma tells us, if we are expecting a tuple, whether to automatically tuple */
/* if no trailling comma, we tuple iff we have more than one thing */
testlist: tests COMMA					{(List.rev $1, true)}
| tests							{(List.rev $1, false)}

tests: tests COMMA test					{ $3 :: $1 }
| test							{ [$1] }

dictorsetmaker: dict					{Ast.Dict (List.rev $1)}
| testlist						{Ast.Set (Ast.exprs2exprl $1)}

dict: dict_core COMMA					{$1}
| dict_core						{$1}

dict_core: dict_core COMMA test COLON test		{($3,$5)::$1}
| test COLON test					{[($1,$3)]}

  /* very inefficient for long string sequences */
strings: strings STRING					{ $1 ^ $2 }
| STRING						{ $1 }

