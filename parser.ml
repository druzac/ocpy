type token =
  | NEWLINE
  | INDENT
  | DEDENT
  | ENDMARKER
  | INDENT_SPACE of (int)
  | FALSE
  | CLASS
  | FINALLY
  | IS
  | RETURN
  | NONE
  | CONTINUE
  | FOR
  | LAMBDA
  | TRY
  | TRUE
  | DEF
  | FROM
  | NONLOCAL
  | WHILE
  | AND
  | DEL
  | GLOBAL
  | NOT
  | WITH
  | AS
  | ELIF
  | IF
  | OR
  | YIELD
  | ASSERT
  | ELSE
  | IMPORT
  | PASS
  | BREAK
  | EXCEPT
  | IN
  | RAISE
  | PLUS
  | MINUS
  | STAR
  | DSTAR
  | SLASH
  | DSLASH
  | PERCENT
  | DBCHEVRON
  | DFCHEVRON
  | AMP
  | PIPE
  | CARET
  | TILDE
  | BCHEVRON
  | FCHEVRON
  | LTEQ
  | GTEQ
  | EQEQ
  | EXEQ
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | COMMA
  | COLON
  | DOT
  | SEMICOLON
  | ATSYM
  | EQ
  | PLUSEQ
  | MINEQ
  | STAREQ
  | SLASHEQ
  | DSLASHEQ
  | PERCENTEQ
  | AMPEQ
  | PIPEEQ
  | CARETEQ
  | DFCHEVRONEQ
  | DBCHEVRONEQ
  | DSTAREQ
  | ELLIPSIS
  | STRING of (string)
  | NAME of (string)
  | NUMBER of (int)

open Parsing;;
# 3 "parser.mly"
(*open Printf*)
# 92 "parser.ml"
let yytransl_const = [|
  257 (* NEWLINE *);
  258 (* INDENT *);
  259 (* DEDENT *);
  260 (* ENDMARKER *);
  262 (* FALSE *);
  263 (* CLASS *);
  264 (* FINALLY *);
  265 (* IS *);
  266 (* RETURN *);
  267 (* NONE *);
  268 (* CONTINUE *);
  269 (* FOR *);
  270 (* LAMBDA *);
  271 (* TRY *);
  272 (* TRUE *);
  273 (* DEF *);
  274 (* FROM *);
  275 (* NONLOCAL *);
  276 (* WHILE *);
  277 (* AND *);
  278 (* DEL *);
  279 (* GLOBAL *);
  280 (* NOT *);
  281 (* WITH *);
  282 (* AS *);
  283 (* ELIF *);
  284 (* IF *);
  285 (* OR *);
  286 (* YIELD *);
  287 (* ASSERT *);
  288 (* ELSE *);
  289 (* IMPORT *);
  290 (* PASS *);
  291 (* BREAK *);
  292 (* EXCEPT *);
  293 (* IN *);
  294 (* RAISE *);
  295 (* PLUS *);
  296 (* MINUS *);
  297 (* STAR *);
  298 (* DSTAR *);
  299 (* SLASH *);
  300 (* DSLASH *);
  301 (* PERCENT *);
  302 (* DBCHEVRON *);
  303 (* DFCHEVRON *);
  304 (* AMP *);
  305 (* PIPE *);
  306 (* CARET *);
  307 (* TILDE *);
  308 (* BCHEVRON *);
  309 (* FCHEVRON *);
  310 (* LTEQ *);
  311 (* GTEQ *);
  312 (* EQEQ *);
  313 (* EXEQ *);
  314 (* LPAREN *);
  315 (* RPAREN *);
  316 (* LBRACKET *);
  317 (* RBRACKET *);
  318 (* LBRACE *);
  319 (* RBRACE *);
  320 (* COMMA *);
  321 (* COLON *);
  322 (* DOT *);
  323 (* SEMICOLON *);
  324 (* ATSYM *);
  325 (* EQ *);
  326 (* PLUSEQ *);
  327 (* MINEQ *);
  328 (* STAREQ *);
  329 (* SLASHEQ *);
  330 (* DSLASHEQ *);
  331 (* PERCENTEQ *);
  332 (* AMPEQ *);
  333 (* PIPEEQ *);
  334 (* CARETEQ *);
  335 (* DFCHEVRONEQ *);
  336 (* DBCHEVRONEQ *);
  337 (* DSTAREQ *);
  338 (* ELLIPSIS *);
    0|]

let yytransl_block = [|
  261 (* INDENT_SPACE *);
  339 (* STRING *);
  340 (* NAME *);
  341 (* NUMBER *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\003\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\002\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\011\000\009\000\010\000\008\000\013\000\
\005\000\006\000\014\000\000\000\003\000\000\000\001\000\000\000\
\012\000\004\000"

let yydgoto = "\002\000\
\011\000\012\000\013\000\014\000"

let yysindex = "\003\000\
\252\254\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\253\254\000\000\178\254\000\000\000\255\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\255\254\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\248\255\000\000"

let yytablesize = 85
let yytable = "\003\000\
\015\000\004\000\007\000\001\000\017\000\004\000\005\000\018\000\
\000\000\000\000\005\000\006\000\000\000\000\000\000\000\006\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\016\000\000\000\007\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\007\000\008\000\009\000\
\010\000\007\000\008\000\009\000\010\000"

let yycheck = "\004\001\
\004\001\006\001\004\001\001\000\083\001\006\001\011\001\016\000\
\255\255\255\255\011\001\016\001\255\255\255\255\255\255\016\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\064\001\255\255\064\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\082\001\083\001\084\001\
\085\001\082\001\083\001\084\001\085\001"

let yynames_const = "\
  NEWLINE\000\
  INDENT\000\
  DEDENT\000\
  ENDMARKER\000\
  FALSE\000\
  CLASS\000\
  FINALLY\000\
  IS\000\
  RETURN\000\
  NONE\000\
  CONTINUE\000\
  FOR\000\
  LAMBDA\000\
  TRY\000\
  TRUE\000\
  DEF\000\
  FROM\000\
  NONLOCAL\000\
  WHILE\000\
  AND\000\
  DEL\000\
  GLOBAL\000\
  NOT\000\
  WITH\000\
  AS\000\
  ELIF\000\
  IF\000\
  OR\000\
  YIELD\000\
  ASSERT\000\
  ELSE\000\
  IMPORT\000\
  PASS\000\
  BREAK\000\
  EXCEPT\000\
  IN\000\
  RAISE\000\
  PLUS\000\
  MINUS\000\
  STAR\000\
  DSTAR\000\
  SLASH\000\
  DSLASH\000\
  PERCENT\000\
  DBCHEVRON\000\
  DFCHEVRON\000\
  AMP\000\
  PIPE\000\
  CARET\000\
  TILDE\000\
  BCHEVRON\000\
  FCHEVRON\000\
  LTEQ\000\
  GTEQ\000\
  EQEQ\000\
  EXEQ\000\
  LPAREN\000\
  RPAREN\000\
  LBRACKET\000\
  RBRACKET\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  COLON\000\
  DOT\000\
  SEMICOLON\000\
  ATSYM\000\
  EQ\000\
  PLUSEQ\000\
  MINEQ\000\
  STAREQ\000\
  SLASHEQ\000\
  DSLASHEQ\000\
  PERCENTEQ\000\
  AMPEQ\000\
  PIPEEQ\000\
  CARETEQ\000\
  DFCHEVRONEQ\000\
  DBCHEVRONEQ\000\
  DSTAREQ\000\
  ELLIPSIS\000\
  "

let yynames_block = "\
  INDENT_SPACE\000\
  STRING\000\
  NAME\000\
  NUMBER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atoms) in
    Obj.repr(
# 41 "parser.mly"
                         ( Ast.Program (List.rev _1) )
# 338 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "parser.mly"
                              ( Ast.Program [] )
# 344 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 50 "parser.mly"
                 ( [_1] )
# 351 "parser.ml"
               : 'atoms))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atoms) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 51 "parser.mly"
                        ( _3::_1)
# 359 "parser.ml"
               : 'atoms))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "parser.mly"
               (Ast.Name _1)
# 366 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 65 "parser.mly"
                ( Ast.Number _1)
# 373 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'strings) in
    Obj.repr(
# 66 "parser.mly"
               ( Ast.String _1)
# 380 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
                ( Ast.Ellipsis)
# 386 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
                (Ast.None)
# 392 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
                ( Ast.True)
# 398 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
                ( Ast.False)
# 404 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'strings) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 72 "parser.mly"
                        ( _1 ^ _2 )
# 412 "parser.ml"
               : 'strings))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 73 "parser.mly"
          ( _1 )
# 419 "parser.ml"
               : 'strings))
(* Entry start *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let start (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
