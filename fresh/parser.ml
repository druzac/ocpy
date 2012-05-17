type token =
  | NEWLINE
  | INDENT
  | DEDENT
  | ENDMARKER
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
# 91 "parser.ml"
let yytransl_const = [|
  257 (* NEWLINE *);
  258 (* INDENT *);
  259 (* DEDENT *);
  260 (* ENDMARKER *);
  261 (* FALSE *);
  262 (* CLASS *);
  263 (* FINALLY *);
  264 (* IS *);
  265 (* RETURN *);
  266 (* NONE *);
  267 (* CONTINUE *);
  268 (* FOR *);
  269 (* LAMBDA *);
  270 (* TRY *);
  271 (* TRUE *);
  272 (* DEF *);
  273 (* FROM *);
  274 (* NONLOCAL *);
  275 (* WHILE *);
  276 (* AND *);
  277 (* DEL *);
  278 (* GLOBAL *);
  279 (* NOT *);
  280 (* WITH *);
  281 (* AS *);
  282 (* ELIF *);
  283 (* IF *);
  284 (* OR *);
  285 (* YIELD *);
  286 (* ASSERT *);
  287 (* ELSE *);
  288 (* IMPORT *);
  289 (* PASS *);
  290 (* BREAK *);
  291 (* EXCEPT *);
  292 (* IN *);
  293 (* RAISE *);
  294 (* PLUS *);
  295 (* MINUS *);
  296 (* STAR *);
  297 (* DSTAR *);
  298 (* SLASH *);
  299 (* DSLASH *);
  300 (* PERCENT *);
  301 (* DBCHEVRON *);
  302 (* DFCHEVRON *);
  303 (* AMP *);
  304 (* PIPE *);
  305 (* CARET *);
  306 (* TILDE *);
  307 (* BCHEVRON *);
  308 (* FCHEVRON *);
  309 (* LTEQ *);
  310 (* GTEQ *);
  311 (* EQEQ *);
  312 (* EXEQ *);
  313 (* LPAREN *);
  314 (* RPAREN *);
  315 (* LBRACKET *);
  316 (* RBRACKET *);
  317 (* LBRACE *);
  318 (* RBRACE *);
  319 (* COMMA *);
  320 (* COLON *);
  321 (* DOT *);
  322 (* SEMICOLON *);
  323 (* ATSYM *);
  324 (* EQ *);
  325 (* PLUSEQ *);
  326 (* MINEQ *);
  327 (* STAREQ *);
  328 (* SLASHEQ *);
  329 (* DSLASHEQ *);
  330 (* PERCENTEQ *);
  331 (* AMPEQ *);
  332 (* PIPEEQ *);
  333 (* CARETEQ *);
  334 (* DFCHEVRONEQ *);
  335 (* DBCHEVRONEQ *);
  336 (* DSTAREQ *);
  337 (* ELLIPSIS *);
    0|]

let yytransl_block = [|
  338 (* STRING *);
  339 (* NAME *);
  340 (* NUMBER *);
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
\000\000\000\000\000\000\254\254\000\000\181\254\000\000\000\255\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\255\254\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\248\255\000\000"

let yytablesize = 84
let yytable = "\003\000\
\004\000\015\000\007\000\001\000\004\000\005\000\017\000\018\000\
\000\000\005\000\006\000\000\000\000\000\000\000\006\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\016\000\007\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\007\000\008\000\009\000\010\000\
\007\000\008\000\009\000\010\000"

let yycheck = "\004\001\
\005\001\004\001\004\001\001\000\005\001\010\001\082\001\016\000\
\255\255\010\001\015\001\255\255\255\255\255\255\015\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\063\001\063\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\081\001\082\001\083\001\084\001\
\081\001\082\001\083\001\084\001"

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
  STRING\000\
  NAME\000\
  NUMBER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atoms) in
    Obj.repr(
# 39 "parser.mly"
                         ( Ast.Program (List.rev _1) )
# 335 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                              ( Ast.Program [] )
# 341 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 48 "parser.mly"
                 ( [_1] )
# 348 "parser.ml"
               : 'atoms))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atoms) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 49 "parser.mly"
                        ( _3::_1)
# 356 "parser.ml"
               : 'atoms))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "parser.mly"
               (Ast.Name _1)
# 363 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 63 "parser.mly"
                ( Ast.Number _1)
# 370 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'strings) in
    Obj.repr(
# 64 "parser.mly"
               ( Ast.String _1)
# 377 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
                ( Ast.Ellipsis)
# 383 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
                (Ast.None)
# 389 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
                ( Ast.True)
# 395 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
                ( Ast.False)
# 401 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'strings) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 70 "parser.mly"
                        ( _1 ^ _2 )
# 409 "parser.ml"
               : 'strings))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 71 "parser.mly"
          ( _1 )
# 416 "parser.ml"
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
