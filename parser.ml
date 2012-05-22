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
  | DLT
  | DGT
  | AMP
  | PIPE
  | CARET
  | TILDE
  | LT
  | GT
  | LTEQ
  | GTEQ
  | DEQ
  | NOTEQ
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
  | MINUSEQ
  | STAREQ
  | SLASHEQ
  | DSLASHEQ
  | PERCENTEQ
  | AMPEQ
  | PIPEEQ
  | CARETEQ
  | DGTEQ
  | DLTEQ
  | DSTAREQ
  | ELLIPSIS
  | STRING of (string)
  | NAME of (string)
  | NUMBER of (int)

open Parsing;;
# 3 "parser.mly"
(*open Printf*)
open Ast
exception Parse_error of string

(*let arith_op_map = function
    | PLUS -> Plus
    | MINUS -> Minus
    | _ -> raise Parse_error ("Unexpected character")
    *)

# 101 "parser.ml"
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
  302 (* DLT *);
  303 (* DGT *);
  304 (* AMP *);
  305 (* PIPE *);
  306 (* CARET *);
  307 (* TILDE *);
  308 (* LT *);
  309 (* GT *);
  310 (* LTEQ *);
  311 (* GTEQ *);
  312 (* DEQ *);
  313 (* NOTEQ *);
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
  327 (* MINUSEQ *);
  328 (* STAREQ *);
  329 (* SLASHEQ *);
  330 (* DSLASHEQ *);
  331 (* PERCENTEQ *);
  332 (* AMPEQ *);
  333 (* PIPEEQ *);
  334 (* CARETEQ *);
  335 (* DGTEQ *);
  336 (* DLTEQ *);
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
\001\000\001\000\002\000\003\000\003\000\003\000\004\000\004\000\
\005\000\005\000\005\000\005\000\006\000\006\000\007\000\007\000\
\007\000\009\000\008\000\008\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\011\000\011\000\000\000"

let yylen = "\002\000\
\003\000\001\000\001\000\003\000\003\000\001\000\003\000\001\000\
\001\000\001\000\001\000\001\000\002\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\002\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\027\000\025\000\026\000\015\000\016\000\
\017\000\024\000\029\000\021\000\022\000\030\000\000\000\000\000\
\000\000\008\000\000\000\014\000\000\000\018\000\000\000\000\000\
\000\000\000\000\009\000\010\000\012\000\011\000\000\000\013\000\
\000\000\028\000\001\000\000\000\000\000\007\000\020\000"

let yydgoto = "\002\000\
\014\000\015\000\016\000\017\000\031\000\018\000\019\000\020\000\
\021\000\022\000\023\000"

let yysindex = "\003\000\
\252\254\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\018\255\225\254\
\235\254\000\000\000\255\000\000\239\254\000\000\199\254\023\255\
\000\255\000\255\000\000\000\000\000\000\000\000\000\255\000\000\
\000\255\000\000\000\000\235\254\235\254\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\027\255\
\002\255\000\000\000\000\000\000\020\255\000\000\013\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\255\009\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\248\255\000\000\238\255\000\000\000\000\
\000\000\000\000\000\000"

let yytablesize = 85
let yytable = "\003\000\
\032\000\004\000\006\000\001\000\004\000\004\000\005\000\025\000\
\026\000\005\000\005\000\006\000\038\000\023\000\039\000\006\000\
\036\000\037\000\024\000\027\000\019\000\028\000\029\000\030\000\
\033\000\034\000\035\000\003\000\000\000\000\000\000\000\000\000\
\000\000\000\000\007\000\008\000\000\000\000\000\007\000\008\000\
\006\000\006\000\004\000\004\000\000\000\000\000\009\000\005\000\
\005\000\000\000\009\000\023\000\023\000\023\000\023\000\023\000\
\023\000\023\000\019\000\019\000\019\000\000\000\019\000\019\000\
\019\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\010\000\011\000\012\000\
\013\000\010\000\011\000\012\000\013\000"

let yycheck = "\004\001\
\019\000\006\001\001\001\001\000\001\001\006\001\011\001\039\001\
\040\001\001\001\011\001\016\001\031\000\001\001\033\000\016\001\
\025\000\026\000\001\001\041\001\001\001\043\001\044\001\045\001\
\042\001\083\001\004\001\001\001\255\255\255\255\255\255\255\255\
\255\255\255\255\039\001\040\001\255\255\255\255\039\001\040\001\
\039\001\040\001\039\001\040\001\255\255\255\255\051\001\039\001\
\040\001\255\255\051\001\039\001\040\001\041\001\042\001\043\001\
\044\001\045\001\039\001\040\001\041\001\255\255\043\001\044\001\
\045\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
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
  DLT\000\
  DGT\000\
  AMP\000\
  PIPE\000\
  CARET\000\
  TILDE\000\
  LT\000\
  GT\000\
  LTEQ\000\
  GTEQ\000\
  DEQ\000\
  NOTEQ\000\
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
  MINUSEQ\000\
  STAREQ\000\
  SLASHEQ\000\
  DSLASHEQ\000\
  PERCENTEQ\000\
  AMPEQ\000\
  PIPEEQ\000\
  CARETEQ\000\
  DGTEQ\000\
  DLTEQ\000\
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
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_exprs) in
    Obj.repr(
# 50 "parser.mly"
                                       ( Ast.Program (List.rev _1) )
# 359 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
                       ( Ast.Program [] )
# 365 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 57 "parser.mly"
            ( let Arith(t, l) = _1 in [Arith(t, List.rev l)] )
# 372 "parser.ml"
               : 'arith_exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 60 "parser.mly"
                               ( let Arith(t, l) = _1 in Arith(t, (Plus,_3)::l))
# 380 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 61 "parser.mly"
                               ( let Arith(t, l) = _1 in Arith(t, (Minus,_3)::l))
# 388 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 62 "parser.mly"
                             (let Term(f, l) = _1 in Arith(Term(f, List.rev l), []))
# 395 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term_op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 67 "parser.mly"
                          ( let Term(f, l) = _1 in Term(f, (_2,_3)::l))
# 404 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 68 "parser.mly"
          ( Term(_1, []))
# 411 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
               (Star)
# 417 "parser.ml"
               : 'term_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
                (Fslash)
# 423 "parser.ml"
               : 'term_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                (Percent)
# 429 "parser.ml"
               : 'term_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
                (Dfslash)
# 435 "parser.ml"
               : 'term_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'factor_op) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 75 "parser.mly"
                         ( Uapp (_1, _2) )
# 443 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'power) in
    Obj.repr(
# 76 "parser.mly"
                         ( Power _1)
# 450 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
                ( Uplus )
# 456 "parser.ml"
               : 'factor_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
         ( Uminus )
# 462 "parser.ml"
               : 'factor_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
         ( Utilde)
# 468 "parser.ml"
               : 'factor_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 85 "parser.mly"
     ( IndAtom (_1, []))
# 475 "parser.ml"
               : 'indexed))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'indexed) in
    Obj.repr(
# 97 "parser.mly"
                (Pow_index _1 )
# 482 "parser.ml"
               : 'power))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'indexed) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 98 "parser.mly"
                       ( Pow_factor (_1, _3))
# 490 "parser.ml"
               : 'power))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 109 "parser.mly"
               (Ast.Name _1)
# 497 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 110 "parser.mly"
                ( Ast.Number _1)
# 504 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'strings) in
    Obj.repr(
# 111 "parser.mly"
               ( Ast.String _1)
# 511 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
                ( Ast.Ellipsis)
# 517 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
                (Ast.None)
# 523 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser.mly"
                ( Ast.True)
# 529 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser.mly"
                ( Ast.False)
# 535 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'strings) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 117 "parser.mly"
                        ( _1 ^ _2 )
# 543 "parser.ml"
               : 'strings))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 118 "parser.mly"
          ( _1 )
# 550 "parser.ml"
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
