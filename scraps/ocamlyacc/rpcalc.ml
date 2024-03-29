type token =
  | NUM of (float)
  | PLUS
  | MINUS
  | MULTIPLY
  | DIVIDE
  | CARET
  | UMINUS
  | NEWLINE

open Parsing;;
# 6 "rpcalc.mly"
open Printf
# 15 "rpcalc.ml"
let yytransl_const = [|
  258 (* PLUS *);
  259 (* MINUS *);
  260 (* MULTIPLY *);
  261 (* DIVIDE *);
  262 (* CARET *);
  263 (* UMINUS *);
  264 (* NEWLINE *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\000\000\002\000\001\000\002\000\001\000\003\000\003\000\003\000\
\003\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\005\000\003\000\002\000\000\000\011\000\
\004\000\000\000\006\000\007\000\008\000\009\000\010\000"

let yydgoto = "\002\000\
\003\000\006\000\010\000"

let yysindex = "\001\000\
\000\000\000\000\000\255\000\000\000\000\000\000\255\254\000\000\
\000\000\008\255\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\003\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\002\000"

let yytablesize = 15
let yytable = "\004\000\
\004\000\001\000\012\000\000\000\007\000\008\000\009\000\005\000\
\004\000\011\000\012\000\013\000\014\000\015\000\008\000"

let yycheck = "\001\001\
\001\001\001\000\000\000\255\255\003\000\007\001\008\001\008\001\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  MULTIPLY\000\
  DIVIDE\000\
  CARET\000\
  UMINUS\000\
  NEWLINE\000\
  "

let yynames_block = "\
  NUM\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 19 "rpcalc.mly"
                       ( )
# 85 "rpcalc.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : unit) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'line) in
    Obj.repr(
# 20 "rpcalc.mly"
                      ( )
# 93 "rpcalc.ml"
               : unit))
; (fun __caml_parser_env ->
    Obj.repr(
# 23 "rpcalc.mly"
                   ( )
# 99 "rpcalc.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 24 "rpcalc.mly"
                       ( printf "\t%.10g\n" _1; flush stdout )
# 106 "rpcalc.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 27 "rpcalc.mly"
                ( _1 )
# 113 "rpcalc.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 28 "rpcalc.mly"
                        ( _1 +. _2 )
# 121 "rpcalc.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 29 "rpcalc.mly"
                         ( _1 -. _2 )
# 129 "rpcalc.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 30 "rpcalc.mly"
                            ( _1 *. _2 )
# 137 "rpcalc.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 31 "rpcalc.mly"
                          ( _1 /. _2 )
# 145 "rpcalc.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 33 "rpcalc.mly"
                         ( _1 ** _2 )
# 153 "rpcalc.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 35 "rpcalc.mly"
                      ( -. _1 )
# 160 "rpcalc.ml"
               : 'exp))
(* Entry input *)
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
let input (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit)
;;
# 38 "rpcalc.mly"

# 187 "rpcalc.ml"
