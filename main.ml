let get_lexer () = Lexer.gen_lexer ()

let parse_file f =
  let lexer = Lexer.gen_lexer () 
  and lexbuf = Lexing.from_channel (open_in f) in
    Parser.start lexer lexbuf

let parse () =
  let lexer = Lexer.gen_lexer () in
  let lexbuf = Lexing.from_channel stdin in
    Parser.start lexer lexbuf

let parse_string s =
  Parser.start (get_lexer ()) (Lexing.from_string s)

let lex lexbuf =
  let lexer = get_lexer () in
  let next_token () = lexer lexbuf in
  let rec get_tokens lst = 
    let next = next_token () in
      match next with 
	  Parser.ENDMARKER -> List.rev (next::lst)
	| _ -> get_tokens (next::lst)
  in
    get_tokens []
  
let lex_str s =
  lex (Lexing.from_string s)

(*let lex_file f =
  let lexer = Lexer.gen_lexer () in
  Lexer.line_start (Lexing.from_channel stdin)
*)

(*let ast = parse ();;*)

(*Ast.print_ast ast;;*)

(*let _ = Ast.print_sexp (Ast.get_sexp ast_tree)*)
(*let _ = print_string "\n"*)
(*
let printer = function
  | INDENT -> print_string "(INDENT)\n"
  | DEDENT -> print_string "(DEDENT)\n"
  | NEWLINE -> print_string "(NEWLINE)\n"
  | NUMBER (n) -> printf "(NUMBER %d)\n" n
  | STRING (s) -> printf "(STRING %s)\n" s
  | NAME (s) -> printf "(ID %s)\n" s
  | ENDMARKER -> printf "(ENDMARKER)\n"
  | FALSE -> printf "(FALSE)\n"
  | CLASS -> printf "(CLASS)\n"
  | FINALLY -> printf "(FINALLY)\n"
  | IS -> printf "(IS)\n"
  | RETURN -> printf "(RETURN)\n"
  | NONE -> printf "(NONE)\n"
  | CONTINUE -> printf "(CONTINUE)\n"
  | FOR -> printf "(FOR)\n"
  | LAMBDA -> printf "(LAMBDA)\n"
  | TRY -> printf "(TRY)\n"
  | TRUE -> printf "(TRUE)\n"
  | DEF -> printf "(DEF)\n"
  | FROM -> printf "(FROM)\n"
  | NONLOCAL -> printf "(NONLOCAL)\n"
  | WHILE -> printf "(WHILE)\n"
  | AND -> printf "(AND)\n"
  | DEL -> printf "(DEL)\n"
  | GLOBAL -> printf "(GLOBAL)\n"
  | NOT -> printf "(NOT)\n"
  | WITH -> printf "(WITH)\n"
  | AS -> printf "(AS)\n"
  | ELIF -> printf "(ELIF)\n"
  | IF -> printf "(IF)\n"
  | OR -> printf "(OR)\n"
  | YIELD -> printf "(YIELD)\n"
  | ASSERT -> printf "(ASSERT)\n"
  | ELSE -> printf "(ELSE)\n"
  | IMPORT -> printf "(IMPORT)\n"
  | PASS -> printf "(PASS)\n"
  | BREAK -> printf "(BREAK)\n"
  | EXCEPT -> printf "(EXCEPT)\n"
  | IN -> printf "(IN)\n"
  | RAISE -> printf "(RAISE)\n"
  | PLUS -> printf "(PLUS)\n"
  | MINUS -> printf "(MINUS)\n"
  | STAR -> printf "(STAR)\n"
  | DSTAR -> printf "(DSTAR)\n"
  | SLASH -> printf "(SLASH)\n"
  | DSLASH -> printf "(DSLASH)\n"
  | PERCENT -> printf "(PERCENT)\n"
  | DLT -> printf "(DLT)\n"
  | DGT -> printf "(DGT)\n"
  | AMP -> printf "(AMP)\n"
  | PIPE -> printf "(PIPE)\n"
  | CARET -> printf "(CARET)\n"
  | TILDE -> printf "(TILDE)\n"
  | LT -> printf "(LT)\n"
  | GT -> printf "(GT)\n"
  | LTEQ -> printf "(LTEQ)\n"
  | GTEQ -> printf "(GTEQ)\n"
  | DEQ -> printf "(DEQ)\n"
  | NOTEQ -> printf "(NOTEQ)\n"
  | LPAREN -> printf "(LPAREN)\n"
  | RPAREN -> printf "(RPAREN)\n"
  | LBRACKET -> printf "(LBRACKET)\n"
  | RBRACKET -> printf "(RBRACKET)\n"
  | LBRACE -> printf "(LBRACE)\n"
  | RBRACE -> printf "(RBRACE)\n"
  | COMMA -> printf "(COMMA)\n"
  | COLON -> printf "(COLON)\n"
  | DOT -> printf "(DOT)\n"
  | SEMICOLON -> printf "(SEMICOLON)\n"
  | ATSYM -> printf "(ATSYM)\n"
  | EQ -> printf "(EQ)\n"
  | PLUSEQ -> printf "(PLUSEQ)\n"
  | MINUSEQ -> printf "(MINUSEQ)\n"
  | STAREQ -> printf "(STAREQ)\n"
  | SLASHEQ -> printf "(SLASHEQ)\n"
  | DSLASHEQ -> printf "(DSLASHEQ)\n"
  | PERCENTEQ -> printf "(PERCENTEQ)\n"
  | AMPEQ -> printf "(AMPEQ)\n"
  | PIPEEQ -> printf "(PIPEEQ)\n"
  | CARETEQ -> printf "(CARETEQ)\n"
  | DGTEQ -> printf "(DGTEQ)\n"
  | DLTEQ -> printf "(DLTEQ)\n"
  | DSTAREQ -> printf "(DSTAREQ)\n"
  | ELLIPSIS -> printf "(ELLIPSIS)\n"
  | _ -> print_string "I don't know...\n"
      *)

(*let lexer = Lexer.gen_lexer ();;*)

(*let rec parse_then_print lexbuf =
  let result = lexer lexbuf in
    printer result;
    if result = ENDMARKER then 
      ()
    else
      parse_then_print lexbuf



let main () =
  let cin = 
    if Array.length Sys.argv > 1 then open_in Sys.argv.(1)
    else stdin in
  let lexbuf = Lexing.from_channel cin in
    try parse_then_print lexbuf
    with End_of_file -> ()
 *)
(*let _ = main ()*)

(*let ast_tree = 
  let lexbuf = Lexing.from_channel stdin in
    Parser.start lexer lexbuf
 *)
