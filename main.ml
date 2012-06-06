let get_lexer () = Lexer.gen_lexer ()

let parse_file f =
  let lexer = Lexer.gen_lexer () 
  and lexbuf = Lexing.from_channel (open_in f) in
    Parser.start lexer lexbuf

let parse () =
  let lexer = Lexer.gen_lexer () in
  let lexbuf = Lexing.from_channel stdin in
    Parser.start lexer lexbuf

let parse_str s =
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

let lex_in () =
  lex (Lexing.from_channel stdin)

