open Printf
open Parser

let printer = function
  | INDENT -> print_string "(INDENT)\n"
  | DEDENT -> print_string "(DEDENT)\n"
  | NEWLINE -> print_string "(NEWLINE)\n"
  | NUMBER (n) -> printf "(NUMBER %d)\n" n
  | STRING (s) -> printf "(STRING %s)\n" s
  | NAME (s) -> printf "(ID %s)\n" s
  | ENDMARKER -> printf "(ENDMARKER)\n"
  | _ -> print_string "I don't know...\n"

let rec parse_then_print lexbuf =
  let lexer = Lexer.gen_lexer () in
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

let _ = main ()
