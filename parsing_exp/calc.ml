(*open Parser
open Lexer *)

print_string "I am here...\n";;
let tree = 
  let lexbuf = Lexing.from_channel stdin in
    Parser.toplevel Lexer.token lexbuf;;

print_string "aha we finished lexing\n";;
Syntax.print_t tree;;
print_string "\n";;
print_string "Ho\n";;

