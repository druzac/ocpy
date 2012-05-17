(* a lexer for numbers and operators *)

{ 
open Printf
open Lexing
}

let digit = ['0' - '9']

rule calc = parse
  | digit+  {printf "(NUM: %s)\n" (lexeme lexbuf); calc lexbuf }
  | '+'
  | '/'
  | '-'
  | '*' {printf "(OP: %s)\n" (lexeme lexbuf); calc lexbuf}
  | [' ' '\t' '\n']+ (* kill whitespace *)
  {calc lexbuf}
  | _ as c {printf "Unrecognized char %c\n" c; calc lexbuf }
  | eof {raise End_of_file}

{
let rec parse lexbuf =
    let token = calc lexbuf in
    (* do nothing in this example *)
parse lexbuf

let main () =
    let cin =
        if Array.length Sys.argv > 1
        then open_in Sys.argv.(1)
        else stdin in
    let lexbuf =
        Lexing.from_channel cin in
    try parse lexbuf
    with End_of_file ->
        ()

let _ = Printexc.print main ()
}

