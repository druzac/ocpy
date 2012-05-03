(* this lexer does a first pass over the input file.
 * Contract:
     * after execution, logical lines and physical lines are the same thing
     * (with the exception of what's inside strings, we don't examine those
     * There are no explicit line joins left to be considered
     * All blank lines have been removed *)

(* header *)
{

    exception End_of_file
    exception Lex_Error of string


    let emit_c arg =
        print_char arg

    let emit_s arg =
        print_string arg

    let num_square = ref 0
    let num_brace = ref 0
    let num_paren = ref 0

    let inc pt =
        pt := !pt + 1

    let dec pt =
        pt := !pt - 1;
        if !pt < 0 then raise (Lex_Error "Unbalanced thingies")

    let process_left = function
        | '[' -> inc num_square
        | '{' -> inc num_brace
        | '(' -> inc num_paren
        | _ -> raise (Lex_Error "Unrecognized opener")

    let process_right = function
        | ']' -> (*print_string "Saw ]\n";*) dec num_square
        | '}' -> (*print_string "Saw }\n"; *)dec num_brace
        | ')' -> (*print_string "Saw )\n"; *) dec num_paren
        | _ -> raise (Lex_Error "Unrecognized opener")

    let can_continue () = 
        (!num_square > 0) || (!num_paren > 0) || (!num_brace > 0)
}

(* '\009' is a horizontal tab
 * '\012' is a form feed *)
let white_space = [' ' '\009' '\012']
let comment = '#' [^'\n']*

let dq_sstring = '"' ( '\\' '"' | [^'"'])* '"'
let sq_sstring = ''' ( '\\' ''' | [^'''])* '''
let sq_lstring = "'''" ( ''' '''? [^'''] | [^'''] | '\\' ''')* "'''"
let dq_lstring = '"' '"' '"' ( '"' '"'? [^'"'] | [^'"'] | '\\' '"')* '"' '"' '"'
let lit_str = sq_sstring | sq_lstring | dq_sstring | dq_lstring

let squote = '\'' | '"'
let lquote = "'''" | "\"\"\""

let left = '[' | '{' | '('
let right = ']' | '}' | ')'

rule start_line = parse
| white_space* (comment?) '\n'        { start_line lexbuf}
| lit_str as s                     { emit_s s; in_line lexbuf}
| left as l                           { emit_c l; process_left l; in_line lexbuf}
| right as r                          { emit_c r; process_right r; in_line lexbuf}
| _                                   { emit_s (Lexing.lexeme lexbuf); in_line lexbuf}
| eof                                 { raise End_of_file}

and in_line = parse
| comment                             { in_line lexbuf}
| '\n'                        { if can_continue ()
        then in_line lexbuf
        else 
            begin
                emit_c '\n';
                start_line lexbuf
            end }
| "\\\n"                              { in_line lexbuf}
| left as l                           { emit_c l; process_left l; in_line lexbuf}
| right as r                          { emit_c r; process_right r; in_line lexbuf}
| lit_str as s                   { emit_s s; in_line lexbuf}
| _                                   { emit_s (Lexing.lexeme lexbuf); in_line lexbuf}
| eof                                 { raise End_of_file}    

{
let rec parse lexbuf =
    let token = start_line lexbuf
    in
    (* do * nothing * in * this * example * *)
    parse lexbuf

let main () =
    let cin = 
        if Array.length Sys.argv > 1 then open_in Sys.argv.(1)
        else stdin in
    let lexbuf = Lexing.from_channel cin in
    try parse lexbuf
    with End_of_file -> ()

let _ = print_string "Beginning to parse...\n"
let _ = Printexc.print main ()
let _ = Printf.printf "Finished Parsing"
}
