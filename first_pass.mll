
(* header *)
{

    exception End_of_file
    exception Lex_Error

    let emit_c arg =
        print_char arg

    let emit_s arg =
        print_string arg
}

(* '\009' is a horizontal tab
 * '\012' is a form feed *)
let white_space = [' ' '\009' '\012']
let comment = '#' [^'\n']*
let squote = '\'' | '"'
let lquote = "'''" | "\"\"\""

rule start_line = parse
| white_space* (comment?) '\n'        { start_line lexbuf}
| _                                   { emit_s (Lexing.lexeme lexbuf); in_line lexbuf}
                                            (* there are problems with this...
                                             * what if its a character we need
                                             * to keep track of? *)
| eof                                 { raise End_of_file}

and in_line = parse
| comment '\n'                        { emit_c '\n'; start_line lexbuf}
| '\n'                                { emit_c '\n'; start_line lexbuf}
| "\\\n"                              { in_line lexbuf}
| squote as quote                              { emit_s quote; s_string lexbuf quote}
| _                                   { emit_s (Lexing.lexeme lexbuf); in_line lexbuf}

and s_string quote_type = parse
(* special characters here to watch out for: the two quotes!*)
| "\\\n"                              { s_string lexbuf quote_type}
| '\n'                                { raise Lex_Error "Unescaped newline in short string"}
| squote as quote                     { if quote = quote_type then
                                        begin
                                            emit_s quote;
                                            in_line lexbuf;
                                        end
                                    else
                                        begin
                                        emit_s quote;
                                        s_string lexbuf quote_type;
                                        end}
| _                                 { emit_s (Lexing.lexeme lexbuf); s_string lexbuf quote_type}
| eof                               { raise Lex_Error "EOF while scanning string"}   




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
}
