
(* deal with the following tokens first: 
    * NEWLINE
    * INDENT
    * DEDENT
    *
 not dealing with tabs for the time being *)

(* by the way, use OCAML compatible strings instead of racket compatible
 * strings, no sense being shackled to this racket love on*)

(* header *)
{

    exception Indent_Error
    exception End_of_file

    let emit_s s =
        print_string s;
        print_newline ()

    let emit_c arg =
        print_char arg

    let indent_stack = ref [0]
    let push num = 
        indent_stack := num::!indent_stack

    let pop () =
        indent_stack := List.tl !indent_stack

    let peek () =
        List.hd !indent_stack

    type token = INDENT | NEWLINE | DEDENT | ENDMARKER

    let emit_token = function
        | INDENT -> emit_s "(INDENT)"
        | NEWLINE -> emit_s "(NEWLINE)"
        | DEDENT -> emit_s "(DEDENT)"
        | ENDMARKER -> emit_s "(ENDMARKER)"

    let rec process_indent s_len =
        let top_stack = (peek ()) in

        let rec pop_all_greater () =
            let curr = peek () in
            if curr < s_len then raise Indent_Error
            else if curr > s_len then
                begin
                    pop ();
                    emit_token DEDENT;
                    pop_all_greater ()
                end
            else 
                ()
            in
            if s_len = top_stack then
                ()
            else if s_len > top_stack then
                begin
                    emit_token INDENT;
                    push s_len
                end
            else
                pop_all_greater ()


        let dedent_remaining () =
            process_indent 0

}

let white_space = ' '*
let form_feed = '\012'

(* main body of lexer *)

rule line_start = parse
| form_feed         { line_start lexbuf}
| white_space as s  { process_indent (String.length s); line_middle lexbuf }
| eof               { dedent_remaining (); emit_token ENDMARKER ; raise End_of_file}

and line_middle = parse
        | '\n'              { emit_token NEWLINE; line_start lexbuf}
        | _ as c               { emit_c c; line_middle lexbuf}

(* footer *)
{
let rec parse lexbuf =
    let token = line_start lexbuf
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
