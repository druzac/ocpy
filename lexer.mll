(* not dealing with tabs for the time being *)

(* by the way, use OCAML compatible strings instead of racket compatible
 * strings, no sense being shackled to this racket love on*)

(* header *)
{

    exception Indent_Error
    exception End_of_file
    exception Lex_Error of string

    open Parser
}
(* from first_pass lexer *)
let dq_sstring = '"' ( '\\' '"' | [^'"'])* '"'
let sq_sstring = ''' ( '\\' ''' | [^'''])* '''
let sq_lstring = "'''" ( ''' '''? [^'''] | [^'''] | '\\' ''')* "'''"
let dq_lstring = '"' '"' '"' ( '"' '"'? [^'"'] | [^'"'] | '\\' '"')* '"' '"' '"'
let lit_str = sq_sstring | sq_lstring | dq_sstring | dq_lstring
(* end stowaways*)



let indent_space = ' '*
let white_space = [' ' '\009' '\012']
let form_feed = '\012'
let identifier = ['A'-'Z' 'a'-'z' '_'] [ 'A'-'Z' 'a'-'z' '_' '0'-'9']*
let op =
    "+"|"-"|"*"|"**"|"/"|"//"|"%"|"<<"|">>"|"&"|"|"|"^"|"~"|"<"|">"|"<="|">="|"=="|"!="
let delimiter = 
        "("|")"|"["|"]"|"{"|"}"| ","|":"|"."|";"|"@"
        | "="| "+="|"-="|"*="|"/="|"//="|"%="|"&="|"|="|"^="|">>="|"<<="|"**="

let punct = op | delimiter

(* numbers *)
let digit = ['0'-'9']
let dec = digit+
let oct = '0' ( 'o' | 'O') ['0'-'7']+
let hex_digit = (digit | ['a'-'f'] | ['A' - 'F'])
let hex = '0' ( 'x' | 'X') hex_digit+
let bin = '0' ('b'|'B') ('0'|'1')+
let int_lit = dec | oct | hex | bin

let digits = digit+
let point_float = ((digits)? '.' digits) | digits '.'
let exp = ('e' | 'E') ('+'|'-')? digits
let exp_float = (digits | point_float) exp
let float_lit = point_float | exp_float

(* maaaaybe we want to change this around a little...
 * the racket way is +1.0i *)
let imag_lit = (float_lit | digits) ('j'|'J')
let num_lit = int_lit | float_lit | imag_lit

(* string literals*)
let sstring_delim = "'" | "\""
let lstring_delim = "'''" | "\"\"\""
let escape_seq = "\\" | "'" | "\"" | "a" | "b" | "f" | "n" | "r" | "t" | "v" 
        | (digit digit? digit?) | ('x' hex_digit hex_digit)


(* main body of lexer *)

(* deal with indentation here 
 * also cover the empty line case here so after we see an endline, we are
 * guaranteed a non-empty line *)
rule line_start = parse
| form_feed         { line_start lexbuf}
| indent_space as s  { INDENT_SPACE (String.length s) }
| eof               { ENDMARKER }

(* we're in the middle of a line here *)
and line_middle = parse
| white_space*       { line_middle lexbuf }
| "\\\n"             { line_middle lexbuf}
| '\n'              { NEWLINE }
| num_lit as num    { NUMBER (int_of_string num) }

| identifier as id   { NAME id} 
(*| punct as p        { emit_token (PUNCT p); line_middle lexbuf}*)
| lit_str as s       { STRING s }
| _                 { raise (Lex_Error ("Unexpected character"))}


(* footer *)
{
let gen_lexer () =
    let last_token = ref NEWLINE 
    and indent_stack = ref [0]
    and curr_indent = ref 0
    (*and l_brace_cnt = ref 0
and l_paren_cnt = ref 0
and l_bracket_cnt = ref 0 *)
    in
    let rec returner lex_fun lexbuf =
        let token = lex_fun lexbuf in
        if token = NEWLINE (*&& do_print_nl () *)|| token != NEWLINE then
            begin 
                last_token := token;
                token
                    end
        else
            lex_wrapper lexbuf
        and lex_wrapper lexbuf = 
            match !last_token with
            NEWLINE -> ws_pusher lexbuf
        | DEDENT -> ws_popper lexbuf
        | _ -> returner line_middle lexbuf

    and ws_pusher lexbuf =
        (* line_start returns either INDENT_SPACE or ENDMARKER *)
        match line_start lexbuf with
        INDENT_SPACE c -> 
            if c > !curr_indent then 
                begin
                    curr_indent := c;
                    indent_stack := c::(!indent_stack); 
                    returner (fun x -> INDENT) lexbuf
            end
        else if c = !curr_indent then returner line_middle lexbuf
            else begin curr_indent := c; ws_popper lexbuf end
        | ENDMARKER -> ENDMARKER
        | _ -> raise (Lex_Error "Line start returned neither INDENT_SPACE nor ENDMARKER")

    and ws_popper lexbuf =
        let stack_top = List.hd (!indent_stack) in
        if stack_top < !curr_indent then raise Indent_Error
        else if stack_top = !curr_indent then returner line_middle lexbuf
        else 
            begin
                indent_stack := List.tl (!indent_stack);
                returner (fun x -> DEDENT) lexbuf
            end
        in
        (fun lexbuf -> lex_wrapper lexbuf)

        (*let rec parse lexbuf =
            let token = line_start lexbuf
            in
            (* do * nothing * in * this * example * *)
            parse lexbuf; token

let main () =
    let cin = 
        if Array.length Sys.argv > 1 then open_in Sys.argv.(1)
        else stdin in
let lexbuf = Lexing.from_channel cin in
try parse lexbuf
with End_of_file -> ()
    *)
}
