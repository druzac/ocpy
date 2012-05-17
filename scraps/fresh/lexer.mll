(* not dealing with tabs for the time being *)

(* by the way, use OCAML compatible strings instead of racket compatible
 * strings, no sense being shackled to this racket love on*)

(* header *)
{

    exception Indent_Error
    exception End_of_file
    exception Lex_Error of string

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

    type token = 
        | INDENT 
        | NEWLINE
        | DEDENT
        | ENDMARKER
        | ID of string
        | LIT of string
        | KEYWORD of string
        | PUNCT of string

    let keyword_map = List.map (fun kwd -> kwd, KEYWORD kwd)
    [ "False"; "class";     "finally";    "is";         "return";
    "None";       "continue";   "for";        "lambda";     "try";
    "True";       "def";        "from";       "nonlocal";   "while";
    "and";        "del";        "global";     "not";        "with";
    "as";         "elif";       "if";         "or";         "yield";
    "assert";     "else";       "import";     "pass";
    "break";      "except";     "in";         "raise"]

    let string_rev s =
        let len = String.length s in
        let new_s = String.create len in
        let rec iter count = 
            if count >= len then new_s
            else 
                begin
                    new_s.[count] <- s.[len - count - 1];
                    iter (count + 1)
                end
        in
        iter 0

                    

    (*let emit_token = function
        | INDENT -> emit_s "(INDENT)"
        | NEWLINE -> emit_s "(NEWLINE)"
        | DEDENT -> emit_s "(DEDENT)"
        | ENDMARKER -> emit_s "(ENDMARKER)"
        | ID name -> emit_s ("(ID \"" ^ name ^ "\")")
        | LIT symbol -> emit_s ("(LIT " ^ symbol ^ ")")
        | KEYWORD word -> emit_s ("(KEYWORD "^ word^ ")")
        | PUNCT sym -> emit_s ("(PUNCT \"" ^ sym^ "\")")
        *)
    let emit_token x = x

    let string_of_char c =
        String.make 1 c

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

(* deal with indentation here *)
rule line_start = parse
| form_feed         { line_start lexbuf}
| indent_space as s  { process_indent (String.length s); line_middle lexbuf }
| eof               { dedent_remaining (); emit_token ENDMARKER ; raise End_of_file}

(* we're in the middle of a line here *)
and line_middle = parse
| white_space*       { line_middle lexbuf }
| '\n'              { emit_token NEWLINE; line_start lexbuf}
| num_lit as num    { emit_token (LIT num); line_middle lexbuf}
| identifier as id  { begin
                      try
                        emit_token (List.assoc id keyword_map) 
                      with
                      Not_found -> emit_token (ID id)
                      end;
                      line_middle lexbuf}
| punct as p        { emit_token (PUNCT p); line_middle lexbuf}
| sstring_delim as quote { sstring quote "\"" lexbuf}
| lstring_delim as quotes { lstring quotes.[0] "\"" lexbuf}
| _ as c                  { raise (Lex_Error ("Unexpected character "^ (string_of_char c)))}

(* short string - single or double quotes*)
and sstring delim rev_accum = parse
| sstring_delim as quote   { if quote = delim then (* for now just return str*)
    begin
    emit_token (LIT (string_rev("\""^rev_accum)));
    line_middle lexbuf
    end
            else sstring delim ((string_of_char quote)^rev_accum) lexbuf }
| '\n'                      { raise (Lex_Error "EOL while scanning string") }
| '\\'                     { escape sstring delim rev_accum lexbuf }
| _ as c                   { sstring delim ((string_of_char c)^rev_accum) lexbuf}

(* long string - triple of single/double quotes*)
and lstring quote rev_accum = parse
| lstring_delim as quotes   
                           { if quotes.[0] = quote then 
                               begin
                                   emit_token (LIT (string_rev("\""^rev_accum)));
                                   line_middle lexbuf
                               end
                           else lstring quote (quotes^rev_accum) lexbuf }
| '\\'                     { escape lstring quote rev_accum lexbuf }
| '\n'                     { lstring quote ("n\\" ^ rev_accum) lexbuf }
| _ as c                   { lstring quote ((string_of_char c)^rev_accum) lexbuf}

(* escape sequence: types are
\newline	
\\	        
\<single quote>
\<double quote>
\a	        
\b	        
\f	        
\n	        
\r	        
\t	        
\v	        
\ooo   - up to 3 octal digits
\xhh   - exactly 2 hex digits
 *)
and escape string_rule quote_type rev_accum = parse
        | escape_seq as es           { string_rule quote_type
        ((string_rev("\\"^es))^rev_accum) lexbuf}
        | '\n'                       { string_rule quote_type rev_accum lexbuf}
        | _ as c                     { string_rule quote_type 
                                        ((string_of_char c)^"\\\\"^rev_accum) lexbuf}


(* footer *)
{
let rec parse lexbuf =
    let token = line_start lexbuf
    in
    (* do * nothing * in * this * example * *)
    parse lexbuf; token

let make_tokenizer =
    let last_token = ref NEWLINE 
    in
    (fun lexbuf -> 
        let tokenizer = if !last_token = NEWLINE then line_start
        else line_middle in
        let result = tokenizer lexbuf in
        last_token := result;
        result)

let main () =
    let cin = 
        if Array.length Sys.argv > 1 then open_in Sys.argv.(1)
        else stdin in
    let lexbuf = Lexing.from_channel cin in
    try parse lexbuf
    with End_of_file -> ()

let _ = Printexc.print main ()
}
