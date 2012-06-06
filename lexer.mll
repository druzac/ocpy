(* not dealing with tabs for the time being
 * Use ocaml strings
 things we don't handle currently:
    * tabs
    * strings (newlines, short strings vs long strings
    * any kind of numbers besides integers
    *)
{

    exception Indent_Error
    exception End_of_file
    exception Lex_Error of string

    open Parser

    let keyword_map =
        [ "False", FALSE;
        "class",CLASS;
        "finally",FINALLY;
        "is",IS;
        "return",RETURN;
        "None",NONE;
        "continue",CONTINUE;
        "for",FOR;
        "lambda",LAMBDA;
        "try",TRY;
        "True",TRUE;
        "def",DEF;
        "from",FROM;
        "nonlocal",NONLOCAL;
        "while",WHILE;
        "and",AND;
        "del",DEL;
        "global",GLOBAL;
        "not",NOT;
        "with",WITH;
        "as",AS;
        "elif",ELIF;
        "if",IF;
        "or",OR;
        "yield",YIELD;
        "assert",ASSERT;
        "else",ELSE;
        "import",IMPORT;
        "pass",PASS;
        "break",BREAK;
        "except",EXCEPT;
        "in",IN;
        "raise", RAISE]

    let punct_map = 
        [ "+", PLUS;
        "-", MINUS;
        "*", STAR;
        "**", DSTAR;
        "/", SLASH;
        "//", DSLASH;
        "%", PERCENT;
        "<<", DLT;
        ">>", DGT;
        "&", AMP;
        "|", PIPE;
        "^", CARET;
        "~", TILDE;
        "<", LT;
        ">", GT;
        "<=", LTEQ;
        ">=", GTEQ;
        "==", DEQ;
        "!=", NOTEQ ;
        "(", LPAREN;
        ")", RPAREN;
        "[", LBRACKET;
        "]", RBRACKET;
        "{", LBRACE;
        "}", RBRACE;
        ",", COMMA;
        ":", COLON;
        ".", DOT;
        ";", SEMICOLON;
        "@", ATSYM;
        "=", EQ;
        "+=", PLUSEQ;
        "-=", MINUSEQ;
        "*=", STAREQ;
        "/=", SLASHEQ;
        "//=", DSLASHEQ;
        "%=", PERCENTEQ;
        "&=", AMPEQ;
        "|=", PIPEEQ;
        "^=", CARETEQ;
        ">>=", DGTEQ;
        "<<=", DLTEQ;
        "**=", DSTAREQ;
        "...", ELLIPSIS]


    let lookup_keyword s =
        List.assoc s keyword_map

    let lookup_punct s =
        List.assoc s punct_map
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
let comment = '#' [^'\n']*
let form_feed = '\012'
let identifier = ['A'-'Z' 'a'-'z' '_'] [ 'A'-'Z' 'a'-'z' '_' '0'-'9']*
let op =
    "+"|"-"|"*"|"**"|"/"|"//"|"%"|"<<"|">>"|"&"|"|"|"^"|"~"|"<"|">"|"<="|">="|"=="|"!="
let delimiter = 
        "("|")"|"["|"]"|"{"|"}"| ","|":"|"."|";"|"@"
        | "="| "+="|"-="|"*="|"/="|"//="|"%="|"&="|"|="|"^="|">>="|"<<="|"**="
        | "..."

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

(* the racket way is +1.0i *)
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
| white_space* (comment?) '\n'  { line_start lexbuf}
| form_feed			{ line_start lexbuf}
| indent_space as s		{ INDENT_SPACE (String.length s) }
| eof				{ ENDMARKER }

(* we're in the middle of a line here *)
and line_middle = parse
| white_space*			{ line_middle lexbuf }
| "\\\n"			{ line_middle lexbuf}
| '\n'				{ NEWLINE }
| num_lit as num		{ NUMBER (int_of_string num) }
| identifier as id		{ begin try (lookup_keyword id) with Not_found -> NAME id end}
| punct as p			{ lookup_punct p}
| lit_str as s			{ STRING s }
| eof				{ ENDMARKER}
| _				{ raise (Lex_Error ("Unexpected character"))}


(* footer *)
{
let incer int_box =
    int_box := !int_box + 1

let decer int_box =
    int_box := !int_box - 1

let gen_lexer () =
    let last_token = ref NEWLINE 
    and indent_stack = ref [0]
    and curr_indent = ref 0
    and l_brace_cnt = ref 0
    and l_paren_cnt = ref 0
    and l_bracket_cnt = ref 0 in

    (* helper helper fcns *)
    let set_return token =
        last_token := token;
        token
    and do_print_nl () =
        !l_brace_cnt + !l_bracket_cnt + !l_paren_cnt <= 0 in

    (* decides whether to return the result of lex_fun on lexbuf, and if so then
        * set up the next function to call appropriately *)
    let rec returner lex_fun lexbuf =
        let token = lex_fun lexbuf in
        match token with
        LBRACKET -> incer l_bracket_cnt; set_return token
        | RBRACKET -> decer l_bracket_cnt; set_return token
        | LPAREN -> incer l_paren_cnt; set_return token
        | RPAREN -> decer l_paren_cnt; set_return token
        | LBRACE -> incer l_brace_cnt; set_return token
        | RBRACE -> decer l_brace_cnt; set_return token
        | NEWLINE -> if do_print_nl () then set_return token
        else lex_wrapper lexbuf
        | _ -> set_return token

    and lex_wrapper lexbuf = 
        match !last_token with
        NEWLINE -> ws_pusher lexbuf
        | DEDENT -> ws_popper lexbuf
        | ENDMARKER -> pop_all_remaining ()
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
        | ENDMARKER -> last_token := ENDMARKER; pop_all_remaining ()
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
    and pop_all_remaining () =
        if List.hd (!indent_stack) = 0 then ENDMARKER
        else 
            begin
                indent_stack := List.tl (!indent_stack);
                DEDENT
            end
        in

        (fun lexbuf -> lex_wrapper lexbuf)
}
