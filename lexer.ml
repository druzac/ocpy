# 7 "lexer.mll"
 

    exception Indent_Error
    exception End_of_file
    exception Lex_Error of string

    open Parser

# 11 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\253\255\001\000\255\255\004\000\249\255\003\000\005\000\
    \079\000\154\000\166\000\196\000\253\255\000\000\008\000\254\255\
    \252\255\013\001\023\001\000\000\033\001\176\000\228\000\088\001\
    \014\000\117\001\056\001\000\000\008\000\193\000\194\000\250\255\
    \195\000\197\000\002\000\007\000\198\000\012\000\059\000\000\000\
    \179\000\204\000\207\000\209\000\221\000\001\000\032\000\001\001\
    \205\000\220\000";
  Lexing.lex_backtrk = 
   "\001\000\255\255\001\000\255\255\000\000\255\255\006\000\006\000\
    \004\000\006\000\003\000\003\000\255\255\006\000\000\000\255\255\
    \255\255\255\255\003\000\255\255\255\255\255\255\003\000\003\000\
    \003\000\003\000\255\255\005\000\255\255\255\255\005\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\005\000\005\000\
    \255\255\255\255\005\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\005\000";
  Lexing.lex_default = 
   "\255\255\000\000\255\255\000\000\005\000\000\000\040\000\028\000\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\000\000\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\028\000\028\000\028\000\000\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\255\255\
    \040\000\040\000\040\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\015\000\000\000\003\000\014\000\012\000\000\000\
    \014\000\014\000\000\000\000\000\014\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\002\000\043\000\046\000\014\000\039\000\006\000\032\000\
    \014\000\035\000\000\000\007\000\027\000\000\000\031\000\031\000\
    \024\000\024\000\009\000\038\000\011\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\024\000\024\000\
    \000\000\000\000\031\000\000\000\000\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\041\000\
    \013\000\029\000\031\000\008\000\029\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\000\000\000\000\000\000\000\000\008\000\000\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\031\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \030\000\031\000\034\000\017\000\036\000\037\000\042\000\049\000\
    \016\000\031\000\018\000\045\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\031\000\047\000\
    \001\000\255\255\255\255\255\255\255\255\255\255\019\000\255\255\
    \255\255\017\000\000\000\017\000\255\255\000\000\016\000\041\000\
    \016\000\000\000\000\000\021\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\020\000\029\000\029\000\033\000\
    \255\255\033\000\033\000\048\000\000\000\000\000\019\000\000\000\
    \041\000\017\000\000\000\041\000\000\000\044\000\016\000\000\000\
    \000\000\000\000\000\000\021\000\000\000\000\000\000\000\000\000\
    \026\000\044\000\026\000\255\255\020\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\000\000\017\000\044\000\000\000\000\000\
    \000\000\016\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\017\000\000\000\000\000\000\000\
    \000\000\016\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\023\000\023\000\023\000\023\000\023\000\023\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\023\000\023\000\023\000\023\000\023\000\023\000\016\000\
    \000\000\255\255\255\255\255\255\000\000\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\255\255\255\255\000\000\255\255\
    \000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\255\255\000\000\016\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\013\000\255\255\000\000\004\000\004\000\255\255\
    \004\000\014\000\255\255\255\255\014\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\002\000\039\000\045\000\004\000\006\000\004\000\027\000\
    \014\000\034\000\255\255\004\000\007\000\255\255\035\000\028\000\
    \019\000\019\000\004\000\037\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\024\000\024\000\
    \255\255\255\255\046\000\255\255\255\255\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\006\000\
    \004\000\007\000\038\000\004\000\028\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\255\255\255\255\255\255\255\255\008\000\255\255\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\010\000\040\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \029\000\030\000\032\000\010\000\033\000\036\000\041\000\048\000\
    \010\000\042\000\011\000\043\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\049\000\044\000\
    \000\000\045\000\034\000\006\000\004\000\007\000\011\000\035\000\
    \028\000\011\000\255\255\010\000\037\000\255\255\011\000\040\000\
    \010\000\255\255\255\255\011\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\011\000\029\000\030\000\032\000\
    \046\000\033\000\036\000\047\000\255\255\255\255\011\000\255\255\
    \041\000\011\000\255\255\042\000\255\255\043\000\011\000\255\255\
    \255\255\255\255\255\255\011\000\255\255\255\255\255\255\255\255\
    \017\000\044\000\017\000\038\000\011\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\255\255\018\000\047\000\255\255\255\255\
    \255\255\018\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\018\000\255\255\255\255\255\255\
    \255\255\018\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\023\000\023\000\023\000\023\000\023\000\023\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\255\255\
    \255\255\255\255\255\255\040\000\255\255\255\255\255\255\255\255\
    \255\255\023\000\023\000\023\000\023\000\023\000\023\000\025\000\
    \255\255\029\000\030\000\032\000\255\255\033\000\036\000\255\255\
    \255\255\255\255\255\255\255\255\041\000\048\000\255\255\042\000\
    \255\255\043\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\049\000\044\000\255\255\025\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\047\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec line_start lexbuf =
    __ocaml_lex_line_start_rec lexbuf 0
and __ocaml_lex_line_start_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 70 "lexer.mll"
                    ( line_start lexbuf)
# 218 "lexer.ml"

  | 1 ->
let
# 71 "lexer.mll"
                  s
# 224 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 71 "lexer.mll"
                     ( INDENT_SPACE (String.length s) )
# 228 "lexer.ml"

  | 2 ->
# 72 "lexer.mll"
                    ( ENDMARKER )
# 233 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_line_start_rec lexbuf __ocaml_lex_state

and line_middle lexbuf =
    __ocaml_lex_line_middle_rec lexbuf 4
and __ocaml_lex_line_middle_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 76 "lexer.mll"
                     ( line_middle lexbuf )
# 244 "lexer.ml"

  | 1 ->
# 77 "lexer.mll"
                     ( line_middle lexbuf)
# 249 "lexer.ml"

  | 2 ->
# 78 "lexer.mll"
                    ( NEWLINE )
# 254 "lexer.ml"

  | 3 ->
let
# 79 "lexer.mll"
             num
# 260 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 79 "lexer.mll"
                    ( NUMBER (int_of_string num) )
# 264 "lexer.ml"

  | 4 ->
let
# 81 "lexer.mll"
                id
# 270 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 81 "lexer.mll"
                     ( NAME id)
# 274 "lexer.ml"

  | 5 ->
let
# 83 "lexer.mll"
             s
# 280 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 83 "lexer.mll"
                     ( STRING s )
# 284 "lexer.ml"

  | 6 ->
# 84 "lexer.mll"
                    ( raise (Lex_Error ("Unexpected character")))
# 289 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_line_middle_rec lexbuf __ocaml_lex_state

;;

# 88 "lexer.mll"
 
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

# 363 "lexer.ml"
