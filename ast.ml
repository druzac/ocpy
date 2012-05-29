(*open Sexplib.Std*)
(* lists are supposed to be non-empty unless otherwise mentioned *)
exception Ast_Abused

type program = Program of stmt list
and stmt = Simple of simple_stmt
	   | Cond of (test * suite) list * (suite option)  (* if statement *)
	   | While of test * suite * (suite option)        (* while statement *)
	   | For of string * test * suite * (suite option) (* for statement *)
	   | Try of suite * (catch * suite) list * (suite option) * (suite option) (* try stmt *)
	   | Def of string * (string list) * suite                  (* Funcdef *)
and simple_stmt = Single of small_stmt
		  | Begin of small_stmt list
and suite = Suite_single of simple_stmt | Suite of stmt list
and catch = Except of (test * (string option)) option
and small_stmt = 
    (* expr stmts *)
    Assignment of assign_op * (test list) * tuple_or_test
  | Expr of tuple_or_test
      (* del stmt *)
  | Del of star_expr
      (* Flow stmts *)
  | Pass
  | Break
  | Continue
  | Return of test list
  | Raise of test option * test option
      (* global, local, assert *)
  | Global of string list
  | Nonlocal of string list
  | Assert of test list
and assign_op = Pluseq | Minuseq | Stareq | Slasheq | Percenteq
		| Ampeq | Pipeeq | Careteq | Dlteq | Dgteq | Dstareq | Dslasheq
 		| Eq
and test = If_test of or_test * or_test * test
	   | Or_test of or_test
	   | Lambda of (string list) * test
and or_test = Or of and_test list
and and_test = And of not_test list
and not_test = NtStexp of star_expr
	       | Comparison of star_expr * (comp_op * star_expr) list
	       | Not of not_test
and comp_op = Lt | Gt | Eqeq | Gteq | Lteq | Ltgt | Noteq | In | Notin | Is | Isnot
and star_expr = Star_exp of expr | Star_sexp of expr
and expr = Bitwise_or of xor_expr list
and xor_expr =  Bitwise_xor of and_expr list
and and_expr = Bitwise_and of shift_expr list
and shift_expr = Shift of arith_expr * (shift_op * arith_expr) list
and shift_op = Dlt | Dgt
and arith_expr = Arith of term * (arith_op * term) list
and arith_op = Plus | Minus
and term = Term of factor * (factor_op * factor) list
and factor_op = Star | Fslash | Percent | Dfslash
and factor = Fact_single of power | Uapp of unary_op * factor
and unary_op = Uplus | Uminus | Utilde
and power = Pow_single of indexed | Power of indexed * factor
and indexed = Atom of atom | Indexed of atom * (trailer list)
and atom =
    Tot of tuple_or_test
  | Empty_tuple
  | List of test list
  | Dict of (test * test) list
  | Set of test list
  | Name of string
  | Number of number
  | String of string
  | Ellipsis
  | None
  | True
  | False
and number = int
and trailer = Called of test list
	      | Subscript of tuple_or_test
	      | Dot of string
and tuple_or_test = Test of test | Tuple of test list 

(* tot -> tot *)
let tot_fin tot =
  match tot with
      Test t -> Test t
    | Tuple l -> Tuple (List.rev l)

let tot_add test = function
    Tuple l -> Tuple (test::l)
  | Test t -> Tuple (test::[t])

let testlist_add test tlist =
  test::tlist

let testlist_fin tlist =
  List.rev tlist

let testlist_to_tot = function
    [t] -> Test t
  | l -> Tuple (List.rev l)

let dict_add key value dict =
  (key, value)::dict

let indexed_add trailer = function
  Atom a -> Indexed (a, [trailer])
  | Indexed (a, l) -> Indexed (a, trailer::l)

let indexed_fin = function
  Atom a -> Atom a
  | Indexed (a, tl) -> Indexed (a, List.rev tl)

let term_add f_op fact = function
  Term(f, factlist) -> Term(f, (f_op, fact)::factlist)

let term_fin = function
    Term(f, fl) -> Term(f, List.rev fl)

let arith_add a_op a_term = function
  Arith(t, tops) -> Arith(t, (a_op, (term_fin a_term))::tops)

let arith_fin = function
  Arith(t, summands) -> Arith(t, List.rev summands)

let arith_new term = Arith (term_fin term, [])

let shift_add shift_op arith_expr = function
  Shift(aexp, shifts) -> Shift(aexp, (shift_op, arith_fin arith_expr)::shifts)

let shift_fin = function
  Shift(a, a_s) -> Shift(a, List.rev a_s)

let shift_new a_exp = Shift(arith_fin a_exp, [])

let and_add s_exp = function
    Bitwise_and (shfts) -> Bitwise_and ((shift_fin s_exp)::shfts)

let and_fin = function
    Bitwise_and (shfts) -> Bitwise_and (List.rev shfts)

let and_new shft_exp = Bitwise_and ([shift_fin shft_exp])

let xor_add aexp = function
  Bitwise_xor (aexps) -> Bitwise_xor ((and_fin aexp)::aexps)

let xor_fin = function
  Bitwise_xor (aexps) -> Bitwise_xor (List.rev aexps)

let xor_new aexp = Bitwise_xor [(and_fin aexp)]

let e_add xorexp = function
 Bitwise_or (xexps) -> Bitwise_or ((xor_fin xorexp)::xexps)

let e_fin = function
  Bitwise_or xexps -> Bitwise_or (List.rev xexps)

let e_new xexp = Bitwise_or [(xor_fin xexp)]

let comp_add comp_op sexpr = function
 Comparison (sxp, ls) -> Comparison(sxp, ((comp_op, sexpr)::ls))
  | _ -> raise Ast_Abused

let comp_new sexpr = Comparison (sexpr, [])

let comp_fin = function
  Comparison (sexpr, comps) -> Comparison (sexpr, List.rev comps)
  | _ -> raise Ast_Abused

let land_new nt_tst = And ([nt_tst])

let land_fin = function
    And(nts) -> And(List.rev nts)

let land_add nt = function
    And(nts) -> And(nt::nts)

let lor_new a_tst = Or([a_tst])

let lor_add a_tst = function Or(atsts) -> Or(a_tst::atsts)

let lor_fin = function
    Or(ats) -> Or(List.rev ats)

let while_fin tsuite suite_op =
  match tsuite with (t,s) -> While(t,s, suite_op)

let for_fin str_test_suite suite_op =
  match str_test_suite with (str,test,s) -> For (str, test, s, suite_op)

let excepts_elfin catch_sts el_fin =
  match el_fin with (sop1, sop2) -> (List.rev catch_sts, sop1, sop2)

let try_fin suite = function
  (a,b,c) -> Try (suite,a,b,c)

(* printer helpers *)

let obox0() = Format.open_hvbox 0
let obox() = Format.open_hvbox 2
let cbox() = Format.close_box ()
let break () = Format.print_break 0 0

let pr_one word printer atom =
  obox (); Format.print_string "("; Format.print_string word; Format.print_space (); printer atom;
  Format.print_string ")"; cbox ()

let pr_slist printer el =
  obox(); Format.print_string "("; List.map printer el; Format.print_string ")"; cbox()

let pr_strwsp str =
  Format.print_string str; Format.print_space ()

let rec print_ast = function
    Program stmts -> pr_one "program" (List.map print_stmt) stmts

and print_stmt = function
    Def (f, args, body) -> pr_one "def" (fun (a,b) -> pr_slist pr_strwsp a; print_suite b) (args,body)
  | _ -> ()

and print_suite = function
    _ -> ()

(*
let print_ast prog = 
  let indent n = n + 2
  and dedent n = n - 2 in
  let rec print_indent n =
    if n <= 0 then ()
    else print_string " "; print_indent (n-1) in

  (* do something if list has a single element, something else if it has > 1 *)
  let cond_print printer word = function
      [el] -> printer el
    | l -> Printf.printf "(%s" word; List.map printer l; print_string ")"
  and gen_printer word next_printer arg =
    Printf.printf "(%s " word; next_printer arg; print_string ")" in

  let rec print_stmt ind = function 
      Simple s -> print_simple ind s;
    | _ -> ()
  and print_simple ind = function
      Single s -> print_small_stmt ind s
    | _ -> ()
  and print_small_stmt ind = function
      Expr tot -> print_indent ind; print_string "(expr "; print_tot tot; print_string ")"
  and print_tot = function
      Test t -> print_test t
    | Tuple ts -> print_string "(tuple "; List.map print_test ts; print_string ")"
  and print_test = function
      Or_test ot -> print_ort ot
  and print_ort  = function 
      Or ands -> cond_print print_andt "or" ands
  and print_andt = function
      And nots -> cond_print print_nott "and" nots
  and print_nott = function
      NtStexp st_exp -> print_stexp st_exp
    | Comparison st_exp * comps -> ()
    | Not nott -> print_string "(not "; print_nott nott; print_string ")"
	gen_printer "not" print_nott nott
  and print_stexp = function
      Star_exp e -> print_expr
    | Star_sexp e -> gen_printer "star" print_expr e
  and print_expr = function
      Bitwise_or xors -> cond_print print_xor "bitwise-or" xors
  and print_xor = function
      Bitwise_xor ands -> cond_print print_and "bitwise-xor" ands
  and print_and = function
      Program stmts -> print_string "(program \n"; List.map (print_stmt (indent 0)) stmts; print_string ")"
	*)

(* 
(try_stmt 
  (seq "try" ":" suite 
    (or (seq (rep+ (seq except_clause ":" suite)) (opt (seq "else" ":" suite)) (opt (seq "finally" ":"suite))) 
(seq "finally" ":" suite))))
*)
