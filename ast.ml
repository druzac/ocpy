(* lists are supposed to be non-empty unless otherwise mentioned *)
exception Ast_Abused

type program = Program of stmt list
and stmt = Simple of simple_stmt
	   | Cond of (test * suite) list * (suite option)  (* if statement *)
	   | While of test * suite * (suite option)        (* while statement *)
	   | For of string * test * suite * (suite option) (* for statement *)
	   | Try of suite * (catch * suite) list * (suite option) * (suite option) (* try stmt *)
           (* try clause, list of except clauses, else clause, finally clause *)
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
and not_test = Comparison of star_expr * (comp_op * star_expr) list
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

let smpl_stmt_add small = function
    Single x -> Begin (small::[x])
  | Begin xs -> Begin (small::xs)

let smpl_stmt_fin = function
  Single x -> Single x
  | Begin xs -> Begin (List.rev xs)

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

let lor_new a_tst = Or([land_fin a_tst])

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
(*
let obox0() = Format.open_hvbox 0
let obox() = Format.open_hvbox 2
let cbox() = Format.close_box ()
let break () = Format.print_break 0 0
let space () = Format.print_space ()

let aoptostr = function 
    _ -> "assign"
(*      = Pluseq | Minuseq | Stareq | Slasheq | Percenteq
  | Ampeq | Pipeeq | Careteq | Dlteq | Dgteq | Dstareq | Dslasheq
  | Eq *)

let shfttostr = function
    _ -> "shifter"

let comptostr = function
    _ -> "comper"

let arithtostr = function
    _ -> "arther"

let foptostr = function
    _ -> "fop"

let uptostr = function
_ -> "uop"

(* optionally print the word depending size of list *)
let pr_one word printer atom =
  obox (); Format.print_string "("; Format.print_string word; Format.print_space (); printer atom;
  Format.print_string ")"; cbox ()

let pr_list printer els =
  let rec it_print = function
      [] -> ()
    | [a] -> printer a
    | x::xs -> printer x; Format.print_space (); it_print xs 
  in 
    obox(); it_print els; cbox()

let pr_opword word printer = function
    [x] -> printer x
  |  (x1::x2::xs as l) -> pr_one word (pr_list printer) l
  |  [] -> ()

let pr_oplist word printer op2str = function
    (a, []) -> printer a
  | (a, l) -> Format.print_string word; space ();
      printer a; space (); pr_list (fun (x,y) -> Format.print_string "(";
				      Format.print_string (op2str x);
				      space ();
				      printer y;
				      Format.print_string ")";
				      space ())
	l
					      
let pr_blist printer els =
  Format.print_string "("; pr_list printer els; Format.print_string ")"

let pr_strwsp str =
  Format.print_string str; Format.print_space ()

let rec print_ast = function
    Program stmts -> pr_one "program" (List.map print_stmt) stmts

and print_stmt = function
    Def (f, args, body) -> pr_one "def" (fun (a,b) -> pr_blist Format.print_string a;
					   Format.print_space (); 
					   print_suite b)
      (args,body)
  | Simple smpstt -> print_smpl_stmt smpstt
  | _ -> ()

and print_suite = function
    Suite_single simple_stmt -> Format.print_string "Simple statment"; print_smpl_stmt simple_stmt
  | Suite stmts -> pr_one "suite" (pr_list print_stmt) stmts;

and print_smpl_stmt = function
    Single small_stmt -> print_small_stmt small_stmt
  | Begin small_stmts -> pr_one "begin" (pr_list print_small_stmt) small_stmts 


and print_small_stmt = function
    Assignment (op, ts, tot) -> pr_one (aoptostr op) (fun () -> 
							pr_blist print_test ts;
							Format.print_space (); 
							print_tot tot)
      ()
  | Expr tot -> pr_one "expr" print_tot tot
  | _ -> ()

and print_tot = function
    Test t -> print_test t
  | Tuple ts -> pr_one "tuple" (pr_list print_test) ts

and print_test = function
    If_test (ot1, ot2, t) -> pr_one "if" (fun (a,b,c) -> print_ot a; space ();
					    print_ot b; space ();
					    print_test c) 
      (ot1, ot2, t)
  | Or_test ot -> print_ot ot
  | Lambda (args, t) -> ()

and print_ot = function
    Or ([at]) -> print_at at; 
  | Or (a1::a2::tl as ats) -> pr_one "or" (pr_list print_at) ats
  | Or [] -> ()

and print_at = function
    And [nt] -> print_not nt; 
  | And (nt1::nt2::tl as nts) -> pr_one "and" (pr_list print_not) nts
  | And [] -> ()

and print_not = function
(*    NtStexp stexp -> print_stexp stexp*)
  | _ -> (); Format.print_string "got to and";

and print_stexp = function
    Star_exp e -> print_exp e
  | _ -> ()

and print_exp = function
    Bitwise_or xors -> pr_opword "bitwise-or" print_xor xors

and print_xor = function
    Bitwise_xor ands -> pr_opword "bitwise-xor" print_and ands

and print_and = function
    Bitwise_and shifts -> pr_opword "bitwise-and" print_shift shifts

and print_shift = function
    Shift (arith, ops_as) -> pr_oplist "shift" print_arith shfttostr (arith, ops_as)

and print_arith = function
    _ -> ()
*)
