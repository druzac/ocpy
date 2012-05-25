(*open Sexplib.Std*)
(* lists are supposed to be non-empty unless otherwise mentioned *)

type program = Program of stmt list
and funcdef = Def of (string list) * suite
and stmt = Smpl_stmt of simple_stmt | Cmpnd_stmt of compound_stmt
and simple_stmt = Single of small_stmt | Begin of small_stmt list
and small_stmt = Expr_stmt of expr_stmt
  | Del_stmt of del_stmt
  | Pass_stmt of pass_stmt
  | Flow_stmt of flow_stmt
  | Global_stmt of global_stmt
  | Nonlocal_stmt of nonlocal_stmt
  | Assert_stmt of assert_stmt
and expr_stmt = Assignment of assign_op * (test list) * (test list) (*this is supposed to be a tuple_or_test*)
  | Expr of test list
(*and tuple_or_test = Test of test | Tuple of test list *)
and assign_op = Pluseq | Minuseq | Stareq | Slasheq | Percenteq
| Ampeq | Pipeeq | Careteq | Dlteq | Dgteq | Dstareq | Dslasheq
| Eq
and del_stmt = Del of star_expr
and pass_stmt = Pass
and flow_stmt = Break_stmt of break_stmt
| Continue_stmt of continue_stmt
| Return_stmt of return_stmt
| Raise_stmt of raise_stmt
and break_stmt = Break
and continue_stmt = Continue
and return_stmt = Return of test list (* this list can be empty *)
and raise_stmt = Raise of test option * test option
and global_stmt = Global of string list
and nonlocal_stmt = Nonlocal of string list
and assert_stmt = Assert of test * test option
and compound_stmt = If_stmt of if_stmt
| While_stmt of while_stmt
| For_stmt of for_stmt
| Try_stmt of try_stmt
| Funcdef of funcdef
and if_stmt = Cond of (test * suite) list * (suite option)
and while_stmt = While of test * suite * (suite option)
and for_stmt = For of string * test * suite * (suite option)
and try_stmt = Try of suite * (catch * suite) list * maybe_result * maybe_result (* this list may be empty*)
and maybe_result = suite option  (* None is a stand in for #f, here *)
and catch = Except of (test * (string option)) option
and suite = Suite_single of simple_stmt | Suite of stmt list
             (* lots of stuff missing here... *)
and test = If_test of or_test * or_test * test
  | Or_test of or_test
  | Lambdadef of lambda_def
and lambda_def = Lambda of (string list) * test
and or_test = Or of and_test list
and and_test = And of not_test list
and not_test = Comp of comparison | Not of not_test
and comparison = Cmp_cmp of star_expr * (comp_op * star_expr) list
and comp_op = Lt | Gt | Eqeq | Gteq | Lteq | Ltgt | Noteq | In | Notin | Is | Isnot
and star_expr = Sexp_exp of expr | Sexp_sexp of expr
and expr = Exp of xor_expr list
and xor_expr = Xor_exp of and_expr list
and and_expr = And_exp of shift_expr list
and shift_expr = Shift of arith_expr * (shift_op * arith_expr) list
and shift_op = Dlt | Dgt


and arith_expr = Arith of term * (arith_op * term) list
and arith_op = Plus | Minus
and term = Term of factor * (factor_op * factor) list
and factor_op = Star | Fslash | Percent | Dfslash
and factor = Power of power | Uapp of unary_op * factor
and unary_op = Uplus | Uminus | Utilde
and indexed = IndAtom of atom * trailer list
and power = Pow_index of indexed | Pow_factor of indexed * factor
and atom = (*T_OR_T of tuple_or_test | TUPLE of tuple
         |  T_LST of testlist
         |  DCT of dict
         |  SET of set*)
         |  Name of string
         |  Number of number
         |  String of string
         |  Ellipsis
         |  None
         |  True
         |  False
and number = int
and trailer = unit
                (*with sexp*)
(*and trailer = CALLED of arglist 
            |  SUBSCRIPT of tuple_or_test
            |  DOT of string
and testlist = test list*)
(*and dict = (test * test) list 
and set = SSET of test list
and arglist = ARGLIST of test list   *)
(*
let get_sexp p = sexp_of_program p

let rec print_sexp = function
    Sexplib.Sexp.List l -> print_string "("; List.iter print_sexp l; print_string ")"; 
  | Sexplib.Sexp.Atom a -> print_string a; print_string " "; 
 *)
