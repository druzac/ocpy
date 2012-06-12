exception Ast_Abused

(* snipped out class system... so what are fields doing here? *)

(* expr types *)

type ident = string
type number = int
type bop = Bplus | Bminus | Bstar | Bslash | Bpercent | Bdslash
	   | Bdstar | Bpipe | Bcaret | Bamp | Bdlt | Bdgt
	   | Blt | Bgt | Bdeq | Bgteq | Blteq | Bnoteq
	   | Bin | Bnotin | Bis | Bisnot
	   | Band | Bor
type uop = Uplus | Uminus | Utilde | Unot

type expr =
  | If of expr * expr * expr
  | Lambda of (ident list) * expr
  | Star of expr
  | Bapp of bop * expr * expr
  | Uapp of uop * expr
  | App of expr * (expr list)
  | Subscript of expr * expr
  | Dot of expr * ident
  | Tuple of expr list
  | List of expr list
  | Dict of (expr * expr) list
  | Set of expr list
  | Name of ident
  | Number of number
  | String of string
  | Ellipsis
  | None
  | True
  | False

(* statements types *)
type catch = (expr * (ident option)) option
type aop = Aeq |
Apluseq | Aminuseq| Astareq| Aslasheq| Apercenteq| Aampeq
| Apipeeq | Acareteq | Adlteq | Adgteq | Adstareq | Adslasheq
 
type stmt = 
(* Simple expressions *)
  | Assign of aop * (expr list) * (expr list)
(* I don't really like this... sleep on it *)
  | Expr of expr
  | Del of expr
  | Begin of suite
  | Pass
  | Break
  | Continue
  | Return of expr
  | Raise of (expr * expr option) option
  | Global of string list
  | Nonlocal of string list
  | Assert of expr list
(* end simple expressions *)
  | Cond of (expr * suite) list * (suite option)  (* if statement *)
  | While of expr * suite * (suite option)        (* while statement *)
  | For of string * expr * suite * (suite option) (* for statement *)
  | Try of suite * (catch * suite) list * (suite option) * (suite option) (* try stmt *)
  | Def of ident * (ident list) * suite                  (* Funcdef *)

and suite = stmt list

type program = Prog of suite

(* a singleton list -> its item
anything else -> wrapped by tuple *)
let exprs2expr elistcom = match elistcom with
  |(l, true) -> Tuple l
  | ([e], false) -> e
  | (l, false) -> (Tuple l)

let exprs2exprl elistcom = match elistcom with
    (l, _) -> l

let revbegin stmt = match stmt with
    Begin stmts -> Begin (List.rev stmts)
  | _ -> stmt

let for_make triple epi =
  let (nm, tst, suite) = triple in
    For (nm, tst, suite, epi)
