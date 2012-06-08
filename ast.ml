exception Ast_Abused

(* snipped out class system... so what are fields doing here? *)

(* expr types *)

type ident = string
type number = int
type bop = Bplus | Bminus
type uop = Uplus | Uminus

type expr =
  | If of expr * expr * expr
  | Lambda of (ident list) * expr
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
type aop = Aeq

type stmt = 
(* Simple expressions *)
  | Assign of aop * (expr list) * (expr list)
(* I don't really like this... sleep on it *)
  | Expr of expr
  | Del of expr
  | Pass
  | Break
  | Continue
  | Return of expr
  | Raise of expr option * expr option
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
