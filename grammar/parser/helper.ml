
type exp =
  Let of binding list * exp
| If of exp * exp * exp
| Binop of exp * op * exp
| Assign of binding
| Var of string
| Const of int
and binding = Bind of string * exp
and op = Add | Sub | Mul | Div

let rec string_of_binding = function
  Bind(name, exp) ->
    name ^ "=" ^ (string_of_exp exp)
and string_of_bindings = function
  [] -> ""
| [b] -> string_of_binding b
| b::bs -> (string_of_binding b) ^ " and " ^ (string_of_bindings  bs)
and string_of_op = function
  Add -> "Add"
| Sub -> "Sub"
| Mul -> "Mul"
| Div -> "Div"
and string_of_exp exp =
  match exp with
  | Let(bds, exp) ->
      "Let " ^ (string_of_bindings bds) ^ " in " ^ (string_of_exp exp)
  | Const(n) ->
      "Const(" ^ (string_of_int n) ^ ")"
  | Var(x) ->
      "Var(" ^ x ^ ")"
  | Binop(e1, op, e2) ->
      (string_of_op op) ^ "(" ^ (string_of_exp e1) ^ ", "
      ^ (string_of_exp e2) ^ ")"
  | _ -> "Not supported yet"
;;
