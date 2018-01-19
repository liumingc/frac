
type exp =
  Let of binding list * exp
| If of exp * exp * exp
| Binop of exp * op * exp
| Assign of binding
| Var of string
| Const of int
and binding = Bind of string * exp
and op = Add | Sub | Mul | Div
