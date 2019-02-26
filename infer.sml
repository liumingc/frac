(* WIP
  26/2/19 prototype 
*)

datatype exp =
    Int of int
    (* Int should be OK, not to conflict with structure Int.
    * Conceptually, they belong to diffrent namespaces, and there is some
    * syntatic ways to distinguish them. If Int follow by dot, or appearing in
    * where a structure should in place, like in functor apolication.
    * Otherwise, if it's in a value constructor position, then it's a val name.
    *)
  | Bool of bool
  | Var of string
  | Fn of string * exp
  | If of exp * exp * exp
  | App of exp * exp
  | Sub of exp * exp

and ty =
    Tint
  | Tbool
  | Tfn of ty * ty
  | Tvar of string


fun typeof (x: exp, env) : ty =
  case x of
       Int i => Tint
    | Bool b => Tbool
    | Fn (arg, body) =>
        let
          val targ = typeof(Var arg, env)
          val _ = extend(env, arg, targ)
          val tbody = typeof(body, env)
        in
          Tfn(targ, tbody)
        end
    | If (tst, ifso, ifnot) =>
        let
          val _ = addEq(env, tst, Tbool)
          val tifso = typeof(ifso, env)
          val tifnot = typeof(ifnot, env)
          val _ = addEq(env, tifso, tifnot)
        in
          tifso
        end

    | App (rator, rand) =>
        let
          val trand = typeof(rand, env)
          val target = newTyVar()
          val tapp = Tfn(trand, target)
          val _ = addEq(env, rator, tapp)
        in
          tapp
        end
    | Sub (x1, x2) =>
        let
          val _ = addEq(env, x1, Tint)
          val _ = addEq(env, x2, Tint)
        in
          Tint
        end
;
          


