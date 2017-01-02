(* ocaml CPS converter *)

type symbol = string

(* lambda exp *)
module L = struct
type exp = Basic of int
          | Symbol of symbol
          | Quote of exp
          | If of exp * exp * exp
          | Fn of symbol * exp
          | App of exp * exp
end


type simpleExp = Basic of int
          | Symbol of symbol
          | Quote of L.exp
          | Fn of symbol * cont * cpsExp
and cpsExp = Simple of simpleExp
          | AppCont of cont * simpleExp
          | App of simpleExp * simpleExp * cont
          | If of simpleExp * cpsExp * cpsExp
and cont = K of symbol * cpsExp | Ks of symbol

let rec cpsConvert (exp, k) =
  match exp with
  | L.Basic x -> AppCont(k, Basic(x))
  | L.Symbol s -> AppCont(k, Symbol(s))
  | L.Quote x -> AppCont(k, Quote(x))
  | L.If (e1, e2, e3) ->
    cpsConvert(e1,
      let v1 = "v1" in
      K(v1, If(Symbol(v1), cpsConvert(e2, k), cpsConvert(e3, k))))
  | L.Fn (arg, e1) -> 
    let k1 = Ks("k2") in
      Simple(Fn(arg, k1, cpsConvert(e1, k1)))
  | L.App (e1, e2) ->
    let v1 = "v1" and v2 = "v2" in
      cpsConvert(e1,
        K(v1, cpsConvert(e1, K(v2, App(Symbol(v1), Symbol(v2), k)))))
;;


open Printf;;
let testExp = L.App(L.Basic(3), L.Basic(4)) in
  cpsConvert(testExp, Ks("k0"))
