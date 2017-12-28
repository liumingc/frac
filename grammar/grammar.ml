(*
%token A
%token B

%%

%start a
a : A | A b B ;
b : A | ;

 *)

open List

module StrSet = Set.Make(String) (* string set *)

(* handle reading *)


let get_tokens = function () ->
  let tokens = ref [] in
  let sep = Str.regexp "[ \t]+" in
  let fp = open_in Sys.argv.(1) in
  (try
    while true do
      let line = input_line fp in
      let rs = Str.split sep line in
      tokens := !tokens @ rs
    done
  with
    End_of_file -> close_in fp);
      !tokens

let terms = ref StrSet.empty
let noterms = ref StrSet.empty

module SymSet = Set.Make(struct
  type t = int
  let compare = Pervasives.compare
end)

type sym_desc = {
  name: string;
  no: int;
  term: bool;
  mutable empty: bool;
  mutable first: SymSet.t;
  mutable follow: SymSet.t
}
and rule_desc = {
  lhs: int;
  mutable rhs: int list;
}

(*let symtab = ref [||] (* contains all the sym, include $eof, $empty *) *)

let build_symtab (terms:StrSet.t) (nterms:StrSet.t) =
  let t_c = StrSet.cardinal terms
  and nt_c = StrSet.cardinal nterms in
  let sym_eof = { name="eof";
    no=0;
    term=true;
    empty=false;
    first=SymSet.(empty |> add 0);
    follow=SymSet.empty } in
  let sym_e = {sym_eof with name="$$";
    no=1;
    empty=true;
    first=SymSet.(empty |> add 1);
  } in
  let symtab = Array.make (t_c + nt_c + 2) sym_eof in
  symtab.(0) <- sym_eof;
  symtab.(1) <- sym_e;
  let i = ref 2 in
  StrSet.iter (fun s ->
    let sym = { name=s;
      no=(!i);
      term=true;
      empty=false;
      first=SymSet.(empty |> add !i);
      follow=SymSet.empty
    } in
    symtab.(!i) <- sym;
    incr i
  ) terms;
  StrSet.iter (fun s ->
    let sym = {name=s;
    no=(!i);
    term=false;
    empty=false;
    first=SymSet.empty;
    follow=SymSet.empty
    } in
    symtab.(!i) <- sym;
    incr i
  ) nterms;
  symtab
;;

let build_lookup_tab symtab =
  let tbl = Hashtbl.create 117 in
  Array.iter (fun sym ->
    Hashtbl.add tbl sym.name sym.no) symtab;
    tbl
;;

let print_rules rules symarr =
  List.iter (fun rule ->
    let lhs = symarr.(rule.lhs).name in
    Printf.printf "%s ->" lhs;
    List.iter (fun n ->
      let name = symarr.(n).name in
      Printf.printf " %s" name) rule.rhs;
    print_newline ()
  ) rules
;;


let build_info toks =
  let rec f rst =
    match rst with
    | ("%token"::x::rst) ->
      terms := StrSet.add x !terms;
      f rst
    | "%%"::rst -> rst
    | rst -> rst
  in
    let rst = f toks in
    List.iter (fun x ->
      match x with
      | ";" | "|" | ":" -> ignore ()
      | x -> if StrSet.mem x !terms then
               terms := StrSet.add x !terms
             else
               noterms := StrSet.add x !noterms
    ) rst;
    (* build symtab etc *)
    let symtab = build_symtab !terms !noterms in
    let tbl = build_lookup_tab symtab in
    let rec f rst rules =
      match rst with
      | (lhs::":"::rst) ->
          let lhs = Hashtbl.find tbl lhs in
          let rule = {lhs=lhs; rhs=[]} in
          (* loop until we match ; or | *)
          let rec lp rst =
            match rst with
            | ";"::rst -> f rst (rule::rules)
            | "|"::rst -> 
                let name = (symtab.(lhs)).name in
                f (name::":"::rst) (rule::rules)
            | x::rst ->
                let sym_no = Hashtbl.find tbl x in begin
                  rule.rhs <- sym_no::rule.rhs;
                  match rst with
                  | [] -> rules
                  | rst -> lp rst
                end
            | [] -> rules
          in lp rst
      | _ -> rules
    in
      let rules = f rst [] in
      iter (fun x ->
        x.rhs <- rev x.rhs) rules;
      let rules = rev rules in begin
        print_rules rules symtab;
        (rules, symtab, tbl)
      end
;;


let print_first symtab =
  Printf.printf "=========\n\n";
  Array.iter (fun sym ->
    let first = sym.first in
    Printf.printf "%s FIRST:\n" sym.name;
    SymSet.iter (fun x ->
      let name = symtab.(x).name in
      Printf.printf "\t%s\n" name) first
  ) symtab
;;

let build_first rules symtab =
  let change = ref false in
  let rec lp () =
    change := false;
    let rec iter_rules rules =
      match rules with
      | [] ->
          if !change then
            lp ()
          else
            print_first symtab
      | rule::rules ->
          let lhs = symtab.(rule.lhs) in
          let rhs = List.map (fun x -> symtab.(x)) rule.rhs in
          let rhs_cnt = List.length rhs in
          if rhs_cnt = 0 then
            lhs.empty <- true
          else
            begin
              let rec iter_rhs rhs =
                match rhs with
                | [] -> lhs.empty <- true
                | x::rhs ->
                    begin
                      let coll = SymSet.union lhs.first x.first in
                      (
                      if SymSet.cardinal coll > SymSet.cardinal lhs.first then
                        change := true;
                        lhs.first <- coll
                      );
                      if x.empty then
                        iter_rhs rhs
                    end
              in iter_rhs rhs
            end;
            iter_rules rules
    in iter_rules rules
  in
    lp ()
;;

let print_follow symtab =
  Printf.printf "=========\n\n";
  Array.iter (fun sym ->
    let fol = sym.follow in
    Printf.printf "%s FOLLOW:\n" sym.name;
    SymSet.iter (fun x ->
      let name = symtab.(x).name in
      Printf.printf "\t%s\n" name) fol
  ) symtab
;;

let build_follow rules symtab =
  let start = ref symtab.(0) in
  begin
  (
  match rules with
  | rule::rules ->
      start := symtab.(rule.lhs);
      (!start).follow <- SymSet.(empty |> add 0)
  | _ -> ignore (failwith "no rules!"));
  let change = ref true in
  while !change do
    change := false;
    let rec lp_rules rules =
      match rules with
      | [] -> ignore () (*print_follow symtab*)
      | rule::rules ->
          let rec lp_rhs rhs fol =
            match rhs with
            | [] ->
                lp_rules rules
            | (x::xs) ->
                let fol' = SymSet.union x.follow fol in begin
                (if SymSet.cardinal fol' > SymSet.cardinal x.follow then begin
                  change := true;
                  x.follow <- fol'
                end);
                (if x.empty then
                  lp_rhs xs (SymSet.union fol' x.first)
                else
                  lp_rhs xs x.first
                )
                end
          in let lhs = symtab.(rule.lhs) in
          let rhs = List.map (fun x -> symtab.(x)) (List.rev rule.rhs) in
          lp_rhs rhs lhs.follow
    in lp_rules rules
  done;
  print_follow symtab
  end
;;


let () =
  let toks = get_tokens () in
  let (rules, symtab, tbl) = build_info toks in
  build_first rules symtab;
  build_follow rules symtab

  (*
  (* TODO *)

let closure (s : ItemSet.t ref) =
  let changing = ref true in
while !changing do
  Set.iter (fun item ->
    let rule = List.nth rules item.rule_no in
let rhs = rule.rhs in
if item.pos < List.length rhs then
  let x = List.nth rhs item.pos in
if Set.mem x noterms then
  List.iter (fun rule ->
    if rule.lhs == x then
      s := 
              else
                yyy
  done

let () =
  let toks = get_tokens () in
let rules = build_info toks in
print_endline "> term";
  StrSet.iter print_endline !terms;
  print_endline "> noterm";
  StrSet.iter print_endline !noterms;
  print_endline "rules";
  List.iter (fun x ->
    print_string (x.lhs ^ " -> ");
    List.iter (fun e -> print_string (e ^ " ")) x.rhs;
    print_newline ()) rules

    *)
