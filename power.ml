
module OrderInt = struct
  type t = int
  let compare (a:int) (b:int) = Pervasives.compare a b
end

module IntSet = Set.Make(OrderInt)

module SetSet = Set.Make(struct
  type t = IntSet.t
  let compare s1 s2 =
    if IntSet.equal s1 s2
    then 0
    else if IntSet.subset s1 s2 then -1
    else 1
end)

(* int set -> int set set *)
let power_set s =
  IntSet.fold (fun x s1 ->
    let s' = SetSet.map (fun y -> IntSet.add x y) s1 in
    SetSet.union s1 s') s SetSet.(empty |> add IntSet.empty)
  ;;

let show_set ss =
  print_string "{";
  SetSet.iter (fun s ->
    print_string "{";
    IntSet.iter (fun x -> print_int x; print_string ", ") s;
    print_string "}, ") ss;
  print_endline "}"
;;

(* test *)
let res = power_set IntSet.(empty |> add 1 |> add 2 |> add 3 |> add 4) in
show_set res
