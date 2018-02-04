open Printf

module NSet = Set.Make(struct
  type t = int
  let compare = Pervasives.compare
end)
module GMap = Map.Make(struct
  type t = int
  let compare = Pervasives.compare
end)

type node = {v: int; mutable neibors: NSet.t}

let del_node x g =
  GMap.mapi (fun k ns ->
    NSet.remove x ns)
    (GMap.remove x g)
;;

let add_ege (a,b) g =
  let r = GMap.find_opt a g in
  match r with
  | None -> GMap.add a NSet.(empty |> add b) g
  | Some(ns) -> GMap.add a (NSet.add b ns) g
;;

let from_list l = 
  let rec f l g =
    match l with
    | [] -> g
    | (a,b)::xs ->
        let g' = add_ege (a,b) g in
        f xs g'
  in
    f l GMap.empty
;;
let show g =
  GMap.iter (fun k ns ->
    printf "%d->\n" k;
    NSet.iter (function v -> printf "  %d\n" v) ns) g
;;

let _ =
  let g = from_list [(1, 2);
    (3,4);
    (3,5);
    (2,5);
    (3,7);
    (5,7);
    (5,8);
    (1,8)] in
  show g;
  printf "after delete 5\n";
  let g' = del_node 5 g in
  show g'
