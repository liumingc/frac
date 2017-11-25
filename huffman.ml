let rec calc_freq l tbl =
  match l with
  | [] -> tbl
  | (x::l) ->
      let r = Hashtbl.find_opt tbl x in
      match r with
      | None -> (Hashtbl.add tbl x 1); calc_freq l tbl
      | Some n -> (Hashtbl.replace tbl x (n+1)); calc_freq l tbl;;


let rec add_freq l =
  let tbl = calc_freq l (Hashtbl.create 1007)
  and r = ref []
  in  Hashtbl.iter
        (fun k n ->
          r := (k,n)::(!r))
        tbl;
      !r;;

type t = char
type 't htree = Leaf of 't | Node of 't htree * 't htree

let rec lst2tree l =
  (*let rec l' = List.map (fun (x, p) -> (Leaf(x), p)) l*)
  let rec find_min l =
    match l with
    | (((x1,n1) as e1)::((x2,n2) as e2)::[]) ->
        if n1 < n2 then
          (e1, e2, [])
        else
          (e2, e1, [])
    | ((x1, n1) as e1)::rest ->
        let (((x2, n2) as e2), ((x3, n3) as e3), rest) = find_min rest
        in  if n1 <= n2 then
              (e1, e2, e3::rest)
            else if n1 <= n3 then
              (e2, e1, e3::rest)
            else
              (e2, e3, e1::rest)
  in
    match l with
    | [a] -> a
    | [(t1,n1); (t2,n2)] -> (Node(t1, t2), n1+n2)
    | _ ->
        let ((x1,n1), (x2,n2), rest) = find_min l
        in  lst2tree ((Node(x1, x2), n1+n2)::rest)
;;


let rec string_of_htree x =
  match x with
  | Leaf(n) -> "Leaf(" ^ (Char.escaped n) ^ ")"
  | Node(l, r) -> "Node(" ^ (string_of_htree l) ^ ", " ^ (string_of_htree r) ^ ")";;

(* 't htree -> (char*string) list *)
let enc_of_htree x =
  let rec helper coll path x =
    match x with
    | Leaf(c) -> coll := (c,path) :: (!coll); coll
    | Node(l, r) ->
        helper (helper coll (path^"0") r) (path^"1") l
  in let r = helper (ref []) "" x
  in !r;;

let () =
  let fp = open_in Sys.argv.(1)
  and buf = ref []
  in
    try
      while true do
        let c = input_char fp in
        buf := c :: (!buf)
      done
    with
    | End_of_file ->
      (* we don't care the sequence, we should rev it, but no need *)
      close_in fp;
      let r = add_freq (!buf)
      in
        (*
        List.iter
          (fun (x,prob) ->
            Printf.printf "%c, %d\n" x prob)
          r;
        *)
        let t = (fst (lst2tree (List.map (fun (x, p) -> (Leaf(x), p)) r)))
        in  (*print_string (string_of_htree t);*)
            List.iter
              (fun (c,s) -> Printf.printf "%c, %s\n" c s)
              (enc_of_htree t)

