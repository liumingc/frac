
let from_list lst =
  let n = List.length lst in
  let m = Array.make_matrix n n 0.0 in
  List.iteri (fun i xi ->
    List.iteri (fun j yj ->
      m.(i).(j) <- yj) xi) lst;
      m
;;

let make_e n =
  let m = Array.make_matrix n n 0.0 in
  Array.iteri (fun i row ->
    m.(i).(i) <- 1.0) m;
  m
;;

let print_matrix m =
  Array.iter (fun row ->
    Printf.printf "[";
    Array.iter (fun col ->
      Printf.printf "%f, " col) row;
    Printf.printf "];\n"
  ) m
;;

let pm m e =
  Printf.printf "m:\n";
  print_matrix m;
  Printf.printf "-----------\ne:\n";
  print_matrix e;
  Printf.printf "\n\n"
;;


(*
 * transform #1, swap rows
 * transform #2, multiply row j with k and add to row i
 * transform #3, multiply row i with k
 *)

let swap m i j =
  let mt = m.(i) in
  m.(i) <- m.(j);
  m.(j) <- mt
;;

let swap2 m e i j =
  Printf.printf "swap r(%d) <-> r(%d)\n" i j;
  swap m i j;
  swap e i j;
  pm m e
;;

let muladd m i j k =
  let row = m.(i) in
  Array.iteri (fun c _ ->
    row.(c) <- row.(c) +. m.(j).(c) *. k) row
;;

let muladd2 m e i j k =
  Printf.printf "r(%d) <- r(%d) + r(%d) * %f\n" i i j k;
  muladd m i j k;
  muladd e i j k;
  pm m e
;;

let mul m i k =
  let row = m.(i) in
  Array.iteri (fun c _ ->
    row.(c) <- row.(c) *. k) row
;;

let mul2 m e i k =
  Printf.printf "r(%d) mul by %f\n" i k;
  mul m i k;
  mul e i k;
  pm m e;
;;

let find_nonzero m c =
  let rec f m r c =
    if m.(r).(c) <> 0.0 then
      r
    else
      f m (r + 1) c (* this may throw boudary exception *)
  in
    f m c c
;;

let redu2 m e c =
  Printf.printf "reducing c(%d)\n" c;
  let n = Array.length m in
  let rec lp m r c =
    if r < n then (
      let x = m.(r).(c) in
      if x <> 0.0 then
        muladd2 m e r c (0. -. x);
      lp m (r+1) c
    )
  in
    lp m (c+1) c;
  pm m e
;;

(*
let redu2 m e c =
  Printf.printf "reducing c(%d)\n" c;
  redu m c;
  redu e c;
  pm m e
;;
*)

let bottomup m e =
  let n = Array.length m in
  let rec line r c =
    if c < n then (
      let x = m.(r).(c) in
      if x <> 0. then
        muladd2 m e r c (0. -. x);
      line r (c+1)
    )
  and mat r =
    if r >= 0 then (
      line r (r+1);
      mat (r-1)
    )
  in
    mat (n-1)
;;


let calc_inverse lst =
  let m = from_list lst in
  let n = Array.length m in
  let e = make_e n in
  let rec bubble m c =
    if c < Array.length m then (
      let r = find_nonzero m c in
      if r > c then
        swap2 m e r c;
      if m.(c).(c) <> 1.0 then
        mul2 m e c (1.0 /. m.(c).(c));
      redu2 m e c;
      bubble m (c+1)
    )
  in
    bubble m 0;
    bottomup m e;
    (m, e);;

let lst = [[-1.0; 1.0; 1.0; 1.0];
    [1.0; -1.0; 1.0; 1.0];
    [1.0; 1.0; -1.0; 1.0];
    [1.0; 1.0; 1.0; -1.0]]
;;

let _ =
  calc_inverse lst
;;
