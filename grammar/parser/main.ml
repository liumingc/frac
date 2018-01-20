open Helper

let _ =
  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let result = Parse.prog Lex.token lexbuf in
  List.iter (fun e ->
    print_endline (string_of_exp e)) result;
  result
