
let _ =
  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let result = Parse.expr Lex.token lexbuf in
  result
