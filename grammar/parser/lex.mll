{
open Parse
open Helper
let kwtbl = Hashtbl.create 31
let () =
  List.iter (fun (kwd,tok) ->
    Hashtbl.add kwtbl kwd tok)
  [("let", LET);
   ("in", IN);
   ("if", IF);
   ("then", THEN);
   ("and", AND);
  ]
}

let ident = ['a'-'z' 'A'-'Z']+

rule token = parse
| [' ' '\t']
  {
    token lexbuf
  }
| (['0'-'9']+) as i
  {
    NUM(int_of_string i)
  }
| (['a'-'z' 'A'-'Z']+) as id
  {
    try
      Hashtbl.find kwtbl id
    with
      Not_found ->
        NAME id
  }
| eof
  {
    EOF
  }
| '='
  {
    EQ
  }
| ";;"
  {
    SEMICOLON
  }
| '+' { PLUS }
| '-' { MINUS }
| _
  {
    token lexbuf
  }

