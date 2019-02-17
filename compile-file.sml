
fun compileFile filename printParsetree printCodetree =
  let
    open TextIO
    val inStream = getInstream(TextIO.openIn filename)
    open StreamIO
    val stream = ref inStream

    val lineNo = ref 1
    val (start, _) = inputN(inStream, 2)
    fun getChar () =
      case input1 (! stream) of
        NONE => NONE
      | SOME (eoln as #"\n", strm) =>
        ( lineNo := !lineNo + 1;
          stream := strm;
          SOME eoln
        )
      | SOME (c, strm) => (stream := strm; SOME c)
  in
    let
    open PolyML.Compiler
    val oldParsetree = !parsetree
    val oldCodetree = !codetree
    val _ = parsetree := printParsetree;
    val _ = codetree := printCodetree;
    val code = PolyML.compiler(getChar,
      [
      CPFileName filename,
      (*Universal.tagInject Debug.parsetreeTag true,*)
      CPLineNo (fn () => !lineNo)])
        handle exn => (closeIn(!stream); PolyML.Exception.reraise exn)
    in
      (
        closeIn (!stream);
        parsetree := oldParsetree;
        codetree := oldCodetree;
        code ()
      )
    end
  end
