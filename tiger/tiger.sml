structure Tiger = struct
    structure TigerLrVals = TigerLrValsFun( structure Token = LrParser.Token )
    structure TigerLexer = TigerLexerFun( structure Tokens = TigerLrVals.Tokens )
    structure TigerParser = Join( structure ParserData = TigerLrVals.ParserData
                                  structure Lex = TigerLexer
                                  structure LrParser = LrParser )
    structure L = TigerLexer.UserDeclarations
    structure A = Absyn

    fun printError linenum msg = print ("Error: Line " ^ (Int.toString linenum) ^ ": " ^ msg ^ "\n")

    fun printParseError (msg, startLine, endLine) = let
        val lineLabel = if startLine = endLine then "Line " ^ (Int.toString startLine)
                        else "Lines " ^ (Int.toString startLine) ^ "-" ^ (Int.toString endLine) in
        print ("Error: " ^ lineLabel ^ ": " ^ msg ^ "\n")
    end

    fun indent 0 = ""
      | indent lvl = " " ^ (indent (lvl-1))

    fun opToString oper = case oper of
          A.PlusOp => "+"
        | A.MinusOp => "-"
        | A.TimesOp => "*"
        | A.DivideOp => "/"
        | A.EqOp => "="
        | A.NeqOp => "<>"
        | A.LtOp => "<"
        | A.LeOp => "<="
        | A.GtOp => ">"
        | A.GeOp => ">="

    fun varToString lvl v = case v of
        A.SimpleVar (sym, _) => Atom.toString sym
      | A.FieldVar (v', sym, _) => (varToString (lvl+1) v') ^ "." ^ (Atom.toString sym)
      | A.SubscriptVar (v', exp, _) => (varToString (lvl+1) v') ^ "[" ^ (expToString (lvl+1) exp) ^ "]"
    and decToString lvl d = case d of
        A.FunctionDec _ => "fundec" (* TODO fundec *)
      | A.VarDec {name=name, escape=_, typ=_, init=init, pos=_} => "var " ^ (Atom.toString name) ^ ":=" ^ (expToString (lvl+1) init)
      | A.TypeDec _ => "tydecs" (* TODO tydecs *)
    and expToString lvl e = case e of
        A.VarExp v => varToString (lvl+1) v
      | A.NilExp => "nil"
      | A.IntExp i => Int.toString i
      | A.StringExp (s, _) => s
      | A.CallExp {func=symbol, args=_, pos=_} => (Atom.toString symbol) ^ "()" (* TODO args *)
      | A.OpExp {left=left, oper=oper, right=right, pos=_} => "(" ^ (expToString (lvl+1) left) ^ (opToString oper) ^ (expToString (lvl+1) right) ^ ")"
      | A.RecordExp {fields=_, typ=_, pos=_} => "recordexp" (* TODO recordexp *)
      | A.SeqExp seq => "\n" ^ (String.concatWith ";\n" (map (fn (ex, _) => (indent lvl) ^ expToString (lvl+1) ex) seq)) ^ "\n"
      | A.AssignExp {var=var, exp=exp, pos=_} => "(" ^ (varToString (lvl+1) var) ^ ":=" ^ (expToString (lvl+1) exp) ^ ")"
      | A.IfExp {test=test, then'=then', else'=else', pos=_} =>
            "(if " ^ (expToString (lvl+1) test) ^ " then " ^ (expToString (lvl+1) then')
            ^ (case else' of NONE => "" | SOME ex => " else " ^ expToString (lvl+1) ex) ^ (indent (lvl-1)) ^ ")" (* TODO(if and while): only indent when last printed expr is a SeqExpr *)
      | A.WhileExp {test=test, body=body, pos=_} => "(while " ^ (expToString (lvl+1) test) ^ " do " ^ (expToString (lvl+1) body) ^ (indent (lvl-1)) ^ ")"
      | A.ForExp {var=var, escape=_, lo=lo, hi=hi, body=body, pos=_} =>
            "(for " ^ (Atom.toString var) ^ ":=" ^ (expToString (lvl+1) lo) ^ "to" ^ (expToString (lvl+1) hi) ^ " do " ^ (expToString (lvl+1) body) ^ ")"
      | A.BreakExp _ => "break"
      | A.LetExp {decs=decs, body=body, pos=_} =>
            "let " ^ (String.concatWith " " (map (decToString (lvl+1)) decs)) ^ " in " ^ (expToString (lvl+1) body) ^ " end"
      | A.ArrayExp _ => "arrayexp" (* TODO: arrayexp *)

    fun parse filename = let 
        val file = TextIO.openIn filename
        fun get _ = TextIO.input file
        val lexer = TigerParser.makeLexer get
        fun run() = let
            val result = TigerParser.parse (0, lexer, printParseError, ()) in
            TextIO.closeIn file; result
        end
    in
        SOME (run()) handle
            (L.LexerError (L.UnclosedComment, linenum)) =>
                (printError linenum "Unclosed comment"; NONE)
            | (L.LexerError (L.ImproperMultilineString, linenum)) =>
                (printError linenum "Improperly formatted multiline string"; NONE)
            | (L.LexerError (L.UnknownEscapeSequence seq, linenum)) =>
                (printError linenum ("Unknown escape sequence: '\\" ^ seq ^ "'"); NONE)
            | TigerParser.ParseError =>
                (print "Parsing failed\n"; NONE)
    end

    fun prettyPrint NONE = ()
      | prettyPrint (SOME (exp, _)) = print ((expToString 0 exp) ^ "\n")

    val // = OS.Path.concat
    infix //

    fun runTestResult testFile = (
        print ("Running: " ^ testFile ^ "...\n");
        parse testFile
    )

    fun runTest testFile = (
        print ("Running: " ^ testFile ^ "...\n");
        parse testFile;
        print "\n"
    )

    fun visitDirs dir f = let
        val dirStream = OS.FileSys.openDir dir
        fun visit () = case OS.FileSys.readDir dirStream of
            NONE      => ()
          | SOME file => (f (dir // file); visit())
    in
        visit();
        OS.FileSys.closeDir dirStream
    end

    fun runTests testDir = visitDirs testDir runTest

    fun runAllTests () = (
        runTests ("tests" // "syntax");
        runTests ("tests" // "semantics");
        print "Done running tests\n"
    )
end