structure Tiger = struct
    structure A = Absyn

    fun printError linenum msg = print ("Error: Line " ^ (Int.toString linenum) ^ ": " ^ msg ^ "\n")

    fun printParseError (msg, startLine, endLine) = let
        val lineLabel = if startLine = endLine then "Line " ^ (Int.toString startLine)
                        else "Lines " ^ (Int.toString startLine) ^ "-" ^ (Int.toString endLine) in
        print ("Error: " ^ lineLabel ^ ": " ^ msg ^ "\n")
    end

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
            (TigerParser.LexerError (TigerParser.UnclosedComment, linenum)) =>
                (printError linenum "Unclosed comment"; NONE)
            | (TigerParser.LexerError (TigerParser.ImproperMultilineString, linenum)) =>
                (printError linenum "Improperly formatted multiline string"; NONE)
            | (TigerParser.LexerError (TigerParser.UnknownEscapeSequence seq, linenum)) =>
                (printError linenum ("Unknown escape sequence: '\\" ^ seq ^ "'"); NONE)
            | TigerParser.ParseError =>
                (print "Parsing failed\n"; NONE)
    end

    fun prettyPrint NONE = ()
      | prettyPrint (SOME (exp, _)) = print ((Absyn.expToString 0 exp) ^ "\n")

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