structure Tiger = struct
    structure TigerLrVals = TigerLrValsFun( structure Token = LrParser.Token )
    structure TigerLexer = TigerLexerFun( structure Tokens = TigerLrVals.Tokens )
    structure TigerParser = Join( structure ParserData = TigerLrVals.ParserData
                                  structure Lex = TigerLexer
                                  structure LrParser = LrParser )
    structure L = TigerLexer.UserDeclarations

	fun printError linenum msg = print ("Error: Line " ^ (Int.toString linenum) ^ ": " ^ msg ^ "\n")

    fun printParseError (msg, startLine, endLine) =
        let
            val lineLabel = if startLine = endLine then "Line " ^ (Int.toString startLine)
                            else "Lines " ^ (Int.toString startLine) ^ "-" ^ (Int.toString endLine)
        in
            print ("Error: " ^ lineLabel ^ ": " ^ msg ^ "\n")
        end

    fun parse filename = let
		val file = TextIO.openIn filename
	  	fun get _ = TextIO.input file
	  	val lexer = TigerParser.makeLexer get
	  	fun run() = let
	    in
            TigerParser.parse (0, lexer, printParseError, ());
            ()
	    end
    in
        run() handle
            (L.LexerError (L.UnclosedComment, linenum)) =>
                printError linenum "Unclosed comment"
          | (L.LexerError (L.ImproperMultilineString, linenum)) =>
                printError linenum "Improperly formatted multiline string"
          | (L.LexerError (L.UnknownEscapeSequence seq, linenum)) =>
                printError linenum ("Unknown escape sequence: '\\" ^ seq ^ "'")
          | TigerParser.ParseError =>
                print "Parsing failed\n";
        TextIO.closeIn file
    end

end