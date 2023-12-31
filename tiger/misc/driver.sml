structure TestLexer = struct
	structure Lexer = LexerFun(Tokens_PrettyPrint)
	structure L = Lexer.UserDeclarations

	fun printError linenum msg = print ("Error: Line " ^ (Int.toString linenum) ^ ": " ^ msg ^ "\n")

	fun lex filename = let
		val file = TextIO.openIn filename
	  	fun get _ = TextIO.input file
	  	val lexer = Lexer.makeLexer get
	  	fun run() = let
	  		val t = lexer()
	    in
			print t; print "\n";
			if substring(t, 0, 3) = "EOF" then () else run()
	    end
    in
        run() handle
            (L.LexerError (L.UnclosedComment, linenum)) =>
                printError linenum "Unclosed comment"
          | (L.LexerError (L.ImproperMultilineString, linenum)) =>
                printError linenum "Improperly formatted multiline string"
          | (L.LexerError (L.UnknownEscapeSequence seq, linenum)) =>
                printError linenum ("Unknown escape sequence: '\\" ^ seq ^ "'");
        TextIO.closeIn file
    end
end

