structure Parse = struct
	fun parse filename = let
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
		run();
	  	TextIO.closeIn file
    end
end

