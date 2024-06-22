structure TigerParser = struct
    structure TigerLrVals = TigerLrValsFun( structure Token = LrParser.Token )
    structure TigerLexer = TigerLexerFun( structure Tokens = TigerLrVals.Tokens )
    structure TigerParser = Join( structure ParserData = TigerLrVals.ParserData
                                  structure Lex = TigerLexer
                                  structure LrParser = LrParser )
    open TigerLexer.UserDeclarations
    open TigerParser
end