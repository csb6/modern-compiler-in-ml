functor Lexer (tokens : Tiger_TOKENS) = struct
    (* Parser combinators *)
    val satisfy = LibParserStr.satisfy
    val symbol = LibParserStr.symbol
    val charLit = LibParserStr.charLit
    val succeed = LibParserStr.succeed
    val zeroOrMoreStr = LibParserStr.zeroOrMoreStr
    val oneOrMoreStr = LibParserStr.oneOrMoreStr
    val zeroOrMore = LibParserStr_Higher_Order.zeroOrMore
    val altList = LibParserStr_Higher_Order.altList
    val bind = LibParserStr_Higher_Order.bind
    val return = LibParserStr_Higher_Order.return
    val || = LibParserStr_Higher_Order.||
    infix ||
    val --> = LibParserStr_Higher_Order.-->
    infix -->
    val *--> = LibParserStr_Higher_Order.*-->
    infix *-->
    val --*> = LibParserStr_Higher_Order.--*>
    infix --*>

    val simpleTokenList =
        map (fn (sym, ctor) => return (symbol sym) ctor)
        [
            (* Keywords *)
            ("while", tokens.WHILE), ("for", tokens.FOR), ("to", tokens.TO), ("break", tokens.BREAK),
            ("let", tokens.LET), ("in", tokens.IN), ("end", tokens.END), ("function", tokens.FUNCTION),
            ("var", tokens.VAR), ("type", tokens.TYPE), ("array", tokens.ARRAY), ("if", tokens.IF),
            ("then", tokens.THEN), ("else", tokens.ELSE), ("do", tokens.DO), ("of", tokens.OF), ("nil", tokens.NIL),
            (* Punctuation *)
            (",", tokens.COMMA), (":=", tokens.ASSIGN), (":", tokens.COLON), (";", tokens.SEMICOLON), ("(", tokens.LPAREN),
            (")", tokens.RPAREN), ("[", tokens.LBRACK), ("]", tokens.RBRACK), ("{", tokens.LBRACE), ("}", tokens.RBRACE),
            (".", tokens.DOT), ("+", tokens.PLUS), ("-", tokens.MINUS), ("*", tokens.TIMES), ("/", tokens.DIVIDE), ("=", tokens.EQ),
            ("<>", tokens.NEQ), ("<=", tokens.LE), (">=", tokens.GE), ("<", tokens.LT), (">", tokens.GT), ("&", tokens.AND),
            ("|", tokens.OR)
        ]
    fun simpleToken (i, j) = bind (altList simpleTokenList) (fn ctor => ctor (i, j))

    val escapeSeqList =
        map (fn (c, value) => return (charLit c) value)
        [
            (#"n", #"\n"), (#"t", #"\t"), (#"\\", #"\\"), (#"\"", #"\"")
        ]
    val escapeSeqs = charLit #"\\" *--> altList escapeSeqList
    val stringLitChar = satisfy (fn c => c <> #"\"" andalso c <> #"\\" andalso Char.isPrint c)
    fun stringLit (i, j) = bind (charLit #"\"" *--> zeroOrMore (escapeSeqs || stringLitChar) --*> charLit #"\"")
        (fn s => tokens.STRING (implode s, i, j))

    fun digitsToInt s = valOf (Int.fromString (Substring.string s))
    fun intLit (i, j) = bind (oneOrMoreStr Char.isDigit) (fn s => tokens.INT (digitsToInt s, i, j))

    fun token (i, j) = simpleToken (i, j) || intLit (i, j) || stringLit (i, j)

    (* TODO: handle comments separate from combinators (need to track line numbers/nesting as we traverse the comment) *)
    (* TODO: try to use ML-Lex, might end up easier/more efficient) *)
end