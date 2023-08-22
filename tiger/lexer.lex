type lexresult = Tokens.token

datatype ErrorInfo = UnclosedComment
                   | ImproperMultilineString
                   | UnknownEscapeSequence of string;

exception LexerError of ErrorInfo * Tokens.linenum

val currLine = ref 1
val currStr = ref ""
val strStartLine = ref 1
val commentNestLevel = ref 0
fun appendStr s = currStr := !currStr ^ s
fun incr v = v := !v + 1
fun resetState () = (
    currLine := 1;
    currStr := "";
    strStartLine := 1;
    commentNestLevel := 0
)
fun raiseError info linenum = (resetState(); raise LexerError (info, linenum))
fun eof () = let
    val commentsClosed = !commentNestLevel = 0;
    val finalLine = !currLine;
in
    resetState();
    if not commentsClosed then raise LexerError (UnclosedComment, finalLine)
    else Tokens.EOF (finalLine, finalLine)
end

%%

%structure Lexer
%s INSTRING INESCAPESEQ INCOMMENT INMULTISTRING;

%%

<INITIAL> "/*" => (YYBEGIN INCOMMENT; commentNestLevel := 1; lex());
<INCOMMENT> "/*" => (incr commentNestLevel; lex());
<INCOMMENT> "*/" => (
    commentNestLevel := !commentNestLevel - 1;
    if !commentNestLevel = 0 then (YYBEGIN INITIAL) else ();
    lex()
);
<INCOMMENT> \n => (incr currLine; lex());
<INCOMMENT> . => (lex());

<INITIAL> [\ \t\r]+ => (lex());
<INITIAL> \n => (incr currLine; lex());

<INITIAL> "while" => (Tokens.WHILE (!currLine, !currLine));
<INITIAL> "for" => (Tokens.FOR (!currLine, !currLine));
<INITIAL> "to" => (Tokens.TO (!currLine, !currLine));
<INITIAL> "break" => (Tokens.BREAK (!currLine, !currLine));
<INITIAL> "let" => (Tokens.LET (!currLine, !currLine));
<INITIAL> "in" => (Tokens.IN (!currLine, !currLine));
<INITIAL> "end" => (Tokens.END (!currLine, !currLine));
<INITIAL> "function" => (Tokens.FUNCTION (!currLine, !currLine));
<INITIAL> "var" => (Tokens.VAR (!currLine, !currLine));
<INITIAL> "type" => (Tokens.TYPE (!currLine, !currLine));
<INITIAL> "array" => (Tokens.ARRAY (!currLine, !currLine));
<INITIAL> "if" => (Tokens.IF (!currLine, !currLine));
<INITIAL> "then" => (Tokens.THEN (!currLine, !currLine));
<INITIAL> "else" => (Tokens.ELSE (!currLine, !currLine));
<INITIAL> "do" => (Tokens.DO (!currLine, !currLine));
<INITIAL> "of" => (Tokens.OF (!currLine, !currLine));
<INITIAL> "nil" => (Tokens.NIL (!currLine, !currLine));

<INITIAL> "," => (Tokens.COMMA (!currLine, !currLine));
<INITIAL> ":=" => (Tokens.ASSIGN (!currLine, !currLine));
<INITIAL> ":" => (Tokens.COLON (!currLine, !currLine));
<INITIAL> ";" => (Tokens.SEMICOLON (!currLine, !currLine));
<INITIAL> "(" => (Tokens.LPAREN (!currLine, !currLine));
<INITIAL> ")" => (Tokens.RPAREN (!currLine, !currLine));
<INITIAL> "[" => (Tokens.LBRACK (!currLine, !currLine));
<INITIAL> "]" => (Tokens.RBRACK (!currLine, !currLine));
<INITIAL> "{" => (Tokens.LBRACE (!currLine, !currLine));
<INITIAL> "}" => (Tokens.RBRACE (!currLine, !currLine));
<INITIAL> "." => (Tokens.DOT (!currLine, !currLine));
<INITIAL> "+" => (Tokens.PLUS (!currLine, !currLine));
<INITIAL> "-" => (Tokens.MINUS (!currLine, !currLine));
<INITIAL> "*" => (Tokens.TIMES (!currLine, !currLine));
<INITIAL> "/" => (Tokens.DIVIDE (!currLine, !currLine));
<INITIAL> "=" => (Tokens.EQ (!currLine, !currLine));
<INITIAL> "<>" => (Tokens.NEQ (!currLine, !currLine));
<INITIAL> "<=" => (Tokens.LE (!currLine, !currLine));
<INITIAL> ">=" => (Tokens.GE (!currLine, !currLine));
<INITIAL> "<" => (Tokens.LT (!currLine, !currLine));
<INITIAL> ">" => (Tokens.GT (!currLine, !currLine));
<INITIAL> "&" => (Tokens.AND (!currLine, !currLine));
<INITIAL> "|" => (Tokens.OR (!currLine, !currLine));

<INITIAL> [a-zA-Z][a-zA-Z0-9_]* => (Tokens.ID (yytext, !currLine, !currLine));

<INITIAL> [0-9]+ => (Tokens.INT (valOf (Int.fromString (yytext)), !currLine, !currLine));

<INITIAL> "\"" => (YYBEGIN INSTRING; currStr := ""; strStartLine := !currLine; lex());
<INSTRING> "\"" => (YYBEGIN INITIAL; Tokens.STRING (!currStr, !strStartLine, !currLine));
<INSTRING> "\\" => (YYBEGIN INESCAPESEQ; lex());
<INSTRING> . => (appendStr yytext; lex());

<INESCAPESEQ> "n" => (YYBEGIN INSTRING; appendStr "\n"; lex());
<INESCAPESEQ> "r" => (YYBEGIN INSTRING; appendStr "\r"; lex());
<INESCAPESEQ> "t" => (YYBEGIN INSTRING; appendStr "\t"; lex());
<INESCAPESEQ> "\"" => (YYBEGIN INSTRING; appendStr "\""; lex());
<INESCAPESEQ> "\\" => (YYBEGIN INSTRING; appendStr "\\"; lex());
<INESCAPESEQ> [\ \t\r] => (YYBEGIN INMULTISTRING; lex());
<INESCAPESEQ> "\n" => (YYBEGIN INMULTISTRING; incr currLine; lex());
<INESCAPESEQ> . => (raiseError (UnknownEscapeSequence yytext) (!currLine));

<INMULTISTRING> "\\" => (YYBEGIN INSTRING; lex());
<INMULTISTRING> "\n" => (incr currLine; lex());
<INMULTISTRING> [\ \t\r] => (lex());
<INMULTISTRING> . => (raiseError ImproperMultilineString (!currLine));