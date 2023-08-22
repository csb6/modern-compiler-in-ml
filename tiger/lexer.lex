type lexresult = Tokens.token

datatype ErrorInfo = UnclosedComment
                   | UnknownEscapeSequence of string;

exception LexerError of ErrorInfo * Tokens.linenum

val currLine = ref 1
val currStr = ref ""
val commentNestLevel = ref 0
fun appendStr s = currStr := !currStr ^ s
fun resetState () = (
    currLine := 1;
    currStr := "";
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
%s INSTRING INESCAPESEQ INCOMMENT;

%%

<INITIAL> "/*" => (YYBEGIN INCOMMENT; commentNestLevel := 1; lex());
<INCOMMENT> "/*" => (commentNestLevel := !commentNestLevel + 1; lex());
<INCOMMENT> "*/" => (
    commentNestLevel := !commentNestLevel - 1;
    if !commentNestLevel = 0 then (YYBEGIN INITIAL) else ();
    lex()
);
<INCOMMENT> \n => (currLine := !currLine + 1; lex());
<INCOMMENT> . => (lex());

<INITIAL> [\ \t\r]+ => (lex());
<INITIAL> \n => (currLine := !currLine + 1; lex());

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

<INITIAL> "\"" => (YYBEGIN INSTRING; currStr := ""; lex());
<INSTRING> "\"" => (YYBEGIN INITIAL; Tokens.STRING (!currStr, !currLine, !currLine));
<INSTRING> "\\" => (YYBEGIN INESCAPESEQ; lex());
<INSTRING> . => (appendStr yytext; lex());
<INESCAPESEQ> "n" => (appendStr "\n"; YYBEGIN INSTRING; lex());
<INESCAPESEQ> "t" => (appendStr "\t"; YYBEGIN INSTRING; lex());
<INESCAPESEQ> . => (raiseError (UnknownEscapeSequence yytext) (!currLine));