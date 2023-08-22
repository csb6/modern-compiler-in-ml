structure Lexer=
   struct
    structure UserDeclarations =
      struct
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

end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\000\000\000\000\000\000\000\000\000\091\092\000\000\091\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\091\000\090\000\000\000\089\000\088\087\086\085\084\083\082\080\
\\079\079\079\079\079\079\079\079\079\079\077\076\073\072\070\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\069\000\068\000\000\
\\000\063\058\012\056\050\040\012\012\037\012\012\034\012\031\029\
\\012\012\012\012\021\012\018\013\012\012\012\011\010\009\000\000\
\\000"
),
 (3, 
"\093\093\093\093\093\093\093\093\093\093\000\093\093\093\093\093\
\\093\093\093\093\093\093\093\093\093\093\093\093\093\093\093\093\
\\093\093\095\093\093\093\093\093\093\093\093\093\093\093\093\093\
\\093\093\093\093\093\093\093\093\093\093\093\093\093\093\093\093\
\\093\093\093\093\093\093\093\093\093\093\093\093\093\093\093\093\
\\093\093\093\093\093\093\093\093\093\093\093\093\094\093\093\093\
\\093\093\093\093\093\093\093\093\093\093\093\093\093\093\093\093\
\\093\093\093\093\093\093\093\093\093\093\093\093\093\093\093\093\
\\093"
),
 (5, 
"\096\096\096\096\096\096\096\096\096\096\000\096\096\096\096\096\
\\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\
\\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\
\\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\
\\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\
\\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\096\
\\096\096\096\096\096\096\096\096\096\096\096\096\096\096\098\096\
\\096\096\096\096\097\096\096\096\096\096\096\096\096\096\096\096\
\\096"
),
 (7, 
"\099\099\099\099\099\099\099\099\099\099\104\099\099\099\099\099\
\\099\099\099\099\099\099\099\099\099\099\099\099\099\099\099\099\
\\099\099\099\099\099\099\099\099\099\099\102\099\099\099\099\100\
\\099\099\099\099\099\099\099\099\099\099\099\099\099\099\099\099\
\\099\099\099\099\099\099\099\099\099\099\099\099\099\099\099\099\
\\099\099\099\099\099\099\099\099\099\099\099\099\099\099\099\099\
\\099\099\099\099\099\099\099\099\099\099\099\099\099\099\099\099\
\\099\099\099\099\099\099\099\099\099\099\099\099\099\099\099\099\
\\099"
),
 (12, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (13, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\014\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\015\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (15, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\012\012\012\016\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (16, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\017\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (18, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\019\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (19, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\020\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (21, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\026\012\012\012\012\012\012\025\
\\012\012\012\012\012\012\012\012\012\022\012\000\000\000\000\000\
\\000"
),
 (22, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\023\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (23, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\024\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (26, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\027\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (27, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\028\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (29, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\030\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (31, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\032\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (32, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\012\012\012\033\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (34, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\035\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (35, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\036\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (37, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\039\012\012\012\012\012\012\012\038\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (40, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\048\
\\012\012\012\012\012\041\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (41, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\042\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (42, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\043\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (43, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\044\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (44, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\045\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (45, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\046\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (46, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\047\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (48, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\049\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (50, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\012\012\012\053\012\051\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (51, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\052\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (53, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\054\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (54, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\055\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (56, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\057\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (58, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\059\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (59, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\060\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (60, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\061\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (61, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\012\012\062\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (63, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\064\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (64, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\065\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (65, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\066\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\
\\000"
),
 (66, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\012\012\000\000\000\000\012\
\\000\012\012\012\012\012\012\012\012\012\012\012\012\012\012\012\
\\012\012\012\012\012\012\012\012\012\067\012\000\000\000\000\000\
\\000"
),
 (70, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\071\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (73, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\075\074\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (77, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\078\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (79, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\079\079\079\079\079\079\079\079\079\079\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (80, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\081\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (91, 
"\000\000\000\000\000\000\000\000\000\091\000\000\000\091\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\091\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (100, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\101\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (102, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\103\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [], trans = 3},
{fin = [], trans = 3},
{fin = [], trans = 5},
{fin = [], trans = 5},
{fin = [], trans = 7},
{fin = [], trans = 7},
{fin = [(N 115)], trans = 0},
{fin = [(N 144)], trans = 0},
{fin = [(N 113)], trans = 0},
{fin = [(N 147)], trans = 12},
{fin = [(N 147)], trans = 13},
{fin = [(N 147)], trans = 14},
{fin = [(N 147)], trans = 15},
{fin = [(N 147)], trans = 16},
{fin = [(N 23),(N 147)], trans = 12},
{fin = [(N 147)], trans = 18},
{fin = [(N 147)], trans = 19},
{fin = [(N 60),(N 147)], trans = 12},
{fin = [(N 147)], trans = 21},
{fin = [(N 147)], trans = 22},
{fin = [(N 147)], trans = 23},
{fin = [(N 65),(N 147)], trans = 12},
{fin = [(N 30),(N 147)], trans = 12},
{fin = [(N 147)], trans = 26},
{fin = [(N 147)], trans = 27},
{fin = [(N 79),(N 147)], trans = 12},
{fin = [(N 147)], trans = 29},
{fin = [(N 90),(N 147)], trans = 12},
{fin = [(N 147)], trans = 31},
{fin = [(N 147)], trans = 32},
{fin = [(N 94),(N 147)], trans = 12},
{fin = [(N 147)], trans = 34},
{fin = [(N 147)], trans = 35},
{fin = [(N 40),(N 147)], trans = 12},
{fin = [(N 147)], trans = 37},
{fin = [(N 43),(N 147)], trans = 12},
{fin = [(N 74),(N 147)], trans = 12},
{fin = [(N 147)], trans = 40},
{fin = [(N 147)], trans = 41},
{fin = [(N 147)], trans = 42},
{fin = [(N 147)], trans = 43},
{fin = [(N 147)], trans = 44},
{fin = [(N 147)], trans = 45},
{fin = [(N 147)], trans = 46},
{fin = [(N 56),(N 147)], trans = 12},
{fin = [(N 147)], trans = 48},
{fin = [(N 27),(N 147)], trans = 12},
{fin = [(N 147)], trans = 50},
{fin = [(N 147)], trans = 51},
{fin = [(N 47),(N 147)], trans = 12},
{fin = [(N 147)], trans = 53},
{fin = [(N 147)], trans = 54},
{fin = [(N 84),(N 147)], trans = 12},
{fin = [(N 147)], trans = 56},
{fin = [(N 87),(N 147)], trans = 12},
{fin = [(N 147)], trans = 58},
{fin = [(N 147)], trans = 59},
{fin = [(N 147)], trans = 60},
{fin = [(N 147)], trans = 61},
{fin = [(N 36),(N 147)], trans = 12},
{fin = [(N 147)], trans = 63},
{fin = [(N 147)], trans = 64},
{fin = [(N 147)], trans = 65},
{fin = [(N 147)], trans = 66},
{fin = [(N 71),(N 147)], trans = 12},
{fin = [(N 111)], trans = 0},
{fin = [(N 109)], trans = 0},
{fin = [(N 140)], trans = 70},
{fin = [(N 136)], trans = 0},
{fin = [(N 127)], trans = 0},
{fin = [(N 138)], trans = 73},
{fin = [(N 130)], trans = 0},
{fin = [(N 133)], trans = 0},
{fin = [(N 103)], trans = 0},
{fin = [(N 101)], trans = 77},
{fin = [(N 99)], trans = 0},
{fin = [(N 150)], trans = 79},
{fin = [(N 125)], trans = 80},
{fin = [(N 2)], trans = 0},
{fin = [(N 117)], trans = 0},
{fin = [(N 121)], trans = 0},
{fin = [(N 96)], trans = 0},
{fin = [(N 119)], trans = 0},
{fin = [(N 123)], trans = 0},
{fin = [(N 107)], trans = 0},
{fin = [(N 105)], trans = 0},
{fin = [(N 142)], trans = 0},
{fin = [(N 152)], trans = 0},
{fin = [(N 15)], trans = 91},
{fin = [(N 17)], trans = 0},
{fin = [(N 158)], trans = 0},
{fin = [(N 156),(N 158)], trans = 0},
{fin = [(N 154),(N 158)], trans = 0},
{fin = [(N 164)], trans = 0},
{fin = [(N 162),(N 164)], trans = 0},
{fin = [(N 160),(N 164)], trans = 0},
{fin = [(N 12)], trans = 0},
{fin = [(N 12)], trans = 100},
{fin = [(N 5)], trans = 0},
{fin = [(N 12)], trans = 102},
{fin = [(N 8)], trans = 0},
{fin = [(N 10)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INCOMMENT = STARTSTATE 7;
val INESCAPESEQ = STARTSTATE 5;
val INITIAL = STARTSTATE 1;
val INSTRING = STARTSTATE 3;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput =
let	val yygone0=1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  10 => (currLine := !currLine + 1; lex())
| 101 => (Tokens.COLON (!currLine, !currLine))
| 103 => (Tokens.SEMICOLON (!currLine, !currLine))
| 105 => (Tokens.LPAREN (!currLine, !currLine))
| 107 => (Tokens.RPAREN (!currLine, !currLine))
| 109 => (Tokens.LBRACK (!currLine, !currLine))
| 111 => (Tokens.RBRACK (!currLine, !currLine))
| 113 => (Tokens.LBRACE (!currLine, !currLine))
| 115 => (Tokens.RBRACE (!currLine, !currLine))
| 117 => (Tokens.DOT (!currLine, !currLine))
| 119 => (Tokens.PLUS (!currLine, !currLine))
| 12 => (lex())
| 121 => (Tokens.MINUS (!currLine, !currLine))
| 123 => (Tokens.TIMES (!currLine, !currLine))
| 125 => (Tokens.DIVIDE (!currLine, !currLine))
| 127 => (Tokens.EQ (!currLine, !currLine))
| 130 => (Tokens.NEQ (!currLine, !currLine))
| 133 => (Tokens.LE (!currLine, !currLine))
| 136 => (Tokens.GE (!currLine, !currLine))
| 138 => (Tokens.LT (!currLine, !currLine))
| 140 => (Tokens.GT (!currLine, !currLine))
| 142 => (Tokens.AND (!currLine, !currLine))
| 144 => (Tokens.OR (!currLine, !currLine))
| 147 => let val yytext=yymktext() in Tokens.ID (yytext, !currLine, !currLine) end
| 15 => (lex())
| 150 => let val yytext=yymktext() in Tokens.INT (valOf (Int.fromString (yytext)), !currLine, !currLine) end
| 152 => (YYBEGIN INSTRING; currStr := ""; lex())
| 154 => (YYBEGIN INITIAL; Tokens.STRING (!currStr, !currLine, !currLine))
| 156 => (YYBEGIN INESCAPESEQ; lex())
| 158 => let val yytext=yymktext() in appendStr yytext; lex() end
| 160 => (appendStr "\n"; YYBEGIN INSTRING; lex())
| 162 => (appendStr "\t"; YYBEGIN INSTRING; lex())
| 164 => let val yytext=yymktext() in raiseError (UnknownEscapeSequence yytext) (!currLine) end
| 17 => (currLine := !currLine + 1; lex())
| 2 => (YYBEGIN INCOMMENT; commentNestLevel := 1; lex())
| 23 => (Tokens.WHILE (!currLine, !currLine))
| 27 => (Tokens.FOR (!currLine, !currLine))
| 30 => (Tokens.TO (!currLine, !currLine))
| 36 => (Tokens.BREAK (!currLine, !currLine))
| 40 => (Tokens.LET (!currLine, !currLine))
| 43 => (Tokens.IN (!currLine, !currLine))
| 47 => (Tokens.END (!currLine, !currLine))
| 5 => (commentNestLevel := !commentNestLevel + 1; lex())
| 56 => (Tokens.FUNCTION (!currLine, !currLine))
| 60 => (Tokens.VAR (!currLine, !currLine))
| 65 => (Tokens.TYPE (!currLine, !currLine))
| 71 => (Tokens.ARRAY (!currLine, !currLine))
| 74 => (Tokens.IF (!currLine, !currLine))
| 79 => (Tokens.THEN (!currLine, !currLine))
| 8 => (
    commentNestLevel := !commentNestLevel - 1;
    if !commentNestLevel = 0 then (YYBEGIN INITIAL) else ();
    lex()
)
| 84 => (Tokens.ELSE (!currLine, !currLine))
| 87 => (Tokens.DO (!currLine, !currLine))
| 90 => (Tokens.OF (!currLine, !currLine))
| 94 => (Tokens.NIL (!currLine, !currLine))
| 96 => (Tokens.COMMA (!currLine, !currLine))
| 99 => (Tokens.ASSIGN (!currLine, !currLine))
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Unsafe.Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(Unsafe.CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(Unsafe.CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
