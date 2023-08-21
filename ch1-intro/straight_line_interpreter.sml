type identifier = string;
datatype binary_op = Plus | Minus | Times | Div;
datatype statement = CompoundStmt of statement * statement
                   | AssignStmt of identifier * expression
                   | PrintStmt of expression list
and expression = IdentifierExpr of identifier
               | NumberExpr of int
               | BinaryExpr of expression * binary_op * expression
               | SeqExpr of statement * expression;

type table = (identifier * int) list;
fun table_insert ident (t : table, value) = (ident, value)::t;
fun table_find ident (t : table) = (case (List.find (fn (i, _) => i = ident) t) of
    NONE            => NONE
  | SOME (_, value) => SOME value
);

(* Return the max number of arguments of all PrintStmts within this statement and its subparts *)
fun maxArgs (CompoundStmt (a, b)) = Int.max (maxArgs a, maxArgs b)
  | maxArgs (AssignStmt (i, e)) = maxArgsExpr e
  | maxArgs (PrintStmt args) =
        foldl (fn (e, maxSoFar) => Int.max (maxSoFar, maxArgsExpr e)) (List.length args) args
and maxArgsExpr (SeqExpr (s, e)) = Int.max (maxArgs s, maxArgsExpr e)
  | maxArgsExpr (BinaryExpr (a, oper, b)) = Int.max (maxArgsExpr a, maxArgsExpr b)
  | maxArgsExpr _ = 0;

exception UnknownIdentifierError;

(* Interpret the given program statement *)
fun interpret stmt = let
    fun interpretStmt t stmt = (case stmt of
        CompoundStmt (a, b)   => interpretStmt (interpretStmt t a) b
      | AssignStmt (ident, e) => table_insert ident (interpretExpr t e)
      | PrintStmt args        => let
            val (t', results) = foldl (fn (expr, (t, resultsSoFar)) => let
                val (t', value) = interpretExpr t expr;
            in
                (t', (Int.toString value)::resultsSoFar)
            end) (t, []) args
        in
            print (String.concatWith " " (rev results)); print "\n"; t'
        end
    )
    and interpretExpr t expr = (case expr of
        IdentifierExpr ident    => (t, case table_find ident t of
            NONE       => raise UnknownIdentifierError
          | SOME value => value
        )
      | NumberExpr n            => (t, n)
      | BinaryExpr (a, oper, b) => let
            val (t', valueA) = interpretExpr t a;
            val (t'', valueB) = interpretExpr t' b;
        in
            case oper of
                Plus  => (t'', valueA + valueB)
              | Minus => (t'', valueA - valueB)
              | Times => (t'', valueA * valueB)
              | Div   => (t'', valueA div valueB)
        end
      | SeqExpr (stmt, e)       => interpretExpr (interpretStmt t stmt) e
    );
in
    (interpretStmt [] stmt; ())
end;

(*
a := 5 + 3;
b := (print(a, a-1); 10 * a);
print(b)
*)

val prog =
    CompoundStmt (AssignStmt ("a", BinaryExpr (NumberExpr 5, Plus, NumberExpr 3)),
    CompoundStmt (AssignStmt ("b",
        SeqExpr (PrintStmt [IdentifierExpr "a", BinaryExpr (IdentifierExpr "a", Minus, NumberExpr 1)],
                 BinaryExpr (NumberExpr 10, Times, IdentifierExpr "a"))),
    PrintStmt [IdentifierExpr "b"]));

val maxArgsProg = maxArgs prog;