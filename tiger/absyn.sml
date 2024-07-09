structure Absyn = struct

type pos = int   and   symbol = Atom.atom

datatype var = SimpleVar of symbol * pos
             | FieldVar of var * symbol * pos
             | SubscriptVar of var * exp * pos

and exp = VarExp of var
        | NilExp
        | IntExp of int
        | StringExp of string * pos
        | CallExp of {func: symbol, args: exp list, pos: pos}
        | OpExp of {left: exp, oper: oper, right: exp, pos: pos}
        | RecordExp of {fields: (symbol * exp * pos) list,
                        typ: symbol, pos: pos}
        | SeqExp of exp list
        | AssignExp of {var: var, exp: exp, pos: pos}
        | IfExp of {test: exp, then': exp, else': exp option, pos: pos}
        | WhileExp of {test: exp, body: exp, pos: pos}
        | ForExp of {var: symbol, escape: bool ref,
                     lo: exp, hi: exp, body: exp, pos: pos}
        | BreakExp of pos
        | LetExp of {decs: dec list, body: exp, pos: pos}
        | ArrayExp of {typ: symbol, size: exp, init: exp, pos: pos}

and dec = FunctionDec of fundec list
        | VarDec of {name: symbol,
                     escape: bool ref,
                     typ: symbol option,
                     init: exp,
                     pos: pos}
        | TypeDec of tydec list

and ty = NameTy of symbol * pos
       | RecordTy of field list
       | ArrayTy of symbol * pos

and oper = PlusOp | MinusOp | TimesOp | DivideOp
         | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

withtype field = {name: symbol, escape: bool ref,
                  typ: symbol, pos: pos}
   and   fundec = {name: symbol,
                   params: field list,
                   result: symbol option,
                   body: exp,
                   pos: pos}
   and   tydec = {name: symbol, ty: ty, pos: pos}

fun indent 0 = ""
  | indent lvl = " " ^ (indent (lvl-1))

fun opToString oper = case oper of
    PlusOp => "+"
  | MinusOp => "-"
  | TimesOp => "*"
  | DivideOp => "/"
  | EqOp => "="
  | NeqOp => "<>"
  | LtOp => "<"
  | LeOp => "<="
  | GtOp => ">"
  | GeOp => ">="

fun varToString lvl v = case v of
    SimpleVar (sym, _) => Atom.toString sym
  | FieldVar (v', sym, _) => (varToString (lvl+1) v') ^ "." ^ (Atom.toString sym)
  | SubscriptVar (v', exp, _) => (varToString (lvl+1) v') ^ "[" ^ (expToString (lvl+1) exp) ^ "]"
and decToString lvl d = case d of
    FunctionDec _ => "fundec" (* TODO fundec *)
  | VarDec {name=name, escape=_, typ=_, init=init, pos=_} => "var " ^ (Atom.toString name) ^ ":=" ^ (expToString (lvl+1) init)
  | TypeDec _ => "tydecs" (* TODO tydecs *)
and expToString lvl e = case e of
    VarExp v => varToString (lvl+1) v
  | NilExp => "nil"
  | IntExp i => Int.toString i
  | StringExp (s, _) => s
  | CallExp {func=symbol, args=_, pos=_} => (Atom.toString symbol) ^ "()" (* TODO args *)
  | OpExp {left=left, oper=oper, right=right, pos=_} => "(" ^ (expToString (lvl+1) left) ^ (opToString oper) ^ (expToString (lvl+1) right) ^ ")"
  | RecordExp {fields=_, typ=_, pos=_} => "recordexp" (* TODO recordexp *)
  | SeqExp seq => "\n" ^ (String.concatWith ";\n" (map (fn ex => (indent lvl) ^ expToString (lvl+1) ex) seq)) ^ "\n"
  | AssignExp {var=var, exp=exp, pos=_} => "(" ^ (varToString (lvl+1) var) ^ ":=" ^ (expToString (lvl+1) exp) ^ ")"
  | IfExp {test=test, then'=then', else'=else', pos=_} =>
          "(if " ^ (expToString (lvl+1) test) ^ " then " ^ (expToString (lvl+1) then')
          ^ (case else' of NONE => "" | SOME ex => " else " ^ expToString (lvl+1) ex) ^ (indent (lvl-1)) ^ ")" (* TODO(if and while): only indent when last printed expr is a SeqExpr *)
  | WhileExp {test=test, body=body, pos=_} => "(while " ^ (expToString (lvl+1) test) ^ " do " ^ (expToString (lvl+1) body) ^ (indent (lvl-1)) ^ ")"
  | ForExp {var=var, escape=_, lo=lo, hi=hi, body=body, pos=_} =>
          "(for " ^ (Atom.toString var) ^ ":=" ^ (expToString (lvl+1) lo) ^ "to" ^ (expToString (lvl+1) hi) ^ " do " ^ (expToString (lvl+1) body) ^ ")"
  | BreakExp _ => "break"
  | LetExp {decs=decs, body=body, pos=_} =>
          "let " ^ (String.concatWith " " (map (decToString (lvl+1)) decs)) ^ " in " ^ (expToString (lvl+1) body) ^ " end"
  | ArrayExp _ => "arrayexp" (* TODO: arrayexp *)

end
