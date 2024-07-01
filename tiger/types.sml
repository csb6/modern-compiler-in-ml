structure Types = struct

  type unique = unit ref
  type symbol = Atom.atom

  datatype ty = RECORD of (symbol * ty) list * unique
              | NIL
              | INT
              | STRING
              | ARRAY of ty * unique
              | NAME of symbol * ty option ref
              | UNIT

  fun equivTypes (a, b) = case (a, b) of
        (NIL,                        NIL)                        => true
      | (INT,                        INT)                        => true
      | (STRING,                     STRING)                     => true
      | (UNIT,                       UNIT)                       => true
      | (NAME (nameA, typeRefA),     NAME (nameB, typeRefB))     => typeRefA = typeRefB andalso Atom.same (nameA, nameB)
      | (ARRAY (elemTypeA, uniqA),   ARRAY (elemTypeB, uniqB))   => uniqA = uniqB andalso equivTypes (elemTypeA, elemTypeB)
      | (RECORD (fieldListA, uniqA), RECORD (fieldListB, uniqB)) => uniqA = uniqB andalso equivFields (fieldListA, fieldListB)
      | _                                                        => false
  and equivFields (fieldListA, fieldListB) = case (fieldListA, fieldListB) of
        ([], []) => true
      | ([], _)  => false
      | (_, [])  => false
      | ((fieldSymA, fieldTypeA)::ax, (fieldSymB, fieldTypeB)::bx) =>
            Atom.same (fieldSymA, fieldSymB) andalso equivTypes (fieldTypeA, fieldTypeB) andalso equivFields (ax, bx)

    val != = fn (a, b) => not (equivTypes (a, b))

  fun typeToString t = (case t of
      RECORD (fieldList, _) => "{" ^ (String.concatWith ", " (map fieldToString fieldList)) ^ "}"
    | NIL                   => "nil"
    | INT                   => "int"
    | STRING                => "string"
    | ARRAY (elemType, _)   => "[" ^ (typeToString elemType) ^ "]"
    | NAME (name, _)        => Atom.toString name
    | UNIT                  => "unit"
  )
  and fieldToString (field, fieldTy) = (Atom.toString field) ^ ":" ^ (typeToString fieldTy)
end
