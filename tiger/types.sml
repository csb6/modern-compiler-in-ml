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

  fun getActualType ty = (case ty of
      NAME (_, ref (SOME rType)) => getActualType rType
    | ty'                        => ty'
  )

  fun equivTypes (a, b) = (case (a, b) of
      (NIL,                     NIL)                      => true
    | (INT,                     INT)                      => true
    | (STRING,                  STRING)                   => true
    | (UNIT,                    UNIT)                     => true
    | (NAME (_, refA),          NAME (_, refB))           => refA = refB
    | (NAME (_, ref (SOME a')), _)                        => equivTypes (a', b)
    | (_,                       NAME (_, ref (SOME b')))  => equivTypes (a, b')
    | (ARRAY (_, uniqA),        ARRAY (_, uniqB))         => uniqA = uniqB
    | (NIL,                     RECORD _)                 => true
    | (RECORD _,                NIL)                      => true
    | (RECORD (_, uniqA),       RECORD (_, uniqB))        => uniqA = uniqB
    | _                                                   => false
  )

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
