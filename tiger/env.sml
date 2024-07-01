structure Env = struct
    datatype entry = VarEntry of {ty: Types.ty}
                   | FunEntry of {formals: Types.ty list, result: Types.ty}

    val empty = AtomMap.empty
    val insert = AtomMap.insert
    val insert' = AtomMap.insert'
    fun insertAll (env, entries) = foldl insert' env entries
    val find = AtomMap.find
    val filter = AtomMap.filter
    val numItems = AtomMap.numItems
    val listKeys = AtomMap.listKeys
    val exists = AtomMap.exists
    val all = AtomMap.all

    val baseTypeEnv = foldl (fn ((sym, ty), m) => insert(m, sym, ty)) empty
        [(Atom.atom "int",    Types.INT),
         (Atom.atom "string", Types.STRING)]

    val baseVarEnv = foldl (fn ((sym, entry), m) => insert(m, sym, entry)) empty
        [(Atom.atom "print",     FunEntry {formals=[Types.STRING],                       result=Types.UNIT}),
         (Atom.atom "flush",     FunEntry {formals=[],                                   result=Types.UNIT}),
         (Atom.atom "getchar",   FunEntry {formals=[],                                   result=Types.STRING}),
         (Atom.atom "ord",       FunEntry {formals=[Types.STRING],                       result=Types.INT}),
         (Atom.atom "chr",       FunEntry {formals=[Types.INT],                          result=Types.STRING}),
         (Atom.atom "size",      FunEntry {formals=[Types.STRING],                       result=Types.INT}),
         (Atom.atom "substring", FunEntry {formals=[Types.STRING, Types.INT, Types.INT], result=Types.STRING}),
         (Atom.atom "concat",    FunEntry {formals=[Types.STRING, Types.STRING],         result=Types.STRING}),
         (Atom.atom "not",       FunEntry {formals=[Types.INT],                          result=Types.INT}),
         (Atom.atom "exit",      FunEntry {formals=[Types.INT],                          result=Types.UNIT})]
end