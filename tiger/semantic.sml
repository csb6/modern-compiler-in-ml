structure Semantic = struct
    structure A = Absyn

    datatype type_list_mismatch = TooManyActuals
                                | TooFewActuals
                                | Mismatch of int * Types.ty * Types.ty

    datatype loop_context = InLoop | NotInLoop

    exception SemanticError of string * A.pos

    fun raiseError pos msg = raise SemanticError (msg, pos)

    fun raiseErrorArgs pos format args = raiseError pos (Format.format format args)

    exception Unreachable

    val != = Types.!=
    infix !=

    fun findTypeListMismatch actuals formals = let
        fun compare i inputs = case inputs of
            ([], [])                                         => NONE
          | ([], _)                                          => SOME TooFewActuals
          | (_, [])                                          => SOME TooManyActuals
          | (actualType::actualRest, formalType::formalRest) =>
              if actualType != formalType then SOME (Mismatch (i, actualType, formalType))
              else compare (i+1) (actualRest, formalRest)
    in
        compare 1 (actuals, formals)
    end

    fun allUniq cmp sortedLst = (case sortedLst of
        []         => true
      | [_]        => true
      | a::b::rest => not (cmp (a, b)) andalso (allUniq cmp rest)
    )

    fun atomGt (a, b) = Atom.lexCompare (a, b) = GREATER

    fun typeToFormatStr t = Format.STR (Types.typeToString t)

    fun opToFormatStr oper = Format.STR (A.opToString oper)

    fun typeCheck rootExp = let
        fun checkExp (varEnv, typeEnv, loopCtx) exp = let
            val checkExp' = checkExp (varEnv, typeEnv, loopCtx)

            fun checkVarExp var = case var of
                A.SimpleVar (varSym, pos)             => (case Env.find (varEnv, varSym) of
                    SOME (Env.VarEntry {ty=varTy}) => varTy
                  | _                              => raiseErrorArgs pos "Missing definition for variable: %s" [Format.ATOM varSym]
                )
              | A.FieldVar (recordVar, fieldSym, pos) => Types.UNIT (*TODO*)
              | A.SubscriptVar (arrVar, subExp, pos)  => Types.UNIT (*TODO*)

            fun checkAssignExp {var=var, exp=assignExp, pos=pos} = let
                val expType = checkExp' assignExp
            in
                case var of
                    A.SimpleVar (varSym, _) => (case Env.find (varEnv, varSym) of
                        SOME (Env.VarEntry {ty=varType}) =>
                            if varType != expType then
                                raiseErrorArgs pos "Type mismatch: '%s' and '%s'" (map typeToFormatStr [varType, expType])
                            else varType
                      | _                                => raiseErrorArgs pos "Missing definition for variable: %s" [Format.ATOM varSym]
                    )
                  | A.FieldVar _          => Types.UNIT (*TODO*)
                  | A.SubscriptVar _      => Types.UNIT (*TODO*)
            end

            fun checkOpExp {left=left, oper=oper, right=right, pos=pos} = let
                val leftType = checkExp' left
                val rightType = checkExp' right
            in
                if leftType != rightType orelse leftType != Types.INT then
                    raiseErrorArgs pos "Operator '%s' does not accept operands of types '%s' and '%s'"
                        [opToFormatStr oper, typeToFormatStr leftType, typeToFormatStr rightType]
                else Types.INT
            end

            fun checkCallExp {func=funSym, args=args, pos=pos} = let
                val argTypes = map checkExp' args
            in
                case Env.find (varEnv, funSym) of
                    SOME (Env.FunEntry {formals=formalTypes, result=resultType}) => (case findTypeListMismatch argTypes formalTypes of
                        NONE                                          => resultType
                      | SOME TooFewActuals                            =>
                            raiseErrorArgs pos "Call to '%s' has too few arguments" [Format.ATOM funSym]
                      | SOME TooManyActuals                           =>
                            raiseErrorArgs pos "Call to '%s' has too many arguments" [Format.ATOM funSym]
                      | SOME (Mismatch (i, actualType, expectedType)) =>
                            raiseErrorArgs pos "Argument %d of call to '%s' has type '%s', but the expected type is '%s'"
                                [Format.INT i, Format.ATOM funSym, typeToFormatStr actualType, typeToFormatStr expectedType]
                    )
                  | _                                                            =>
                        raiseErrorArgs pos "Missing definition for function: '%s'" [Format.ATOM funSym]
            end

            fun checkRecordExp {fields=expFields, typ=recordTypeSym, pos=pos} = let
                val expFieldTypes = map (fn (_, fieldExp, _) => checkExp' fieldExp) expFields
            in
                case Env.find (typeEnv, recordTypeSym) of
                    SOME (Types.RECORD (typeFieldInfo, uniq)) => let
                        val typeFieldTypes = map (fn (_, ty) => ty) typeFieldInfo
                    in
                        case findTypeListMismatch expFieldTypes typeFieldTypes of
                            NONE                => Types.RECORD (typeFieldInfo, uniq)
                          | SOME TooFewActuals  =>
                                raiseErrorArgs pos "Expression of type '%s' is missing field(s)" [Format.ATOM recordTypeSym]
                          | SOME TooManyActuals =>
                                raiseErrorArgs pos "Expression of type '%s' has too many fields" [Format.ATOM recordTypeSym]
                          | SOME (Mismatch (i, actualType, expectedType)) =>
                                raiseErrorArgs pos "Field %d in expression of type '%s' has type '%s', but the expected type is '%s'"
                                    [Format.INT i, Format.ATOM recordTypeSym, typeToFormatStr actualType, typeToFormatStr expectedType]
                    end
                  | _                                         =>
                        raiseErrorArgs pos "Missing definition for record type: '%s'" [Format.ATOM recordTypeSym]
            end

            fun checkArrayExp {typ=arrTypeSym, size=_, init=initExp, pos=pos} = case Env.find (typeEnv, arrTypeSym) of
                SOME (Types.ARRAY (elemType, uniq)) => let
                    val initExpType = checkExp' initExp
                in
                    if elemType != initExpType then
                        raiseErrorArgs pos "Initialization expression of array has type '%s', but expected type '%s'"
                            [typeToFormatStr initExpType, typeToFormatStr elemType]
                    else Types.ARRAY (elemType, uniq)
                end
              | _                                   =>
                    raiseErrorArgs pos "Missing definition for array type: '%s'" [Format.ATOM arrTypeSym]

            fun checkSeqExp exps = case exps of
                  []                   => Types.UNIT
                | [(exp', _)]          => checkExp' exp'
                | ((exp', _)::expRest) => (checkExp' exp'; checkSeqExp expRest)

            fun checkIfExp {test=testExp, then'=thenExp, else'=maybeElseExp, pos=pos} = let
                val testExpType = checkExp' testExp
            in
                if testExpType != Types.INT then
                    raiseErrorArgs pos "If-condition has non-boolean type ('%s')" [typeToFormatStr testExpType]
                else let
                    val thenExpType = checkExp' thenExp
                in
                    case maybeElseExp of
                        NONE         =>
                            if thenExpType != Types.UNIT then
                                raiseError pos "If-statements without corresponding else-statements must have type 'unit'"
                            else thenExpType
                      | SOME elseExp => let
                            val elseExpType = checkExp' elseExp
                        in
                            if thenExpType != elseExpType then
                                raiseErrorArgs pos "If-statement has type '%s', but corresponding else-statement has type '%s'"
                                    [typeToFormatStr thenExpType, typeToFormatStr elseExpType]
                            else thenExpType
                        end
                end
            end

            fun checkWhileExp {test=testExp, body=bodyExp, pos=pos} = let
                val testExpType = checkExp' testExp
            in
                if testExpType != Types.INT then
                    raiseErrorArgs pos "While-condition has non-boolean type ('%s')" [typeToFormatStr testExpType]
                else let
                    val bodyExpType = checkExp (varEnv, typeEnv, InLoop) bodyExp
                in
                    if bodyExpType != Types.UNIT then
                        raiseError pos "While-statement body must have type 'unit'"
                    else bodyExpType
                end
            end

            fun checkForExp {var=iterVar, escape=_, lo=loExp, hi=hiExp, body=bodyExp, pos=pos} = let
                val loExpType = checkExp' loExp
            in
                if loExpType != Types.INT then
                    raiseError pos "Lower bound of for-statement must have type 'int'"
                else let
                    val hiExpType = checkExp' hiExp
                in
                    if hiExpType != Types.INT then
                        raiseError pos "Upper bound of for-statement must have type 'int'"
                    else let
                        val varEnv' = Env.insert (varEnv, iterVar, Env.VarEntry {ty=Types.INT})
                        val bodyExpType = checkExp (varEnv', typeEnv, InLoop) bodyExp
                    in
                        if bodyExpType != Types.UNIT then
                            raiseError pos "For-statement body must have type 'unit'"
                        else bodyExpType
                    end
                end
            end

            fun checkLetExp {decs=letDecs, body=bodyExp, pos=_} = let
                val (varEnv', typeEnv') = checkDecls (varEnv, typeEnv) letDecs
            in
                checkExp (varEnv', typeEnv', loopCtx) bodyExp
            end
        in
            case exp of
                A.VarExp v      => checkVarExp v
              | A.NilExp        => Types.NIL
              | A.IntExp _      => Types.INT
              | A.StringExp _   => Types.STRING
              | A.OpExp opExp   => checkOpExp opExp
              | A.CallExp c     => checkCallExp c
              | A.RecordExp r   => checkRecordExp r
              | A.ArrayExp a    => checkArrayExp a
              | A.SeqExp exps   => checkSeqExp exps
              | A.AssignExp a   => checkAssignExp a
              | A.IfExp i       => checkIfExp i
              | A.WhileExp w    => checkWhileExp w
              | A.ForExp f      => checkForExp f
              | A.BreakExp pos => (case loopCtx of
                    NotInLoop => raiseError pos "'break' expression can only occur in body of loops"
                  | InLoop    => Types.UNIT
                )
              | A.LetExp l      => checkLetExp l
        end
        and checkDecls context decls = foldl checkDecl context decls
        and checkDecl (decl, (varEnv, typeEnv)) = let
            fun checkVarDecl {name=varName, escape=_, typ=maybeVarTypeSym, init=initExp, pos=pos} = (case maybeVarTypeSym of
                (* Type is explicitly provided in the declaration *)
                SOME varTypeSym => (case Env.find (typeEnv, varTypeSym) of
                    NONE         => raiseErrorArgs pos "Missing definition for type '%s'" [Format.ATOM varTypeSym]
                  | SOME varType => let
                        val initExpType = checkExp (varEnv, typeEnv, NotInLoop) initExp
                    in
                        if initExpType != varType then
                            raiseErrorArgs pos "Initialization expression of variable '%s' has unexpected type: '%s'"
                                [typeToFormatStr varType, typeToFormatStr initExpType]
                        else Env.insert (varEnv, varName, Env.VarEntry {ty=varType})
                    end
                )
                (* Type is inferred from initExp *)
              | NONE => let
                    val initExpTy = checkExp (varEnv, typeEnv, NotInLoop) initExp
                in
                    Env.insert (varEnv, varName, Env.VarEntry {ty=initExpTy})
                end
            )

            (* Define dummy decl for each type in a recursive group. This ensures the symbol is
               in the typeEnv when processing the actual definition *)
            fun createDummyTypeDecl ({name=lTypeSym, ty=_, pos=pos}, (typeEnv', dummyTypeDecls)) = (case Env.find (typeEnv', lTypeSym) of
                SOME (Types.NAME (typeSym, ref (SOME _))) =>
                    raiseErrorArgs pos "Duplicate definition of recursive type: '%s'" [Format.ATOM typeSym]
              | _                                         => let
                    val dummyTypeDecl = Types.NAME (lTypeSym, ref NONE)
                in
                    (Env.insert (typeEnv', lTypeSym, dummyTypeDecl), dummyTypeDecl::dummyTypeDecls)
                end
            )

            fun checkTypeDecl ({name=lTypeSym, ty=ty, pos=_}, typeEnv') = (case ty of
                  (* Type that renames an existing type *)
                  A.NameTy (rTypeSym, pos')  => (case Env.find (typeEnv', rTypeSym) of
                      NONE       => raiseErrorArgs pos' "Missing definition for type: '%s'" [Format.ATOM rTypeSym]
                    | SOME rType => Env.insert (typeEnv', lTypeSym, rType)
                  )
                | A.RecordTy fields          => typeEnv' (*TODO*)
                | A.ArrayTy (nameSym, pos)   => typeEnv' (*TODO*)
            )

            fun completeDummyTypeDecl typeEnv' (Types.NAME (typeSym, typeRef)) = (case Env.find (typeEnv', typeSym) of
                    SOME typeDef => (typeRef := SOME typeDef)
                  | NONE         => raise Unreachable
                )
              | completeDummyTypeDecl _ _ = raise Unreachable

            fun checkRecursiveTypeDecls typeDecs = let
                val (typeEnv', dummyTypeDecls) = foldl createDummyTypeDecl (typeEnv, []) typeDecs
                val typeEnv'' = foldl checkTypeDecl typeEnv' typeDecs
            in
                List.app (completeDummyTypeDecl typeEnv'') dummyTypeDecls;
                typeEnv''
            end

            fun checkFunSig {name=funSym, params=params, result=maybeResultTypeSym, body=_, pos=pos} = let
                fun checkFunParam {name=paramSym, escape=_, typ=typeSym, pos=pos'} = (case Env.find (typeEnv, typeSym) of
                    SOME paramType => (paramSym, paramType)
                  | NONE           =>
                        raiseErrorArgs pos' "Missing definition of type '%s' for parameter '%s' of function '%s'"
                            [Format.ATOM typeSym, Format.ATOM paramSym, Format.ATOM funSym]
                )

                val paramInfo = map checkFunParam params
                val sortedParamSyms = ListMergeSort.sort atomGt (map (fn (sym, _) => sym) paramInfo)
            in
                if not (allUniq Atom.same sortedParamSyms) then
                    raiseErrorArgs pos "Duplicate parameter names in function '%s'" [Format.ATOM funSym]
                else let
                    val returnType = (case maybeResultTypeSym of
                        (* Non-void function *)
                        SOME resultTypeSym => (case Env.find (typeEnv, resultTypeSym) of
                            SOME resultType => resultType
                          | NONE            =>
                                raiseErrorArgs pos "Missing definition for return type '%s' for function '%s'"
                                    [Format.ATOM resultTypeSym, Format.ATOM funSym]
                        )
                        (* Void function *)
                      | NONE               => Types.UNIT
                    )
                in
                    (paramInfo, returnType)
                end
            end

            fun checkFunBody varEnv' ({name=funSym, params=_, result=_, body=body, pos=pos}, (paramInfo, resultType)) = let
                val bodyEnv = Env.insertAll (varEnv', map (fn (s, ty) => (s, Env.VarEntry {ty=ty})) paramInfo)
                val bodyType = checkExp (bodyEnv, typeEnv, NotInLoop) body
            in
                if bodyType != resultType then
                    raiseErrorArgs pos "Function '%s' has an expected return type of '%s', but body has type '%s'"
                        [Format.ATOM funSym, typeToFormatStr resultType, typeToFormatStr bodyType]
                else ()
            end

            fun makeFunEntry (paramInfo, resultType) = let
                val paramTypes = map (fn (_, ty) => ty) paramInfo
            in
                Env.FunEntry {formals=paramTypes, result=resultType}
            end

            fun checkFunDecl funDec = let
                val funSig = checkFunSig funDec
                val varEnv' = Env.insert (varEnv, #name funDec, makeFunEntry funSig)
            in
                checkFunBody varEnv' (funDec, funSig);
                varEnv'
            end

            fun checkRecursiveFunDecls funDecs = let
                val funSyms : Atom.atom list = map (fn f => #name f) funDecs
                val sortedFunSyms = ListMergeSort.sort atomGt funSyms
            in
                if not (allUniq Atom.same sortedFunSyms) then
                    raiseError (#pos (hd funDecs)) "Duplicate function names in the function group starting here."
                else let
                    val funSigs = map checkFunSig funDecs
                    val varEnv' = Env.insertAll (varEnv, ListPair.zipEq (funSyms, map makeFunEntry funSigs))
                in
                    List.app (checkFunBody varEnv') (ListPair.zipEq (funDecs, funSigs));
                    varEnv'
                end
            end
        in
            case decl of
                A.VarDec varDec        => (checkVarDecl varDec,            typeEnv)
              | A.FunctionDec [funDec] => (checkFunDecl funDec,            typeEnv)
              | A.FunctionDec funDecs  => (checkRecursiveFunDecls funDecs, typeEnv)
              | A.TypeDec [typeDec]    => (varEnv,                         checkTypeDecl (typeDec, typeEnv))
              | A.TypeDec typeDecs     => (varEnv,                         checkRecursiveTypeDecls typeDecs)
        end
    in
        checkExp (Env.baseVarEnv, Env.baseTypeEnv, NotInLoop) rootExp; ()
    end
end