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
        fun compare i inputs = (case inputs of
            ([], [])                 => NONE
          | ([], _)                  => SOME TooFewActuals
          | (_, [])                  => SOME TooManyActuals
          | (actual::ax, formal::fx) =>
              if actual != formal then SOME (Mismatch (i, actual, formal))
              else compare (i+1) (ax, fx)
        )
    in
        compare 1 (actuals, formals)
    end

    fun atomGt (a, b) = Atom.lexCompare (a, b) = GREATER

    fun allUniq lst = let
        fun allUniq' lst' = (case lst' of
            []         => true
          | [_]        => true
          | a::b::rest => not (Atom.same (a, b)) andalso (allUniq' rest)
        )
    in
        allUniq' (ListMergeSort.sort atomGt lst)
    end

    fun typeToFormatStr t = Format.STR (Types.typeToString t)

    fun opToFormatStr oper = Format.STR (A.opToString oper)

    fun typeCheck rootExp = let
        fun checkExp (varEnv, typeEnv, loopCtx) exp = let
            val checkExp' = checkExp (varEnv, typeEnv, loopCtx)

            fun checkVar var = (case var of
                A.SimpleVar (varSym, pos)             => (case Env.find (varEnv, varSym) of
                    SOME (Env.VarEntry {ty=varTy}) => varTy
                  | _                              =>
                        raiseErrorArgs pos "Missing definition for variable: %s" [Format.ATOM varSym]
                )
              | A.FieldVar (recordVar, fieldSym, pos) => let
                    val varType = checkVar recordVar
                in
                    case Types.getActualType varType of
                        Types.RECORD (fields, _) => (case List.find (fn (sym, _) => Atom.same (sym, fieldSym)) fields of
                            SOME (_, fieldTy) => fieldTy
                          | NONE              =>
                                raiseErrorArgs pos "Record type '%s' does not have field '%s'"
                                    [typeToFormatStr varType, Format.ATOM fieldSym]
                        )
                      | _                        =>
                            raiseErrorArgs pos "Attempted to access a field on variable, '%s', which is not a record"
                                [Format.STR (A.varToString 0 recordVar)]
                end
              | A.SubscriptVar (arrVar, subExp, pos) => (case Types.getActualType (checkVar arrVar) of
                    Types.ARRAY (elemType, _) =>
                        if (checkExp' subExp) != Types.INT then
                            raiseError pos "Subscript index expressions must have type 'int'"
                        else elemType
                    | _                         =>
                        raiseErrorArgs pos "Attempted to subscript a variable, '%s', which is not an array"
                            [Format.STR (A.varToString 0 arrVar)]
                )
            )

            fun checkAssignExp {var=var, exp=assignExp, pos=pos} = let
                val varType = checkVar var
                val expType = checkExp' assignExp
            in
                if varType != expType then
                    raiseErrorArgs pos "Type mismatch in assignment: '%s' and '%s'" (map typeToFormatStr [varType, expType])
                else varType
            end

            fun checkOpExp {left=left, oper=oper, right=right, pos=pos} = let
                val leftType = checkExp' left
                val rightType = checkExp' right

                fun checkOps kind ty =
                    if oper <> A.EqOp andalso oper <> A.NeqOp then
                        raiseErrorArgs pos "Cannot use operator '%s' with %s types"
                            [opToFormatStr oper, Format.STR kind]
                    else ty
                fun checkUniqTypes kind (uniq1:Types.unique) (uniq2:Types.unique) =
                    if uniq1 <> uniq2 then
                        raiseErrorArgs pos "Cannot compare distinct instances of %s types '%s' and '%s'"
                            ((Format.STR kind)::(map typeToFormatStr [leftType, rightType]))
                    else checkOps kind leftType
            in
                case (Types.getActualType leftType, Types.getActualType rightType) of
                    (Types.INT, Types.INT)                             => leftType
                  | (Types.ARRAY (_, uniq1), Types.ARRAY (_, uniq2))   => checkUniqTypes "array" uniq1 uniq2
                  | (Types.RECORD (_, uniq1), Types.RECORD (_, uniq2)) => checkUniqTypes "record" uniq1 uniq2
                  | (Types.RECORD _, Types.NIL)                        => checkOps "record" leftType
                  | (Types.NIL, Types.RECORD _)                        => checkOps "record" rightType
                  | _                                                  =>
                        raiseErrorArgs pos "Operator '%s' does not accept operands of types '%s' and '%s'"
                            [opToFormatStr oper, typeToFormatStr leftType, typeToFormatStr rightType]
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

            fun checkRecordExp {fields=expFields, typ=recordTypeSym, pos=pos} = (case Env.find (typeEnv, recordTypeSym) of
                SOME declaredType => (case Types.getActualType declaredType of
                    Types.RECORD (typeFieldInfo, _) => let
                        val expFieldTypes = map (fn (_, fieldExp, _) => checkExp' fieldExp) expFields
                        val typeFieldTypes = map (fn (_, ty) => ty) typeFieldInfo
                    in
                        case findTypeListMismatch expFieldTypes typeFieldTypes of
                            NONE                                          => declaredType
                          | SOME TooFewActuals                            =>
                                raiseErrorArgs pos "Expression of type '%s' is missing field(s)" [Format.ATOM recordTypeSym]
                          | SOME TooManyActuals                           =>
                                raiseErrorArgs pos "Expression of type '%s' has too many fields" [Format.ATOM recordTypeSym]
                          | SOME (Mismatch (i, actualType, expectedType)) =>
                                raiseErrorArgs pos "Field %d in expression of type '%s' has type '%s', but the expected type is '%s'"
                                    [Format.INT i, Format.ATOM recordTypeSym, typeToFormatStr actualType, typeToFormatStr expectedType]
                    end
                  | nonRecordType => raiseErrorArgs pos "Record expression has non-record type: '%s'" [typeToFormatStr nonRecordType]
                )
              | NONE              =>
                    raiseErrorArgs pos "Missing definition for record type: '%s'" [Format.ATOM recordTypeSym]
            )

            fun checkArrayExp {typ=arrTypeSym, size=sizeExp, init=initExp, pos=pos} = (case Env.find (typeEnv, arrTypeSym) of
                SOME declaredType => (case Types.getActualType declaredType of
                    Types.ARRAY (elemType, _) => let
                        val initExpType = checkExp' initExp
                    in
                        if elemType != initExpType then
                            raiseErrorArgs pos "Initialization expression of array has type '%s', but expected type '%s'"
                                (map typeToFormatStr [initExpType, elemType])
                        else if (checkExp' sizeExp) != Types.INT then
                            raiseError pos "Size expression of array must have type 'int'"
                        else declaredType
                    end
                  | nonArrayType =>
                        raiseErrorArgs pos "Initialization expression of array has non-array type '%s'"
                            [typeToFormatStr nonArrayType]
                )
              | NONE              =>
                    raiseErrorArgs pos "Missing definition for array type: '%s'" [Format.ATOM arrTypeSym]
            )

            fun checkSeqExp exps = (case exps of
                []            => Types.UNIT
              | exp0::expRest => foldl (fn (exp', _) => checkExp' exp') (checkExp' exp0) expRest
            )

            fun checkIfExp {test=testExp, then'=thenExp, else'=maybeElseExp, pos=pos} = let
                val testExpType = checkExp' testExp
                val thenExpType = checkExp' thenExp
            in
                if testExpType != Types.INT then
                    raiseErrorArgs pos "If-condition has non-boolean type ('%s')" [typeToFormatStr testExpType]
                else (case maybeElseExp of
                    NONE         =>
                        if thenExpType != Types.UNIT then
                            raiseError pos "If-statements without corresponding else-statements must have type 'unit'"
                        else thenExpType
                  | SOME elseExp => let
                        val elseExpType = checkExp' elseExp
                    in
                        if thenExpType != elseExpType then
                            raiseErrorArgs pos "If-statement has type '%s', but corresponding else-statement has type '%s'"
                                (map typeToFormatStr [thenExpType, elseExpType])
                        else thenExpType
                    end
                )
            end

            fun checkWhileExp {test=testExp, body=bodyExp, pos=pos} = let
                val testExpType = checkExp' testExp
                val bodyExpType = checkExp (varEnv, typeEnv, InLoop) bodyExp
            in
                if testExpType != Types.INT then
                    raiseErrorArgs pos "While-condition has non-boolean type ('%s')" [typeToFormatStr testExpType]
                else if bodyExpType != Types.UNIT then
                    raiseError pos "While-statement body must have type 'unit'"
                else bodyExpType
            end

            fun checkForExp {var=iterVar, escape=_, lo=loExp, hi=hiExp, body=bodyExp, pos=pos} = let
                val loExpType = checkExp' loExp
                val hiExpType = checkExp' hiExp
            in
                if loExpType != Types.INT then
                    raiseError pos "Lower bound of for-statement must have type 'int'"
                else if hiExpType != Types.INT then
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

            fun checkLetExp {decs=letDecs, body=bodyExp, pos=_} = let
                val (varEnv', typeEnv') = checkDecls (varEnv, typeEnv) letDecs
            in
                checkExp (varEnv', typeEnv', loopCtx) bodyExp
            end
        in
            case exp of
                A.VarExp v     => checkVar v
              | A.NilExp       => Types.NIL
              | A.IntExp _     => Types.INT
              | A.StringExp _  => Types.STRING
              | A.OpExp opExp  => checkOpExp opExp
              | A.CallExp c    => checkCallExp c
              | A.RecordExp r  => checkRecordExp r
              | A.ArrayExp a   => checkArrayExp a
              | A.SeqExp exps  => checkSeqExp exps
              | A.AssignExp a  => checkAssignExp a
              | A.IfExp i      => checkIfExp i
              | A.WhileExp w   => checkWhileExp w
              | A.ForExp f     => checkForExp f
              | A.BreakExp pos => (case loopCtx of
                    NotInLoop => raiseError pos "'break' expression can only occur in body of loops"
                  | InLoop    => Types.UNIT
                )
              | A.LetExp l     => checkLetExp l
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
                                (map typeToFormatStr [varType, initExpType])
                        else Env.insert (varEnv, varName, Env.VarEntry {ty=varType})
                    end
                )
                (* Type is inferred from initExp *)
              | NONE => let
                    val initExpTy = checkExp (varEnv, typeEnv, NotInLoop) initExp
                in
                    case initExpTy of
                        Types.NIL =>
                            raiseErrorArgs pos "Cannot infer type of variable '%s' from initialization expression 'nil'"
                                [Format.ATOM varName]
                      | _         => Env.insert (varEnv, varName, Env.VarEntry {ty=initExpTy})
                end
            )

            fun checkTypeDecl ({name=lTypeSym, ty=ty, pos=pos}, typeEnv') = (case ty of
                (* Type that renames an existing type *)
                A.NameTy (rTypeSym, pos')     => (case Env.find (typeEnv', rTypeSym) of
                    NONE       => raiseErrorArgs pos' "Missing definition for type: '%s'" [Format.ATOM rTypeSym]
                  | SOME rType => Env.insert (typeEnv', lTypeSym, rType)
                )
              | A.RecordTy fields             => let
                    fun checkField {name=fieldSym, escape=_, typ=fieldTySym, pos=pos'} = let
                        val fieldType = (case Env.find (typeEnv', fieldTySym) of
                            SOME fieldType => fieldType
                          | NONE           =>
                                raiseErrorArgs pos' "Missing definition for type '%s' of field '%s'"
                                    (map Format.ATOM [fieldTySym, fieldSym])
                        )
                    in
                        (fieldSym, fieldType)
                    end
                in
                    if not (allUniq (map (fn field => #name field) fields)) then
                        raiseErrorArgs pos "Record type '%s' has duplicate fields" [Format.ATOM lTypeSym]
                    else
                        Env.insert (typeEnv', lTypeSym, Types.RECORD (map checkField fields, ref ()))
                end
              | A.ArrayTy (elemTypeSym, pos') => let
                    val elemType = (case Env.find (typeEnv', elemTypeSym) of
                        SOME elemType => elemType
                      | NONE          => raiseErrorArgs pos' "Missing definition for type '%s'" [Format.ATOM elemTypeSym]
                    )
                in
                    Env.insert (typeEnv', lTypeSym, Types.ARRAY (elemType, ref ()))
                end
            )

            fun checkTypeDecls typeDecs = let
                val typeSyms : Atom.atom list = map (fn dec => #name dec) typeDecs
                val pos : A.pos = (#pos (hd typeDecs))
            in
                if not (allUniq typeSyms) then
                    raiseError pos "Duplicate type names in group of recursive types starting here"
                else let
                    val dummyDecls : (Atom.atom * Types.ty option ref) list = map (fn dec => (#name dec, ref NONE)) typeDecs
                    fun insertDummy ((typeSym, typeRef), typeEnv') =
                        Env.insert (typeEnv', typeSym, Types.NAME (typeSym, typeRef))
                    val typeEnv' = foldl checkTypeDecl (foldl insertDummy typeEnv dummyDecls) typeDecs
                    fun completeDummyDecl (typeSym, typeRef) = (case Env.find (typeEnv', typeSym) of
                        SOME realDecl => typeRef := SOME realDecl
                      | NONE          => raise Unreachable
                    )
                    (* Note: this is more permissive than Appel's specification, which requires passing through
                       record or array types. I don't see a good reason for primitive types to not be included. *)
                    fun isValidRecursiveGroup typeSyms' = (case typeSyms' of
                        []            => false
                      | typeSym::rest => (case Env.find (typeEnv', typeSym) of
                            SOME (Types.NAME _)   => isValidRecursiveGroup rest
                          | SOME _                => true
                          | NONE                  => isValidRecursiveGroup rest
                        )
                    )
                in
                    List.app completeDummyDecl dummyDecls;
                    if not (isValidRecursiveGroup typeSyms) then
                        raiseError pos "No types in the recursive group starting here pass through a record, array, or primitive type"
                    else typeEnv'
                end
            end

            fun checkFunSig {name=funSym, params=params, result=maybeResultTypeSym, body=_, pos=pos} = let
                fun checkFunParam {name=paramSym, escape=_, typ=typeSym, pos=pos'} = (case Env.find (typeEnv, typeSym) of
                    SOME paramType => (paramSym, paramType)
                  | NONE           =>
                        raiseErrorArgs pos' "Missing definition of type '%s' for parameter '%s' of function '%s'"
                            (map Format.ATOM [typeSym, paramSym, funSym])
                )

                val paramInfo = map checkFunParam params
            in
                if not (allUniq (map (fn (sym, _) => sym) paramInfo)) then
                    raiseErrorArgs pos "Duplicate parameter names in function '%s'" [Format.ATOM funSym]
                else let
                    val returnType = (case maybeResultTypeSym of
                        (* Non-void function *)
                        SOME resultTypeSym => (case Env.find (typeEnv, resultTypeSym) of
                            SOME resultType => resultType
                          | NONE            =>
                                raiseErrorArgs pos "Missing definition for return type '%s' for function '%s'"
                                    (map Format.ATOM [resultTypeSym, funSym])
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
            in
                if not (allUniq funSyms) then
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
              | A.TypeDec typeDecs     => (varEnv,                         checkTypeDecls typeDecs)
        end
    in
        checkExp (Env.baseVarEnv, Env.baseTypeEnv, NotInLoop) rootExp; ()
    end
end
