structure Semant : 
sig 
    type venv = Env.enventry Symbol.table
    type tenv = Types.ty Symbol.table
    
    type expty = {exp: Translate.exp, ty: Types.ty}

    val transExp: venv * tenv -> Absyn.exp -> expty
    val transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
    val transTy:  tenv * Absyn.ty -> Types.ty

    val transProg : Absyn.exp -> unit

end =

struct 

    type venv = Env.enventry Symbol.table
    type tenv = Types.ty Symbol.table
    
    type expty = {exp: Translate.exp, ty: Types.ty}

    structure A = Absyn
    structure E = Env
    structure S = Symbol

    fun checkInt ({exp, ty}, pos) = Types.checkType(ty, Types.INT, pos)


    fun actual_ty ty = case ty of
        Types.NAME(_, tyref) => (case !tyref of
            NONE     => Types.IMPOSSIBILITY |
            SOME(ty) => actual_ty ty) |
        _            => ty

    fun lookupVarType (venv, id, pos) = 
        case Symbol.look(venv, id)
            of SOME(E.VarEntry{ty}) => actual_ty ty
            |  SOME(E.FunEntry _)   => (ErrorMsg.error pos ("undefined variable " ^ S.name id); Types.IMPOSSIBILITY)
            |  NONE                 => (ErrorMsg.error pos ("undefined variable " ^ S.name id); Types.IMPOSSIBILITY)

    fun lookupTypeDec (tenv, id, pos) =
        case Symbol.look(tenv, id)
            of SOME(ty) => actual_ty ty
            |  NONE     => (ErrorMsg.error pos ("undefined type " ^ S.name id); Types.IMPOSSIBILITY)
    
    fun transDecs (venv, tenv, []) = {venv=venv, tenv=tenv} |
        transDecs (venv, tenv, dec::l) =
            let val {venv=venv', tenv=tenv'} = transDec(venv, tenv, dec)
            in transDecs (venv', tenv', l) end

        (* -- Types -- *)
    and transTy (tenv, A.NameTy(sym, pos)) = lookupTypeDec (tenv, sym, pos) |
        transTy (tenv, A.ArrayTy(sym, pos)) = Types.ARRAY(lookupTypeDec (tenv, sym, pos), ref ()) |
        transTy (tenv, A.RecordTy(fieldlist)) =
            let fun buildRec (recty, []) = recty |
                    buildRec ((symlst, uniq), {name, typ, escape=_, pos}::l) = buildRec (((name, lookupTypeDec (tenv, typ, pos)) :: symlst, uniq), l)
            in
                Types.RECORD (buildRec (([], ref ()), fieldlist))
            end

        (* -- Var Decs -- *)
        (* var x := exp *)
    and transDec (venv, tenv, A.VarDec{name, typ=NONE, init, escape, pos}) = 
            let val {exp, ty} = transExp (venv, tenv) init
            in 
                {tenv=tenv, venv=S.enter (venv, name, E.VarEntry{ty=ty})}
            end |
        (* var x: type := exp *)
        transDec (venv, tenv, A.VarDec{name, typ=SOME((tysym, typos)), init, escape, pos}) =
            let val {exp, ty=initty} = transExp (venv, tenv) init
                val decty = lookupTypeDec (tenv, tysym, typos)
                val evalty = Types.checkType(initty, decty, pos)
            in
                {tenv=tenv, venv=S.enter (venv, name, E.VarEntry{ty=evalty})}
            end |

        (* -- Type Decs -- *)
        (* type t = ty *)
        transDec (venv, tenv, A.TypeDec[{name, ty, pos}]) =
            {venv=venv, tenv=S.enter(tenv, name, transTy(tenv, ty))}

    
    and transExp (venv, tenv) = 
            (* -- Expressions -- *)
            (* Constants *)
        let fun trexp (A.IntExp (int)) = {exp=(), ty=Types.INT}
            |   trexp (A.StringExp (str, pos)) = {exp=(), ty=Types.STRING}
        
            (* Ops *)
            |   trexp (A.OpExp{left, oper=A.PlusOp, right, pos}) =
                (checkInt(trexp left, pos); checkInt(trexp right, pos); {exp=(), ty=Types.INT})
            
            (* SeqExps *)
            |   trexp (A.SeqExp (exps)) =
                    let fun trseq [] = {exp=(), ty=Types.UNIT}
                        |   trseq ((exp, pos)::[]) = trexp exp
                        |   trseq ((exp, pos)::seq) = (trexp exp; trseq seq)
                    in
                        trseq exps
                    end

            (* VarExp *)
            |   trexp (A.VarExp (var)) = trvar var

            (* Let *)
            |   trexp (A.LetExp{decs, body, pos}) =
                    let val {venv=venv', tenv=tenv'} = transDecs(venv, tenv, decs)
                    in transExp (venv', tenv') body
                end


                (* -- Vars -- *)
                (* foo *)
            and trvar (A.SimpleVar(id, pos)) = {exp=(), ty=lookupVarType (venv, id, pos)}
                (* foo[bar] *)
            |   trvar (A.SubscriptVar(var, exp, pos)) =
                    let fun tycheck ({exp=varexp, ty=Types.ARRAY(arrty, _)}, {exp=expexp, ty=Types.INT}) = 
                                {exp=(), ty=arrty}
                        |   tycheck ({exp=varexp, ty=Types.ARRAY(arrty, _)}, {exp=expexp, ty=_}) = (ErrorMsg.error pos ("index expression must be of type int");
                                {exp=(), ty=arrty})
                        |   tycheck ({exp=varexp, ty=_}, _) = (ErrorMsg.error pos ("cannot index non-array type");
                                {exp=(), ty=Types.IMPOSSIBILITY})
                    in
                        tycheck (trvar var, trexp exp) 
                    end
                (* foo.bar *)
            |   trvar (A.FieldVar(var, symbol, pos)) =
                    let fun findsymty (sym, (sym', ty)::l) = if Symbol.eq (sym, sym') then SOME(ty) else findsymty(sym, l) |
                            findsymty (sym, []) = NONE
                        fun tycheck {exp=varexp, ty=Types.RECORD(symtys, _)} = (
                                case (findsymty (symbol, symtys)) of
                                    SOME(ty) => {exp=(), ty=ty} |
                                    NONE     => (ErrorMsg.error pos ("must reference record type"); {exp=(), ty=Types.IMPOSSIBILITY})
                            )
                        |   tycheck {exp=varexp, ty=_} = (ErrorMsg.error pos ("must reference record type");
                                {exp=(), ty=Types.IMPOSSIBILITY})
                    in
                        tycheck (trvar var)
                    end
    in
        trexp
    end

    fun transProg exp = (transExp (Env.base_venv, Env.base_tenv) exp; ())

end