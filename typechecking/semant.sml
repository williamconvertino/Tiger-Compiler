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

    fun checkInt ({exp, ty=Types.INT}, pos) = () |
        checkInt (_, pos) = ErrorMsg.error pos "integer required"

    fun actual_ty ty = case ty of
        Types.NAME(_, tyref) => (case !tyref of
            NONE     => Types.IMPOSSIBILITY |
            SOME(ty) => actual_ty ty) |
        _            => ty
    
    fun transDecs (venv, tenv, []) = {venv=venv, tenv=tenv} |
        transDecs (venv, tenv, dec::l) =
            let val {venv=venv', tenv=tenv'} = transDec(venv, tenv, dec)
            in transDecs (venv', tenv', l) end

        (* -- Types -- *)
    and transTy  (tenv, ty)        = Types.IMPOSSIBILITY

        (* -- Var Decs -- *)
        (* var x := exp *)
    and transDec (venv, tenv, A.VarDec{name, typ=NONE, init, escape, pos}) = 
        let val {exp, ty} = transExp (venv, tenv) init
        in 
            {tenv=tenv, venv=S.enter (venv, name, E.VarEntry{ty=ty})}
        end |
        (* var x: type := exp *)
        (* TODO *)

        (* -- Type Decs -- *)
        (* type t = ty *)
        transDec (venv, tenv, A.TypeDec[{name, ty, pos}]) =
            {venv=venv, tenv=S.enter(tenv, name, transTy(tenv, ty))}

    
    and transExp (venv, tenv) = 
                (* -- Expressions -- *)
                (* Ops *)
        let fun trexp (A.OpExp{left, oper=A.PlusOp, right, pos}) =
                (checkInt(trexp left, pos); checkInt(trexp right, pos); {exp=(), ty=Types.INT})
            
            (* Let *)
            |   trexp (A.LetExp{decs, body, pos}) =
                    let val {venv=venv', tenv=tenv'} = transDecs(venv, tenv, decs)
                    in transExp (venv', tenv') body
                end


                (* -- Vars -- *)
                (* foo *)
            and trvar (A.SimpleVar(id, pos)) = (
                case Symbol.look(venv, id)
                    of SOME(E.VarEntry{ty}) => {exp=(), ty=actual_ty ty}
                    |  SOME(E.FunEntry _) => (ErrorMsg.error pos ("undefined variable " ^ S.name id);
                                                {exp=(), ty=Types.IMPOSSIBILITY})
                    |  NONE                 => (ErrorMsg.error pos ("undefined variable " ^ S.name id);
                                                {exp=(), ty=Types.IMPOSSIBILITY})
            )
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