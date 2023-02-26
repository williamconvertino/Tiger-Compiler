structure Semant : 
sig 
    type venv = Env.enventry Symbol.table
    type tenv = Types.ty Symbol.table
    
    type expty = {exp: Translate.exp, ty: Types.ty}

    (* Don't think this is needed -- included in transExp val transVar: venv * tenv * Absyn.var -> expty *)
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
            SOME(ty) => ty) |
        _            => ty

    (* Don't know if this is needed -- fun transVar (venv, tenv, var) = {exp = (), ty = Types.IMPOSSIBILITY} *)
    
    fun transExp (venv, tenv) = 
        let fun trexp (A.OpExp{left, oper=A.PlusOp, right, pos}) =
                (checkInt(trexp left, pos); checkInt(trexp right, pos); {exp=(), ty=Types.INT})
            and trvar (A.SimpleVar(id, pos)) = (
                case Symbol.look(venv, id)
                    of SOME(E.VarEntry{ty}) => {exp=(), ty=actual_ty ty}
                    |  SOME(E.FunEntry _) => (ErrorMsg.error pos ("undefined variable " ^ S.name id);
                                                {exp=(), ty=Types.INT})
                    |  NONE                 => (ErrorMsg.error pos ("undefined variable " ^ S.name id);
                                                {exp=(), ty=Types.INT})
            )
    in
        trexp
    end

    fun transDec (venv, tenv, dec) = {venv = venv, tenv = tenv}
    fun transTy  (tenv, ty)        = Types.IMPOSSIBILITY

    fun transProg exp = () (* TODO *)

end