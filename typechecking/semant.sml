structure Semant : 
sig 
    type venv = Env.enventry Symbol.table
    type tenv = Types.ty Symbol.table
    
    type expty = {exp: Translate.exp, ty: Types.ty}

    val transVar: venv * tenv * Absyn.var -> expty
    val transExp: venv * tenv * Absyn.exp -> expty
    val transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
    val transTy:  tenv * Absyn.ty -> Types.ty

    val transProg : Absyn.exp -> unit

end =

struct 

    type venv = Env.enventry Symbol.table
    type tenv = Types.ty Symbol.table
    
    type expty = {exp: Translate.exp, ty: Types.ty}


    fun transVar (venv, tenv, var) = {exp = (), ty = Types.IMPOSSIBILITY}
    fun transExp (venv, tenv, exp) = {exp = (), ty = Types.IMPOSSIBILITY}
    fun transDec (venv, tenv, dec) = {venv = venv, tenv = tenv}
    fun transTy  (tenv, ty)        = Types.IMPOSSIBILITY

    fun transProg exp = () (* TODO *)

end