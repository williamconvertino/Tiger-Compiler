structure Semant : 
sig 
    
    type venv : Env.enventry Symbol.table
    type tenv = ty Symbol.table

    val transVar : venv * tenv * Absyn.var -> expty
    val transExp: venv * tenv * Absyn.exp -> expty
    val transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
    val transTy: tenv * Absyn.ty -> Types.ty

    val transProg : Absyn.exp -> unit

end =

struct 

fun transProg absyntExp = () (* TODO *)

end