structure Env : 
sig
    type access
    type ty
    datatype enventry = VarEntry of {ty: ty}
                      | FunEntry of {formals: ty list, result: ty} (* formals = parameter types, result = return type *)
    val base_tenv : ty Symbol.table (* Predefined Types*)
    val base_venv : enventry Symbol.table (* Predefined Variables *)
end =
struct 

    type access = Types.ty (* not sure what this is actually supposed to be *)
    type ty = Types.ty

    datatype enventry = VarEntry of {ty: ty}
                      | FunEntry of {formals: ty list, result: ty} (* formals = parameter types, result = return type *)

    val base_tenv = Symbol.enter (Symbol.enter (Symbol.empty, Symbol.symbol "int", Types.INT), Symbol.symbol "string", Types.STRING)
    val base_venv = Symbol.empty (* need to add base stdlib functions *)

end