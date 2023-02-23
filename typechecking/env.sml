structure Env : sig
    type access
    type ty
    datatype enventry = VarEntry of {ty: ty}
                    |   FunEntry of {formals: ty list, result: ty}
    val base_tenv : ty Symbol.table (* Predefined Types*)
    val base_venv : enventry Symbol.table (* Predefined Variables *)
end =
struct 

    val base_tenv

end