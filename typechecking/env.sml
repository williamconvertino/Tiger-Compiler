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

    val base_tenv = 
    Symbol.enter (
    Symbol.enter (
        Symbol.empty, 
        Symbol.symbol "int", 
        Types.INT
    ), 
        Symbol.symbol "string", 
        Types.STRING
    )
    
    
    val base_venv = 
    Symbol.enter (
    Symbol.enter (
    Symbol.enter (
    Symbol.enter (
    Symbol.enter (
    Symbol.enter (
    Symbol.enter (
    Symbol.enter (
    Symbol.enter (
    Symbol.enter (
        Symbol.empty,
        Symbol.symbol "print",
        FunEntry{formals=[Types.STRING], result=Types.UNIT}    
    ),
        Symbol.symbol "flush",
        FunEntry{formals=[], result=Types.UNIT}
    ),
        Symbol.symbol "getchar",
        FunEntry{formals=[], result=Types.STRING}
    ),
        Symbol.symbol "ord",
        FunEntry{formals=[Types.STRING], result=Types.INT}
    ),
        Symbol.symbol "chr",
        FunEntry{formals=[Types.INT], result=Types.STRING}
    ),
        Symbol.symbol "size",
        FunEntry{formals=[Types.STRING], result=Types.INT}
    ),
        Symbol.symbol "substring",
        FunEntry{formals=[Types.STRING, Types.INT, Types.INT], result=Types.STRING}
    ),
        Symbol.symbol "concat",
        FunEntry{formals=[Types.STRING, Types.STRING], result=Types.STRING}
    ),
        Symbol.symbol "not",
        FunEntry{formals=[Types.INT], result=Types.INT}
    ),
        Symbol.symbol "exit",
        FunEntry{formals=[Types.INT], result=Types.IMPOSSIBILITY}
    )
    

end