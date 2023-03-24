structure Env : 
sig
    type access
    type ty
    
    datatype enventry = VarEntry of {access: Translate.access, ty: ty}
                      | FunEntry of {level: Translate.level, label: Temp.label, formals: ty list, result: ty} (* formals = parameter types, result = return type *)
    val base_tenv : ty Symbol.table (* Predefined Types*)
    val base_venv : enventry Symbol.table (* Predefined Variables *)
end =
struct 

    type access = Types.ty (* not sure what this is actually supposed to be *)
    type ty = Types.ty

    datatype enventry = VarEntry of {access: Translate.access, ty: ty}
                      | FunEntry of {level: Translate.level, label: Temp.label, formals: ty list, result: ty} (* formals = parameter types, result = return type *)


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
        FunEntry{formals=[Types.STRING], result=Types.UNIT, level=Translate.outermost, label=Temp.namedlabel "print"}    
    ),
        Symbol.symbol "flush",
        FunEntry{formals=[], result=Types.UNIT, level=Translate.outermost, label=Temp.namedlabel "flush"}
    ),
        Symbol.symbol "getchar",
        FunEntry{formals=[], result=Types.STRING, level=Translate.outermost, label=Temp.namedlabel "getchar"}
    ),
        Symbol.symbol "ord",
        FunEntry{formals=[Types.STRING], result=Types.INT, level=Translate.outermost, label=Temp.namedlabel "ord"}
    ),
        Symbol.symbol "chr",
        FunEntry{formals=[Types.INT], result=Types.STRING, level=Translate.outermost, label=Temp.namedlabel "chr"}
    ),
        Symbol.symbol "size",
        FunEntry{formals=[Types.STRING], result=Types.INT, level=Translate.outermost, label=Temp.namedlabel "size"}
    ),
        Symbol.symbol "substring",
        FunEntry{formals=[Types.STRING, Types.INT, Types.INT], result=Types.STRING, level=Translate.outermost, label=Temp.namedlabel "substring"}
    ),
        Symbol.symbol "concat",
        FunEntry{formals=[Types.STRING, Types.STRING], result=Types.STRING, level=Translate.outermost, label=Temp.namedlabel "concat"}
    ),
        Symbol.symbol "not",
        FunEntry{formals=[Types.INT], result=Types.INT, level=Translate.outermost, label=Temp.namedlabel "not"}
    ),
        Symbol.symbol "exit",
        FunEntry{formals=[Types.INT], result=Types.IMPOSSIBILITY, level=Translate.outermost, label=Temp.namedlabel "exit"}
    )
    

end
