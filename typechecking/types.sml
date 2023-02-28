structure Types =
struct

  type unique = unit ref

  datatype ty = 
            RECORD of (Symbol.symbol * ty) list * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
	        | NAME of Symbol.symbol * ty option ref
	        | UNIT
          | IMPOSSIBILITY

  fun toString ty = 
    let fun unpack_record ([], recstr, _) = recstr ^ "}" |
            unpack_record ((sym, ty)::[], recstr, cnt)  = unpack_record([], recstr ^ (Symbol.name sym) ^ ":" ^ (toStr (ty, cnt)), cnt) |
            unpack_record ((sym, ty)::l, recstr, cnt)  = unpack_record(l, recstr ^ (Symbol.name sym) ^ ":" ^ (toStr (ty, cnt)) ^ ", ", cnt)
        
        and unravel_name (sym, NONE, _) = (print("ERROR: type ref of " ^ Symbol.name sym ^ " not properly set by compiler\n") ; "impossibility")
        |   unravel_name (sym, SOME(ty), 0) = Symbol.name sym ^ ":... (depth exceeded)"
        |   unravel_name (sym, SOME(ty), cnt) = Symbol.name sym ^ ":" ^ toStr (ty, (cnt-1))
    
        and toStr (ty, cnt) =
          case ty of
            NIL             => "nil" |
            INT             => "int" |
            STRING          => "string" |
            UNIT            =>  "unit"  |
            IMPOSSIBILITY   => "impossibility" |
            ARRAY(ty, uniq) =>  toString ty  |
            NAME(sym, tyop)   =>  unravel_name(sym, !tyop, cnt) |
            RECORD(syms, uniq) => unpack_record (syms, "{", cnt)
    in
      toStr (ty, 2)
    end

  fun checkType (NAME(sym, tyop), ty', pos) = (case !tyop of
        NONE => (ErrorMsg.error pos ("type " ^ Symbol.name (sym) ^ " has not been declared"); IMPOSSIBILITY) |
        SOME(ty) => checkType (ty, ty', pos)
      )
    | checkType (ty, NAME(sym, tyop), pos) = (case !tyop of
        NONE => (ErrorMsg.error pos ("type " ^ Symbol.name (sym) ^ " has not been declared"); IMPOSSIBILITY) |
        SOME(ty') => checkType (ty, ty', pos)
      )
    | checkType (RECORD(symlist, uniq), RECORD(_, uniq'), pos) = 
      if (uniq = uniq')
        then RECORD(symlist, uniq)
        else (ErrorMsg.error pos "record types do not match"; NIL) 
    | checkType (NIL, RECORD(symlist, uniq), _) = RECORD(symlist, uniq)
    | checkType (IMPOSSIBILITY, RECORD(symlist, uniq), _) = RECORD(symlist, uniq)
    | checkType (INT, INT, _) = INT
    | checkType (IMPOSSIBILITY, INT, _) = INT
    | checkType (STRING, STRING, _) = STRING
    | checkType (IMPOSSIBILITY, STRING, _) = STRING
    | checkType (ARRAY(arrty, uniq), ARRAY(_, uniq'), pos) = 
      if (uniq = uniq')
        then ARRAY(arrty, uniq)
        else (ErrorMsg.error pos "array types do not match"; IMPOSSIBILITY) 
    | checkType (IMPOSSIBILITY, ARRAY(arrty, uniq), _) = ARRAY(arrty, uniq)
    | checkType (_, UNIT, _) = UNIT
    | checkType (ty, reqty, pos) = (ErrorMsg.error pos ("type " ^ (toString ty) ^ " does not match required type " ^ (toString reqty)); IMPOSSIBILITY)


    fun closestDescendant (NIL, NIL) = NIL
      | closestDescendant (INT, INT) = INT
      | closestDescendant (STRING, STRING) = STRING
      | closestDescendant (UNIT, ty) = ty
      | closestDescendant (ty, UNIT) = ty
      | closestDescendant (NIL, RECORD(symlist, uniq)) = NIL
      | closestDescendant (RECORD(symlist, uniq), NIL) = NIL
      | closestDescendant (RECORD(symlist, uniq), RECORD(_, uniq')) = 
        if (uniq = uniq')
          then RECORD(symlist, uniq)
          else IMPOSSIBILITY
      | closestDescendant (ARRAY(ty, uniq), ARRAY(_, uniq')) = 
        if (uniq = uniq')
          then ARRAY(ty, uniq)
          else IMPOSSIBILITY
      | closestDescendant (_, _) = IMPOSSIBILITY


end

