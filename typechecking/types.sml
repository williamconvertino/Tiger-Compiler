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
    let fun unpack_record ([], recstr) = recstr ^ "}" |
            unpack_record ((sym, ty)::[], recstr)  = unpack_record([], recstr ^ (Symbol.name sym) ^ ":" ^ (toString ty)) |
            unpack_record ((sym, ty)::l, recstr)  = unpack_record(l, recstr ^ (Symbol.name sym) ^ ":" ^ (toString ty) ^ ", ")
    in
      case ty of
        NIL             => "nil" |
        INT             => "int" |
        STRING          => "string" |
        UNIT            =>  "unit"  |
        IMPOSSIBILITY   => "impossibility" |
        ARRAY(ty, uniq) =>  toString ty  |
        NAME(sym, tyop)   =>  (case !tyop of
          NONE => (print("ERROR: type ref of " ^ Symbol.name sym ^ " not properly set by compiler\n") ;"impossibility") |
          SOME(ty)  => Symbol.name sym ^ ":" ^ toString ty ) |
        RECORD(syms, uniq) => unpack_record (syms, "{")
    end

  fun checkType (RECORD(symlist, uniq), RECORD(_, uniq'), pos) = 
      if (uniq = uniq')
        then RECORD(symlist, uniq)
        else (ErrorMsg.error pos "record types do not match"; NIL) 
    | checkType (NIL, RECORD(symlist, uniq), _) = RECORD(symlist, uniq)
    | checkType (INT, INT, _) = INT
    | checkType (IMPOSSIBILITY, INT, _) = INT
    | checkType (STRING, STRING, _) = STRING
    | checkType (IMPOSSIBILITY, STRING, _) = STRING
    | checkType (ARRAY(arrty, uniq), ARRAY(_, uniq'), pos) = 
      if (uniq = uniq')
        then ARRAY(arrty, uniq)
        else (ErrorMsg.error pos "array types do not match"; IMPOSSIBILITY) 
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

