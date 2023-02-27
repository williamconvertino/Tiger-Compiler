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

  (* 
    unfinished type to string for debugging help
  fun str ty = 
    let fun unpack_record ([], recstr)      = recstr |
        fun unpack_record (sym::l, recstr)  = 
  
  case ty of
    NIL             => "nil" |
    INT             => "int" |
    STRING          => "string" |
    UNIT            =>  "unit"  |
    IMPOSSIBILITY   => "impossibility" |
    ARRAY(ty, uniq) =>  str ty  |
    NAME(sym, tyop) =>  (case !tyop of
      NONE => "impossibility" |
      SOME(ty)  => str ty )
    RECORD(syms, uniq) => (
      
    ) *)

end

