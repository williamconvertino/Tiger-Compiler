signature SYMBOL =
sig
  eqtype symbol
  val symbol : string -> symbol
  val name : symbol -> string
  
  type 'a table
  val empty : 'a table
  val enter : 'a table * symbol * 'a -> 'a table
  val look  : 'a table * symbol -> 'a option
  val eq    : symbol * symbol -> bool
end

structure Symbol :> SYMBOL =
struct

  type symbol = string * int

  structure H = HashTable

  exception Symbol
  val nextsym = ref 0
  val sizeHint = 128
  val hashtable : (string,int) H.hash_table = 
		H.mkTable(HashString.hashString, op = ) (sizeHint, Symbol)
  
  fun symbol name =
      case H.find hashtable name
       of SOME i => (name,i)
        | NONE => let val i = !nextsym
	           in nextsym := i+1;
		      H.insert hashtable (name,i);
		      (name,i)
		  end

  fun eq ((_, id1), (_, id2)) = if id1 = id2 then true else false

  fun name(s,n) = s

  type 'a table = 'a IntRedBlackMap.map
  
  val empty = IntRedBlackMap.empty
  fun enter(t: 'a table, (s,n): symbol, a: 'a) = IntRedBlackMap.insert(t,n,a)
  fun look(t: 'a table, (s,n): symbol) = IntRedBlackMap.find(t,n)
end
