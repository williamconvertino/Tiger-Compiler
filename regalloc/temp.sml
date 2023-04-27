structure Temp : TEMP =
struct
    type temp = int
    val labelCount = ref 0
    val temps = ref 100
    val labelReset = ref 0
    val tempReset = ref 100
    fun start() =
        let val () = labelReset := !labelCount
            val () = tempReset := !temps
        in
            ()
        end
                        
    fun reset () = 
	let val () = temps := !tempReset
	    val () = labelCount := !labelReset
	in
	    ()
	end

    fun newtemp() = 
	let val t  = !temps 
	    val () = temps := t+1
	in 
	    t
	end
    fun makestring t = "temp" ^ Int.toString t
		       
    type label = Symbol.symbol
    val compare = Int.compare
    
    structure TempOrd =
    struct 
      type ord_key = Int.int
      val compare = Int.compare
    end

    structure Set = SplaySetFn(TempOrd)
    structure Map = SplayMapFn(TempOrd)
			 
    fun newlabel() = 
	let 
	    val x  = !labelCount
	    val _ = labelCount := x + 1
	in
	    Symbol.symbol ("L" ^ Int.toString x)
	end

    val namedlabel = Symbol.symbol


end
