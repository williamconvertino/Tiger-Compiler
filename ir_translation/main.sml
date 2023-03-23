structure Main : sig 
    val compile : string -> unit 
    val escape  : string -> unit
end =
struct 

    fun escape filename =
        let val absyn = Parse.parse filename
        in
            FindEscape.findEscape absyn;
            PrintAbsyn.print (TextIO.stdOut, absyn)
        end


    fun compile filename = 
        let val absyn = Parse.parse filename
        in 
            FindEscape.findEscape absyn;
            Semant.transProg absyn
        end

end