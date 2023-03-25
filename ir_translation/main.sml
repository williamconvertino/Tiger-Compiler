structure Main : sig 
    val compile : string -> Translate.frag list 
    val escape  : string -> unit
    val debugCompile : string -> unit
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

    fun debugCompile filename = 
        let val frags = compile (filename)
            fun printFrags ([]) = ()
            |   printFrags (MipsFrame.PROC({body, frame})::frags) = 
                    let val (label, _, _, _) = frame
                    in
                        print ("--- FUNC [" ^ Symbol.name label ^ "] ---\n");
                        Printtree.printtree (TextIO.stdOut, body);
                        print ("--- ---\n");
                        printFrags(frags)
                    end
            |   printFrags (MipsFrame.STRING((label, str))::frags) = (
                    print ("--- STRING [" ^ Symbol.name label ^ "=" ^ str ^ "] ---\n");
                    printFrags(frags)
                )
        in
            printFrags (List.rev (frags))
        end

end
