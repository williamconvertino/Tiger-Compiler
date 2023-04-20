structure Main : sig
    val compile : string -> unit
    val escape  : string -> unit
    val debugCompile : string -> unit
end =

struct 
    structure Tr = Translate
    structure F = Frame
    (*structure M = MipsFrame*)
    (*structure R = RegAlloc*)

    fun escape filename =
        let val absyn = Parse.parse filename
        in
            FindEscape.findEscape absyn;
            PrintAbsyn.print (TextIO.stdOut, absyn)
        end

    fun emitproc out (F.PROC{body, frame}) =
      let val _ = print ("emit " ^ (Symbol.name (Frame.name frame)) ^  "\n")
(*        val _ = Printtree.printtree(out,body); *)
          val stms = Canon.linearize body
(*        val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
          val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
          val instrs = List.concat(List.map (MipsGen.codegen frame) stms')
          val proc2 = MipsFrame.procEntryExit2(frame, instrs)

          (* Register allocation*)
          val coloredInstrs = RegisterAllocator.allocate(proc2)


          val {prolog, body=newbody, epilog} = MipsFrame.procEntryExit3(frame, coloredInstrs)
          val format0 = Assem.format(MipsFrame.getRegisterName)
      in 
        TextIO.output(out, prolog);
        List.app (fn i => TextIO.output(out, format0 i)) newbody; 
         TextIO.output(out, epilog);
        () 
     end
    | emitproc out (F.STRING(lab, s)) = TextIO.output(out, F.string(lab,s))

   fun withOpenFile fname f =
       let val out = TextIO.openOut fname
        in (f out before TextIO.closeOut out)
            handle e => (TextIO.closeOut out; raise e)
       end
    
    fun compile filename = 
        let val _ = Temp.reset ()
            val absyn = Parse.parse filename
            val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
        in             
            (* List.app (emitproc TextIO.stdOut) frags;
            frags *)
            withOpenFile (filename ^ ".s") (fn out => (List.app (emitproc out) frags))  
        end

    fun debugCompile filename = 
        let val absyn = Parse.parse filename
            val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
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
            List.app (emitproc TextIO.stdOut) frags;
            printFrags (List.rev (frags))
        end

end

