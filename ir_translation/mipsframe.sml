signature FRAME =
sig 
  type access
  type frame = Temp.label * access list * int ref * Tree.exp list
  val newFrame : {name: Temp.label, formals: bool list} -> frame
  val name : frame -> Temp.label
  val formals : frame -> access list
  val allocLocal : frame -> bool -> access
  val allocR0 : unit -> access

  val FP : Temp.temp
  val wordSize: int
  val exp : access -> Tree.exp -> Tree.exp
  val externalCall: string * Tree.exp list -> Tree.exp
  val procEntryExit1 : frame * Tree.stm -> Tree.stm

  (* val RV : Temp.temp
  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string *)
end

structure MipsFrame : FRAME = struct 
    datatype access = InFrame of int | InReg of Temp.temp

    structure T = Tree

    type frame = Temp.label * access list * int ref * Tree.exp list
    

    fun newFrame {name, formals} =
      let fun setupFormals (true::formalescs, esccount, regcount)   = (InFrame (esccount * 4)) :: (setupFormals (formalescs, esccount+1, regcount))
          |   setupFormals (false::formalescs, esccount, 4)         = (InFrame (esccount * 4)) :: (setupFormals (formalescs, esccount+1, 4))
          |   setupFormals (false::formalescs, esccount, regcount)  = (InReg (Temp.newtemp())) :: (setupFormals (formalescs, esccount, regcount+1))
          |   setupFormals ([], _, _)                               = []

          val formals = setupFormals (formals, 0, 0)
      in 
        (name, formals, ref 0, [])
      end

    fun name (name, _, _, _) = name
    fun formals (_, formals, _, _) = formals
    
    fun allocLocal (_, _, locals, _) true = (locals := !locals + 1 ; InFrame (!locals * ~4))
    |   allocLocal _ false                = InReg (Temp.newtemp())

    fun allocR0 () = InReg(0)
    val FP = 30
    val wordSize = 4

    fun exp (InFrame(frameOffset)) frameAddr = T.MEM(T.BINOP(T.PLUS, frameAddr, T.CONST(frameOffset)))
    |   exp (InReg(r)) _ = T.TEMP(r)

    fun externalCall (s, args) = T.CALL(T.NAME(Temp.namedlabel s), args)

    fun procEntryExit1(frame,body) = body


end

