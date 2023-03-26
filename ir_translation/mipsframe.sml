signature FRAME =
sig 
  type access
  type frame = Temp.label * access list * int ref * Tree.exp list
  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
  val newFrame : {name: Temp.label, formals: bool list} -> frame
  val name : frame -> Temp.label
  val formals : frame -> access list
  val allocLocal : frame -> bool -> access
  val allocR0 : unit -> access

  val SP : Temp.temp
  val FP : Temp.temp
  val wordSize: int
  val exp : access -> Tree.exp -> Tree.exp
  val externalCall: string * Tree.exp list -> Tree.exp
  val procEntryExit1 :  Tree.stm * frame -> Tree.stm

  val printAccess: access -> unit

  val RV : Temp.temp
end

structure MipsFrame : FRAME = struct 
    datatype access = InFrame of int | InReg of Temp.temp

    structure T = Tree
    

    type frame = Temp.label * access list * int ref * Tree.exp list
     datatype frag = PROC of {body: Tree.stm, frame: frame}
                                  | STRING of Temp.label * string

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
    

    fun printAccess (InFrame(addr)) = print("in frame addr: " ^ Int.toString(addr) ^ "\n")
    |   printAccess (InReg(temp)) = print("in reg: " ^ Int.toString(temp) ^ "\n")

    fun allocLocal (_, _, locals, _) true = (locals := !locals + 1 ; InFrame (!locals * ~4))
    |   allocLocal _ false                = InReg (Temp.newtemp())

    fun allocR0 () = InReg(0)
    val RV = 2
    val SP = 29
    val FP = 30
    val wordSize = 4
    val calleeSavedRegs = [16, 17, 18, 19, 20, 21, 22, 23, 29]

    fun exp (InFrame(frameOffset)) frameAddr = T.MEM(T.BINOP(T.PLUS, frameAddr, T.CONST(frameOffset)))
    |   exp (InReg(r)) _ = T.TEMP(r)

    fun externalCall (s, args) = T.CALL(T.NAME(Temp.namedlabel s), args)

    fun rollupSeq (stm::[]) = stm
    |   rollupSeq (stm::stmlist) = T.SEQ(stm, rollupSeq(stmlist))
    |   rollupSeq ([]) = T.EXP(T.CONST(0))

    fun procEntryExit1(body, frame) = 
      let val (label, formals, numLocals, _) = frame
          fun moveInRegForms (formals, 4) = []
          |   moveInRegForms (InReg(temp)::formals, regCount) = T.MOVE(T.TEMP(temp), T.TEMP(4 + regCount)) :: moveInRegForms(formals, regCount + 1)
          |   moveInRegForms (InFrame(_)::formals, regCount) = moveInRegForms(formals, regCount)
          |   moveInRegForms ([], regCount) = []
          
          fun moveCalleeSaved ([]) = (
                [T.MOVE(T.TEMP(SP), T.BINOP(T.PLUS, T.TEMP(SP), T.CONST(List.length(calleeSavedRegs) * wordSize)))],
                [T.MOVE(T.TEMP(SP), T.BINOP(T.MINUS, T.TEMP(SP), T.CONST(List.length(calleeSavedRegs) * wordSize)))] 
              )
          |   moveCalleeSaved (reg::regs) = 
                let val (stores, loads) = moveCalleeSaved(regs)
                    val stackAddr = exp (allocLocal frame true) (T.TEMP(SP))
                in
                (T.MOVE(stackAddr, T.TEMP(reg))::stores, T.MOVE(T.TEMP(reg), stackAddr)::loads)
                end
          val (loads, stores) = moveCalleeSaved(calleeSavedRegs)
      in
        rollupSeq ([
          rollupSeq (moveInRegForms (formals, 0)),
          rollupSeq (loads),
          body,
          rollupSeq (List.rev stores)
        ])
      end

   fun procEntryExitl(body,frame) = body

end

