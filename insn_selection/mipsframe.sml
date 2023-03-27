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
  (*Special Regs*)
  val SP : Temp.temp
  val FP : Temp.temp
  val RV : Temp.temp
  val ra : Temp.temp
  val zero : Temp.temp
  val specialregs : Temp.temp list
  (*Argument Registers*)
  val a0 : Temp.temp
  val a1 : Temp.temp
  val a2 : Temp.temp
  val a3 : Temp.temp
  val argregs : Temp.temp list
  (*Callee Save Registers*)
  val s0 : Temp.temp
  val s1 : Temp.temp
  val s2 : Temp.temp
  val s3 : Temp.temp
  val s4 : Temp.temp
  val s5 : Temp.temp
  val s6 : Temp.temp
  val s7 : Temp.temp
  val calleesaves : Temp.temp list
  (*Caller Saved*)
  val t0 : Temp.temp
  val t1 : Temp.temp
  val t2 : Temp.temp
  val t3 : Temp.temp
  val t4 : Temp.temp
  val t5 : Temp.temp
  val t6 : Temp.temp
  val t7 : Temp.temp
  val t8 : Temp.temp
  val t9 : Temp.temp
  val callersaves : Temp.temp list
  (*Return Values*)
  (*val v0 : Temp.temp
  val v1 : Temp.temp*)

  val wordSize: int
  val exp : access -> Tree.exp -> Tree.exp
  val externalCall: string * Tree.exp list -> Tree.exp
  val procEntryExit1 :  Tree.stm * frame -> Tree.stm
  val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
  val printAccess: access -> unit

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
   
  val ra = Temp.newtemp()
  val zero = Temp.newtemp()
  val a0 = Temp.newtemp()
  val a1 = Temp.newtemp()
  val a2 = Temp.newtemp()
  val a3 = Temp.newtemp()
  val s0 = Temp.newtemp()
  val s1 = Temp.newtemp()
  val s2 = Temp.newtemp()
  val s3 = Temp.newtemp()
  val s4 = Temp.newtemp()
  val s5 = Temp.newtemp()
  val s6 = Temp.newtemp()
  val s7 = Temp.newtemp()
  val t0 = Temp.newtemp()
  val t1 = Temp.newtemp()
  val t2 = Temp.newtemp()
  val t3 = Temp.newtemp()
  val t4 = Temp.newtemp()
  val t5 = Temp.newtemp()
  val t6 = Temp.newtemp()
  val t7 = Temp.newtemp()
  val t8 = Temp.newtemp()
  val t9 = Temp.newtemp()

  val callersaves = [t0,t1,t2,t3,t4,t5,t6,t7,t8,t9]
  val calleesaves = [s0,s1,s2,s3,s4,s5,s6,s7]
  val specialregs = [zero,ra,SP,RV]
  val argregs = [a0,a1,a2,a3]
  
   fun procEntryExit2(frame, body) = body
(*   fun procEntryExit2(frame, body) =
     body @
     [A.OPER{assem="",dst=[],
     src =[zero,ra,SP]@calleeSavedRegs, (*calleesaves,*)
     jump=SOME[]}]*)

end

