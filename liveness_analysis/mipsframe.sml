signature FRAME =
sig 
  type access
  type register
  type frame = Temp.label * access list * int ref * Tree.exp list
  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
  val newFrame : {name: Temp.label, formals: bool list} -> frame
  val string : Temp.label * string -> string
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
  val tempMap : register Temp.Table.table
  val getRegisterName : Temp.temp -> string
  val wordSize: int
  val exp : access -> Tree.exp -> Tree.exp
  val externalCall: string * Tree.exp list -> Tree.exp
  val procEntryExit1 :  Tree.exp * frame -> Tree.stm
  val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
  val procEntryExit3 : frame * Assem.instr list -> {prolog:string, body:Assem.instr list, epilog: string}
  val printAccess: access -> unit

end

structure MipsFrame : FRAME = struct 
    datatype access = InFrame of int | InReg of Temp.temp

    structure T = Tree
    structure A = Assem
    

    val zero = 0
    val RV = 2
    val v1 = 3
    val a0 = 4
    val a1 = 5
    val a2 = 6
    val a3 = 7
    val t0 = 8
    val t1 = 9
    val t2 = 10
    val t3 = 11
    val t4 = 12
    val t5 = 13
    val t6 = 14
    val t7 = 15
    val s0 = 16
    val s1 = 17
    val s2 = 18
    val s3 = 19
    val s4 = 20
    val s5 = 21
    val s6 = 22
    val s7 = 23
    val t8 = 24
    val t9 = 25
    val SP = 29
    val FP = 30
    val ra = 31

    type register = string
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
          |   moveInRegForms (InFrame(_)::formals, regCount) = (
                T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP(FP), T.CONST(regCount * wordSize))), T.TEMP(4 + regCount)) :: moveInRegForms(formals, regCount + 1)
          )
          |   moveInRegForms ([], regCount) = []
          
          fun moveCalleeSaved ([]) = (
                [T.MOVE(T.TEMP(SP), T.BINOP(T.MINUS, T.TEMP(SP), T.CONST(List.length(calleeSavedRegs) * wordSize)))],
                [T.MOVE(T.TEMP(SP), T.BINOP(T.PLUS, T.TEMP(SP), T.CONST(List.length(calleeSavedRegs) * wordSize)))] 
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
          (* rollupSeq (loads), *)
          T.MOVE(T.TEMP(RV), body)
          (* rollupSeq (List.rev stores) *)
        ])
      end


  val callersaves = [t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,RV, v1, ra]
  val callersavesstr = ["t0","t1","t2","t3","t4","t5","t6","t7","t8","t9", "v0", "v1", "ra"]
  val calleesaves = [s0,s1,s2,s3,s4,s5,s6,s7]
  val calleesavesstr = ["s0","s1","s2","s3","s4","s5","s6","s7"]
  val specialregs = [zero, ra, SP, RV]
  val specialregsstr = ["zero", "ra", "SP", "RV"]
  val argregs = [a0, a1, a2, a3]
  val argregsstr = ["a0","a1","a2","a3"]
  
  fun string (label, str) = Symbol.name (label) ^ ": .asciiz \"" ^ str ^ "\"\n"

  fun procEntryExit2(frame, body) =
     body @
     [A.OPER{assem="", dst=[],
     src =[zero, ra, SP] @ calleesaves,
     jump=SOME[]}]


  fun procEntryExit3(frame, body) =
    {prolog = (Symbol.name (name frame) ^ ":\n"),
          body = body,
          epilog = ""}
          
  val allRegStrList = callersavesstr@calleesavesstr@specialregsstr@argregsstr
  val allRegList = callersaves@calleesaves@specialregs@argregs
  val tempMap = Array.foldri 
  (fn(ind,t,table) => Temp.Table.enter(table,t,List.nth(allRegStrList,ind))) Temp.Table.empty (Array.fromList allRegList)

  fun getRegisterName t = case Temp.Table.look (tempMap,t) of
                    SOME (regstr) => regstr 
                    | NONE => Temp.makestring t

end

