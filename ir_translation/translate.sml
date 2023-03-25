signature TRANSLATE =
sig
  type exp
  type level
  type access (* not the same as Frame.access *)
  type frag 
  val outermost : level
  val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
  val formals: level -> access list
  val allocLocal: level -> bool -> access

  val simpleVar : access * level -> exp 

  val nop : unit -> exp

  val opExp : Absyn.oper * exp * exp * Types.ty -> exp
  val const : int -> exp
  val label : Temp.label -> exp
  val seq : exp list -> exp

  val assign : exp * exp -> exp

  val call : Temp.label * exp list * level * level -> exp

  val whileLoop : exp * exp * Temp.label -> exp
  val forLoop : exp * exp * exp * Temp.label * access -> exp
  val break : Temp.label -> exp

  val letExp : exp list * exp -> exp

  val subscriptVar : exp * exp -> exp
  val fieldVar : exp * int -> exp
  val arrayExp : exp * exp -> exp
  val recordExp : exp list -> exp
  val procEntryExit : {level: level, body: exp} -> unit
  val getResult : unit -> frag list 
  val rememberedFrags : frag list ref
  val stringVar : string -> exp
  val ifExp : exp * exp * exp -> exp

  val printAccess: access -> unit

end

structure Frame = MipsFrame
structure A = Absyn
structure T = Tree

structure Translate : TRANSLATE = struct 
  datatype exp = Ex of Tree.exp
               | Nx of Tree.stm
               | Cx of Temp.label * Temp.label -> Tree.stm
  
  datatype level = TOP | LEVEL of (level * Frame.frame) * unit ref
  type frag = Frame.frag
  type access = level * Frame.access
  val outermost: level = TOP

  fun printAccess (level, frameAccess) = MipsFrame.printAccess(frameAccess)


  fun rollupSeq ([]) = (print ("error: rollupSeq called with empty list.\n"); T.EXP(T.CONST(0)))
  |   rollupSeq (stm::[]) = stm
  |   rollupSeq (stm::stmlist) = T.SEQ(stm, rollupSeq(stmlist))

  fun newLevel {parent, name, formals} = LEVEL((parent, MipsFrame.newFrame
    {name=name, formals=(true::formals)}), ref ())


  fun formals (TOP) = []
  |   formals (LEVEL(level)) =
        let val (_, acclist, _, _) = (#2 (#1 level))
        in
          List.drop(List.map (fn (frameAccess) => (LEVEL(level), frameAccess)) acclist, 1)
        end

  fun allocLocal (TOP) escapes = (TOP, MipsFrame.allocR0())
    | allocLocal (LEVEL(lev)) escapes =
        let val ((_, frame), _) = lev
        in
          (LEVEL(lev), (MipsFrame.allocLocal frame escapes))
        end

  fun unEx (Ex e) = e
    | unEx (Cx genstm) =
        let val r = Temp.newtemp()
            val t = Temp.newlabel() and f = Temp.newlabel()
            in T.ESEQ(rollupSeq[T.MOVE(T.TEMP r, T.CONST 1),
                                genstm(t,f),
                                T.LABEL f,
                                T.MOVE(T.TEMP r, T.CONST 0),
                                T.LABEL t],
                                T.TEMP r)
            end
    | unEx (Nx s) = T.ESEQ(s,T.CONST 0)
  
  fun unCx (Ex (T.CONST 0)) = (fn (t,f) => T.JUMP(T.NAME f, [f]))
    | unCx (Ex (T.CONST 1)) = (fn (t,f) => T.JUMP(T.NAME t, [t]))
    | unCx (Ex e) = (fn (t,f) => T.CJUMP (T.NE, e, T.CONST 0, t, f))
    | unCx (Cx c) = c
    | unCx (Nx _) = (fn (t,f) => T.EXP(T.CONST(0))) (* this should not happen in
  valid Tiger program, but kept for error purposes*)

  fun unNx (Ex e) = Tree.EXP(e)
    | unNx (Cx c) = 
        let val l = Temp.newlabel()
        in
          T.SEQ(c(l,l), T.LABEL(l))
        end
    | unNx (Nx s) = s

  fun nop () = Nx(T.EXP(T.CONST(0)))

  fun staticLink (defLevel as LEVEL(_, defId), LEVEL((currParent, currFrame), currId)) =
        let fun checkLevel true = T.TEMP(MipsFrame.FP)
            |   checkLevel false = 
                  let val linkOp = staticLink(defLevel, currParent)
                  in
                    case linkOp of
                      SOME(link) => T.MEM(link)
                    | NONE       => (print("error: def level changed during static link computation\n"); T.CONST(0)) 
                  end
        in
          SOME(checkLevel((defId = currId)))
        end
  |   staticLink (TOP, _) = NONE
  |   staticLink (_, TOP) = (print ("error: current level became TOP during static link computation\n"); SOME(T.CONST(0)))

  fun simpleVar ((defLevel, frameAccess), useLevel) = 
    let val linkOp = staticLink (defLevel, useLevel)
    in
      case linkOp of
        SOME(link) => Ex(MipsFrame.exp frameAccess link)
      | NONE       => (print ("error: variable cannot be accessed in TOP level\n"); Ex(T.CONST(0)))
    end


    fun opExp (oper, left, right,ty) =
       let val tleft = unEx(left)
           val tright = unEx(right)
           fun trOp (A.PlusOp) = Ex(T.BINOP(T.PLUS, tleft, tright))
             |   trOp (A.MinusOp) = Ex(T.BINOP(T.MINUS,tleft, tright))
             |   trOp (A.TimesOp) = Ex(T.BINOP(T.MUL, tleft, tright))
             |   trOp (A.DivideOp) = Ex(T.BINOP(T.DIV, tleft,tright))
             |   trOp (A.EqOp) = Cx(fn (t,f) => T.CJUMP(T.EQ, tleft,tright, t, f))
             |   trOp (A.NeqOp) = Cx(fn (t,f) =>T.CJUMP(T.NE, tleft, tright, t, f))
             |   trOp (A.LtOp) = Cx(fn (t,f) => T.CJUMP(T.LT, tleft, tright,t, f))
             |   trOp (A.LeOp) = Cx(fn (t,f) => T.CJUMP(T.LE, tleft, tright,t, f))
             |   trOp (A.GtOp) = Cx(fn (t,f) => T.CJUMP(T.GT, tleft, tright, t,f))
             |   trOp (A.GeOp) = Cx(fn (t,f) => T.CJUMP(T.GE, tleft, tright, t, f))
           fun trOpStr (A.EqOp) = Ex(MipsFrame.externalCall("stringEqual",[tleft,tright]))
             |   trOpStr (A.LtOp) = Ex(MipsFrame.externalCall("stringLtOp", [tleft,tright]))
             |   trOpStr (A.LeOp) = Ex(MipsFrame.externalCall("stringLeOp",[tleft,tright]))
             |   trOpStr (A.GtOp) = Ex(MipsFrame.externalCall("stringGtOp",[tleft,tright]))
             |   trOpStr (A.GeOp) = Ex(MipsFrame.externalCall("stringGeOp",[tleft,tright]))            
             |   trOpStr (A.NeqOp) = Cx(fn (t,f) => T.CJUMP(T.NE, tleft, tright, t, f))
             |   trOpStr _ = (print ("error: should not reach STRING with any INT operand\n"); nop())
    in
      case ty of
           Types.INT => trOp oper
         | Types.STRING => trOpStr oper
         | _ => nop()
    end
         
         
         
   fun call (label, formalExps, defLevel, currLevel) = 
        let val linkOp = staticLink (defLevel, currLevel)
            val unwrappedFormals = List.map unEx formalExps
        in
          case linkOp of
            SOME(link) => Ex(T.CALL(T.NAME(label), link::unwrappedFormals))
          | NONE       => Ex(T.CALL(T.NAME(label), unwrappedFormals))
        end

  fun assign (varexp, valexp) = Nx(T.MOVE(unEx(varexp), unEx(valexp)))

  fun const const = Ex(T.CONST(const))

  fun label label = Nx(T.LABEL(label))

  fun seq [] = Nx(T.EXP(T.CONST(0)))
  |   seq (exp::[]) = exp
  |   seq (exps) = 
        let val stms = List.take (exps, (List.length exps)-1)
            val unwrappedExp = unEx(List.last exps)
            val unwrappedStms = List.map unNx stms
        in
          Ex(T.ESEQ(rollupSeq unwrappedStms, unwrappedExp))
        end

  fun break label = Nx(T.JUMP(T.NAME(label), [label]))

  fun whileLoop (condition, body, done) = 
    let val testLabel = Temp.newlabel()
        val bodyLabel = Temp.newlabel()
        val testCondition = (unCx(condition) (bodyLabel, done))
    in
      Nx(rollupSeq ([
        T.JUMP(T.NAME(testLabel), [testLabel]), 
        T.LABEL(bodyLabel), 
        unNx(body), 
        T.LABEL(testLabel), 
        testCondition,
        T.LABEL(done)]))
    end

  fun forLoop (loexp, hiexp, bodyexp, done, (_, varaccess)) =
    let val loval = unEx (loexp)
        val hival = unEx (hiexp)
        val varLoc = (Frame.exp varaccess (T.TEMP(Frame.FP)))
        val hiLoc = T.TEMP(Temp.newtemp())
        val l1 = Temp.newlabel()
        val l2 = Temp.newlabel()
    in
      Nx(rollupSeq[
        T.MOVE(varLoc, loval),
        T.MOVE(hiLoc, hival),
        T.CJUMP(T.LE, varLoc, hiLoc, l2, done),
        T.LABEL(l1),
        T.MOVE(varLoc, T.BINOP(T.PLUS, varLoc, T.CONST(1))),
        T.LABEL(l2),
        unNx(bodyexp),
        T.CJUMP(T.LT, varLoc, hiLoc, l1, done),
        T.LABEL(done)
      ])
    end


  fun letExp (decexps, bodyexp) =
      let val unwrappedStms = List.map unNx decexps
      in
        if (List.length unwrappedStms > 0)
          then Ex(T.ESEQ(rollupSeq unwrappedStms, unEx bodyexp))
          else bodyexp
      end


    val rememberedFrags = ref [] : Frame.frag list ref
    fun getResult () = 
      let val frags = !rememberedFrags
      in
        rememberedFrags := []; frags
      end

  
    fun procEntryExit {level=level, body=exp} =
      let fun applyViewShift (frame) = MipsFrame.procEntryExit1(unNx(exp), frame)
      in
        case level of
          LEVEL((parent, frame), _) => rememberedFrags := Frame.PROC({body=(applyViewShift(frame)), frame=frame})::(!rememberedFrags)
        | TOP => print("Error should not reach TOP level at procEntry")
      end

    fun stringVar lit = 
      let fun getLab () =
        case List.find 
            (fn(remfrag) => (case remfrag of 
                  Frame.PROC(_) => false
                | Frame.STRING(lab,lit') => String.compare(lit, lit') = EQUAL))
            (!rememberedFrags)
            of
            SOME(Frame.STRING(lab,lit')) => lab
            | SOME (_) => Temp.newlabel()
            | NONE => Temp.newlabel()
        in
          (rememberedFrags := Frame.STRING((getLab(),lit))::(!rememberedFrags);
          Ex(T.NAME(getLab ())))
        end
 
  fun ifExp (test, t', e) =
     let val join = Temp.newlabel()
          val t = Temp.newlabel()
          val f = Temp.newlabel()
          val r = Temp.newtemp()
          val cond = (unCx(test))(t, f)
          
     in
       Ex(T.ESEQ(
        rollupSeq ([
          cond,
          T.LABEL(t),
          T.MOVE(T.TEMP(r), unEx(t')),
          T.JUMP(T.NAME(join), [join]),
          T.LABEL(f),
          T.MOVE(T.TEMP(r), unEx(e)),
          T.LABEL(join)
        ]),
        T.TEMP(r)
       ))
     end
  

  fun arrayExp (arrsize, initVal) = 
    let val extraSize = T.BINOP(T.PLUS, unEx(arrsize), T.CONST(MipsFrame.wordSize))
        val r = Temp.newtemp()
        val mallocExp = MipsFrame.externalCall("initArray", [extraSize, unEx(initVal)])
    in 
        Ex(T.ESEQ(
          rollupSeq [
            T.MOVE(T.TEMP(r), mallocExp),
            T.MOVE(T.MEM(T.TEMP(r)), unEx(arrsize))
          ],
          T.BINOP(T.PLUS, T.TEMP(r), T.CONST(MipsFrame.wordSize))
        ))
    end

  fun subscriptVar (baseAddr, index) =
    let val ind = unEx(index)
        val size = T.MEM(T.BINOP(T.MINUS, unEx(baseAddr), T.CONST(MipsFrame.wordSize)))
        val lowerCheck = Temp.newlabel()
        val inboundLabel = Temp.newlabel()
        val outBoundLabel = Temp.newlabel()
     in
       Ex(T.ESEQ(
        rollupSeq ([
          T.CJUMP(T.LT, ind, size, lowerCheck, outBoundLabel),
          T.LABEL(lowerCheck),
          T.CJUMP(T.GE, ind, T.CONST(0), inboundLabel, outBoundLabel),
          
          T.LABEL(outBoundLabel),
          unNx(call(Temp.namedlabel "print", [stringVar ("Error array index out of bounds\n")], TOP, TOP)),
          unNx(call(Temp.namedlabel "exit", [Ex(T.CONST(1))], TOP, TOP)),

          T.LABEL(inboundLabel)
        ]),
        T.MEM(
          T.BINOP(T.PLUS, 
            unEx(baseAddr), 
            T.BINOP(T.MUL, 
              unEx(index),
              T.CONST(MipsFrame.wordSize) 
            )
          )
        )
       ))
     end


  fun recordExp (fields) =
    let val r = Temp.newtemp()
        val moveStms = List.foldr (fn (field, moveExps) => 
          T.MOVE(
            T.MEM(
              T.BINOP(T.PLUS, 
                T.TEMP(r), 
                T.CONST((List.length moveExps) * MipsFrame.wordSize)
              )
            ),
            unEx field
          ) :: moveExps) [] fields
        val mallocExp = T.MOVE(T.TEMP(r), MipsFrame.externalCall("malloc", [T.CONST((List.length fields) * MipsFrame.wordSize)]))
    in
      Ex(T.ESEQ(
        if (List.length moveStms > 0) then T.SEQ(mallocExp, rollupSeq (moveStms)) else mallocExp, 
        T.TEMP(r)
      ))
    end

  fun fieldVar (baseAddr, index) = 
    let val nullPointerLabel = Temp.newlabel()
        val validPointerLabel = Temp.newlabel()
    in
      Ex(T.ESEQ(
        rollupSeq ([
          T.CJUMP(T.EQ, unEx(baseAddr), T.CONST(0), nullPointerLabel, validPointerLabel),
          T.LABEL(nullPointerLabel),
          unNx(call(Temp.namedlabel "print", [stringVar ("Error cannot derefernece null pointer\n")], TOP, TOP)),
          unNx(call(Temp.namedlabel "exit", [Ex(T.CONST(1))], TOP, TOP)),
          T.LABEL(validPointerLabel)
        ]),
        T.MEM(
          T.BINOP(T.PLUS,
            unEx(baseAddr), 
            T.CONST(index * MipsFrame.wordSize)
          )
        )
      ))
    end
    

end
