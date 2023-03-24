signature TRANSLATE =
sig
  type exp
  type level
  type access (* not the same as Frame.access *)
  
  val outermost : level
  val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
  val formals: level -> access list
  val allocLocal: level -> bool -> access
  
  val simpleVar : access * level -> exp 

  val nop : unit -> exp

  val opExp : Absyn.oper * exp * exp -> exp
  val const : int -> exp
  val label : Temp.label -> exp
  val seq : exp list -> exp

  val whileLoop : exp * exp * Temp.label -> exp
  val forLoop : exp * exp * exp * Temp.label * access -> exp
  val break : Temp.label -> exp

  val subscriptVar : exp * exp -> exp
  val fieldVar : exp * int -> exp
  val arrayExp : exp * exp -> exp
  val recordExp : exp list -> exp
    
  (* val procEntryExit : {level: level, body: exp} -> unit

  val getResult : unit -> Frame.frag list *)
end

structure Frame = MipsFrame
structure A = Absyn
structure T = Tree

structure Translate : TRANSLATE = struct 
  datatype exp = Ex of Tree.exp
               | Nx of Tree.stm
               | Cx of Temp.label * Temp.label -> Tree.stm
  
  datatype level = TOP | LEVEL of (level * Frame.frame) * unit ref
  type access = level * Frame.access
  val outermost: level = TOP

  fun rollupSeq (stm::[]) = stm
  |   rollupSeq (stm::stmlist) = T.SEQ(stm, rollupSeq(stmlist))

  fun newLevel {parent, name, formals} = LEVEL((parent, MipsFrame.newFrame
    {name=name, formals=(true::formals)}), ref ())


  fun formals (TOP) = []
    |   formals (LEVEL(level)) =
    let val (_, acclist, _, _) = (#2 (#1 level))
    in
      List.map (fn (frameAccess) => (LEVEL(level), frameAccess)) acclist
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
    | unCx (Nx _) = (print "error cannot unwrap Cx from Nx"; (fn (t,f) => T.LABEL(Temp.newlabel())))

  fun unNx (Ex e) = Tree.EXP(e)
    | unNx (Cx c) = 
        let val l = Temp.newlabel()
        in
          T.SEQ(c(l,l), T.LABEL(l))
        end
    | unNx (Nx s) = s

  (* fun staticLink (deflev as LEVEL((pdef, fdef), rdef)) (uselev as
    LEVEL((puse,fuse),ruse)) e a =
           if rdef = ruse then e
             else (MipsFrame.exp a (staticLink deflev puse e a)) *)

  fun staticLink (defLevel as LEVEL(_, defId), LEVEL((currParent, currFrame), currId)) =
        if defId = currId then T.TEMP(MipsFrame.FP) else T.MEM(staticLink(defLevel, currParent))
  |   staticLink (_, _) = (print("error: cannot static link into the TOP level"); T.TEMP(MipsFrame.FP))

  fun simpleVar ((defLevel, frameAccess), useLevel) = Ex(MipsFrame.exp frameAccess (staticLink (defLevel, useLevel)))

  fun subscriptVar (baseAddr, index) = Ex(T.MEM(T.BINOP(T.PLUS,
    T.MEM(unEx(baseAddr)), T.BINOP(T.MUL, unEx(index), T.CONST( MipsFrame.wordSize) ))))

  fun fieldVar (baseAddr, ind) = Ex(T.MEM(T.BINOP(T.PLUS,
        T.MEM(unEx(baseAddr)), T.BINOP(T.MUL, T.CONST(ind), T.CONST(
        MipsFrame.wordSize) ))))

  fun arrayExp (arrLen, initVal) = Ex(MipsFrame.externalCall("initArray",
    [unEx(arrLen), unEx(initVal)]))
  
  fun recordExp (fields) =  
   let val r = Temp.newtemp()  
     fun seq ([a], i) = T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP(r), T.CONST( i *
                 MipsFrame.wordSize))), unEx a)
       | seq (a::l, i) =  T.SEQ( T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP(r),
       T.CONST( i * MipsFrame.wordSize))), unEx a), seq (l, i+1))
       | seq ([],_) = T.EXP(T.CONST 0)
      in
        Ex(T.ESEQ(T.SEQ(T.MOVE(T.TEMP (r),MipsFrame.externalCall("initRecord",[T.CONST (List.length fields)])), seq
        (fields, 0)), T.TEMP(r)))
       end

  fun opExp (oper, left, right) =
    let val tleft = unEx(left)
        val tright = unEx(right)
        fun trOp (A.PlusOp) = Ex(T.BINOP(T.PLUS, tleft, tright))
        |   trOp (A.MinusOp) = Ex(T.BINOP(T.MINUS, tleft, tright))
        |   trOp (A.TimesOp) = Ex(T.BINOP(T.MUL, tleft, tright))
        |   trOp (A.DivideOp) = Ex(T.BINOP(T.DIV, tleft, tright))
        |   trOp (A.EqOp) = Cx(fn (t,f) => T.CJUMP(T.EQ, tleft, tright, t, f))
        |   trOp (A.NeqOp) = Cx(fn (t,f) => T.CJUMP(T.NE, tleft, tright, t, f))
        |   trOp (A.LtOp) = Cx(fn (t,f) => T.CJUMP(T.LT, tleft, tright, t, f))
        |   trOp (A.LeOp) = Cx(fn (t,f) => T.CJUMP(T.LE, tleft, tright, t, f))
        |   trOp (A.GtOp) = Cx(fn (t,f) => T.CJUMP(T.GT, tleft, tright, t, f))
        |   trOp (A.GeOp) = Cx(fn (t,f) => T.CJUMP(T.GE, tleft, tright, t, f))
    in
      trOp oper
    end


  fun nop () = Nx(T.EXP(T.CONST(0)))

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
end
