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

  val seq : Tree.stm list -> Tree.stm
    
  (* val procEntryExit : {level: level, body: exp} -> unit

  val getResult : unit -> Frame.frag list *)
end

structure Frame = MipsFrame
structure T = Tree

structure Translate : TRANSLATE = struct 
  datatype exp = Ex of Tree.exp
               | Nx of Tree.stm
               | Cx of Temp.label * Temp.label -> Tree.stm
  
  datatype level = TOP | LEVEL of (level * Frame.frame) * unit ref
  type access = level * Frame.access
  val outermost: level = TOP

  fun seq (stm::[]) = stm
  |   seq (stm::stmlist) = T.SEQ(stm, seq(stmlist))

  fun newLevel {parent, name, formals} = LEVEL((parent, MipsFrame.newFrame
    {name=name, formals=(true::formals)}), ref ())


  fun formals (TOP) = []
    |   formals (LEVEL(level)) =
    let val (_, acclist, _, _) = (#2 (#1 level))
    in
      List.map (fn (frameAccess) => (LEVEL(level), frameAccess)) acclist
    end

  fun allocLocal (TOP) escapes = (TOP, MipsFrame.allocR0())
    |   allocLocal (LEVEL(lev)) escapes =
    let val ((_, frame), _) = lev
    in
      (LEVEL(lev), (MipsFrame.allocLocal frame escapes))
    end

  fun unEx (Ex e) = e
    | unEx (Cx genstm) =
        let val r = Temp.newtemp()
            val t = Temp.newlabel() and f = Temp.newlabel()
            in T.ESEQ(seq[T.MOVE(T.TEMP r, T.CONST 1),
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

  fun unNx (Ex e) = Tree.EXP(e)
    | unNx (Cx c) = T.EXP((unEx(Cx(c))))
    | unNx (Nx s) = s

  fun staticLink deflev uselev e a = T.MEM (T.BINOP (T.PLUS, e, a))
    (*MISSING dealing with static link*)
  fun simpleVar ((deflev, frameaccess), uselev) = 
        Ex(MipsFrame.exp frameaccess (staticLink deflev uselev (T.TEMP(MipsFrame.FP)) frameaccess))
    (*MISSING dealing with static link*)


end
