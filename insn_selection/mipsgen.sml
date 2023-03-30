signature CODEGEN =
    sig
        
        structure Frame : FRAME
        val codegen : Frame.frame -> Tree.stm -> Assem.instr list

    end

structure MipsGen : CODEGEN =
    struct
        
        structure Frame : FRAME = MipsFrame
        
        structure T = Tree

        structure A = Assem

        fun codegen frame (stm: Tree.stm) : Assem.instr list =
            let
                val ilist = ref (nil: A.instr list)
                
                fun emit x = ilist := x :: !ilist
                fun result(gen) = let val t = Temp.newtemp() in gen t; t end


                fun checkImmed (i) =
                    let fun loadLargeImmed () =
                            let open Word
                                infix andb >>
                                val word = Word.fromInt i
                                val upper = Word.toInt (word >> (Word.fromInt 16))
                                val lower = Word.toInt (word andb (Word.fromInt 65536))
                                val bigI = Temp.newtemp()
                            in
                                emit(A.OPER {
                                    assem="lui 'd0, " ^ Int.toString upper ^ "\n", 
                                    src=[], dst=[bigI], jump=NONE
                                });
                                emit(A.OPER {
                                    assem="ori 'd0, 's0, " ^ Int.toString lower ^ "\n", 
                                    src =[bigI], dst=[bigI], jump=NONE
                                });
                                SOME(bigI)
                            end
                    in
                        if (i < 65536 andalso i > ~65537) 
                        then NONE
                        else loadLargeImmed ()
                    end
                    

                fun munchArgs (i,args) = 
                let val arg = List.nth(Frame.argregs, i) 
                    fun temp_move ()= munchStm(T.MOVE(T.TEMP(arg),List.nth(args,i) ))
                in
                    temp_move(); arg::munchArgs (i+1,args)
                end

                and munchExp(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i))) = 
                    let val check = checkImmed(i)
                    in
                        case check of
                            SOME(r) => munchExp(T.MEM(T.TEMP(r)))
                        |   NONE    => result(fn r => emit(A.OPER 
                            {assem="lw 'd0, " ^ Int.toString i ^ "('s0)\n", 
                            src =[munchExp e1], dst=[r], jump=NONE})) 
                    end
                | munchExp(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1))) =
                    let val check = checkImmed(i)
                    in
                        case check of
                            SOME(r) => munchExp(T.MEM(T.TEMP(r)))
                        |   NONE    => result(fn r => emit(A.OPER 
                            {assem="lw 'd0, " ^ Int.toString i ^ "('s0)\n", 
                            src =[munchExp e1], dst=[r], jump=NONE})) 
                    end
                | munchExp(T.MEM(T.CONST i)) =
                    let val check = checkImmed(i)
                    in
                        case check of
                            SOME(r) => munchExp(T.MEM(T.TEMP(r)))
                        |   NONE    => result(fn r => emit(A.OPER 
                        {assem="lw 'd0, " ^ Int.toString i ^ "($r0)\n", 
                        src=[], dst=[r], jump=NONE}))
                    end
                | munchExp(T.MEM(e1)) =
                    result(fn r => emit(A.OPER 
                        {assem="lw 'd0, 0('s0)\n", 
                        src=[munchExp e1], dst=[r], jump=NONE}))
                | munchExp(T.BINOP(T.PLUS, e1, T.CONST i)) =
                    let val check = checkImmed(i)
                    in
                        case check of
                            SOME(r) => munchExp(T.BINOP(T.PLUS, e1, T.TEMP(r)))
                        |   NONE    => result(fn r => emit(A.OPER
                        {assem="addi 'd0, 's0, " ^ Int.toString i ^ "\n",
                        src=[munchExp e1], dst=[r], jump=NONE}))
                    end
                | munchExp(T.BINOP(T.PLUS, T.CONST i, e1)) = 
                    let val check = checkImmed(i)
                    in
                        case check of
                            SOME(r) => munchExp(T.BINOP(T.PLUS, e1, T.TEMP(r)))
                        |   NONE    => result(fn r => emit(A.OPER
                        {assem="addi 'd0, 's0, " ^ Int.toString i ^ "\n",
                        src=[munchExp e1], dst=[r], jump=NONE}))
                    end
                | munchExp(T.CONST 0) = 0
                | munchExp(T.CONST i) =
                    let val check = checkImmed(i)
                    in
                        case check of
                            SOME(r) => r
                        |   NONE    => result(fn r => emit(A.OPER
                        {assem="addi 'd0, $r0, " ^ Int.toString i ^ "\n", 
                        src=[], dst=[r], jump=NONE}))
                    end
                | munchExp(T.BINOP(T.PLUS, e1, e2)) =
                    result(fn r => emit(A.OPER 
                        {assem="add 'd0, 's0, 's1\n", 
                        src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))                
                | munchExp(T.BINOP(T.MINUS, e1, T.CONST i)) =
                    let val check = checkImmed(i)
                    in
                        case check of
                            SOME(r) => munchExp(T.BINOP(T.MINUS, e1, T.TEMP(r)))
                        |   NONE    => result(fn r => (
                                if (i = ~65536) then (
                                    emit(A.OPER 
                                    {assem="addi 'd0, 's0, " ^ Int.toString (~1 * (i+10)) ^ "\n",    
                                    src=[munchExp e1], dst=[r], jump=NONE});
                                    emit(A.OPER 
                                    {assem="addi 'd0, 's0, " ^ Int.toString (~10) ^ "\n",    
                                    src=[r], dst=[r], jump=NONE})
                                )
                                else emit(A.OPER 
                                    {assem="addi 'd0, 's0, " ^ Int.toString (~1 * i) ^ "\n",    
                                    src=[munchExp e1], dst=[r], jump=NONE})   
                        ))
                    end
                    
                | munchExp(T.BINOP(T.MINUS, e1, e2)) =
                    result(fn r => emit(A.OPER 
                        {assem="sub 'd0, 's0, 's1\n", 
                        src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
                | munchExp(T.BINOP(T.MUL, e1, e2)) =
                    result(fn r => emit(A.OPER 
                        {assem="mul 'd0, 's0, 's1\n", 
                        src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
                | munchExp(T.BINOP(T.DIV, e1, e2)) =
                    result(fn r => emit(A.OPER 
                        {assem="div 's0, 's1\nmflo 'd0\n", 
                        src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
                | munchExp(T.BINOP(T.AND, e1, T.CONST i)) =
                    result(fn r => emit(A.OPER 
                        {assem="andi 'd0, 's0, " ^ Int.toString i ^ "\n",
                        src=[munchExp e1], dst=[r], jump=NONE}))
                | munchExp(T.BINOP(T.AND, T.CONST i, e1)) =
                    let val check = checkImmed(i)
                    in
                        case check of
                            SOME(r) => munchExp(T.BINOP(T.AND, e1, T.TEMP(r)))
                        |   NONE    => result(fn r => emit(A.OPER 
                        {assem="andi 'd0, 's0, " ^ Int.toString i ^ "\n",
                        src=[munchExp e1], dst=[r], jump=NONE}))
                    end
                | munchExp(T.BINOP(T.AND, e1, e2)) =
                    result(fn r => emit(A.OPER 
                        {assem="and 'd0, 's0, 's1\n", 
                        src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
                | munchExp(T.BINOP(T.OR, e1, T.CONST i)) =
                    let val check = checkImmed(i)
                    in
                        case check of
                            SOME(r) => munchExp(T.BINOP(T.OR, e1, T.TEMP(r)))
                        |   NONE    => result(fn r => emit(A.OPER 
                        {assem="ori 'd0, 's0, " ^ Int.toString i ^ "\n", 
                        src=[munchExp e1], dst=[r], jump=NONE}))
                    end
                | munchExp(T.BINOP(T.OR, T.CONST i, e1)) =
                    let val check = checkImmed(i)
                    in
                        case check of
                            SOME(r) => munchExp(T.BINOP(T.OR, e1, T.TEMP(r)))
                        |   NONE    => result(fn r => emit(A.OPER 
                        {assem="ori 'd0, 's0, " ^ Int.toString i ^ "\n", 
                        src=[munchExp e1], dst=[r], jump=NONE}))
                    end
                | munchExp(T.BINOP(T.OR, e1, e2)) =
                    result(fn r => emit(A.OPER 
                        {assem="or 'd0, 's0, 'sl\n", 
                        src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
                (* Tiger can't do shift so need need to check CONST here... *)
                | munchExp(T.BINOP(T.RSHIFT, e1, T.CONST i)) =
                    result(fn r => emit(A.OPER 
                        {assem="srl 'd0, 's0, " ^ Int.toString i ^ "\n", 
                        src=[munchExp e1], dst=[r], jump=NONE}))
                | munchExp(T.BINOP(T.LSHIFT, e1, T.CONST i)) =
                    result(fn r => emit(A.OPER 
                        {assem="sll 'd0, 's0, " ^ Int.toString i ^ "\n", 
                        src=[munchExp e1], dst=[r], jump=NONE}))
                | munchExp(T.BINOP(T.XOR, e1, e2)) =
                    result(fn r => emit(A.OPER 
                        {assem="xor 'd0, 's0, 's1\n", 
                        src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
                | munchExp(T.BINOP(testOp, e1, e2)) = 0
                | munchExp(T.ESEQ (e1, e2)) = (munchStm e1; munchExp e2)
                | munchExp(T.NAME t) = 
                    result(fn r => emit(A.OPER 
                        {assem="la 'd0, " ^ (Symbol.name t) ^ "\n", 
                        src=[], dst=[r], jump=NONE}))
                | munchExp(T.CALL(e, args)) =
                    (emit(A.OPER{assem="jal 's0\n",
                                src=munchExp(e) :: munchArgs(0, args),
                                dst=MipsFrame.callersaves, 
                                jump=NONE}); MipsFrame.RV)
                | munchExp(T.TEMP t) = t
                




                and munchStm(T.SEQ(a, b)) = (munchStm a; munchStm b)
                | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) =
                    let val check = checkImmed(i)
                    in
                        case check of
                            SOME(r) => munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.TEMP(r))), e2))
                        |   NONE    => 
                                emit(A.OPER{assem="sw 's1, " ^ Int.toString i ^ "('s0)\n",
                                    src=[munchExp e1, munchExp e2],
                                    dst=[],jump=NONE})
                    end
                | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) =
                    let val check = checkImmed(i)
                    in
                        case check of
                            SOME(r) => munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.TEMP(r))), e2))
                        |   NONE    => 
                                emit(A.OPER{assem="sw 's1, " ^ Int.toString i ^ "('s0)\n",
                                    src=[munchExp e1, munchExp e2],
                                    dst=[],jump=NONE})
                    end
                | munchStm(T.MOVE(T.MEM(T.CONST i), e2)) =
                    let val check = checkImmed(i)
                    in
                        case check of
                            SOME(r) => munchStm(T.MOVE(T.MEM(T.TEMP(r)), e2))
                        |   NONE    => 
                                emit(A.OPER{assem="sw 's0, " ^ Int.toString i ^ "($r0)\n",
                                    src=[munchExp e2], dst=[],jump=NONE})
                    end
                    
                | munchStm(T.MOVE(T.MEM(e1), e2)) =
                    emit(A.OPER{assem="sw 's1, 0('s0)\n",
                                src=[munchExp e1, munchExp e2],
                                dst=[] ,jump=NONE})
                | munchStm(T.MOVE(T.TEMP t, T.CONST i)) =
                    let val check = checkImmed(i)
                    in
                        case check of
                            SOME(r) => munchStm(T.MOVE(T.TEMP t, T.TEMP r))
                        |   NONE    => 
                                emit(A.OPER{assem="li 'd0, " ^ Int.toString i ^ "\n",
                                src=[],
                                dst=[t], jump=NONE})
                    end
                    
                | munchStm(T.MOVE(T.TEMP t, e2) ) =
                    emit(A.MOVE{assem="move 'd0, 's0\n",
                                src=(munchExp e2),
                                dst=t})
                | munchStm(T.MOVE(_)) = print ("error: must move into MEM or TEMP\n")
                
                (* Branches *)
                | munchStm(T.CJUMP(T.EQ, e1, e2, t, f) ) =
                    emit(A.OPER{assem="beq 's0, 's1, "^ (Symbol.name t) ^"\n",
                                src=[munchExp e1, munchExp e2],
                                dst=[],jump=SOME([t,f])})
                | munchStm(T.CJUMP(T.NE, e1, e2, t, f) ) =
                    emit(A.OPER{assem="bne 's0, 's1, "^ (Symbol.name t) ^"\n",
                                src=[munchExp e1, munchExp e2],
                                dst=[],jump=SOME([t,f])})
                | munchStm(T.CJUMP(T.LT, e1, T.CONST(0), t, f) ) =
                    emit(A.OPER{assem="bltz 's0, "^ (Symbol.name t) ^"\n",
                                src=[munchExp e1],
                                dst=[],jump=SOME([t,f])})
                | munchStm(T.CJUMP(T.LT, e1, e2, t, f) ) =
                    emit(A.OPER{assem="blt 's0, 's1, "^ (Symbol.name t) ^"\n",
                                src=[munchExp e1, munchExp e2],
                                dst=[],jump=SOME([t,f])})
                | munchStm(T.CJUMP(T.GT, e1, T.CONST(0), t, f) ) =
                    emit(A.OPER{assem="bgtz 's0, "^ (Symbol.name t) ^"\n",
                                src=[munchExp e1],
                                dst=[],jump=SOME([t,f])})
                | munchStm(T.CJUMP(T.GT, e1, e2, t, f) ) =
                    emit(A.OPER{assem="bgt 's0, 's1, "^ (Symbol.name t) ^"\n",
                                src=[munchExp e1, munchExp e2],
                                dst=[],jump=SOME([t,f])})
                | munchStm(T.CJUMP(T.LE, e1, T.CONST(0), t, f) ) =
                    emit(A.OPER{assem="blez 's0, "^ (Symbol.name t) ^"\n",
                                src=[munchExp e1],
                                dst=[],jump=SOME([t,f])})
                | munchStm(T.CJUMP(T.LE, e1, e2, t, f) ) =
                    emit(A.OPER{assem="ble 's0, 's1, "^ (Symbol.name t) ^"\n",
                                src=[munchExp e1, munchExp e2],
                                dst=[],jump=SOME([t,f])})
                | munchStm(T.CJUMP(T.GE, e1, T.CONST(0), t, f) ) =
                    emit(A.OPER{assem="bgez 's0, "^ (Symbol.name t) ^"\n",
                                src=[munchExp e1],
                                dst=[],jump=SOME([t,f])})
                | munchStm(T.CJUMP(T.GE, e1, e2, t, f) ) =
                    emit(A.OPER{assem="bge 's0, 's1, "^ (Symbol.name t) ^"\n",
                                src=[munchExp e1, munchExp e2],
                                dst=[],jump=SOME([t,f])})
                | munchStm(T.CJUMP(testop, e1, e2, t, f)) = print "error: unsigned comparison not implemented\n"
                
                | munchStm(T.JUMP(T.NAME(t), labs) ) =
                    emit(A.OPER{assem="j " ^ Symbol.name t ^ "\n",
                                src=[],
                                dst=[], jump=SOME(labs)})
                | munchStm(T.JUMP(e1, labs) ) =
                    emit(A.OPER{assem="jr 's0\n",
                                src=[munchExp e1],
                                dst=[], jump=SOME(labs)})
                | munchStm(T.EXP(T.CONST(0))) = ()
                | munchStm(T.EXP e1) = (munchExp e1; ())
                | munchStm(T.LABEL lab) =
                    emit(A.LABEL{assem=MipsFrame.string(lab, ":\n"), lab=lab})
            in
                munchStm stm; rev(!ilist)
            end
    end