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

                fun munchArgs (i,args) = 
                let val arg = List.nth(Frame.argregs, i) 
                    fun temp_move ()= munchStm(T.MOVE(T.TEMP(arg),List.nth(args,i) ))
                in
                    temp_move(); arg::munchArgs (i+1,args)
                end

                and munchExp(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i))) = 
                    result(fn r => emit(A.OPER 
                        {assem="lw 'd0, " ^ Int.toString i ^ "('s0)\n", 
                        src =[munchExp e1], dst=[r], jump=NONE}))
                | munchExp(T.MEM(T.BINOP(T.PLUS,T.CONST i,e1))) =
                    result(fn r => emit(A.OPER
                        {assem="lw 'd0, " ^ Int.toString i ^ "('s0)\n", 
                        src=[munchExp e1], dst=[r], jump=NONE}))
                | munchExp(T.MEM(T.CONST i)) =
                    result(fn r => emit(A.OPER 
                        {assem="lw 'd0, " ^ Int.toString i ^ "($r0)\n", 
                        src=[], dst=[r], jump=NONE}))
                | munchExp(T.MEM(e1)) =
                    result(fn r => emit(A.OPER 
                        {assem="lw 'd0, 0('s0)\n", 
                        src=[munchExp e1], dst=[r], jump=NONE}))
                | munchExp(T.BINOP(T.PLUS, e1, T.CONST i)) =
                    result(fn r => emit(A.OPER
                        {assem="addi 'd0, 's0, " ^ Int.toString i ^ "\n",
                        src=[munchExp e1], dst=[r], jump=NONE}))
                | munchExp(T.BINOP(T.PLUS, T.CONST i, e1)) = 
                    result(fn r => emit(A.OPER 
                        {assem="addi 'd0, 's0, " ^ Int.toString i ^ "\n",
                        src=[munchExp e1], dst=[r], jump=NONE}))
                | munchExp(T.CONST i) =
                    result(fn r => emit(A.OPER
                        {assem="addi 'd0, $r0, " ^ Int.toString i ^ "\n", 
                        src=[], dst=[r], jump=NONE}))
                | munchExp(T.BINOP(T.PLUS, e1, e2)) =
                    result(fn r => emit(A.OPER 
                        {assem="add 'd0, 's0, 's1\n", 
                        src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))                
                | munchExp(T.BINOP(T.MINUS, e1, T.CONST i)) =
                    result(fn r => emit(A.OPER 
                        {assem="addi 'd0, 's0, " ^ Int.toString (~1 * i) ^ "\n",    
                        src=[munchExp e1], dst=[r], jump=NONE}))
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
                    result(fn r => emit(A.OPER 
                        {assem="andi 'd0, 's0, " ^ Int.toString i ^ "\n",
                        src=[munchExp e1], dst=[r], jump=NONE}))
                | munchExp(T.BINOP(T.AND, e1, e2)) =
                    result(fn r => emit(A.OPER 
                        {assem="and 'd0, 's0, 's1\n", 
                        src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
                | munchExp(T.BINOP(T.OR, e1, T.CONST i)) =
                    result(fn r => emit(A.OPER 
                        {assem="ori 'd0, 's0, " ^ Int.toString i ^ "\n", 
                        src=[munchExp e1], dst=[r], jump=NONE}))
                | munchExp(T.BINOP(T.OR, T.CONST i, e1)) =
                    result(fn r => emit(A.OPER 
                        {assem="ori 'd0, 's0, " ^ Int.toString i ^ "\n", 
                        src=[munchExp e1], dst=[r], jump=NONE}))
                | munchExp(T.BINOP(T.OR, e1, e2)) =
                    result(fn r => emit(A.OPER 
                        {assem="or 'd0, 's0, 'sl\n", 
                        src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
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
                | munchExp(T.ESEQ (e1, e2)) = (munchStm e1; munchExp e2)
                | munchExp(T.NAME t) = 
                    result(fn r => emit(A.OPER 
                        {assem="la 'd0, " ^ (Symbol.name t) ^ "\n", 
                        src=[], dst=[r], jump=NONE}))
                | munchExp(T.CALL (T.NAME(t), args)) = 
                    result(fn r => emit(A.OPER 
                        {assem="jal " ^ Symbol.name t ^"\n", 
                        src=munchArgs(0,args), dst=[MipsFrame.ra], jump=NONE}))
                | munchExp(T.TEMP t) = t
                
                and munchStm(T.SEQ(a, b)) = (munchStm a; munchStm b)
                | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) =
                    emit(A.OPER{assem="sw 's1, " ^ Int.toString i ^ "('s0)\n",
                                src=[munchExp e1, munchExp e2],
                                dst=[],jump=NONE})
                | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) =
                    emit(A.OPER{assem="sw 's1, " ^ Int.toString i ^ "('s0)\n",
                                src=[munchExp e1, munchExp e2],
                                dst=[],jump=NONE})
                | munchStm(T.MOVE(T.MEM(T.CONST i), e2)) =
                    emit(A.OPER{assem="sw 's0, " ^ Int.toString i ^ "($r0)\n",
                                src=[munchExp e2], dst=[],jump=NONE})
                | munchStm(T.MOVE(T.MEM(e1), e2)) =
                    emit(A.OPER{assem="sw 's1, 0('s0)\n",
                                src=[munchExp e1, munchExp e2],
                                dst=[] ,jump=NONE})
                | munchStm(T.MOVE(T.TEMP t, T.CONST j)) =
                    emit(A.OPER{assem="li 'd0, " ^ Int.toString j ^ "\n",
                                src=[],
                                dst=[t], jump=NONE})
                | munchStm(T.MOVE(T.TEMP t, e2) ) =
                    emit(A.MOVE{assem="move 'd0, 's0\n",
                                src=(munchExp e2),
                                dst=t})
                | munchStm(T.MOVE(_)) = print ("error: must move into MEM or TEMP")
                | munchStm(T.CJUMP(test, e1, e2, lab1, lab2) ) =
                    emit(A.OPER{assem="CJUMP 'to 's0 or 's1\n",
                                src=[munchExp e1,munchExp e2],
                                dst=[],jump=SOME([lab1,lab2])})
                | munchStm(T.JUMP(e1,lab) ) =
                    emit(A.OPER{assem="JUMP 'to s0\n",
                                src=[munchExp e1],
                                dst=[],jump=SOME(lab)})
                | munchStm(T.EXP(T.CONST(0))) = ()
                | munchStm(T.EXP(T.CALL(e,args))) =
                    emit(A.OPER{assem="CALL 's0\n",
                                src=munchExp(e)::munchArgs(0,args),
                                dst=MipsFrame.calleesaves, jump=NONE})
                | munchStm(T.EXP e1) = (munchExp e1; ())
                | munchStm(T.LABEL lab) =
                    emit(A.LABEL{assem=MipsFrame.string(lab, ":\n"), lab=lab})
            in
                munchStm stm; rev(!ilist)
            end
    end