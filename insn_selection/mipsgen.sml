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
                
                fun emit x= ilist := x :: !ilist
                fun result(gen) = let val t = Temp.newtemp() in gen t; t end


                fun munchArgs (i,args) = let val t = Temp.newtemp()::[] in t end 

                fun munchExp(T.MEM(T.BINOP(T.PLUS,el,T.CONST i))) = 
                    result(fn r => emit(A.OPER 
                            {assem="LOAD 'd0 <- M['s0+" ^ Int.toString i ^ "]\n", 
                            src =[munchExp el], dst=[r], jump=NONE}))
                | munchExp(T.MEM(T.BINOP(T.PLUS,T.CONST i,el))) =
                    result(fn r => emit(A.OPER
                        {assem="LOAD 'd0 <- M['s0+" ^ Int.toString i ^ "]\n", 
                        src=[munchExp el], dst=[r], jump=NONE}))
                | munchExp(T.MEM(T.CONST i)) =
                    result(fn r => emit(A.OPER 
                        {assem="LOAD 'd0 <- M[r0+" ^ Int.toString i ^ "]\n", 
                        src=[], dst=[r], jump=NONE}))
                | munchExp(T.MEM(el)) =
                    result(fn r => emit(A.OPER 
                        {assem="LOAD 'd0 <- M['s0+0]\n", 
                        src=[munchExp el], dst=[r], jump=NONE}))
                | munchExp(T.BINOP(T.PLUS,el,T.CONST i)) =
                    result(fn r => emit(A.OPER
                        {assem="ADDI 'd0 <- 's0+" ^ Int.toString i ^ "\n",
                        src=[munchExp el], dst=[r], jump=NONE}))
                | munchExp(T.BINOP(T.PLUS,T.CONST i,el)) = 
                    result(fn r => emit(A.OPER 
                        {assem="ADDI 'd0 <- 's0+" ^ Int.toString i ^ "\n", 
                        src=[munchExp el], dst=[r], jump=NONE}))
                | munchExp(T.CONST i) =
                    result(fn r => emit(A.OPER
                        {assem="ADDI 'd0 <- r0+" ^ Int.toString i ^ "\n", 
                        src=[], dst=[r], jump=NONE}))
                | munchExp(T.BINOP(T.PLUS,el,e2)) =
                    result(fn r => emit(A.OPER 
                        {assem="ADD 'd0 <- 's0+'sl\n", 
                        src=[munchExp el, munchExp e2], dst=[r], jump=NONE}))
                | munchExp(T.TEMP t) = t
                
                fun munchStm(T.SEQ(a,b)) = (munchStm a; munchStm b)
                | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS,el,T.CONST i)),e2)) =
                    emit(A.OPER{assem="STORE M['s0+" ^ Int.toString i ^ "] <- 'sl\n",
                                src=[munchExp el, munchExp e2],
                                dst=[],jump=NONE})
                | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS,T.CONST i,el)),e2)) =
                    emit(A.OPER{assem="STORE M['s0+" ^ Int.toString i ^ "] <- 'sl\n",
                                src=[munchExp el, munchExp e2],
                                dst=[],jump=NONE})
                | munchStm(T.MOVE(T.MEM(el),T.MEM(e2))) =
                    emit(A.OPER{assem="MOVE M['s0] <- M['sl]\n",
                                src=[munchExp el, munchExp e2],
                                dst=[],jump=NONE})
                | munchStm(T.MOVE(T.MEM(T.CONST i),e2)) =
                    emit(A.OPER{assem="STORE M[r0+" ^ Int.toString i ^ "] <- 's0\n",
                                src=[munchExp e2], dst=[],jump=NONE})
                | munchStm(T.MOVE(T.MEM(el),e2)) =
                    emit(A.OPER{assem="STORE M['s0] <- 'sl\n",
                                src=[munchExp el, munchExp e2],
                                dst= [] ,jump=NONE})
                | munchStm(T.MOVE(T.TEMP i, e2) ) =
                    emit(A.OPER{assem="ADD 'd0 <- 's0 + r0\n",
                                src=[munchExp e2],
                                dst=[i],jump=NONE})
                | munchStm(T.EXP(T.CALL(e,args))) =
                    emit(A.OPER{assem="CALL 's0\n",
                                src=munchExp(e)::munchArgs(0,args),
                                dst=MipsFrame.calleesaves, jump=NONE})
                | munchStm(T.LABEL lab) =
                    emit(A.LABEL{assem=MipsFrame.string(lab, ":\n"), lab=lab})
            in
                munchStm stm; rev(!ilist)
            end
    end