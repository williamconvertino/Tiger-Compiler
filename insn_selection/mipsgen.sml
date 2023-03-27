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

                
                fun munchExp exp = case exp of
                    (T.MEM (T.BINOP (T.PLUS, e1, T.CONST i))) => (munchExp e1; emit (A.LABEL({assem="LOAD",lab=Temp.newlabel()})))
                |   (T.MEM (T.BINOP (T.PLUS, T.CONST i, e1))) => (munchExp e1; emit (A.LABEL({assem="LOAD",lab=Temp.newlabel()})))
                |   (T.MEM(T.CONST i)) => (emit (A.LABEL{assem="LOAD",lab=Temp.newlabel()}))
                |   (T.MEM(e1)) => (munchExp e1; emit (A.LABEL{assem="LOAD",lab=Temp.newlabel()}))
                |   (T.BINOP (T.PLUS, e1, T.CONST i)) => (munchExp e1; emit (A.LABEL({assem="ADDI",lab=Temp.newlabel()})))
                |   (T.BINOP (T.PLUS, T.CONST i, e1)) => (munchExp e1; emit (A.LABEL{assem="ADDI",lab=Temp.newlabel()}))
                |   (T.CONST i) => (emit (A.LABEL{assem="ADDI",lab=Temp.newlabel()}))
                |   (T.BINOP (T.PLUS, e1, e2)) => (munchExp e1; munchExp e2; emit (A.LABEL{assem="ADD",lab=Temp.newlabel()}))
                |   (T.TEMP t) => ()

                fun munchStm stm = case stm of
                    (T.MOVE (T.MEM (T.BINOP( T.PLUS, e1, T.CONST i)), e2)) => (munchExp e1; munchExp e2; emit (A.LABEL{assem="STORE",lab=Temp.newlabel()}))
                |   (T.MOVE (T.MEM (T.BINOP( T.PLUS, T.CONST i, e1)), e2)) => (munchExp e1; munchExp e2; emit (A.LABEL{assem="STORE",lab=Temp.newlabel()}))
                |   (T.MOVE(T.MEM(e1),T.MEM(e2))) => (munchExp e1; munchExp e2; emit (A.LABEL{assem="MOVEM",lab=Temp.newlabel()}))
                |   (T.MOVE(T.MEM(T.CONST i) , e2)) => (munchExp e2; emit (A.LABEL{assem="STORE",lab=Temp.newlabel()}))
                |   (T.MOVE(T.MEM(e1), e2)) => (munchExp e1; munchExp e2; emit (A.LABEL{assem="STORE",lab=Temp.newlabel()}))
                |   (T.MOVE(T.TEMP i, e2)) => (munchExp e2; emit (A.LABEL{assem="ADD",lab=Temp.newlabel()}))

            in
                munchStm stm; rev(!ilist)
            end
    end