signature CODEGEN =
    sig
        
        structure Frame : FRAME
        
        val codegen : Frame.frame -> Tree.stm -> Assem.instr list

    end

structure MipsGen : CODEGEN =
    struct
        
        structure Frame : FRAME = MipsFrame
        
        structure T = Tree

        (* This code DOES NOT choose registers or specify operand syntax *)
        fun munchStm stm = case stm of
            (T.MOVE (T.MEM (T.BINOP( T.PLUS, e1, T.CONST i)), e2)) => (munchExp e1; munchExp e2; emit "STORE")
        |   (T.MOVE (T.MEM (T.BINOP( T.PLUS, T.CONST i, e1)), e2)) => (munchExp e1; munchExp e2; emit "STORE")
        |   (T.MOVE(T.MEM(e1)/T.MEM(e2))) => (munchExp e1; munchExp e2; emit "MOVEM")
        |   (T.MOVE(T.MEM(T.CONST i) , e2)) => (munchExp e2; emit "STORE")
        |   (T.MOVE(T.MEM(e1), e2)) => (munchExp e1; munchExp e2; emit "STORE")
        |   (T.MOVE(T.TEMP i, e2)) => (munchExp e2; emit "ADD")

        fun munchExp exp = case exp of
            (T.MEM (T.BINOP (T.PLUS, e1, T.CONST i))) => (munchExp e1; emit "LOAD")
        |   (T.MEM (T.BINOP (T.PLUS, T.CONST i, e1))) => (munchExp e1; emit "LOAD")
        |   (T.MEM(T.CONST i)) => (emit "LOAD")
        |   (T.MEM(e1)) => (munchExp e1; emit "LOAD")
        |   (T.BINOP (T.PLUS, e1, T.CONST i)) => (munchExp e1; emit "ADDI")
        |   (T.BINOP (T.PLUS, T.CONST i, e1)) => (munchExp e1; emit "ADDI")
        |   (T.CONST i) => (emit "ADDI")
        |   (T.BINOP (T.PLUS, e1, e2)) => (munchExp e1; munchExp e2; emit "ADD")
        |   (T.TEMP t) => ()

        val codegen : Frame.frame -> Tree.stm -> Assem.instr list =
            (* TODO *)

    end