signature CODEGEN =
    sig
        
        structure Frame : FRAME
        
        val codegen : Frame.frame -> Tree.stm -> Assem.instr list

    end

structure MipsGen : CODEGEN =
    struct
        
        structure Frame : FRAME = MipsFrame
        
        fun munchStm stm = 
            case stm of
                (T.SEQ(a,b)) =>
            (* TODO *)

        fun munchExp exp = 
            case exp of
                (T.CONST const) =>
            |   (T.BINOP(p,a,b)) =>
            (* TODO *)

        val codegen : Frame.frame -> Tree.stm -> Assem.instr list =
            (* TODO *)

    end