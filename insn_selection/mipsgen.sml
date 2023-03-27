signature CODEGEN =
    sig
        
        structure Frame : FRAME
        
        val codegen : Frame.frame -> Tree.stm -> Assem.instr list

    end

structure MipsGen : CODEGEN =
    struct
        
        structure Frame : FRAME = MipsFrame
        
        structure T = Tree

        fun munchStm stm = case stm of
            (T.MOVE (T.TEMP i, e2)) => (munchExp(e2) ; emit "ADD")

        fun munchExp exp = case exp of
            (T.TEMP t) => ()
        

        val codegen : Frame.frame -> Tree.stm -> Assem.instr list =
            (* TODO *)

    end