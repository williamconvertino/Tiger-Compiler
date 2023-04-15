structure MakeGraph:
sig
    val instrs2graph: Assem.instr list -> Flow.graph
end =
struct
    structure Graph = Flow.Graph
    structure A = Assem

    structure S = IntRedBlackSet


    fun instrs2graph assemList =
        let fun blockify (graph, currentBlock, []) = graph
            |   blockify (graph, currentBlock, instr::instrs) =
                    let val {instrs=blockinstrs, defs=blockdefs, uses=blockuses, targets=blocktargets} = currentBlock
                        fun procInstr (A.LABEL {assem, lab}) = 
                                let val block' = {instrs=(instr :: blockinstrs), defs=blockdefs, uses=blockuses, targets=blocktargets}
                                in
                                    (Flow.addBlock(graph, block'), Block.withLabel lab)
                                end
                        |   procInstr (A.MOVE {assem, dst, src}) = 
                                let val defs' = S.add(blockdefs, dst)
                                    val uses' = S.add(S.subtract(blockuses, dst), src)
                                in
                                    (graph, {instrs=(instr :: blockinstrs), defs=defs', uses=uses', targets=blocktargets})
                                end
                        |   procInstr (A.OPER {assem, dst=dsts, src=srcs, jump=NONE}) =
                                let val defs' = S.addList(blockdefs, dsts)
                                    val uses' = S.addList(S.subtractList(blockuses, dsts), srcs)
                                in
                                    (graph, {instrs=(instr :: blockinstrs), defs=defs', uses=uses', targets=blocktargets}) 
                                end
                        |   procInstr (A.OPER {assem, dst=dsts, src=srcs, jump=SOME(jmps)}) =
                                let val {instrs=blockinstrs, defs=blockdefs, uses=blockuses, targets=blocktargets} = Block.empty
                                    val defs' = S.addList(blockdefs, dsts)
                                    val uses' = S.addList(S.subtractList(blockuses, dsts), srcs)
                                in
                                    (graph, {instrs=(instr :: blockinstrs), defs=defs', uses=uses', targets=Block.TargetSet.addList(blocktargets, jmps)}) 
                                end

                        val (graph', block') = procInstr(instr)

                    in
                       blockify(graph', block', instrs)
                    end

            fun connectBlocks (graph, []) = graph
            |   connectBlocks (graph, node::nodes) =
                    let val {instrs, defs, uses, targets} = Graph.nodeInfo node
                        val graph' = Block.TargetSet.foldr (fn (label, graph) => Graph.addEdge(graph, {from=(Graph.getNodeID node), to=label})) graph targets
                    in
                        connectBlocks(graph', nodes)   
                    end

            val graph = blockify (Graph.empty, Block.empty, (List.rev assemList));
            val graph' = connectBlocks(graph, Graph.nodes(graph))

        
        in
            Flow.debugGraph graph';
            graph'
        end
        
end
