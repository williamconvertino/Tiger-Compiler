structure MakeGraph:
sig
    val instrs2graph: Assem.instr list -> Flow.flowgraph * Flow.Graph.node list
end =
struct
    structure Graph = Flow.Graph
    structure A = Assem
    structure T = Graph.Table
    
    fun instrs2graph assemList = 
        let
            
            fun makeFlowGraph (assem, ((Flow.FGRAPH {control, def, use, ismove}), nodeList)) =
                let
                    val node = Graph.newNode(control)
                in
                    case assem of
                        (A.OPER {assem, dst, src, jump}) => (Flow.FGRAPH { control = control, def = (T.enter (def, node, dst)), use = use, ismove = ismove}, node :: nodeList)
                    |   (A.MOVE {assem, dst, src}) =>       (Flow.FGRAPH { control = control, def = (T.enter (def, node, (dst::[]))), use = use, ismove = (T.enter (ismove, node, true))}, node :: nodeList)
                    |   (A.LABEL {assem, lab}) =>           (Flow.FGRAPH { control = control, def = def, use = use, ismove = ismove}, node :: nodeList)
                end
            
        in
            foldl makeFlowGraph (Flow.FGRAPH { control = Graph.newGraph(), def = T.empty, use = T.empty, ismove = T.empty}, []) assemList
        end
        

end
