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
            fun addEdge (graph, node, targetLabel) = () 

            fun addEdges (graph, node, NONE) = []
            |   addEdges (graph, node, SOME labelList) = (map (fn label => addEdge(graph, node, label)) labelList);

            fun makeFlowGraph (assem, ((Flow.FGRAPH {control, def, use, ismove}), nodeList)) =
                let
                    val node = Graph.newNode(control)
                in
                    case assem of
                        (A.OPER {assem, dst, src, jump}) =>  (addEdges(control, node, jump); (Flow.FGRAPH { control = control, def = (T.enter (def, node, dst)), use = (T.enter (use, node, src)), ismove = ismove}, node :: nodeList))
                    |   (A.MOVE {assem, dst, src}) =>       (Flow.FGRAPH { control = control, def = (T.enter (def, node, (dst::[]))), use = (T.enter (use, node, (src::[]))), ismove = (T.enter (ismove, node, true))}, node :: nodeList)
                    |   (A.LABEL {assem, lab}) =>           (Flow.FGRAPH { control = control, def = def, use = use, ismove = ismove}, node :: nodeList)
                end
        
        in
            foldl makeFlowGraph (Flow.FGRAPH { control = Graph.newGraph(), def = T.empty, use = T.empty, ismove = T.empty}, []) assemList
        end
        

end
