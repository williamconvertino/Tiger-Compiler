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
            fun generateEdges (labelMap, jumpList) = 
                let
                    fun addEdge (srcNode, SOME dstNode) = Graph.mk_edge({from=srcNode,to=dstNode})
                    |   addEdge (srcNode, NONE) = ErrorMsg.impossible "Error: Label specified by Jump command was not found."

                    fun findAndConnectNodes (node, label) = addEdge (node, (Symbol.look(labelMap, label)))

                in
                    map (fn (node, labelList) => map (fn (label) => findAndConnectNodes(node,label)) labelList) jumpList
                end

            fun processList (graph, nodeList, labelMap, jumpList, []) = (generateEdges (labelMap, jumpList); (graph, nodeList)) 
            |   processList ((Flow.FGRAPH {control, def, use, ismove}), nodeList, labelMap, jumpList, (assem::rst)) = 
                let 
                    val newNode = Graph.newNode(control)
                    val newGraph = 
                        case assem of
                            (A.OPER {assem, dst, src, jump}) => (Flow.FGRAPH { control = control, def = (T.enter (def, newNode, dst)),          use = (T.enter (use, newNode, src)),        ismove = ismove})
                        |   (A.MOVE {assem, dst, src}) =>       (Flow.FGRAPH { control = control, def = (T.enter (def, newNode, (dst::[]))),    use = (T.enter (use, newNode, (src::[]))),  ismove = (T.enter (ismove, newNode, true))})
                        |   (A.LABEL {assem, lab}) =>           (Flow.FGRAPH { control = control, def = def, use = use, ismove = ismove})
                    val newLabelMap = 
                        case assem of
                            (A.LABEL {assem, lab}) => Symbol.enter (labelMap, lab, newNode)
                        |   _ => labelMap
                    val newJumpList =
                        case assem of
                            (A.OPER {assem, dst, src, jump=SOME jump}) => (newNode,jump) :: jumpList
                        |   _ => jumpList
                in
                    processList (newGraph, newNode :: nodeList, newLabelMap, newJumpList, rst)
                end
        
        in
            processList (
                (Flow.FGRAPH { control = Graph.newGraph(), def = T.empty, use = T.empty, ismove = T.empty}),
                [],
                Symbol.empty,
                [],
                assemList
                )
        end


    fun instrs2graphOLD assemList = 
        let
            
            val LabelMap : Graph.node Symbol.table ref = ref Symbol.empty
            val JumpNodesToProcess : Graph.node list ref = ref []

            fun addEdge (graph, node, targetLabel) = () 

            fun addEdges (graph, node, NONE) = []
            |   addEdges (graph, node, SOME labelList) = (map (fn label => addEdge(graph, node, label)) labelList);

            fun makeFlowGraph (assem, ((Flow.FGRAPH {control, def, use, ismove}), nodeList)) =
                let
                    val node = Graph.newNode(control)
                in
                    case assem of
                        (A.OPER {assem, dst, src, jump}) => (addEdges(control, node, jump); (Flow.FGRAPH { control = control, def = (T.enter (def, node, dst)), use = (T.enter (use, node, src)), ismove = ismove}, node :: nodeList))
                    |   (A.MOVE {assem, dst, src}) =>       (Flow.FGRAPH { control = control, def = (T.enter (def, node, (dst::[]))), use = (T.enter (use, node, (src::[]))), ismove = (T.enter (ismove, node, true))}, node :: nodeList)
                    |   (A.LABEL {assem, lab}) =>           (Flow.FGRAPH { control = control, def = def, use = use, ismove = ismove}, node :: nodeList)
                end
        
        in
            foldl makeFlowGraph (Flow.FGRAPH { control = Graph.newGraph(), def = T.empty, use = T.empty, ismove = T.empty}, []) assemList
        end
        

end
