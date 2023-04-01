structure MakeGraph:
sig
    val instrs2graph: Assem.instr list -> Flow.flowgraph * Flow.Graph.node list
end =
struct
    structure Graph = Flow.Graph
    
    fun instrs2graph assemList = 
        let
            fun helper (flowGraph, nodeList, []) =
                (flowGraph, nodeList)
            |   helper (flowGraph, nodeList, (assemItem::rst)) =
                    helper(flowGraph, nodeList, rst)
                
        in
            helper(Flow.FGRAPH { control = Graph.newGraph(), def = Graph.Table.empty, use = Graph.Table.empty, ismove = Graph.Table.empty}, [], assemList)
        end
        

end
