structure MakeGraph:
sig
    val instrs2graph: Assem.instr list -> Flow.flowgraph * Flow.Graph.node list
end =
struct
    structure Graph = Flow.Graph
    
    fun instrs2graph assemList = 
        let
            fun helper (currentControlGraph, currentNodeList, []) =
                (Flow.FGRAPH {  
                    control = currentControlGraph, 
                    def = Graph.Table.empty,
                    use = Graph.Table.empty,
                    ismove = Graph.Table.empty
                }, currentNodeList)
            |   helper (currentControlGraph, currentNodeList, (assemItem::rst)) =
                    helper(currentControlGraph, currentNodeList, rst)
                
        in
            helper(Graph.newGraph(), [], assemList)
        end
        

end
