structure MakeGraph:
sig
    val instrs2graph: Assem.instr list -> Flow.flowgraph * Flow.Graph.node list
end =
struct
    structure Graph = Flow.Graph
    structure A = Assem
    
    fun instrs2graph assemList = 
        let

            fun helper ((A.OPER {assem, dst, src, jump}),   (flowGraph, nodeList)) = (flowGraph, nodeList)
            |   helper ((A.LABEL {assem, lab}),             (flowGraph, nodeList)) = (flowGraph, nodeList)
            |   helper ((A.MOVE {assem, dst, src}),         (flowGraph, nodeList)) = (flowGraph, nodeList)
                
        in
            foldl helper (Flow.FGRAPH { control = Graph.newGraph(), def = Graph.Table.empty, use = Graph.Table.empty, ismove = Graph.Table.empty}, []) assemList
        end
        

end
