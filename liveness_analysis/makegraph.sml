structure MakeGraph:
sig
    val instrs2graph: Assem.instr list -> Flow.flowgraph * Flow.Graph.node list
end =
struct
    structure Graph = Flow.Graph
    
    fun instrs2graph assemList = 
        (Flow.FGRAPH {
            control = Graph.newGraph(), 
            def = Graph.Table.empty,
            use = Graph.Table.empty,
            ismove = Graph.Table.empty
        }, [])

end
