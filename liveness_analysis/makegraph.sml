structure MakeGraph:
sig
    val instrs2graph: Assem.instr list -> Flow.flowgraph * Flow.Graph.node list
end =
struct
    
    fun instrs2graph assemList = 
        (Flow.FGRAPH {
            control = Flow.Graph.newGraph(), 
            def = Flow.Graph.Table.empty,
            use = Flow.Graph.Table.empty,
            ismove = Flow.Graph.Table.empty
        }, [])

end
