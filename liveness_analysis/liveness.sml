structure Liveness:
sig
    (* NOTE: This module is not included in sources, for the sake of compiling while I test makegraph *)
    datatype igraph = IGRAPH of {graph: IGraph.graph, tnode: Temp.temp -> IGraph.node, gtemp: IGraph.node -> Temp.temp, moves: (IGraph.node * IGraph.node) list}
    
    val interferenceGraph : Flow.flowgraph -> igraph * (Flow.Graph.node -> Temp.temp list)
    
    val show : outstream * igraph -> unit

    type liveSet = unit Temp.Table.table * temp list
    
    type liveMap = liveSet Flow.Graph.Table.table

end =
struct

end