structure Liveness =
struct

    structure LabelKey : ORD_KEY = 
      struct 
            type ord_key = Temp.label 
            val compare = (fn (l1, l2) => String.compare (Symbol.name l1, Symbol.name l2))
      end

    structure LivenessMap = RedBlackMapFn(LabelKey)

    structure Graph = Flow.Graph

    type liveness = {li: IntRedBlackSet.set, lo: IntRedBlackSet.set}


    fun dataflow2liveness dataflowGraph =
        let val liveness = List.foldl 
                    (fn (node, livenessMap) => LivenessMap.insert(livenessMap, (Graph.getNodeID node), {li=IntRedBlackSet.empty, lo=IntRedBlackSet.empty})) 
                    LivenessMap.empty 
                    (Graph.nodes dataflowGraph)
            
            
            fun findFixedPoint liveness = 
                let val changed = ref false
                    fun updateLiveness (node, map) =
                            let val {instrs, defs, uses, targets} = Graph.nodeInfo node
                                val {li=liveIn, lo=liveOut} = LivenessMap.lookup(map, Graph.getNodeID node)
                                val lo' = Graph.foldSuccs 
                                        (fn (succId, lo') => (
                                            let val {li, lo} = LivenessMap.lookup(map, succId)
                                            in
                                                IntRedBlackSet.union(lo', li)
                                            end
                                        )) 
                                        IntRedBlackSet.empty 
                                        node
                                val li' = IntRedBlackSet.union(IntRedBlackSet.difference(lo', defs), uses)
                            in
                                if (IntRedBlackSet.equal(liveIn, li') andalso IntRedBlackSet.equal(liveOut, lo')) then map 
                                else (changed := true; LivenessMap.insert(map, Graph.getNodeID node, {li=li', lo=lo'}))
                            end

                    val liveness' = Graph.foldNodes updateLiveness liveness dataflowGraph
                in
                    if (!changed) then findFixedPoint liveness' else liveness'
                end
        in
            findFixedPoint liveness
        end 
    

    fun printGraph (graph, liveness) = Graph.printGraph (fn (id) => (Symbol.name id)) (fn (id, block) => (
        let fun foldTemps temps = IntRedBlackSet.foldl (fn (temp, str) => str ^ (Int.toString temp) ^ ", ") "" temps
            val {li, lo} = LivenessMap.lookup(liveness, id)
        in
            "\nLiveIn: " ^ (foldTemps li) ^ "\nLiveOut: " ^ (foldTemps lo) ^ "\nBlock:\n" ^ (Block.toString block)
        end
    )) graph
end

structure Interference =
struct
    (* Interference Graph *)
    structure TempKey : ORD_KEY = 
        struct 
            type ord_key = Temp.temp 
            val compare = Int.compare
        end

    structure Graph = FuncGraph(TempKey)

    type iNode = {temp: Temp.temp, moves: IntRedBlackSet.set}
    type iGraph = iNode Graph.graph


    fun dataflow2interference dataflowGraph =
            let val liveness = Liveness.dataflow2liveness dataflowGraph
        in
            Liveness.printGraph (dataflowGraph, liveness);
            ()
        end


    fun printGraph graph = Graph.printGraph (fn (id) => (Int.toString id)) (fn (id, inode) => (Int.toString id)) graph
end



    (* structure TempPairKey : ORD_KEY = 
        struct 
            type ord_key = (Temp.temp * Temp.temp)
            val compare = (fn ((t1, t2), (t1', t2')) => (
                let val t1cmp = Int.compare(t1, t1')
                    val t2cmp = Int.compare(t2, t2')

                in
                    case (t1cmp, t2cmp) of
                        (EQUAL, _) => t2cmp
                    |   (t1cmp, _) => t1cmp
                end
            ))
        end *)