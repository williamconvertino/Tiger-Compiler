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

    structure A = Assem
    structure S = IntRedBlackSet

    structure TempPairKey : ORD_KEY = 
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
        end
    
    structure PairSet = SplaySetFn(TempPairKey)

    (* Interference Graph *)
    structure TempKey : ORD_KEY = 
        struct 
            type ord_key = Temp.temp 
            val compare = Int.compare
        end

    structure Graph = FuncGraph(TempKey)

    type Graph = Temp.temp Graph.graph


    fun dataflow2interference dataflowGraph =
        let val liveness = Liveness.dataflow2liveness dataflowGraph
            fun addNodes (graph, temps) = S.foldl (fn (temp, graph) => Graph.addNodeIfNotExists(graph, temp, temp)) graph temps
            fun addEdges (graph, temp, liveTemps) = S.foldl (fn (liveTemp, graph) => Graph.doubleEdge(graph, temp, liveTemp)) graph liveTemps

            fun procInstr (A.LABEL {assem, lab}, accumulator) = accumulator
            |   procInstr (A.MOVE {assem, dst, src}, (graph, moves, liveOut)) = 
                    let val moves' = PairSet.add(moves, (dst, src))
                        val graph' = addNodes(graph, S.addList(liveOut, [src, dst]))
                        val graph'' = addEdges (graph', dst, S.subtractList(liveOut, [src, dst]))
                        val liveOut' = S.add(S.subtract(liveOut, dst), src)
                    in
                        (graph'', moves', liveOut')
                    end
            |   procInstr (A.OPER {assem, dst=dsts, src=srcs, jump}, (graph, moves, liveOut)) =
                    let val graph' = addNodes(graph, S.addList(S.addList(liveOut, dsts), srcs))
                        val graph'' = List.foldl (fn (dst, graph) => addEdges (graph, dst, S.subtract(liveOut, dst))) graph' dsts
                        val liveOut' = S.addList(S.subtractList(liveOut, dsts), srcs)
                    in
                        (graph'', moves, liveOut') 
                    end
                    
        in
            (* Liveness.printGraph (dataflowGraph, liveness); *)
            Flow.Graph.foldNodes 
                (fn (node, (graph, moves)) => (
                    let val {li, lo} = Liveness.LivenessMap.lookup(liveness, (Flow.Graph.getNodeID node))
                        val {instrs, defs, uses, targets} = Flow.Graph.nodeInfo node
                        val (graph', moves', lo') = List.foldr procInstr (graph, PairSet.empty, lo) instrs
                    in
                        (graph', moves')
                    end
                ))
                (Graph.empty, PairSet.empty)
                dataflowGraph

        end


    fun printGraph (graph, moves) = Graph.printGraph (fn (id) => (Int.toString id)) (fn (id, node) => (Int.toString id)) graph
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