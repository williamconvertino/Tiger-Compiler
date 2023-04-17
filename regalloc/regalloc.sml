structure RegisterAllocator:
sig 
    val allocate: Assem.instr list -> Assem.instr list
end =
struct

    fun allocate instrs = 
        let val dataflow = MakeGraph.instrs2graph instrs
            (* val _ = Flow.debugGraph dataflowGraph *)
            val interference = Interference.dataflow2interference dataflow
            (* val _ = Interference.printGraph interferenceGraph *)
        in
            color interference;
            instrs
        end


    structure IG = Interference.Graph
    structure PS = Interference.PairSet

    structure TempKey : ORD_KEY = 
        struct 
            type ord_key = Temp.temp 
            val compare = Int.compare
        end


    structure S = SplaySetFn(TempKey)
    structure M = RedBlackMapFn(TempKey)

    val mipsColors = S.fromList([0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 28, 29, 30, 31])
    val mipsColorable = S.subtract(mipsColors, 0)

    fun color (graph, moves) = 
        let val numColors = S.numItems(mipsColors)
            val colors = S.foldl (fn (temp, colors) => M.insert(colors, temp, temp)) M.empty mipsColors

            fun simplify (graph, colors, moves) = 
                let fun folder (node, (graph, colors)) =
                        let val temp = IG.getNodeID(node)
                            val trivial = IG.inDegree(node) < numColors
                            val precolored = S.member(colors, temp)
                            val frozen = IG.inDomain(moves, temp)
                            fun colorTemp ((graph', colors'), tempNode) = 
                                let val neighboringColors = IG.foldSuccs (fn (succId, succColors) => S.add(succColors, M.lookup(colors', succId))) S.empty tempNode
                                    val possibleColors = S.difference(mipsColorable, neighboringColors) 
                                    val colors'' = M.insert(colors', IG.getNodeID(tempNode), List.hd(S.toList(possibleColors)))
                                in
                                    (graph, colors'')
                                end

                        in
                            if (trivial andalso not(precolored) andalso not(frozen)) then (
                                colorTemp (simplify (IG.removeNode(temp), colors, moves), node)
                            )
                            else
                                (graph, colors)
                        end
                    val (graph', colors') = IG.foldNodes folder graph graph
                    val fullyColored = IG.foldNodes (fn (node, check) => if (not(check)) then check else M.inDomain(colors', IG.getNodeID(node))) true graph' 
                in
                    (* If fully colored then return. Else coalesce.*)
                    if (fullyColored) then (graph', colors') else coalesce (graph', colors', moves)
                end

            fun coalesce (graph, colors, moves) =
                let fun coalesceCycle (graph, moves, pairs) =
                    let fun squash (survivorId, squashedId) = 
                            let val survivor = IG.getNode(graph, survivorId)
                                val squashed = IG.getNode(graph, squashedId)
                                val adj' = S.fromList([IG.adj(survivor), IG.adj(squashed)])
                                val adj'' = S.subtractList(adj', [survivorId, squashedId])
                                val graph' = IG.removeNode(squashed)
                            in
                                S.foldl (fn (temp, graph) => IG.doubleEdge(graph, survivorId, temp)) graph' adj''
                            end
                        fun folder (movesNode, (graph, moves, pairs)) = 
                            let val nodeId = IG.getNodeID(movesNode)
                                fun briggs (graph, survivorId) = 
                                    let val adj' = IG.adj(IG.getNode(survivorId))
                                        val nontrivialAdj = S.foldl (fn (temp, count) => if (IG.inDegree(IG.getNode(graph, temp)) < numColors) count else count + 1) 0 adj'
                                    in
                                        nontrivialAdj < numColors
                                    end
                                fun squasher (linkedId, (graph, moves, pairs)) = 
                                    let val graph' = squash(nodeId, linkedId)
                                        val moves' = squash(nodeId, linkedId)
                                        val moves'' = if IG.degree(IG.getNode(moves', nodeId)) = 0 then IG.removeNode(moves', nodeId) else moves'
                                    if (briggs(graph', nodeId)) then 
                                        (graph', moves'', (nodeId, linkedId) :: pairs)
                                    else
                                        (graph, moves, pairs)
                            in
                                (* check to make sure node not already squashed *)
                                if (IG.inDomain(moves, nodeId)) then
                                    IG.foldSuccs squasher (graph, moves, pairs) movesNode
                                else
                                    (graph, moves, pairs)
                            end
                        val (graph', moves', pairs') = IG.foldNodes folder (graph, moves, []) moves
                    in
                        (* If we squashed nodes then try to squash more. Else return *)
                        if ((List.length pairs') > (List.length pairs)) then
                            coalesceCycle (graph', moves', pairs')
                        else
                            (graph, moves, pairs)
                    end

                    fun colorPairs (graph, colors, moves, pairs) =
                        let val (graph', colors') = simplify (graph, colors, moves)
                            fun colorFolder ((survivor, squashed), colors) = M.insert(colors, squashed, M.lookup(colors, survivor))
                        in
                            (graph', List.foldl colorFolder colors' pairs)
                        end

                    val (graph', moves', pairs) = coalesceCycle (graph, moves, [])

                in
                    (* If we squashed any pairs, then simplify/color. Else unfreeze. *)
                    if ((List.length pairs) > 0) then
                        colorPairs (graph', colors, moves', pairs)
                    else
                        unfreeze (graph, colors, moves)
                end
            
            fun unfreeze (graph, colors, moves) = (graph, colors)
                let fun getMoveNodeDegree moveNode = IG.inDegree(IG.getNode(graph, IG.getNodeID(moveNode)))
                
                    fun unfreezeNode frozenNode = 
                        let val moves' = IG.remove(moves, frozenNode)
                        in
                            simplify (graph, colors, moves')
                        end

                    val trivialMoveNodes = List.filter (fn (node) => (getMoveNodeDegree node) < numColors) IG.nodeList(moves)
                in
                    if ((List.length trivialMoveNodes) = 0) then
                        potentialSpill (graph, colors, moves)
                    else
                        unfreezeNode (
                            List.foldl 
                                (fn (node, bestNode) => if (getMoveNodeDegree(node) > getMoveNodeDegree(bestNode) then node else bestNode)) 
                                List.hd(trivialMoveNodes)
                                List.drop(trivialMoveNodes, 1)
                        )
                end

            fun potentialSpill (graph, colors, moves) = 


            
        in
            simplify (graph, colors, moves)
        end
end