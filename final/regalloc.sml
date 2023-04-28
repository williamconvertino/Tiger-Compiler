structure RegisterAllocator:
sig     
    val allocate: MipsFrame.frame * Assem.instr list -> Assem.instr list

end =
struct

    structure IG = Interference.Graph
    structure PS = Interference.PairSet

    structure TempKey : ORD_KEY = 
        struct 
            type ord_key = Temp.temp 
            val compare = Int.compare
        end


    structure S = SplaySetFn(TempKey)
    structure M = RedBlackMapFn(TempKey)

    fun printColors colors = List.app (fn (key) => print ((Int.toString key) ^ "=" ^ (Int.toString (M.lookup(colors, key))) ^ "\n")) (M.listKeys(colors))

    val mipsColors = S.fromList([0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 29, 30, 31])
    val mipsColorable = S.subtractList(mipsColors, [0])

    fun color (graph, moves) = 
        let val numColors = S.numItems(mipsColorable)
            val initialColors = S.foldl (fn (temp, colors) => M.insert(colors, temp, temp)) M.empty mipsColors

            fun simplify (graph, colors, moves) = 
                let fun folder (node, (graph, colors)) =
                        let val temp = IG.getNodeID(node)
                            val trivial = IG.inDegree(node) < numColors
                            val precolored = M.inDomain(colors, temp)
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
                                colorTemp (simplify (IG.removeNode(graph, temp), colors, moves), node)
                            )
                            else
                                (graph, colors)
                        end
                    val (graph', colors') = IG.foldNodes folder (graph, colors) graph
                    val fullyColored = IG.foldNodes (fn (node, check) => if (not(check)) then check else M.inDomain(colors', IG.getNodeID(node))) true graph' 
                in
                    (* If fully colored then return. Else coalesce.*)
                    if (fullyColored) then (graph', colors') else coalesce (graph', colors', moves)
                end

            and coalesce (graph, colors, moves) =
                let fun coalesceCycle (graph, moves, pairs) =
                    let fun squash (graph, survivorId, squashedId) = 
                            let val survivor = IG.getNode(graph, survivorId)
                                val squashed = IG.getNode(graph, squashedId)
                                val adj' = S.fromList(IG.adj(squashed))
                                val adj'' = S.subtractList(adj', [survivorId, squashedId])
                                val graph' = IG.remove(graph, squashed)
                            in
                                S.foldl (fn (temp, graph) => IG.doubleEdge(graph, survivorId, temp)) graph' adj''
                            end
                        fun folder (movesNode, (graph, moves, pairs)) = 
                            let val nodeId = IG.getNodeID(movesNode)
                                (* val _ = print("folder called with nodeId " ^ (Int.toString nodeId) ^ "\n") *)
                                (* val _ = print((Bool.toString (IG.inDomain(graph, nodeId))) ^ " " ^ (Bool.toString (IG.inDomain(moves, nodeId))) ^ "\n") *)
                                fun briggs (graph, survivorId) = 
                                    let val adj' = IG.adj(IG.getNode(graph, survivorId))
                                        val nontrivialAdj = List.foldl (fn (temp, count) => if (IG.inDegree(IG.getNode(graph, temp)) < numColors) then count else count + 1) 0 adj'
                                    in
                                        nontrivialAdj < numColors
                                    end
                                fun squasher (linkedId, (graph, moves, pairs)) = 
                                    let fun safesquasher () =
                                        let val _ = ()
                                            (* val _ = print((Int.toString nodeId) ^ ":" ^ (Int.toString linkedId) ^ "\n") *)
                                            (* val _ = print((Bool.toString (IG.inDomain(graph, nodeId))) ^ " " ^ (Bool.toString (IG.inDomain(graph, linkedId))) ^ "\n") *)
                                            val graph' = squash(graph, nodeId, linkedId)
                                            val moves' = squash(moves, nodeId, linkedId)
                                            val moves'' = if IG.degree(IG.getNode(moves', nodeId)) = 0 then IG.removeNode(moves', nodeId) else moves'
                                        in
                                            (* If node not precolored and briggs then squash. *)
                                            if (linkedId > 31 andalso briggs(graph', nodeId)) then
                                                (graph', moves'', (nodeId, linkedId) :: pairs)
                                            else (
                                                (* if (linkedId > 31) then print ("not briggs\n") else (); *)
                                                (graph, moves, pairs)
                                            )
                                        end
                                    in
                                        (* added guard to make sure node not already squashed *)
                                        if (IG.inDomain(graph, linkedId) andalso not(IG.isAdjacent ((IG.getNode (graph, nodeId)), ( IG.getNode (graph, linkedId) )))) 
                                        (* if (IG.inDomain(graph, linkedId) )  *)
                                            then (
                                                safesquasher () 
                                            )
                                            else ( 
                                                (graph, moves, pairs)
                                            )
                                    end
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
                            List.foldl colorFolder colors' pairs
                        end

                    val (graph', moves', pairs) = coalesceCycle (graph, moves, [])

                in
                    (* If we squashed any pairs, then simplify/color. Else unfreeze. *)
                    if ((List.length pairs) > 0) then
                        (graph, colorPairs (graph', colors, moves', pairs))
                    else
                        unfreeze (graph, colors, moves)
                end
            
            and unfreeze (graph, colors, moves) =
                let fun getMoveNodeDegree moveNode = IG.inDegree (IG.getNode (graph, (IG.getNodeID moveNode)))
                
                    fun unfreezeNode frozenNode = 
                        let val moves' = IG.remove(moves, frozenNode)
                        in
                            simplify (graph, colors, moves')
                        end

                    (* Filter move nodes to those that are trivial and are not precolored *)
                    val uncoloredMoveNodes = List.filter (fn (node) => not (M.inDomain (colors, (IG.getNodeID node)))) (IG.nodes moves)
                    val trivialMoveNodes = List.filter (fn (node) => (getMoveNodeDegree node) < numColors) uncoloredMoveNodes
                in
                    if ((List.length trivialMoveNodes) = 0) then
                        potentialSpill (graph, colors, moves)
                    else
                        unfreezeNode (
                            List.foldl 
                                (fn (node, bestNode) => if (getMoveNodeDegree(node) > getMoveNodeDegree(bestNode)) then node else bestNode) 
                                (List.hd trivialMoveNodes)
                                (List.drop (trivialMoveNodes, 1))
                        )
                end

            and potentialSpill (graph, colors, moves) = 
                let val uncoloredNodes = List.filter (fn (node) => not (M.inDomain(colors, IG.getNodeID(node)))) (IG.nodes graph)
                    fun spillNode spilledNode = 
                        let val graph' = IG.remove(graph, spilledNode)
                            val moves' = IG.remove(moves, spilledNode)
                            val (graph'', colors') = simplify (graph', colors, moves')
                            val neighboringColors = IG.foldSuccs (fn (succId, succColors) => S.add(succColors, M.lookup(colors', succId))) S.empty spilledNode
                            val possibleColors = S.toList(S.difference(mipsColorable, neighboringColors))

                            (* Try to color possible spilled node. If no possible color give value of -1 to denote spill. *)
                            val color = if ((List.length possibleColors) > 0) then List.hd (possibleColors) else ~1
                            (* val _ = print ("color chosen: " ^ (Int.toString color) ^ "\n") *)
                            val colors'' = M.insert(colors', IG.getNodeID(spilledNode), color)
                        in
                            (graph, colors'')
                        end
                in
                    spillNode (
                        List.foldl (fn (node, bestNode) => if (IG.inDegree(node) > IG.inDegree(bestNode)) then node else bestNode)
                        (List.hd(uncoloredNodes))
                        (List.drop(uncoloredNodes, 1))
                    )
                end


            val (_, colors) = simplify (graph, initialColors, moves)
        in
            colors
        end

    
    structure A = Assem
    
    fun applyColors (instrs, coloring) = 
        let fun color temp = M.lookup(coloring, temp)
            fun colorInstr ((A.MOVE {assem, dst, src}) :: instrs) = 
                    let val dst' = color dst
                        val src' = color src
                    in
                        if (src' = dst') then (
                            (* print((Int.toString src') ^ " " ^ (Int.toString dst') ^ "\n"); *)
                            (colorInstr (instrs))
                        ) else (
                            (* print((Int.toString src') ^ " " ^ (Int.toString dst') ^ "\n"); *)
                            (A.MOVE{assem=assem, dst=dst', src=src'}) :: (colorInstr (instrs))
                        )
                    end
            |   colorInstr ((A.OPER {assem, dst=dsts, src=srcs, jump}) :: instrs) =
                    let val dsts' = List.map color dsts 
                        val srcs' = List.map color srcs
                    in
                        (A.OPER{assem=assem, dst=dsts', src=srcs', jump=jump}) :: (colorInstr (instrs))
                    end
            |   colorInstr (instr :: instrs) = instr :: (colorInstr (instrs))
            |   colorInstr ([]) = []

        in
            colorInstr (instrs)
        end

    fun spill (frame, instrs, colors) = 
        let val spillBase = !(MipsFrame.maxArgs frame)
            val _ = (MipsFrame.maxArgs frame) := !(MipsFrame.maxArgs frame) + (M.numItems colors)
            fun procSpill (temp, (instrs, spillAddr)) = 
                let val spillTemp = ref (Temp.newtemp())
                    fun contains lst = List.exists (fn t => t = temp) lst
                    fun replace lst = List.map (fn t => if t = temp then !spillTemp else t) lst

                    fun sw () = (
                        spillTemp := Temp.newtemp();
                        [A.OPER{assem="sw `s0, " ^ (Int.toString spillAddr) ^ "(`s1)\n", src=[!spillTemp, MipsFrame.SP], dst=[], jump=NONE}]
                    )
                    fun lw () = [A.OPER{assem="lw `d0, " ^ (Int.toString spillAddr) ^ "(`s0)\n", src=[MipsFrame.SP], dst=[!spillTemp], jump=NONE}]
                    fun procInstr ([]) = []
                    |   procInstr (instr as (A.OPER{assem, src, dst, jump}) :: instrs) =  
                        let val (ld, src') = if (contains src) then (lw (), replace src) else (nil, src)
                            val (st, dst') = if (contains dst) then (sw (), replace dst) else (nil, dst)
                        in
                            ld @ [(A.OPER{assem=assem, src=src', dst=dst', jump=jump})] @ st @ procInstr(instrs)
                        end
                            
                    |   procInstr (instr as (A.MOVE{assem, src, dst}) :: instrs) =  
                        let val (ld, src') = if (src = temp) then (lw (), !spillTemp) else (nil, src)
                            val (st, dst') = if (dst = temp) then (sw (), !spillTemp) else (nil, dst)
                        in
                            ld @ [(A.MOVE{assem=assem, src=src', dst=dst'})] @ st @ procInstr(instrs)
                        end
                    |   procInstr (instr :: instrs) = instr :: procInstr(instrs)
                in
                    (procInstr (instrs), spillAddr + 4)
                end
            val (instrs', _) = List.foldr procSpill (instrs, spillBase * 4) (M.listKeys colors)
        in
            (frame, instrs')
        end


    fun allocate (frame, instrs) = 
        let val instrs' = MipsFrame.procEntryExit2(frame, instrs)
            val dataflow = MakeGraph.instrs2graph instrs'
            (* val _ = Flow.debugGraph dataflow *)
            val interference = Interference.dataflow2interference dataflow
            (* val _ = Interference.printGraph interference *)
            val colors = color interference
            (* val _ = printColors colors *)

            fun colorSpilled color = color = ~1
            val (frame', instrs'') = spill (frame, instrs, M.filter colorSpilled colors)
        in
            if (M.exists colorSpilled colors) then (
                allocate (frame', instrs'')
            ) else (
                applyColors (instrs', colors)
            )
            (* instrs'' *)
        end
end