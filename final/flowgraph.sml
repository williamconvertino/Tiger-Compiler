structure Block =
struct
      structure LabelKey : ORD_KEY = 
      struct 
            type ord_key = Temp.label 
            val compare = (fn (l1, l2) => String.compare (Symbol.name l1, Symbol.name l2))
      end

      structure TargetSet = SplaySetFn(LabelKey)
      
      
      type block = {instrs: Assem.instr list, defs: IntRedBlackSet.set, uses: IntRedBlackSet.set, targets: TargetSet.set}

      val empty = {instrs=[], defs=IntRedBlackSet.empty, uses=IntRedBlackSet.empty, targets=TargetSet.empty}

      fun withLabel label = {instrs=[], defs=IntRedBlackSet.empty, uses=IntRedBlackSet.empty, targets=TargetSet.add(TargetSet.empty, label)}

      fun toString {instrs, defs, uses, targets} = 
            let val instrsStr = List.foldr (fn (instr, str) => "\t" ^ Assem.format(Temp.makestring) instr ^ str) "" instrs 
                fun foldTemps (temp, str) = (Temp.makestring temp) ^ ", " ^ str
            in
                "Defs: " ^ (IntRedBlackSet.foldr foldTemps "" defs) ^ "\nUses: " ^ (IntRedBlackSet.foldr foldTemps "" uses) ^ "\n" ^ instrsStr
            end
end


structure Flow =
struct

      structure LabelKey : ORD_KEY = 
      struct 
            type ord_key = Temp.label 
            val compare = (fn (l1, l2) => String.compare (Symbol.name l1, Symbol.name l2))
      end

      structure Graph = FuncGraph(LabelKey)

      type graph = Block.block Graph.graph

      fun addBlock (graph, block as {instrs=(Assem.LABEL {assem, lab})::instrs, defs, uses, targets}) = Graph.addNode(graph, lab, block)
      |   addBlock (graph, block) = (print ("Error: cannot add block to dataflow graph since block does not begin with label.\n"); graph)
        

      fun debugGraph graph = Graph.printGraph (fn (id) => (Symbol.name id)) (fn (id, block) => "---" ^ (Symbol.name id) ^ "---\n" ^ Block.toString block) graph
      fun printGraph graph = Graph.printGraph (fn (id) => (Symbol.name id)) (fn (id, block) => (Symbol.name id)) graph


      (* datatype flowgraph = FGRAPH of {
                              control: Graph.graph,
                              def: Temp.temp list Graph.Table.table,
				      use: Temp.temp list Graph.Table.table,
				      ismove: bool Graph.Table.table
                        } *)

      (* Note:  any "use" within the block is assumed to be BEFORE a "def" 
      of the same variable.  If there is a def(x) followed by use(x)
      in the same block, do not mention the use in this data structure,
      mention only the def.

      More generally:
      If there are any nonzero number of defs, mention def(x).
      If there are any nonzero number of uses BEFORE THE FIRST DEF,
      mention use(x).

      For any node in the graph,  
      Graph.Table.look(def,node) = SOME(def-list)
      Graph.Table.look(use,node) = SOME(use-list)
      *)

end
