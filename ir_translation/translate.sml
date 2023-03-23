signature TRANSLATE =
sig
  type exp
  type level
  type access (* not the same as Frame.access *)
  
  val outermost : level
  val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
  val formals: level -> access list
  val allocLocal: level -> bool -> access
  
  (* val procEntryExit : {level: level, body: exp} -> unit

  structure Frame : FRAME
  val getResult : unit -> Frame.frag list *)
end

structure Frame = MipsFrame

structure Translate : TRANSLATE = struct 
  type exp = Tree.exp
  datatype level = TOP | LEVEL of (level * Frame.frame) * unit ref

  type access = level * Frame.access

  val outermost: level = TOP

  fun newLevel {parent, name, formals} = LEVEL((parent, MipsFrame.newFrame {name=name, formals=(true::formals)}), ref ())

  fun formals (TOP) = []
  |   formals (LEVEL(level)) = 
        let val (_, acclist, _, _) = (#2 (#1 level))
        in
          List.map (fn (frameAccess) => (LEVEL(level), frameAccess)) acclist
        end
  


  fun allocLocal (TOP) escapes = (TOP, MipsFrame.allocR0())
  |   allocLocal (LEVEL(lev)) escapes = 
        let val ((_, frame), _) = lev
        in
          (LEVEL(lev), (MipsFrame.allocLocal frame escapes))
        end
end
