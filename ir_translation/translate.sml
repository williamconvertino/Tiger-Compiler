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
  type level = Frame.frame
  type access = level * Frame.access

  val outermost: level = MipsFrame.newFrame {name=(Temp.newlabel()), formals=[]}

  fun newLevel {parent, name, formals} = MipsFrame.newFrame {name=name, formals=(true::formals)}

  fun formals (name, acclist, locals, shift) = (List.map (fn (frameAccess) => ((name, acclist, locals, shift), frameAccess)) acclist)

  fun allocLocal lev escapes = (lev, MipsFrame.allocLocal lev escapes)
end
