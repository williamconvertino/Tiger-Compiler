signature TRANSLATE =
sig
  type level
  type access (* not sure if this happens here Pg 141 Book => = level * Frame.access *)
  val outermost : level
  val newLevel : {parent: level, name: Temp.label,
  formals: bool list} -> level
  val formals: level -> access list
  val allocLocal: level -> bool -> access
  val procEntryExit : {level: level, body: exp} -> unit

  structure Frame : FRAME
  val getResult : unit -> Frame.frag list
end

structure Frame = MipsFrame
structure Translate : TRANSLATE = struct type exp = unit end
