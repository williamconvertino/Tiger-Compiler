signature FRAME =
sig type frame
type access
  val newFrame : {name: Temp.label,
  formals: bool list} -> frame
  val name : frame -> Temp.label
  val formals : frame -> access list
  val allocLocal : frame -> bool -> access
end

structure MipsFrame : FRAME = struct 
    datatype access = InFrame of int I InReg of Temp.temp

end

structure Frame : FRAME = MipsFrame
