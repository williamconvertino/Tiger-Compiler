CM.make "sources.cm";



fun runLoop (counter:int) =
    if (counter > 1) then
        let
            val runParse = (Parse.parse ("../testcases/test" ^ (Int.toString counter) ^ ".tig"); ()) handle e => (print ("\nError in test" ^ (Int.toString counter) ^ "\n"))
        in
            runLoop (counter - 1)
        end
    else
        0

val x = runLoop 49