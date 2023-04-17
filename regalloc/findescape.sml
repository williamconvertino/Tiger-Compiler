structure FindEscape: sig val findEscape: Absyn.exp -> unit end =
struct
    type depth = int
    type escEnv = (depth * bool ref) Symbol.table

    structure A = Absyn

    fun traverseVar(env: escEnv, d: depth, A.SimpleVar(symbol, pos)) = 
        (case (Symbol.look (env, symbol)) of
            SOME((vd, eref)) => if (d > vd) then eref := true else ()
        |   NONE => ())
    |   traverseVar(env, d, A.FieldVar(var, symbol, pos)) = traverseVar(env, d, var)
    |   traverseVar(env, d, A.SubscriptVar(var, exp, pos)) = (traverseVar(env, d, var); traverseExp (env, d, exp))
        

    and traverseExp(env: escEnv, d: depth, s: Absyn.exp): unit =
        let fun trexp (A.VarExp(var)) = traverseVar (env, d, var)

            |   trexp (A.CallExp{func, args, pos}) = List.app trexp args
            |   trexp (A.OpExp{left, oper, right, pos}) = (trexp left; trexp right)
            |   trexp (A.RecordExp{fields, typ, pos}) = List.app (fn (_, exp, _) => trexp exp) fields
            |   trexp (A.SeqExp(seq)) = List.app (fn (exp, _) => trexp exp) seq
            |   trexp (A.AssignExp{var, exp, pos}) = (traverseVar (env, d, var); trexp exp)
            |   trexp (A.IfExp{test, then', else', pos}) = (trexp test; trexp then'; case else' of SOME(elseexp) => trexp elseexp | NONE => ())
            |   trexp (A.WhileExp{test, body, pos}) = (trexp test; trexp body)
            |   trexp (A.ForExp{var, escape, lo, hi, body, pos}) = 
                    let val env' = Symbol.enter (env, var, (d, escape))
                    in
                        trexp lo;
                        trexp hi;
                        traverseExp (env', d, body)
                    end
            |   trexp (A.LetExp{decs, body, pos}) = 
                    let val env' = traverseDecs (env, d, decs)
                    in
                        traverseExp (env', d, body)
                    end
            |   trexp (A.ArrayExp{typ, size, init, pos}) = (trexp size; trexp init)
            |   trexp _ = ()

        in
            trexp s
        end

    and traverseDecs(env: escEnv, d: depth, s: Absyn.dec list): escEnv =
            let fun trdec (A.VarDec{name, escape, typ, init, pos}, env) = (traverseExp (env, d, init); Symbol.enter (env, name, (d, escape)))
                |   trdec (A.FunctionDec(fundecs), env) = 
                        let fun trfundec ({name, params, result, body, pos}) = 
                                    let val env' = List.foldl (fn ({name, escape, typ, pos}, env) => Symbol.enter (env, name, (d+1, escape))) env params
                                    in
                                        traverseExp (env', d+1, body)
                                    end
                        in
                            List.app trfundec fundecs;
                            env
                        end
                |   trdec (_, env) = env
            in
                List.foldl trdec env s
            end

    fun findEscape(prog: Absyn.exp): unit = traverseExp (Symbol.empty, 0, prog)
end
