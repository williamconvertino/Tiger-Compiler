structure Semant : 
sig 
    type venv = Env.enventry Symbol.table
    type tenv = Types.ty Symbol.table
    
    type expty = {exp: Translate.exp, ty: Types.ty}

    val transExp: venv * tenv * Temp.label option * Translate.level -> Absyn.exp -> expty
    val transDec: venv * tenv * Temp.label option * Absyn.dec * Translate.level -> {venv: venv, tenv: tenv}
    val transTy:  tenv * Absyn.ty -> Types.ty

    val transProg : Absyn.exp -> unit

end =

struct 

    type venv = Env.enventry Symbol.table
    type tenv = Types.ty Symbol.table
    
    type expty = {exp: Translate.exp, ty: Types.ty}

    structure A = Absyn
    structure E = Env
    structure S = Symbol
    structure T = Translate

    fun checkInt (ty, pos) = Types.checkType(ty, Types.INT, pos)

    fun checkComparisonOp (lty, rty, check_op, pos) =
        let fun checkOp (Types.INT, rty, _) = Types.checkType(rty, Types.INT, pos)
            |   checkOp (Types.STRING, rty, _) = Types.checkType(rty, Types.STRING, pos)
            |   checkOp (Types.ARRAY(lty), rty, A.EqOp) = Types.checkType(rty, Types.ARRAY(lty), pos)
            |   checkOp (Types.ARRAY(lty), rty, A.NeqOp) = Types.checkType(rty, Types.ARRAY(lty), pos)
            |   checkOp (Types.RECORD(lty), rty, A.EqOp) = Types.checkType(rty, Types.RECORD(lty), pos)
            |   checkOp (Types.RECORD(lty), rty, A.NeqOp) = Types.checkType(rty, Types.RECORD(lty), pos)
            |   checkOp (lty, rty, _) = (ErrorMsg.error pos ("types " ^ (Types.toString lty) ^ " and " ^ (Types.toString rty) ^ " cannot be compared using this comparison operator"); Types.IMPOSSIBILITY)
        in
            checkOp(lty, rty, check_op)
        end


    fun actual_ty ty = case ty of
        Types.NAME(_, tyref) => (case !tyref of
            NONE     => Types.IMPOSSIBILITY |
            SOME(ty) => actual_ty ty) |
        _            => ty

    fun lookupTypeDec (tenv, id, pos) =
        case Symbol.look(tenv, id)
            of SOME(ty) => ty
            |  NONE     => (ErrorMsg.error pos ("undefined type " ^ S.name id); Types.IMPOSSIBILITY)
    
    fun transDecs (venv, tenv, in_loop, [], _) = {venv=venv, tenv=tenv} |
        transDecs (venv, tenv, in_loop, dec::l, level) =
            let val {venv=venv', tenv=tenv'} = transDec(venv, tenv, in_loop, dec, level)
            in transDecs (venv', tenv', in_loop, l, level) end

        (* -- Types -- *)
    and transTy (tenv, A.NameTy(sym, pos)) = lookupTypeDec (tenv, sym, pos) |
        transTy (tenv, A.ArrayTy(sym, pos)) = Types.ARRAY(lookupTypeDec (tenv, sym, pos), ref ()) |
        transTy (tenv, A.RecordTy(fieldlist)) =
            let fun buildRec (recty, []) = recty |
                    buildRec ((symlst, uniq), {name, typ, escape=_, pos}::l) = buildRec (((name, lookupTypeDec (tenv, typ, pos)) :: symlst, uniq), l)
            in
                Types.RECORD (buildRec (([], ref ()), fieldlist))
            end

        (* -- Var Decs -- *)
        (* var x := exp *)
    and transDec (venv, tenv, in_loop, A.VarDec{name, typ=NONE, init, escape, pos}, level) = 
            let val {exp, ty} = transExp (venv, tenv, in_loop, level) init
                val access = Translate.allocLocal level (!escape)
            in 
                {tenv=tenv, venv=S.enter (venv, name, E.VarEntry{ty=ty, access=access})}
            end |
        (* var x: type := exp *)
        transDec (venv, tenv, in_loop, A.VarDec{name, typ=SOME((tysym, typos)), init, escape, pos}, level) =
            let val {exp, ty=initty} = transExp (venv, tenv, in_loop, level) init
                val decty = lookupTypeDec (tenv, tysym, typos)
                val evalty = Types.checkType(initty, decty, pos)
                val access = Translate.allocLocal level (!escape)
            in
                {tenv=tenv, venv=S.enter (venv, name, E.VarEntry{ty=evalty, access=access})}
            end |

        (* -- Type Decs -- *)
        (* type t = ty *)
        transDec (venv, tenv, in_loop, A.TypeDec(tydecs), _) =
            let fun setupHeaders (venv, tenv, {name=decname, ty=decty, pos=decpos}::tydecs) = 
                let val tyref = ref NONE
                    val {venv=venv', tenv=tenv'} = setupHeaders (venv, S.enter(tenv, decname, Types.NAME(decname, tyref)), tydecs)
                in
                    (
                        tyref := SOME(transTy (tenv', decty));
                        {tenv=tenv', venv=venv'}
                    )
                end
                |   setupHeaders (venv, tenv, []) = {venv=venv, tenv=tenv}

                fun cyclicGuard ({tenv, venv}) =
                    let fun checkTyDec ({name=decname, ty=decty, pos=decpos}::tydecs) =
                            let fun cycle (Types.NAME(sym, tyop), cnt) = 
                                if cnt > (Symbol.numItems tenv) 
                                then ErrorMsg.error decpos ("cycle detected in type dec " ^ Symbol.name sym)
                                else (case !tyop of
                                        SOME(ty) => cycle(ty, cnt + 1) |
                                        NONE     => ErrorMsg.error decpos ("type " ^ Symbol.name sym ^ " has not been declared"))
                                |   cycle (_, cnt) = ()
                            in
                                (cycle((transTy (tenv, decty)), 0); checkTyDec(tydecs))
                            end
                        |   checkTyDec ([]) = ()
                    in
                        (checkTyDec(tydecs); {tenv=tenv, venv=venv})
                    end
            in
                cyclicGuard (setupHeaders (venv, tenv, tydecs))
            end |
        
        (* -- Function Decs -- *)
        (* function f(a: ta, b: tb) : rt = body *)
        (* function f(a: ta, b: tb) = body *)
        transDec (venv, tenv, in_loop, A.FunctionDec(fundecs), level) =
            let fun trFun (venv, tenv, {name, params, body, pos, result}::fundecs) =
                    let val result_ty = case result of
                            SOME(rt, respos) => (case S.look(tenv, rt) of
                                SOME(resty) => resty |
                                NONE => (
                                    ErrorMsg.error respos ("result type " ^ Symbol.name rt ^ " has not been declared");
                                    Types.IMPOSSIBILITY
                                )
                            ) |
                            NONE => Types.UNIT
                        
                        fun transparam{name, typ, pos, escape} =
                            (
                                case S.look(tenv, typ)
                                    of  SOME t => {name=name, ty=t, escape=(!escape)} |
                                        NONE => (
                                            ErrorMsg.error pos ("param type " ^ Symbol.name typ ^ " has not been declared");
                                            {name=name, ty=Types.IMPOSSIBILITY, escape=(!escape)}
                                        )
                            )
                        
                        val params' = List.map transparam params
                        
                        val level' = Translate.newLevel {parent=level, name=(Temp.newlabel()), formals=(List.map #escape params')}
                        
                        val venv' = S.enter(venv, name, E.FunEntry{formals= List.map #ty params', result=result_ty, level=level', label=(Temp.newlabel())})
                        fun enterparams (venv, {name, ty, escape}::params) = 
                            let val access = Translate.allocLocal level' escape
                            in
                                S.enter(enterparams (venv, params), name, E.VarEntry{ty=ty, access=access})
                            end
                        |   enterparams (venv, []) = venv
                        

                        val {venv=venv'', tenv} = trFun (venv', tenv, fundecs)
                        val venv''' = enterparams (venv'', params')
                        val {exp=bodyexp, ty=bodyty} = transExp(venv''', tenv, NONE, level') body
                    in 
                        ( 
                            Types.checkType(bodyty, result_ty, pos);
                            {venv=venv'', tenv=tenv}
                        )
                    end

                |   trFun (venv, tenv, []) = {venv=venv, tenv=tenv}

            in
                trFun (venv, tenv, fundecs)
            end

    
    and transExp (venv, tenv, in_loop, level) = 
            (* -- Expressions -- *)
            (* IntExp *)
        let fun trexp (A.IntExp (int)) = {exp=T.const(int), ty=Types.INT}

            (* StringExp *)
            |   trexp (A.StringExp (str, pos)) = {exp=(), ty=Types.STRING}
        
            (* Arithmetic & Comparison Ops *)
            |   trexp (A.OpExp{left, oper, right, pos}) =
                    let val {lty, lexp} = trexp left
                        val {rty, rexp} = trexp right
                        fun trOpExp (A.PlusOp) = {exp=T.opExp(A.PlusOp, lexp, rexp), ty=Types.closestDescendant (checkInt(lty, pos), checkInt(rty, pos))}
                        |   trOpExp (A.MinusOp) = {exp=T.opExp(A.MinusOp, lexp, rexp), ty=Types.closestDescendant (checkInt(lty, pos), checkInt(rty, pos))}
                        |   trOpExp (A.TimesOp) = {exp=T.opExp(A.TimesOp, lexp, rexp), ty=Types.closestDescendant (checkInt(lty, pos), checkInt(rty, pos))}
                        |   trOpExp (A.DivideOp) = {exp=T.opExp(A.DivideOp, lexp, rexp), ty=Types.closestDescendant (checkInt(lty, pos), checkInt(rty, pos))}
                        |   trOpExp (compOp) = (
                            checkComparisonOp (lty, rty, compOp, pos);
                            {exp=T.opExp(compOp, lexp, rexp), ty=Types.INT}
                        )
                    in
                        trOpExp oper
                    end
            
            (* SeqExps *)
            |   trexp (A.SeqExp (exps)) =
                    let fun trseq [] = {exp=(T.seq []), ty=Types.UNIT}
                        |   trseq (seq) = 
                                let val trseqs = List.map trexp seq
                                    val seqexps = List.map (fn transexp => (#exp transexp)) trseqs
                                in
                                    {exp=(T.seq seqexps), ty=(#ty (List.last trseqs))}
                                end

                    in
                        trseq exps
                    end

            (* VarExp *)
            |   trexp (A.VarExp (var)) = trvar var

            (* NilExp *)
            |   trexp (A.NilExp) = {exp=T.const(0), ty=Types.NIL}


            (* CallExp *)
            |   trexp (A.CallExp{func, args, pos}) = 
                    let fun checkParams ([], []) = ()
                        |   checkParams (args, []) = (ErrorMsg.error pos ("too many args provided: " ^ Int.toString (List.length args)))
                        |   checkParams ([], formals) = (ErrorMsg.error pos ("missing params: " ^ (List.foldr (fn (ty, str) => str ^ Types.toString ty ^ " ") "" formals)))
                        |   checkParams (exp::args, ty::formals) = 
                                let val {exp=argexp, ty=argty} = trexp exp
                                in 
                                    (
                                        Types.checkType(argty, ty, pos); 
                                        checkParams(args, formals)
                                    )
                                end
                in
                    case Symbol.look(venv, func) of
                        SOME(E.FunEntry ({formals, result, level, label})) => (checkParams (args, formals); {exp=(), ty=result}) |
                        SOME(E.VarEntry _) => (ErrorMsg.error pos ("symbol " ^ Symbol.name func ^ " declared as a var not a function"); {exp=(), ty=Types.IMPOSSIBILITY}) |
                        NONE => (ErrorMsg.error pos ("function " ^ Symbol.name func ^ " not declared"); {exp=(), ty=Types.IMPOSSIBILITY})
                end

            (* RecordExp *)
            |   trexp (A.RecordExp{fields, typ=tysym, pos}) =
                    let fun checkType (Types.RECORD(symtyps, uniq)) = 
                            let fun stripExp xs = foldr (fn ((_,exp,_),l) =>
                            (T.exp exp)::l) [] xs (*MISSING TO CHECK THIS FOR
                            CORRECTION/ ADDED A FUN TO STRIP EXP FROM FIELD TUPLE*)
                              fun checkElements (ret, [], []) = ret |
                                    checkElements ({exp, ty}, fields, []) = (ErrorMsg.error pos ("too many fields provided to type " ^ Types.toString (Types.RECORD(symtyps, uniq))); {exp=exp, ty=Types.IMPOSSIBILITY}) |
                                    checkElements ({exp, ty}, [], tylist) = (ErrorMsg.error pos ("missing required fields from type " ^ Types.toString (Types.RECORD(symtyps, uniq))); {exp=exp, ty=Types.IMPOSSIBILITY}) |
                                    checkElements ({exp, ty}, (fieldsym, fieldexp, fieldpos)::l, tylist) = 
                                        let val fieldty = List.find (fn (tysym, tyty) => Symbol.eq(tysym, fieldsym)) tylist
                                            val filteredtylist = List.filter (fn (tysym, tyty) => not (Symbol.eq(tysym, fieldsym))) tylist
                                            val {exp, ty=trfieldty} = (trexp fieldexp)
                                            fun checkElementType (Types.IMPOSSIBILITY) = checkElements ({exp=exp, ty=Types.IMPOSSIBILITY}, l, filteredtylist) |
                                                checkElementType (ty) = checkElements ({exp=exp, ty=Types.RECORD(symtyps, uniq)}, l, filteredtylist)
                                        in
                                            case fieldty of
                                                SOME((tysym, tyty)) => checkElementType (Types.checkType (trfieldty, tyty, fieldpos)) |
                                                NONE                => (ErrorMsg.error pos ("field " ^ Symbol.name fieldsym ^ " does not exist on type " ^  Types.toString (Types.RECORD(symtyps, uniq))); 
                                                                        checkElements ({exp=exp, ty=Types.IMPOSSIBILITY}, l, filteredtylist))
                                        end
                            in
                                checkElements ({exp=T.recordExp(stripExp fields), ty=(Types.RECORD(symtyps, uniq))}, fields, symtyps)
                            end
                        |   checkType _ = (ErrorMsg.error pos ("declared type not record type: " ^ Symbol.name tysym); {exp=(), ty=Types.IMPOSSIBILITY})
                        in
                            checkType (actual_ty (lookupTypeDec (tenv, tysym, pos)))
                        end

            (* ArrayExp *)
            | trexp (A.ArrayExp{typ, size, init, pos}) =
                let val dectyp = case S.look(tenv, typ) of
                            SOME(ty) => ty |
                            NONE => (
                                ErrorMsg.error pos ("type " ^ Symbol.name typ ^ " has not been declared"); 
                                Types.IMPOSSIBILITY
                            )
                    val valty = case actual_ty dectyp of 
                                    Types.ARRAY(ty, _) => ty |
                                    _ => (
                                        ErrorMsg.error pos ("declared type not array type: " ^ Symbol.name typ ^ " is " ^ Types.toString (dectyp) ); 
                                        Types.IMPOSSIBILITY
                                    )
                    val {exp=initexp, ty=initty} = trexp init
                    val {exp=sizeexp, ty=sizety} = trexp size
                in 
                    (
                        Types.checkType(initty, valty, pos);
                        Types.checkType(sizety, Types.INT, pos);
                        {exp=T.arrayExp(#exp (trexp size), #exp (trexp init)), ty=dectyp}
                    )
                end

            (* AssignExp *)
            | trexp (A.AssignExp{var, exp, pos}) =
                let val {exp=varexp, ty=varty} = trvar var
                    val {exp=expexp, ty=expty} = trexp exp
                in
                    Types.checkType(expty, varty, pos);
                    {exp=(), ty=Types.UNIT}
                end

            (* IfExp *)
            | trexp (A.IfExp{test, then', else'=NONE, pos}) = 
                let val {exp=testexp, ty=testty} = trexp test
                in
                    Types.checkType(testty, Types.INT, pos);
                    {exp=(), ty=Types.UNIT}
                end
            | trexp (A.IfExp{test, then', else'=SOME(else'), pos}) = 
                let val {exp=testexp, ty=testty} = trexp test
                    val {exp=thenexp, ty=thenty} = trexp then'
                    val {exp=elseexp, ty=elsety} = trexp else'
                    val resty = Types.nearestAncestor(thenty, elsety, pos)
                in
                    Types.checkType(testty, Types.INT, pos);
                    {exp=(), ty=resty}
                end

            (* LetExp *)
            |   trexp (A.LetExp{decs, body, pos}) =
                    let val {venv=venv', tenv=tenv'} = transDecs(venv, tenv, in_loop, decs, level)
                    in transExp (venv', tenv', in_loop, level) body
                end

            (* WhileExp *)
            |   trexp (A.WhileExp{test, body, pos}) =
                    let val doneLabel = Temp.newlabel()
                        val {exp=testexp, ty=testty} = transExp (venv, tenv, SOME(doneLabel), level) test
                        val {exp=bodyexp, ty=bodyty} = transExp (venv, tenv, SOME(doneLabel), level) body
                    in
                        Types.checkType(testty, Types.INT, pos);
                        {exp=(T.whileLoop (testexp, bodyexp, doneLabel)), ty=Types.UNIT}
                    end

            (* ForExp *)
            |   trexp (A.ForExp{var, escape, lo, hi, body, pos}) =
                    let val loopvaraccess = Translate.allocLocal level (!escape)
                        val venv' = S.enter (venv, var, E.VarEntry{ty=Types.INT, access=loopvaraccess})
                        val {exp=loexp, ty=loty} = trexp lo
                        val {exp=hiexp, ty=hity} = trexp hi
                        val doneLabel = Temp.newlabel()
                        val {exp=bodyexp, ty=bodyty} = transExp (venv', tenv, SOME(doneLabel), level) body
                    in
                        Types.checkType(loty, Types.INT, pos);
                        Types.checkType(hity, Types.INT, pos);
                        {exp=T.forLoop(loexp, hiexp, bodyexp, doneLabel, loopvaraccess), ty=Types.UNIT}
                    end


            (* BreakExp *)
            | trexp (A.BreakExp (pos)) = (
                case in_loop of
                    SOME(label) => {exp=T.break(label), ty=Types.IMPOSSIBILITY} |
                    NONE => (ErrorMsg.error pos ("break not contained within loop")); {exp=T.nop(), ty=Types.IMPOSSIBILITY}
            )

                (* -- Vars -- *)
                (* foo *)
            and trvar (A.SimpleVar(id, pos)) = 
                (case Symbol.look(venv, id)
                    of SOME(E.VarEntry{access, ty}) => {exp=T.simpleVar(access, level), ty=(actual_ty ty)}
                    |  SOME(E.FunEntry _)   => (ErrorMsg.error pos ("undefined variable " ^ S.name id); {exp=T.simpleVar((level, 0), level), ty=Types.IMPOSSIBILITY})
                    |  NONE                 => (ErrorMsg.error pos ("undefined variable " ^ S.name id); {exp=T.simpleVar((level, 0), level), ty=Types.IMPOSSIBILITY}))

                (* foo[bar] *)
            |   trvar (A.SubscriptVar(var, exp, pos)) =
                    let fun tycheck ({exp=varexp, ty=Types.ARRAY(arrty, _)}, {exp=expexp, ty=Types.INT}) = 
                                {exp=T.subscriptVar(#exp (trvar var), #exp (trexp exp)) , ty=arrty}
                        |   tycheck ({exp=varexp, ty=Types.ARRAY(arrty, _)}, {exp=expexp, ty=_}) = (ErrorMsg.error pos ("index expression must be of type int");
                                {exp=(), ty=arrty})
                        |   tycheck ({exp=varexp, ty=_}, _) = (ErrorMsg.error pos ("cannot index non-array type");
                                {exp=(), ty=Types.IMPOSSIBILITY})
                        val {exp=varexp, ty=varty} = trvar var
                    in
                        tycheck ({exp=varexp, ty=actual_ty varty}, trexp exp) 
                    end
                (* foo.bar *)(*MISSING the weird addition of a counter to track
                field index*)
            |   trvar (A.FieldVar(var, symbol, pos)) =
                    let fun findsymty (sym, (sym', ty)::l,i) = if Symbol.eq (sym,
                        sym') then (SOME(ty),i+1) else findsymty(sym, l,i+1) |
                            findsymty (sym, [], i) = (NONE, i+1)
                        fun tycheck {exp=varexp, ty=Types.RECORD(symtys, uniq)} = (
                                case (findsymty (symbol, symtys, 0)) of
                                    (SOME(ty),ind) => {exp=T.fieldVar(#exp (trvar var),ind), ty=ty} |
                                     (NONE, _)     => (ErrorMsg.error pos ("field " ^ Symbol.name symbol ^ " not found in record type " ^ (Types.toString (Types.RECORD(symtys, uniq)))); {exp=(), ty=Types.IMPOSSIBILITY})
                            )
                        |   tycheck {exp=varexp, ty=symty} = (ErrorMsg.error pos ("must reference record type got " ^ (Types.toString symty));
                                {exp=(), ty=Types.IMPOSSIBILITY})
                        val {exp=varexp, ty=varty} = trvar var
                    in
                        tycheck {exp=varexp, ty=(actual_ty varty)}
                    end
    in
        trexp
    end

    fun transProg exp = (transExp (Env.base_venv, Env.base_tenv, NONE, Translate.outermost) exp; ())

end
