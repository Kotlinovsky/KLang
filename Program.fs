open FParsec
open Parser

// 1) Читаем файл.
let tree = runParserOnFile expr () "/home/kotlinovsky/Рабочий стол/Projects/KLang/examples/factorial.k" System.Text.Encoding.UTF8

// 2) Определим интерпретатор
let rec eval (expr: KLang) (env: Map<string, KLang>) = 
    match expr with 
        | KInteger(integer) -> KInteger(integer)
        | KLet(name, rvalue) -> 
            // Заглушка для фикса рекурсии.
            let rvalue_stub = 
                match rvalue with 
                    | KLambda(args, exprs) -> KClosure(args, exprs, env)
                    | _ -> rvalue
            eval rvalue (Map.add name rvalue_stub env)
        | KLambda(args, exprs) -> KClosure(args, exprs, env)
        | KVariableRef(name) -> eval (Map.find name env) env
        | KBinaryOp(left, right, op) -> apply expr env
        | KReturn(value) -> KInteger(2)
        | KCall(name, args) -> handleCall name args env
        | KIf(condition, exprs) -> KInteger(2)
        | KClosure(_, _, _) -> failwith "Not Implemented"
and apply (expr: KLang) (env: Map<string, KLang>) =
    match expr with
        | KClosure(_, exprs, env) -> applyClosure exprs env
        | KBinaryOp(left, right, op) -> applyBinaryOp left right op env
        
and handleCall (name: string) (args: KLang list) (env: Map<string, KLang>) =
    // 1. Проверяем, что вызываем кложуру.
    // 2. Если это кложура, то добавляем в окружение значения аргументов.
    let calling_closure = Map.find name env
    match calling_closure with
        | KClosure(arg_names, exps, closure_env) ->
            // todo: чекнуть, что количество аргументов != переданным аргументам.
            let args_map = Map.ofList (List.init arg_names.Length (fun i -> (arg_names[i], eval args[i] env)))
            let env_with_args = Map.fold (fun acc key value -> Map.add key value acc) env args_map
            let closure_with_env = KClosure(arg_names, exps, env_with_args)
            apply closure_with_env closure_env
and applyClosure (exprs: KLang list) (env: Map<string, KLang>) =
    // 1. Аргументы уже должны находиться в окружении.
    // 2. Пробуем выполнить выражения лямбды в окружении.
    // 3. По мере выполнения изменяем окружение и передаем его далее.
    let mutable current_env = env
    let mutable result = KInteger(1)
    let mutable Break = false
    for iter_expr in exprs do
        if not Break then
            let evaluated_expr: KLang = eval iter_expr current_env
            match iter_expr with
                | KLet(name, _) -> current_env <- Map.add name evaluated_expr current_env
                | KIf(condition, exprs) -> 
                    // Тут в качестве условия передается только бинарная операция (==, <, > и т.п.).
                    let condition_result = apply condition current_env
                    if condition_result = KInteger(1) then
                        let closure = KClosure([], exprs, env)
                        apply closure env |> ignore
                | KReturn(expr) -> 
                    result <- eval expr env
                    Break <- true
                
    result
and applyBinaryOp (left: KLang) (right: KLang) (op: string) (env: Map<string, KLang>) =
    let left_evaluated = eval left env
    let right_evaluated = eval right env
    if op = "==" then
        if right_evaluated = left_evaluated then KInteger(1) else KInteger(0)
    else if op = "!=" then
        if not (right_evaluated = left_evaluated) then KInteger(1) else KInteger(0)
    else if op = "<" then
        if left_evaluated < right_evaluated then KInteger(1) else KInteger(0)
    else if op = "<=" then
        if left_evaluated < right_evaluated then KInteger(1) else KInteger(0)
    else if op = ">" then
        if left_evaluated < right_evaluated then KInteger(1) else KInteger(0)
    else if op = ">=" then
        if left_evaluated < right_evaluated then KInteger(1) else KInteger(0)
    else if op = "+" then
        match left_evaluated with
            | KInteger(left) ->
                match right_evaluated with
                    | KInteger(right) -> KInteger(left + right)
    else if op = "-" then
        match left_evaluated with
            | KInteger(left) ->
                match right_evaluated with
                    | KInteger(right) -> KInteger(left - right)
    else if op = "*" then
        match left_evaluated with
            | KInteger(left) ->
                match right_evaluated with
                    | KInteger(right) -> KInteger(left * right)
    else if op = "/" then
        match left_evaluated with
            | KInteger(left) ->
                match right_evaluated with
                    | KInteger(right) -> KInteger(left / right)
    else
        failwith "Unsupported binary operation."

// 3) Запускаем интерпретацию если все ОК.
match tree with
| Success (result, _, _) -> 
    // todo: чекнуть отсутствие main функции
    let initial_env = Map<string, KLang> ["x", KInteger(10)]
    let main_iter_res = eval result initial_env
    let main_run_res = apply main_iter_res initial_env
    printf "%s" (main_run_res.ToString())
| Failure(error, _, _) -> printf "%s" error
