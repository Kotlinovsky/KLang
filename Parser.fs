module Parser
open FParsec

// 0) Вспомогательные функции.
let isValidChar c =
    ['A'..'Z'] @ ['a'..'z']
    |> Seq.exists (fun ch -> ch = c)

// 1) Описываем типы токенов языка.
type KLang = 
    | KInteger of int
    | KLet of string * KLang
    | KLambda of string list * KLang list
    | KClosure of string list * KLang list * Map<string, KLang>
    | KVariableRef of string
    | KBinaryOp of KLang * KLang * string
    | KReturn of KLang
    | KCall of string * KLang list
    | KIf of KLang * KLang list

// 3) Создаем парсер и используем указатель на него.
let expr, exprRef = createParserForwardedToRef<KLang, unit>()

// 4) Декларируем токены парсера.
// 4.1) Объявим корневые токены языка.
let kblock = between (pstring "{" >>. spaces) (spaces >>. pstring "}") (sepEndBy (expr) (pstring ";" >>. spaces))
let kinteger = pint32 |>> KInteger
let kvariable = manySatisfy isValidChar |>> KVariableRef
let klambda = 
    pipe2 (pstring "lambda" >>. pstring " " >>. (sepBy (manySatisfy isValidChar) (pstring "," >>. spaces))) (spaces >>. kblock) 
        (fun (args_names) (expression) -> KLambda(args_names, expression))
// 4.2) Теперь объявим операции для вычислимых типов.
let calculable_values = choice [kinteger; kvariable]
let supported_binary_ops = choice [pstring "+"; pstring "-"; pstring "*"; pstring "/"; pstring "<"; pstring "<="; pstring "=="; pstring "!="; pstring ">"; pstring ">="]
let kbinaryop = pipe3 (calculable_values) (supported_binary_ops) (calculable_values) 
                    (fun (left) (op) (right) -> KBinaryOp(left, right, op))
// 4.3) Объявим саму конструкцию let и ее возможные rvalue (способы инициализации).
let kcall = pipe2 (manySatisfy isValidChar) (between (pstring "(") (pstring ")") (sepEndBy (calculable_values) (pstring "," >>. spaces)))
                (fun (name) (args) -> KCall(name, args));
let krvalue = attempt klambda <|> attempt kbinaryop <|> attempt kcall <|> attempt kinteger <|> attempt kvariable
let klet = 
    pipe2 (pstring "let" >>. spaces >>. manySatisfy isValidChar) (spaces >>. pstring "=" >>. spaces >>. krvalue) 
        (fun (name: string) (expression) -> KLet(name, expression))
let kreturn = pstring "return" >>. spaces >>. krvalue |>> KReturn;
let kif = pstring "if" >>. pipe2 (spaces >>. kbinaryop) (spaces >>. kblock) 
                            (fun (binary_op) (block) -> KIf(binary_op, block))

// 5) Конфигурируем парсер.
do exprRef.Value <- choice [attempt klet; attempt kreturn; attempt kcall; attempt kif]
