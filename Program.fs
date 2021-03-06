open System


type Expression =
    | X
    | Const of float
    | Neg of Expression
    | Add of Expression * Expression
    | Sub of Expression * Expression
    | Mul of Expression * Expression
    | Div of Expression * Expression
    | Pow of Expression * Expression
    | Exp of Expression
    | Log of Expression
    | Sin of Expression
    | Cos of Expression

let (|Op|_|) (x : Expression) =
    match x with
    | Add(e1, e2) -> Some(Add, e1, e2)
    | Sub(e1, e2) -> Some(Sub, e1, e2)
    | Mul(e1, e2) -> Some(Mul, e1, e2)
    | Div(e1, e2) -> Some(Div, e1, e2)
    | Pow(e1, e2) -> Some(Pow, e1, e2)
    | _ -> None

let (|Func|_|) (x : Expression) =
    match x with
    | Exp(e) -> Some(Exp, e)
    | Log(e) -> Some(Log, e)
    | Sin(e) -> Some(Sin, e)
    | Cos(e) -> Some(Cos, e)
    | _ -> None

let rec Simplify x : Expression =
    match x with
    | Add(Const(n1), Const(n2)) -> Const(n1 + n2)
    | Sub(Const(n1), Const(n2)) -> Const(n1 - n2)
    | Mul(Const(n1), Const(n2)) -> Const(n1 * n2)
    | Div(Const(n1), Const(n2)) -> Const(n1 / n2)
    | Neg(Const(0.)) -> Const(0.)
    | Neg(Neg(e)) -> e |> Simplify
    | Add(e, Const(0.)) -> e |> Simplify
    | Add(Const(0.), e) -> e |> Simplify
    | Add(Const(n), e) -> Add(e, Const(n)) |> Simplify
    | Add(e1, Neg(e2)) -> Sub(e1, e2) |> Simplify
    | Add(Neg(e1), e2) -> Sub(e2, e1) |> Simplify
    | Sub(e, Const(0.)) -> e |> Simplify
    | Sub(Const(0.), e) -> Neg(e) |> Simplify
    | Mul(e, Const(1.)) -> e |> Simplify
    | Mul(Const(1.), e) -> e |> Simplify
    | Mul(e, Const(0.)) -> Const(0.)
    | Mul(Const(0.), e) -> Const(0.)
    | Mul(e, Const(n)) -> Mul(Const(n), e) |> Simplify
    | Mul(Div(Const(n), e1), e2) -> Mul(Const(n), Div(e2, e1)) |> Simplify
    | Mul(e1, Div(Const(n), e2)) -> Mul(Const(n), Div(e1, e2)) |> Simplify
    | Mul(Neg(e1), e2) -> Neg(Mul(e1, e2)) |> Simplify
    | Mul(e1, Neg(e2)) -> Neg(Mul(e1, e2)) |> Simplify
    | Div(Const(0.), e) -> Const(0.)
    | Div(e, Const(1.)) -> e |> Simplify
    | Div(Neg(e1), e2) -> Neg(Div(e1, e2)) |> Simplify
    | Div(e1, Neg(e2)) -> Neg(Div(e1, e2)) |> Simplify
    | Pow(Const(0.), e) -> Const(0.)
    | Pow(Const(1.), e) -> Const(1.)
    | Pow(e, Const(0.)) -> Const(1.)
    | Pow(e, Const(1.)) -> e |> Simplify
    | Op (op, e1, e2)
        ->
        let e1s = Simplify e1
        let e2s = Simplify e2
        if e1s <> e1 || e2s <> e2 then
            op(Simplify e1, Simplify e2) |> Simplify
        else
            op(e1, e2)
    | _ -> x

let rec Derivative x : Expression =
   let y =
       match x with
       | X -> Const(1.)
       | Const(n) -> Const(0.)
       | Neg(e) -> Neg(Derivative(e))
       | Add(e1, e2) -> Add(Derivative(e1), Derivative(e2))
       | Sub(e1, e2) -> Sub(Derivative(e1), Derivative(e2))
       | Mul(e1, e2) -> Add(Mul(Derivative(e1), e2), Mul(e1, Derivative(e2)))
       | Pow(e, Const(n)) -> Mul(Const(n), Pow(e, Const(n-1.)))
       | Pow(Const(n), e) -> Mul(Mul(Log(Const(n)), Pow(Const(n), e)), Derivative(e))
       | Exp(X) -> Exp(X)
       | Log(X) -> Div(Const(1.), X)
       | Sin(X) -> Cos(X)
       | Cos(X) -> Neg(Sin(X))
       | Div(Const(1.), e) -> Div(Derivative(e), Pow(e, Const(2.)))
       | Func(g, f) ->
           let dg = Derivative(g(X))
           let df = Derivative(f)
           match dg with
           | Func(dgf, dge) -> Mul(dgf(f), df)
           | Op (op, e1, e2) -> Mul(op(e1, e2), df)
           | _ -> failwith(sprintf "Unable to match compound function [%A]" dg)
       | _ -> failwith(sprintf "Unable to match expression [%A]" x)
   Simplify y

let OpName (e: Expression) : string =
    match e with
    | Add(e1, e2) -> "+"
    | Sub(e1, e2) -> "-"
    | Mul(e1, e2) -> "*"
    | Div(e1, e2) -> "/"
    | Pow(e1, e2) -> "^"
    | _ -> failwith(sprintf "Unrecognized operator [%A]" e)

let FuncName (e: Expression) (a : string) : string =
    match e with
    | Exp(x) -> sprintf "e^(%s)" a
    | Log(x) -> sprintf "log(%s)" a
    | Sin(x) -> sprintf "sin(%s)" a
    | Cos(x) -> sprintf "cos(%s)" a
    | _ -> failwith(sprintf "Unrecognized function [%A]" e)

let FormatExpression x =
    let rec FormatSubExpression (outer : Expression option, inner : Expression) : string =
        match inner with
        | X -> "x"
        | Const(n) -> sprintf "%f" n
        | Neg x -> sprintf "-%s" (FormatSubExpression(Some(inner), x))
        | Op(op, e1, e2) ->
            let s = FormatSubExpression(Some(inner), e1) + " " + OpName(inner) + " " + FormatSubExpression(Some(inner), e2)
            match outer with
            | None -> s
            | _ -> "(" + s + ")"
        | Func(f, e) -> FuncName(inner) (FormatSubExpression(None, e))
    FormatSubExpression(None, x)

let IsOperator (x : string) =
    match x with
    | "+" | "-" | "*" | "/" | "^" -> true
    | _ -> false

let rec LevelTokens (lst : string list) (level : int) : (string * int) list =
    match lst with
    | [] -> []
    | "(" :: tail -> LevelTokens tail (level+1)
    | ")" :: tail -> LevelTokens tail (level-1)
    | x :: tail when IsOperator(x) -> (x, level) :: LevelTokens tail level
    | head :: tail -> (head, level) :: LevelTokens tail level

let GroupTokens (item : (string * int)) (acc : (string list * int) list) : (string list * int) list =
    match acc, item with
    | [], (s, l) -> [([s], l)]
    | (s1, l1) :: tail, (s, l) when l = l1 -> (s :: s1, l) :: tail
    | head :: tail, (s, l) -> ([s], l) :: head :: tail

let IsFunction (x : string) =
    match x with
    | "e" | "log" | "sin" | "cos" -> true
    | _ -> false

let (|ToVar|_|) s =
    if s = "x" then
        Some(X)
    else
        None

let ApplyOperator (op : string, e1 : Expression, e2 : Expression) : Expression =
    match op with
    | "+" -> Add(e1, e2)
    | "-" -> Sub(e1, e2)
    | "*" -> Mul(e1, e2)
    | "/" -> Div(e1, e2)
    | "^" -> Pow(e1, e2)
    | _ -> failwith(sprintf "Unrecognized operator [%s]" op)

let ApplyFunction (func : string, e : Expression) : Expression =
    match func with
    | "e" -> Exp(e)
    | "log" -> Log(e)
    | "sin" -> Sin(e)
    | "cos" -> Cos(e)
    | _ -> failwith(sprintf "Unrecognized function [%s]" func)

let (|ToConst|_|) s =
    let success, result = Double.TryParse(s.ToString())
    if success then
        Some(Const(result))
    else
        None

let ParseItem (s : string) : Expression =
    match s with
    | ToVar e -> e
    | ToConst e -> e

let Tokenize (value : System.String) =
    let value = value.Replace(" ", "")
    let value = value.Replace("e^(", "e(")
    let value = value.Replace("(", " ( ")
    let value = value.Replace(")", " ) ")
    let value = value.Replace("+", " + ")
    let value = value.Replace("-", " - ")
    let value = value.Replace("*", " * ")
    let value = value.Replace("/", " / ")
    let value = value.Replace("^", " ^ ")
    value.Trim().Split([|' '|]) |> Seq.toList |> List.filter (fun e -> e.Length > 0)

let rec ParseExpression (s : string) : Expression =

    let rec LevelTokens (lst : string list) (level : int) : (string * int) list =
        match lst with
        | [] -> []
        | "(" :: tail -> LevelTokens tail (level+1)
        | ")" :: tail -> LevelTokens tail (level-1)
        | x :: tail when IsOperator(x) -> (x, level) :: LevelTokens tail level
        | head :: tail -> (head, level) :: LevelTokens tail level

    let GroupTokens (item : (string * int)) (acc : (string list * int) list) : (string list * int) list =
        match acc, item with
        | [], (s, l) -> [([s], l)]
        | (s1, l1) :: tail, (s, l) when l = l1 -> (s :: s1, l) :: tail
        | head :: tail, (s, l) -> ([s], l) :: head :: tail

    let rec MergeExpressions (e : Expression, items : string list) : Expression =
        match items with
        | [] -> e
        | op :: x :: tail when IsOperator(op) -> MergeExpressions(ApplyOperator(op, e, ParseItem(x)), tail)
        | x :: op :: tail when IsOperator(op) -> MergeExpressions(ApplyOperator(op, ParseItem(x), e), tail)
        | _ -> failwith(sprintf "Unable to build expression from [%A]" items)

    let ParseFlatExpression (tokens : string list) : Expression =
        match tokens with
        | [] -> failwith("Expression string is empty")
        | "-" :: x :: tail -> MergeExpressions(Neg(ParseItem(x)), tail)
        | x :: tail -> MergeExpressions(ParseItem(x), tail)

    let rec MergeTokensWithExpressions (e : Expression, items : (string list) list) : Expression =
        match items with
        | [] -> e
        | [[func]] when IsFunction(func) -> ApplyFunction(func, e)
        | [op; x] :: tail when IsOperator(op) -> MergeTokensWithExpressions(ApplyOperator(op, e, ParseItem(x)), tail)
        | [x; op] :: tail when IsOperator(op) -> MergeTokensWithExpressions(ApplyOperator(op, ParseItem(x), e), tail)
        | (op::x::rest) :: tail when IsOperator(op) -> MergeTokensWithExpressions(ApplyOperator(op, e, ParseFlatExpression(x::rest)), tail)
        | (x::op::y::rest) :: tail when IsOperator(op) -> ApplyOperator(op, ParseItem(x), MergeTokensWithExpressions(e, (y::rest)::tail))
        | _ -> failwith(sprintf "Unable to build expression from [%A]" items)

    let rec ParseTokenGroups (lst : (string list) list) : Expression =
        match lst with
        | [ls] -> ParseFlatExpression(ls)
        | ls :: [op] :: tail when IsOperator(op) -> ApplyOperator(op, ParseFlatExpression(ls), ParseTokenGroups(tail))
        | ls :: [op :: optail] when IsOperator(op) -> MergeTokensWithExpressions(ParseFlatExpression(ls), [op :: optail])
        | ls :: (op :: optail) :: tail when IsOperator(op) -> ApplyOperator(op, ParseFlatExpression(ls), MergeTokensWithExpressions(ParseTokenGroups(tail), [optail]))
        | ls :: tail -> MergeTokensWithExpressions(ParseTokenGroups(tail), [ls])

    let leveledTokens = (Tokenize s |> LevelTokens) 0
    let tokenGroups = List.foldBack GroupTokens leveledTokens [] |> List.map(fun (x, y) -> x)
    ParseTokenGroups(tokenGroups)
