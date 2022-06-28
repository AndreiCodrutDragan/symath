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

let rec Derivative x : Expression =
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