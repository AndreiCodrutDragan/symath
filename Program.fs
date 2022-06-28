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