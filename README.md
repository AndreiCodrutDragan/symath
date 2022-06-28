# symath
The application parse math expressions & compute derivatives.

```shell
   input: let exp = ParseExpression "log(cos(x))" |> Derivative |> FormatExpression
   output: val exp: string = "-(sin(x) / x)"
```
