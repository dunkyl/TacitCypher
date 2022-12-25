module Return_projection_via_quotations

open FSharp.Quotations

let (+++) a b = (a, b)

let executor (expr: Expr<'t>) : 't = null :> obj :?> 't

let p = <@ let ((x, y), z) = 0 +++ 1 +++ "b" in y, z @> |> executor