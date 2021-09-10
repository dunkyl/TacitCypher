module Clause

open TacitCypher
open Pattern
open Query

type ClauseOps () =
    
    static member MATCH (path: Path): Query<unit> = Query<_>()

    static member MATCH (path: Path<'a>): Query<'a> = Query<_>()