module Clause

open TacitCypher
open Query

type ClauseOps () =
    
    static member MATCH (path: Path): Query<unit> = Query<_>()

    static member MATCH (path: BoundPath<'a>): Query<'a> = Query<_>()