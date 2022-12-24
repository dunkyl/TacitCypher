module Clause

open System
open TacitCypher
open Pattern
open Query

type Variable<'a> = {
    Name: string
}

type Expr = class end

type Ordering = Ascending | Descending

type Subclause =
| OrderBy of Expr list * Ordering
//| OnCreate of Clause list

type Subgraph<'bind> = class end

type Where = Where of Expr

type Clause<'a> =
| Match of Subgraph<'a> * Where
| OptionalMatch of Subgraph<'a> * Where
| Return of Expr list * Subclause option
| With of Expr list * Subclause option
| Unwind of Expr * string
| Skip of uint
| Limit of uint // CyExpr<int>?
| Create of Subgraph<'a>
| Delete of Subgraph<'a>
| Merge of Subgraph<'a>
| Set of Expr * Expr
| Union of KeepDuplicates: bool
| Call of Expr * Expr list
// foreach
// call {}
// call proc
// use, load, show


[<AutoOpen>]
type ClauseOps () =
    
    static member CREATE (path: IPath): Query<unit> =
        Query<_>($"CREATE {path}")

    static member CREATE (path: IPath<'a>): Query<'a> =
        Query<_>(
            $"CREATE {path}\n\
            RETURN {String.Join(',', path.Names())}"
            )

    static member MATCH (path: IPath<'a>): Query<'a> =
        Query<_>(
            $"MATCH {path}\n\
            RETURN {String.Join(',', path.Names())}"
            )

    static member DELETE (path: IPath<'a>): Query<unit> =
        Query<_>(
            $"MATCH {path}\n\
            DELETE {String.Join(',', path.Names())}"
            )