module PatternLangBind

open System
open Serialization

type Direction = Undirected | Left | Right

type GraphStep (name: String option, labels: String list, properties: obj option) = class end
    //member _.Name = name
    //member _.Labels = labels
    //member _.Properties = properties
    
type Rel (name: String option, labels: String list, properties: obj option) =
    inherit GraphStep (name, labels, properties)
    
//type IPath =
//    abstract member AsPath: unit -> Path
//    abstract member Step: Rel -> Direction -> IPath -> IPath
    
//and
type Node (name: String option, labels: String list, properties: obj option) =
    inherit GraphStep (name, labels, properties)
    //interface IPath with
    //    member this.Step rel direction path =
    //        Next (this, rel, direction, path)
    //    member this.AsPath () = Just this
//and
type Path =
    | Just of Node
    | Next of Node * Rel * Direction * Path
    //interface IPath with
        member this.Step rel dir path =
            match this with
            | Just n -> Next (n, rel, dir, path) //(n :> IGraphPath).Step rel path
            | Next (n, r, d, rest) ->
                Next(n, r, d, rest.Step rel dir path)
        //member this.AsPath () = this
    
module Util' =
    let asObj o = o :> obj

type Util () =
    static member N<'T>(?props: 'T): Path =
        Node(None, [typeof<'T>.Name], Option.map Util'.asObj props)
        |> Path.Just
    
module Test =

    open type Util

    let AnonRel = Rel (None, [], None)
    let AnonNode = Node (None, [], None)
    
    let (-|)  (lhs: Path) rhs = lhs, rhs, Undirected
    let (<-|) (lhs: Path) rhs = lhs, rhs, Left

    let (|-)  (start: Path, rel, dir) rhs = start.Step rel dir rhs
    let (|->) (start: Path, rel, _) rhs = start.Step rel Right rhs

    let (--)  lhs rhs = lhs  -| AnonRel |- rhs
    let (-->) lhs rhs = lhs  -| AnonRel |-> rhs
    let (<--) lhs rhs = lhs <-| AnonRel |- rhs
    
    
    type MyType = {
        Value: int
    }
    
    let mutable a = N<MyType>()
    let mutable b = N({Value = 5})
    
    let p0 = a

    let p1 = a --> b