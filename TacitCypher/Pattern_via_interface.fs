module Pattern

open Serialization

type Direction = Undirected | Left | Right with
    member this.Combine rhs =
        if this = Right then Right else rhs

type MaybeNamed<'T> = ('T * string option)

type Rel = {
    Label: string option
    Props: obj option 
} with
    static member Any = { Label = None; Props = None }

    member this.Serialize name =
        "[" + SerializeInner name ConformRelationLabel this.Label this.Props + "]"

    override this.ToString () = this.Serialize None

type Rel<'B> = {
    Name: string
    Rel: Rel
} with
    static member Any name = { Name = name; Rel = Rel.Any }

    override this.ToString () = this.Rel.Serialize (Some this.Name)

type IPath =
    abstract member First: Node
    abstract member Rest: (Direction * Rel * Node) list

and Node = {
    Label: string option
    Props: obj option 
} with
    interface IPath with
        member this.First = this
        member this.Rest = []

    static member Any = { Label = None; Props = None }

    member this.Serialize name =
        "(" + SerializeInner name ConformNodeLabel this.Label this.Props + ")"

    override this.ToString () = this.Serialize None

type IPath<'A> =
    abstract member First: Node MaybeNamed
    abstract member Rest: (Direction * Rel MaybeNamed * Node MaybeNamed) list

type Node<'A> = {
    Name: string
    Node: Node
} with
    interface IPath<'A> with
        member this.First = this.Node, Some this.Name
        member this.Rest = []

    override this.ToString () = this.Node.Serialize (Some this.Name)

type Partial = {
    Path: IPath
    Direction: Direction
    Relation: Rel
}

type Partial<'A> = {
    Path: IPath<'A>
    Direction: Direction
    Relation: Rel MaybeNamed
}

let rec serializePath (firstnode: MaybeNamed<Node>) rest =
    let first, firstname = firstnode
    first.Serialize firstname +
    match rest with
    | [] -> ""
    | (dir, (rel, (relname: string option)), node)::rest ->
        let la, ra =
            match dir with
            | Direction.Undirected -> "-", "-"
            | Direction.Left -> "<-", "-"
            | Direction.Right -> "-", "->"
        la +

        if rel = Rel.Any && relname.IsNone then
            ""
        else
            rel.Serialize relname
        + ra
        + serializePath node rest

let makeRestNamed l = List.map (fun (dir, rel, node) -> dir, (rel, None), (node, None)) l

type Path = {
    First: Node
    Rest: (Direction * Rel * Node) list
} with
    interface IPath with
        member this.First = this.First
        member this.Rest = this.Rest

    static member Unit = { First = Node.Any; Rest = [] }

    override this.ToString() =
        serializePath (this.First, None) (makeRestNamed this.Rest)

type Path<'A> = {
    First: Node MaybeNamed
    Rest: (Direction * Rel MaybeNamed * Node MaybeNamed) list
} with
    interface IPath<'A> with
        member this.First = this.First
        member this.Rest = this.Rest

    override this.ToString() =
        serializePath this.First this.Rest

let promote<'a> (path: IPath) =
    {
        First = path.First, None
        Rest = makeRestNamed path.Rest
    } :> IPath<'a>

type Rel with
    member this.Step (lhs: IPath, dir): Partial = {
        Path = lhs
        Direction = dir
        Relation = this
    }
    member this.Step (lhs: IPath<'A>, dir): Partial<'A> = {
        Path = lhs
        Direction = dir
        Relation = this, None
    }

type Rel<'B> with
    member this.Step (lhs: IPath, dir): Partial<'A> = {
        Path = promote<'A> lhs
        Direction = dir
        Relation = this.Rel, Some this.Name
    }
    member this.Step (lhs: IPath<'A>, dir): Partial<'A * 'B> = {
        Path = { First = lhs.First; Rest = lhs.Rest }
        Direction = dir
        Relation = this.Rel, Some this.Name
    }

type Partial with
    member this.Step (rhs: IPath, dir: Direction): Path =
        {
            First = this.Path.First
            Rest = List.append this.Path.Rest ((dir.Combine this.Direction, this.Relation, rhs.First)::rhs.Rest)
        }
    member this.Step (rhs: IPath<'B>, dir: Direction) =
        {
            First = this.Path.First, None
            Rest = List.append
                (makeRestNamed this.Path.Rest)
                ((dir.Combine this.Direction, (this.Relation, None), rhs.First)::rhs.Rest)
        }

type Partial<'A> with
    member this.Step (rhs: IPath, dir: Direction): Path<'A> =
        {
            First = this.Path.First
            Rest = List.append
                this.Path.Rest
                ((dir.Combine this.Direction, this.Relation, (rhs.First, None))
                ::(makeRestNamed rhs.Rest))
        }
    member this.Step (rhs: IPath<'B>, dir: Direction): Path<'A * 'B> =
        {
            First = this.Path.First
            Rest = List.append
                this.Path.Rest
                ((dir.Combine this.Direction, this.Relation, rhs.First)
                ::rhs.Rest)
        }

// Infix ops

type Rel with
    static member (  -| ) (lhs: IPath,    rhs: Rel) = rhs.Step (lhs, Undirected)
    static member (  -| ) (lhs: IPath<_>, rhs: Rel) = rhs.Step (lhs, Undirected)
    static member (  -| ) ((),            rhs: Rel) = Path.Unit -| rhs
    static member ( <-| ) (lhs: IPath,    rhs: Rel) = rhs.Step (lhs, Left)
    static member ( <-| ) (lhs: IPath<_>, rhs: Rel) = rhs.Step (lhs, Left)
    static member ( <-| ) ((),            rhs: Rel) = Path.Unit <-| rhs

type Rel<'B> with 
    static member (  -| ) (lhs: IPath,    rhs: Rel<_>) = rhs.Step (lhs, Undirected)
    static member (  -| ) (lhs: IPath<_>, rhs: Rel<_>) = rhs.Step (lhs, Undirected)
    static member (  -| ) ((),            rhs: Rel<_>) = Path.Unit -| rhs
    static member ( <-| ) (lhs: IPath,    rhs: Rel<_>) = rhs.Step (lhs, Left)
    static member ( <-| ) (lhs: IPath<_>, rhs: Rel<_>) = rhs.Step (lhs, Left)
    static member ( <-| ) ((),            rhs: Rel<_>) = Path.Unit <-| rhs

type Partial with
    static member ( |-  ) (lhs: Partial, rhs: IPath)    = lhs.Step (rhs, Undirected)
    static member ( |-  ) (lhs: Partial, rhs: IPath<_>) = lhs.Step (rhs, Undirected)
    static member ( |-  ) (lhs: Partial, ())            = lhs |- Path.Unit
    static member ( |-> ) (lhs: Partial, rhs: IPath)    = lhs.Step (rhs, Right)
    static member ( |-> ) (lhs: Partial, rhs: IPath<_>) = lhs.Step (rhs, Right)
    static member ( |-> ) (lhs: Partial, ())            = lhs |-> Path.Unit

type Partial<'A> with
    static member ( |-  ) (lhs: Partial<_>, rhs: IPath)    = lhs.Step (rhs, Undirected)
    static member ( |-  ) (lhs: Partial<_>, rhs: IPath<_>) = lhs.Step (rhs, Undirected)
    static member ( |-  ) (lhs: Partial<_>, ())            = lhs |- Path.Unit
    static member ( |-> ) (lhs: Partial<_>, rhs: IPath)    = lhs.Step (rhs, Right)
    static member ( |-> ) (lhs: Partial<_>, rhs: IPath<_>) = lhs.Step (rhs, Right)
    static member ( |-> ) (lhs: Partial<_>, ())            = lhs |-> Path.Unit

type Path with
    static member (  --  ) (lhs: Path, rhs: IPath)    = lhs :> IPath  -| Rel.Any |-  rhs
    static member (  --  ) (lhs: Path, rhs: IPath<_>) = lhs :> IPath  -| Rel.Any |-  rhs
    static member (  --  ) (lhs: unit, rhs: Path)     = lhs -| Rel.Any |- rhs
    static member (  --  ) (lhs: Path, rhs: unit)     = lhs -| Rel.Any |- rhs
    static member (  --> ) (lhs: Path, rhs: IPath)    = lhs :> IPath  -| Rel.Any |-> rhs
    static member (  --> ) (lhs: Path, rhs: IPath<_>) = lhs :> IPath  -| Rel.Any |-> rhs
    static member (  --> ) (lhs: unit, rhs: Path)     = lhs -| Rel.Any |-> rhs
    static member (  --> ) (lhs: Path, rhs: unit)     = lhs -| Rel.Any |-> rhs
    static member ( <--  ) (lhs: Path, rhs: IPath)    = lhs :> IPath <-| Rel.Any |-  rhs
    static member ( <--  ) (lhs: Path, rhs: IPath<_>) = lhs :> IPath <-| Rel.Any |-  rhs
    static member ( <--  ) (lhs: unit, rhs: Path)     = lhs <-| Rel.Any |- rhs
    static member ( <--  ) (lhs: Path, rhs: unit)     = lhs <-| Rel.Any |- rhs

type Path<'A> with
    static member (  --  ) (lhs: Path<_>, rhs: IPath)    = lhs :> IPath<_>  -| Rel.Any |-  rhs
    static member (  --  ) (lhs: Path<_>, rhs: IPath<_>) = lhs :> IPath<_>  -| Rel.Any |-  rhs
    static member (  --  ) (lhs: unit, rhs: Path<_>)     = lhs -| Rel.Any |- rhs
    static member (  --  ) (lhs: Path<_>, rhs: unit)     = lhs -| Rel.Any |- rhs
    static member (  --> ) (lhs: Path<_>, rhs: IPath)    = lhs :> IPath<_>  -| Rel.Any |-> rhs
    static member (  --> ) (lhs: Path<_>, rhs: IPath<_>) = lhs :> IPath<_>  -| Rel.Any |-> rhs
    static member (  --> ) (lhs: unit, rhs: Path<_>)     = lhs -| Rel.Any |-> rhs
    static member (  --> ) (lhs: Path<_>, rhs: unit)     = lhs -| Rel.Any |-> rhs
    static member ( <--  ) (lhs: Path<_>, rhs: IPath)    = lhs :> IPath<_> <-| Rel.Any |-  rhs
    static member ( <--  ) (lhs: Path<_>, rhs: IPath<_>) = lhs :> IPath<_> <-| Rel.Any |-  rhs
    static member ( <--  ) (lhs: unit, rhs: Path<_>)     = lhs <-| Rel.Any |- rhs
    static member ( <--  ) (lhs: Path<_>, rhs: unit)     = lhs <-| Rel.Any |- rhs

type Node with
    static member (  --  ) (lhs: Node, rhs: IPath)    = lhs :> IPath  -| Rel.Any |-  rhs
    static member (  --  ) (lhs: Node, rhs: IPath<_>) = lhs :> IPath  -| Rel.Any |-  rhs
    static member (  --  ) (lhs: unit, rhs: Node)     = lhs -| Rel.Any |- rhs
    static member (  --  ) (lhs: Node, rhs: unit)     = lhs -| Rel.Any |- rhs
    static member (  --> ) (lhs: Node, rhs: IPath)    = lhs :> IPath  -| Rel.Any |-> rhs
    static member (  --> ) (lhs: Node, rhs: IPath<_>) = lhs :> IPath  -| Rel.Any |-> rhs
    static member (  --> ) (lhs: unit, rhs: Node)     = lhs -| Rel.Any |-> rhs
    static member (  --> ) (lhs: Node, rhs: unit)     = lhs -| Rel.Any |-> rhs
    static member ( <--  ) (lhs: Node, rhs: IPath)    = lhs :> IPath <-| Rel.Any |-  rhs
    static member ( <--  ) (lhs: Node, rhs: IPath<_>) = lhs :> IPath <-| Rel.Any |-  rhs
    static member ( <--  ) (lhs: unit, rhs: Node)     = lhs <-| Rel.Any |- rhs
    static member ( <--  ) (lhs: Node, rhs: unit)     = lhs <-| Rel.Any |- rhs

type Node<'A> with
    static member (  --  ) (lhs: Node<_>, rhs: IPath)    = lhs :> IPath<_>  -| Rel.Any |-  rhs
    static member (  --  ) (lhs: Node<_>, rhs: IPath<_>) = lhs :> IPath<_>  -| Rel.Any |-  rhs
    static member (  --  ) (lhs: unit, rhs: Node<'A>)     = lhs -| Rel.Any |- rhs
    static member (  --  ) (lhs: Node<_>, rhs: unit)     = lhs -| Rel.Any |- rhs
    static member (  --> ) (lhs: Node<_>, rhs: IPath)    = lhs :> IPath<_>  -| Rel.Any |-> rhs
    static member (  --> ) (lhs: Node<_>, rhs: IPath<_>) = lhs :> IPath<_>  -| Rel.Any |-> rhs
    static member (  --> ) (lhs: unit, rhs: Node<_>)     = lhs -| Rel.Any |-> rhs
    static member (  --> ) (lhs: Node<_>, rhs: unit)     = lhs -| Rel.Any |-> rhs
    static member ( <--  ) (lhs: Node<_>, rhs: IPath)    = lhs :> IPath<_> <-| Rel.Any |-  rhs
    static member ( <--  ) (lhs: Node<_>, rhs: IPath<_>) = lhs :> IPath<_> <-| Rel.Any |-  rhs
    static member ( <--  ) (lhs: unit, rhs: Node<_>)     = lhs <-| Rel.Any |- rhs
    static member ( <--  ) (lhs: Node<_>, rhs: unit)     = lhs <-| Rel.Any |- rhs

[<AutoOpen>]
module Helpers' =
    let N (props: 't) = { Node.Label = Some (typeof<'t>.Name); Props = Some (props :> obj)  }
    let BindN (props: 't) name = { Name = name; Node = N props }: Node<'t>

    let R (props: 't) = { Rel.Label = Some (typeof<'t>.Name); Props = Some (props :> obj)  }
    let BindR (props: 't) name = { Name = name; Rel = R props }: Rel<'t>

module Ops =

    let nodeu = Node.Any
    let relu = Rel.Any
    let pathu = nodeu -- nodeu

    let partu = nodeu -| relu

    let pathb = BindN () "a" -| (Rel.Any) |- nodeu