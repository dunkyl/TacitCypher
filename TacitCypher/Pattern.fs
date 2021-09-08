namespace TacitCypher

open Serialization

type Node =
    {
        Label: string option
        Props: obj option 
    }

    static member Empty = { Label = None; Props = None}

    static member Labeled label =
        { Node.Empty with Label = Some label}

    member this.Serialize name =
        "(" + SerializeInner name ConformNodeLabel this.Label this.Props + ")"

    override this.ToString () = this.Serialize None

type BoundNode<'T> =
    {
        Name: string
        Node: Node
    }

    override this.ToString () = this.Node.Serialize (Some this.Name)

type Rltn =
    {
        Label: string option
        Props: obj option 
    }

    static member Empty = { Label = None; Props = None}

    static member Labeled label =
        { Rltn.Empty with Label = Some label}

    member this.Serialize name =
        "[" + SerializeInner name ConformRelationLabel this.Label this.Props + "]"

    override this.ToString () = this.Serialize None

type BoundRltn<'T> =
    {   
        Name: string
        Rltn: Rltn
    }

    override this.ToString () = this.Rltn.Serialize (Some this.Name)

//type PartialDirection = Undirected | Left
type Direction = Undirected | Left | Right

type Path = 
    {
        First: Node
        Rest: (Direction * Rltn * Node) list
    }

    static member OfNode n: Path = { First = n; Rest = [] }

    static member Unit = (Path.OfNode Node.Empty)

    static member Connect lhs (dir, rel) rhs =
        {lhs with Rest = List.append lhs.Rest ((dir, rel, rhs.First)::rhs.Rest) }

    static member OfObj (o: 'T) =
        match (o :> obj) with
        | :? unit ->
            Path.OfNode (Node.Empty)
        | :? Node as n ->
            Path.OfNode n
        | otherObj ->
            raise (System.ArgumentException())
            Path.OfNode ({ Node.Empty with Props = Some otherObj})

    member this.Serialize() =
        this.First.Serialize(None) +
        match this.Rest with
        | [] -> ""
        | (dir, rel, node)::rest ->
            let la, ra =
                match dir with
                | Direction.Undirected -> "-", "-"
                | Direction.Left -> "<-", "-"
                | Direction.Right -> "-", "->"
            la +

            if rel = Rltn.Empty then
                ""
            else
                rel.Serialize(None)
            + ra
            + node.Serialize(None)

    override this.ToString() =
        this.Serialize()

type BoundPath<'Return> =
    {
        First: Node * string option
        Rest: (Direction * (Rltn * string option) * (Node * string option)) list
    }

    static member Connect (lhs: BoundPath<'a>, dir, rel: Rltn, rhs: BoundPath<'b>): BoundPath<'a * 'b> =
        { First = lhs.First; Rest = List.append lhs.Rest ((dir, (rel, None), rhs.First)::rhs.Rest) }

    static member Connect (lhs: BoundPath<'a>, dir, rel: Rltn, rhs: Path): BoundPath<'a> =
        let rest' = List.map (fun (dir, rel, node) -> (dir, (rel, None), (node, None))) rhs.Rest
        { First = lhs.First; Rest = List.append lhs.Rest ((dir, (rel, None), (rhs.First, None))::rest') }

    static member Connect (lhs: BoundPath<'a>, dir, rel: BoundRltn<'b>, rhs: BoundPath<'c>): BoundPath<'a * 'b * 'c> =
        { First = lhs.First; Rest = List.append lhs.Rest ((dir, (rel.Rltn, Some rel.Name), rhs.First)::rhs.Rest) }

    static member Connect (lhs: BoundPath<'a>, dir, rel: BoundRltn<'b>, rhs: Path): BoundPath<'a * 'b> =
        let rest' = List.map (fun (dir, rel, node) -> (dir, (rel, None), (node, None))) rhs.Rest
        { First = lhs.First; Rest = List.append lhs.Rest ((dir, (rel.Rltn, Some rel.Name), (rhs.First, None))::rest') }

    static member Connect (lhs: Path, dir, rel: BoundRltn<'a>, rhs: BoundPath<'b>): BoundPath<'a * 'b> =
        let rest' = List.map (fun (dir, rel, node) -> (dir, (rel, None), (node, None))) lhs.Rest
        { First = (lhs.First, None); Rest = List.append rest' ((dir, (rel.Rltn, Some rel.Name), rhs.First)::rhs.Rest) }

    static member Connect (lhs: Path, dir, rel: Rltn, rhs: BoundPath<'a>): BoundPath<'a> =
        let rest' = List.map (fun (dir, rel, node) -> (dir, (rel, None), (node, None))) lhs.Rest
        { First = (lhs.First, None); Rest = List.append rest' ((dir, (rel, None), rhs.First)::rhs.Rest) }

    static member OfBoundNode (a: BoundNode<'T>): BoundPath<'T> =
        { First = a.Node, Some a.Name; Rest = [] }
    
    static member OfPath<'T> (p: Path): BoundPath<'T> =
        { First = p.First, None; Rest = List.map (fun (dir, rel, node) -> (dir, (rel, None), (node, None))) p.Rest }

    member this.Serialize() =
        let first, firstname = this.First
        first.Serialize firstname +
        match this.Rest with
        | [] -> ""
        | (dir, (rel, relname), (node, nodename))::rest ->
            let la, ra =
                match dir with
                | Direction.Undirected -> "-", "-"
                | Direction.Left -> "<-", "-"
                | Direction.Right -> "-", "->"
            la +

            if rel = Rltn.Empty then
                ""
            else
                rel.Serialize relname
            + ra
            + node.Serialize nodename

    override this.ToString() =
        this.Serialize()

type Partial =
    {
        Path: Path
        Direction: Direction
        Relation: Rltn
    }

    static member Of (path, dir, rel) =
        { Path = path; Direction = dir; Relation = rel }

    member this.Finish (isRight, path: Path) =
        let combinedDir =
            if isRight then
                Right
            else
                this.Direction
        Path.Connect
            this.Path
            (combinedDir, this.Relation)
            path

    member this.Finish (isRight, node: Node) = this.Finish (isRight, Path.OfNode node)

type BoundPartial<'PathAndRel> =
    {
        Path: BoundPath<'PathAndRel>
        Direction: Direction
        Relation: Rltn
        RelName: string option
    }

// OPERTATORS

// path producing operators:
// infix (5) : --, -->, <--, |-, |->
// LHS (4+N) : () | Node | BoundNode<'T> | PathUnbound | Path<'Bound>
// RHS (4+N) : () | Node | BoundNode<'T> | PathUnbound | Path<'Bound>
// except (5): () op ()
// total count = 75 + 40N + 5N²


//  LHS   OP   RHS    IN
// ------------------------

// undirected

// unit  --  unit        ❌ ?
// unit  --  Node    Node ✅
// unit  -|  Rltn    Rltn ✅
// unit  --  Path    Path ✅
// unit  --  :Node
// unit  -|  :Rltn
// unit  --  :Path
// Node  --  unit    Node ✅
// Node  --  Node    Node ✅
// Node  -|  Rltn    Rltn ✅
// Node  --  Path    Path ✅
// Node  --  :Node   Node ✅
// Node  -|  :Rltn
// Node  --  :Path
// Path  --  unit    Path ✅
// Path  --  Node    Path ✅
// Path  -|  Rltn    Path ✅
// Path  --  Path    Path ✅
// Path  --  :Node   Path ✅
// Path  -|  :Rltn
// Path  --  :Path   Path ✅
// Part  |-  unit    Part ✅
// Part  |-  Node    Node ✅
// Part  |-  Path    Path ✅
// Part  |-  :Node   
// Part  |-  :Path 
// :Node  --  unit 
// :Node  --  Node   Node ✅
// :Node  -|  Rltn  
// :Node  --  Path   Path ✅
// :Node  --  :Node
// :Node  -|  :Rltn
// :Node  --  :Path
// :Path  --  unit 
// :Path  --  Node  
// :Path  -|  Rltn  
// :Path  --  Path   Path ✅
// :Path  --  :Node
// :Path  -|  :Rltn
// :Path  --  :Path
// :Part  |-  unit 
// :Part  |-  Node  
// :Part  |-  Path   Path ✅
// :Part  --  :Node
// :Part  --  :Path

[<AutoOpen>]
module Ops = begin end

    //let ( -- ) () () = Path.Connect Path.EmptyNode (Undirected, Rltn.Empty) Path.EmptyNode

type Path with

    // ... -- ...
    static member ( -- ) (lhs, rhs) = Path.Connect lhs (Undirected, Rltn.Empty) rhs
    static member ( --> ) (lhs, rhs) = Path.Connect lhs (Right, Rltn.Empty) rhs
    static member ( <-- ) (lhs, rhs) = Path.Connect lhs (Left, Rltn.Empty) rhs

    // ... -- :...
    static member ( -- ) (lhs: Path, rhs: BoundPath<'a>): BoundPath<'a> = BoundPath<'a>.Connect (lhs, Undirected, Rltn.Empty, rhs)
    static member ( --> ) (lhs: Path, rhs: BoundPath<'a>): BoundPath<'a> = BoundPath<'a>.Connect (lhs, Right, Rltn.Empty, rhs)
    static member ( <-- ) (lhs: Path, rhs: BoundPath<'a>): BoundPath<'a> = BoundPath<'a>.Connect (lhs, Left, Rltn.Empty, rhs)

    // :... -- ...
    static member ( -- ) (lhs: BoundPath<'a>, rhs: Path): BoundPath<'a> = BoundPath<'a>.Connect (lhs, Undirected, Rltn.Empty, rhs)
    static member ( --> ) (lhs: BoundPath<'a>, rhs: Path): BoundPath<'a> = BoundPath<'a>.Connect (lhs, Right, Rltn.Empty, rhs)
    static member ( <-- ) (lhs: BoundPath<'a>, rhs: Path): BoundPath<'a> = BoundPath<'a>.Connect (lhs, Left, Rltn.Empty, rhs)

    // ... -- ()
    static member ( --  ) (lhs: Path, ()) = lhs --  Path.Unit
    static member ( --> ) (lhs: Path, ()) = lhs --> Path.Unit
    static member ( <-- ) (lhs: Path, ()) = lhs <-- Path.Unit

    // () -- ...
    static member ( --  ) ((), rhs: Path) = Path.Unit --  rhs
    static member ( --> ) ((), rhs: Path) = Path.Unit --> rhs
    static member ( <-- ) ((), rhs: Path) = Path.Unit <-- rhs

    // ... -- (node)
    static member ( -- ) (lhs: Path, rhs) = lhs -- Path.OfNode rhs
    static member ( --> ) (lhs: Path, rhs) = lhs --> Path.OfNode rhs
    static member ( <-- ) (lhs: Path, rhs) = lhs <-- Path.OfNode rhs

    // ... -- (:node)
    static member ( -- ) (lhs: Path, rhs: BoundNode<'a>): BoundPath<'a> = lhs -- BoundPath<_>.OfBoundNode rhs
    static member ( --> ) (lhs: Path, rhs: BoundNode<'a>): BoundPath<'a> = lhs --> BoundPath<_>.OfBoundNode rhs
    static member ( <-- ) (lhs: Path, rhs: BoundNode<'a>): BoundPath<'a> = lhs <-- BoundPath<_>.OfBoundNode rhs

    // (node) -- ...
    static member ( -- ) (lhs, rhs: Path) = Path.OfNode lhs -- rhs
    static member ( --> ) (lhs, rhs: Path) = Path.OfNode lhs --> rhs
    static member ( <-- ) (lhs, rhs: Path) = Path.OfNode lhs <-- rhs

    // (:node) -- ...
    static member ( -- ) (lhs: BoundNode<'a>, rhs: Path): BoundPath<'a> = BoundPath<_>.OfBoundNode lhs -- rhs
    static member ( --> ) (lhs: BoundNode<'a>, rhs: Path): BoundPath<'a> = BoundPath<_>.OfBoundNode lhs --> rhs
    static member ( <-- ) (lhs: BoundNode<'a>, rhs: Path): BoundPath<'a> = BoundPath<_>.OfBoundNode lhs <-- rhs


    // ... -| [rel]
    static member (-|) (lhs: Path, rhs: Rltn): Partial =
        Partial.Of (lhs, Undirected, rhs)
    static member (<-|) (lhs: Path, rhs: Rltn): Partial =
        Partial.Of (lhs, Left, rhs)

    // ... -| [:rel]
    static member (-|) (lhs: Path, rhs: BoundRltn<'a>): BoundPartial<'a> =
        {
            Path = BoundPath<'a>.OfPath lhs
            Direction = Undirected
            Relation = rhs.Rltn
            RelName = Some rhs.Name
        }
    static member (<-|) (lhs: Path, rhs: BoundRltn<'a>): BoundPartial<'a> =
        {
            Path = BoundPath<'a>.OfPath lhs
            Direction = Left
            Relation = rhs.Rltn
            RelName = Some rhs.Name
        }

    // {partial} |- ...
    static member ( |- ) (part: Partial, rhs: Path) =
        part.Finish (false, rhs)
    static member ( |-> ) (part: Partial, rhs: Path) =
        part.Finish (true, rhs)

    // {:partial} |- ...
    static member ( |- ) (part: BoundPartial<'a>, rhs: Path): BoundPath<'a> =
        let dir =
            if part.Direction = Left then Left else Undirected
        let rest' = List.map (fun (dir, rel, node) -> (dir, (rel, None), (node, None))) rhs.Rest
        { First = part.Path.First; Rest = List.append part.Path.Rest ((dir, (part.Relation, part.RelName), (rhs.First, None))::rest') }
    static member ( |-> ) (part: BoundPartial<'a>, rhs: Path): BoundPath<'a> =
        let rest' = List.map (fun (dir, rel, node) -> (dir, (rel, None), (node, None))) rhs.Rest
        { First = part.Path.First; Rest = List.append part.Path.Rest ((Right, (part.Relation, part.RelName), (rhs.First, None))::rest') }

type Node with
    
    // () -- (node)
    static member ( -- ) ((), rhs) = Path.Unit -- Path.OfNode rhs
    static member ( --> ) ((), rhs) = Path.Unit --> Path.OfNode rhs
    static member ( <-- ) ((), rhs) = Path.Unit <-- Path.OfNode rhs

    // (node) -- ()
    static member ( -- ) (lhs, ()) = Path.OfNode lhs -- Path.Unit
    static member ( --> ) (lhs, ()) = Path.OfNode lhs --> Path.Unit
    static member ( <-- ) (lhs, ()) = Path.OfNode lhs <-- Path.Unit

    // (node) -- (node)
    static member ( -- ) (lhs, rhs) = Path.OfNode lhs -- Path.OfNode rhs
    static member ( --> ) (lhs, rhs) = Path.OfNode lhs --> Path.OfNode rhs
    static member ( <-- ) (lhs, rhs) = Path.OfNode lhs <-- Path.OfNode rhs

    // (node) -- (:node)
    static member ( -- ) (lhs, rhs: BoundNode<'a>) = Path.OfNode lhs -- BoundPath<'a>.OfBoundNode rhs
    static member ( --> ) (lhs, rhs: BoundNode<'a>) = Path.OfNode lhs --> BoundPath<'a>.OfBoundNode rhs
    static member ( <-- ) (lhs, rhs: BoundNode<'a>) = Path.OfNode lhs <-- BoundPath<'a>.OfBoundNode rhs

    // (:node) -- (node)
    static member ( -- ) (lhs: BoundNode<'a>, rhs) = BoundPath<'a>.OfBoundNode lhs -- Path.OfNode rhs
    static member ( --> ) (lhs: BoundNode<'a>, rhs) = BoundPath<'a>.OfBoundNode lhs --> Path.OfNode rhs
    static member ( <-- ) (lhs: BoundNode<'a>, rhs) = BoundPath<'a>.OfBoundNode lhs <-- Path.OfNode rhs

    // {partial} |- (node)
    static member ( |- ) (part, rhs) = part |- Path.OfNode rhs
    static member ( |-> ) (part, rhs) = part |- Path.OfNode rhs

    // {:partial} |- (node)
    static member ( |- ) (part: BoundPartial<'a>, rhs): BoundPath<'a> = part |- Path.OfNode rhs
    static member ( |-> ) (part: BoundPartial<'a>, rhs): BoundPath<'a> = part |-> Path.OfNode rhs

    // :... -- (node)
    static member ( -- ) (lhs: BoundPath<'a>, rhs) = lhs -- Path.OfNode rhs
    static member ( --> ) (lhs: BoundPath<'a>, rhs) = lhs --> Path.OfNode rhs
    static member ( <-- ) (lhs: BoundPath<'a>, rhs) = lhs <-- Path.OfNode rhs

    // (node) -- :...
    static member ( -- ) (lhs, rhs: BoundPath<'a>) = Path.OfNode lhs -- rhs
    static member ( --> ) (lhs, rhs: BoundPath<'a>) = Path.OfNode lhs --> rhs
    static member ( <-- ) (lhs, rhs: BoundPath<'a>) = Path.OfNode lhs <-- rhs

type Rltn with
    
    // () -| [rel]
    static member ( -| ) ((), rhs) = Partial.Of (Path.Unit, Undirected, rhs)
    static member ( <-| ) ((), rhs) = Partial.Of (Path.Unit, Left, rhs)

    // (node) -| [rel]
    static member ( -| ) (lhs, rhs: Rltn) = Partial.Of (Path.OfNode lhs, Undirected, rhs)
    static member ( <-| ) (lhs, rhs: Rltn) = Partial.Of (Path.OfNode lhs, Left, rhs)

    // (:node) -| [rel]
    static member ( -| ) (lhs: BoundNode<'a>, rhs: Rltn): BoundPartial<'a> =
        {
            Path = BoundPath<'a>.OfBoundNode lhs
            Direction = Undirected
            Relation = rhs
            RelName = None
        }
    static member ( <-| ) (lhs: BoundNode<'a>, rhs: Rltn): BoundPartial<'a> =
        {
            Path = BoundPath<'a>.OfBoundNode lhs
            Direction = Left
            Relation = rhs
            RelName = None
        }

type Partial with

    // {partial} |- ()
    static member ( |- ) (part, ()) = part |- Path.Unit
    static member ( |-> ) (part, ()) = part |-> Path.Unit

type BoundPath<'T> with
    
    // :... -- :...
    static member ( -- ) (lhs: BoundPath<'a>, rhs: BoundPath<'b>): BoundPath<'a * 'b> = BoundPath<'a>.Connect (lhs, Undirected, Rltn.Empty, rhs)
    static member ( --> ) (lhs: BoundPath<'a>, rhs: BoundPath<'b>): BoundPath<'a * 'b> = BoundPath<'a>.Connect (lhs, Right, Rltn.Empty, rhs)
    static member ( <-- ) (lhs: BoundPath<'a>, rhs: BoundPath<'b>): BoundPath<'a * 'b> = BoundPath<'a>.Connect (lhs, Left, Rltn.Empty, rhs)

    // :... -- ()
    static member ( --  ) (lhs: BoundPath<'a>, ()) = lhs --  Path.Unit
    static member ( --> ) (lhs: BoundPath<'a>, ()) = lhs --> Path.Unit
    static member ( <-- ) (lhs: BoundPath<'a>, ()) = lhs <-- Path.Unit

    // () -- :...
    static member ( --  ) ((), rhs: BoundPath<'a>) = Path.Unit --  rhs
    static member ( --> ) ((), rhs: BoundPath<'a>) = Path.Unit --> rhs
    static member ( <-- ) ((), rhs: BoundPath<'a>) = Path.Unit <-- rhs

    // :... -- (:node)
    static member ( -- ) (lhs: BoundPath<'a>, rhs: BoundNode<'b>): BoundPath<'a * 'b> =
        BoundPath<'a>.Connect (lhs, Undirected, Rltn.Empty, BoundPath<'b>.OfBoundNode rhs)
    static member ( --> ) (lhs: BoundPath<'a>, rhs: BoundNode<'b>): BoundPath<'a * 'b> =
        BoundPath<'a>.Connect (lhs, Right, Rltn.Empty, BoundPath<'b>.OfBoundNode rhs)
    static member ( <-- ) (lhs: BoundPath<'a>, rhs: BoundNode<'b>): BoundPath<'a * 'b> =
        BoundPath<'a>.Connect (lhs, Left, Rltn.Empty, BoundPath<'b>.OfBoundNode rhs)

    // (:node) -- :...
    static member ( -- ) (lhs: BoundNode<'a>, rhs: BoundPath<'b>): BoundPath<'a * 'b> =
        BoundPath<'a>.Connect (BoundPath<'a>.OfBoundNode lhs, Undirected, Rltn.Empty, rhs)
    static member ( --> ) (lhs: BoundNode<'a>, rhs: BoundPath<'b>): BoundPath<'a * 'b> =
        BoundPath<'a>.Connect (BoundPath<'a>.OfBoundNode lhs, Right, Rltn.Empty, rhs)
    static member ( <-- ) (lhs: BoundNode<'a>, rhs: BoundPath<'b>): BoundPath<'a * 'b> =
        BoundPath<'a>.Connect (BoundPath<'a>.OfBoundNode lhs, Left, Rltn.Empty, rhs)

    // {partial} |- ...
    static member ( |- ) (part: Partial, rhs: BoundPath<'a>): BoundPath<'a> =
        let dir =
            if part.Direction = Left then Left else Undirected
        BoundPath<'a>.Connect (part.Path, dir, part.Relation, rhs)
    static member ( |-> ) (part: Partial, rhs: BoundPath<'a>): BoundPath<'a> =
        BoundPath<'a>.Connect (part.Path, Right, part.Relation, rhs)

    // :... -| [rel]
    static member (-|) (lhs: BoundPath<'a>, rhs: Rltn): BoundPartial<'a> =
        {
            Path = lhs
            Direction = Undirected
            Relation = rhs
            RelName = None
        }
    static member (<-|) (lhs: BoundPath<'a>, rhs: Rltn): BoundPartial<'a> =
        {
            Path = lhs
            Direction = Left
            Relation = rhs
            RelName = None
        }

    // :... -| [:rel]
    static member (-|) (lhs: BoundPath<'a>, rhs: BoundRltn<'b>): BoundPartial<'a * 'b> =
        {
            Path = { First = lhs.First; Rest = lhs.Rest }
            Direction = Undirected
            Relation = rhs.Rltn
            RelName = Some rhs.Name
        }
    static member (<-|) (lhs: BoundPath<'a>, rhs: BoundRltn<'b>): BoundPartial<'a * 'b> =
        {
            Path = { First = lhs.First; Rest = lhs.Rest }
            Direction = Left
            Relation = rhs.Rltn
            RelName = Some rhs.Name
        }

type BoundNode<'T> with
    
    // () -- (:node)
    static member ( -- ) ((), rhs: BoundNode<'a>) = Path.Unit -- BoundPath<'a>.OfBoundNode rhs
    static member ( --> ) ((), rhs: BoundNode<'a>) = Path.Unit --> BoundPath<'a>.OfBoundNode rhs
    static member ( <-- ) ((), rhs: BoundNode<'a>) = Path.Unit <-- BoundPath<'a>.OfBoundNode rhs

    // (:node) -- ()
    static member ( -- ) (lhs: BoundNode<'a>, ()) =  BoundPath<'a>.OfBoundNode lhs -- Path.Unit
    static member ( --> ) (lhs: BoundNode<'a>, ()) =  BoundPath<'a>.OfBoundNode lhs --> Path.Unit
    static member ( <-- ) (lhs: BoundNode<'a>, ()) =  BoundPath<'a>.OfBoundNode lhs <-- Path.Unit

    // {partial} |- (:node)
    static member ( |- ) (part, rhs: BoundNode<'a>): BoundPath<'a> = part |- BoundPath<'a>.OfBoundNode rhs
    static member ( |-> ) (part, rhs: BoundNode<'a>): BoundPath<'a> = part |- BoundPath<'a>.OfBoundNode rhs

    // (:node) -- (:node)
    static member ( -- ) (lhs: BoundNode<'a>, rhs: BoundNode<'b>) = BoundPath<'a>.OfBoundNode lhs -- BoundPath<'b>.OfBoundNode rhs
    static member ( --> ) (lhs: BoundNode<'a>, rhs: BoundNode<'b>) = BoundPath<'a>.OfBoundNode lhs --> BoundPath<'b>.OfBoundNode rhs
    static member ( <-- ) (lhs: BoundNode<'a>, rhs: BoundNode<'b>) = BoundPath<'a>.OfBoundNode lhs <-- BoundPath<'b>.OfBoundNode rhs

type BoundRltn<'T> with
    
    // () -| [:rel]
    static member ( -| ) ((), rhs: BoundRltn<'a>): BoundPartial<'a> =
        {
            Path = { First = (Node.Empty, None); Rest = [] }
            Direction = Undirected
            Relation = rhs.Rltn
            RelName = Some rhs.Name
        }
    static member ( <-| ) ((), rhs: BoundRltn<'a>): BoundPartial<'a> =
        {
            Path = { First = (Node.Empty, None); Rest = [] }
            Direction = Left
            Relation = rhs.Rltn
            RelName = Some rhs.Name
        }

    // (node) -| [:rel]
    static member ( -| ) (lhs: Node, rhs: BoundRltn<'a>): BoundPartial<'a> =
        {
            Path = { First = (lhs, None); Rest = [] }
            Direction = Undirected
            Relation = rhs.Rltn
            RelName = Some rhs.Name
        }
    static member ( <-| ) (lhs: Node, rhs: BoundRltn<'a>): BoundPartial<'a> =
        {
            Path = { First = (lhs, None); Rest = [] }
            Direction = Left
            Relation = rhs.Rltn
            RelName = Some rhs.Name
        }

    // (:node) -| [:rel]
    static member ( -| ) (lhs: BoundNode<'a>, rhs: BoundRltn<'b>): BoundPartial<'a * 'b> =
        {
            Path = { First = (lhs.Node, Some lhs.Name); Rest = [] }
            Direction = Undirected
            Relation = rhs.Rltn
            RelName = Some rhs.Name
        }
    static member ( <-| ) (lhs: BoundNode<'a>, rhs: BoundRltn<'b>): BoundPartial<'a * 'b> =
        {
            Path = { First = (lhs.Node, Some lhs.Name); Rest = [] }
            Direction = Left
            Relation = rhs.Rltn
            RelName = Some rhs.Name
        }

type BoundPartial<'T> with

    // {:partial} |- :...
    static member ( |- ) (part: BoundPartial<'a>, rhs: BoundPath<'b>): BoundPath<'a * 'b> =
        { First = part.Path.First; Rest = List.append part.Path.Rest ((Undirected, (part.Relation, part.RelName), rhs.First)::rhs.Rest) }
    static member ( |-> ) (part: BoundPartial<'a>, rhs: BoundPath<'b>): BoundPath<'a * 'b> =
        { First = part.Path.First; Rest = List.append part.Path.Rest ((Right, (part.Relation, part.RelName), rhs.First)::rhs.Rest) }
    
    // {:partial} |- ()
    static member ( |- ) (part: BoundPartial<'a>, ()) =
        { First = part.Path.First; Rest = List.append part.Path.Rest [(Undirected, (part.Relation, part.RelName), (Node.Empty, None))] }
    static member ( |-> ) (part: BoundPartial<'a>, ()) =
        { First = part.Path.First; Rest = List.append part.Path.Rest [(Right, (part.Relation, part.RelName), (Node.Empty, None))] }

    // {:partial} |- (:node)
    static member ( |- ) (part: BoundPartial<'a>, rhs: BoundNode<'b>): BoundPath<'a * 'b> = part |- { First = (rhs.Node, Some rhs.Name); Rest = [] }
    static member ( |-> ) (part: BoundPartial<'a>, rhs: BoundNode<'b>): BoundPath<'a * 'b> = part |-> { First = (rhs.Node, Some rhs.Name); Rest = [] }

    