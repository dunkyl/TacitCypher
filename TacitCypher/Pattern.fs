namespace TacitCypher

type Node =
    {
        Name: string option
        Label: string option
        Props: obj option 
    }

    static member Empty = { Name = None; Label = None; Props = None}

    static member Labeled label =
        { Node.Empty with Label = Some label}

type BoundNode<'T> =
    {
        Name: string option
        Label: string option
        Props: 'T option 
    }

type Rltn =
    {
        Name: string option
        Label: string option
        Props: obj option 
    }

    static member Empty = { Name = None; Label = None; Props = None}

    static member Labeled label =
        { Rltn.Empty with Label = Some label}

type PartialDirection = Undirected | Left
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

    //static member OfObj (o: 'T) =
    //    match (o :> obj) with
    //    | :? unit ->
    //        Path.OfNode (Node.Empty)
    //    | :? Node as n ->
    //        Path.OfNode n
    //    | otherObj ->
    //        raise (System.ArgumentException())
            //Path.OfNode ({ Node.Empty with Props = Some otherObj})

type Partial =
    {
        Path: Path
        Direction: PartialDirection
        Relation: Rltn
    }

    static member Of (path, dir, rel) =
        { Path = path; Direction = dir; Relation = rel }

    member this.Finish (dir2, path: Path) =
        let combinedDir =
            match dir2 with
            | Undirected ->
                match this.Direction with
                | PartialDirection.Undirected ->
                    Undirected
                | PartialDirection.Left ->
                    Left
            | Left -> Left
            | Right ->
                match this.Direction with
                | PartialDirection.Undirected ->
                    Right
                | PartialDirection.Left ->
                    Undirected
        Path.Connect
            this.Path
            (combinedDir, this.Relation)
            path

    member this.Finish (dir2, node: Node) = this.Finish (dir2, Path.OfNode node)

// OPERTATORS

//  LHS   OP   RHS    IN
// ------------------------

// undirected

// unit  -|-  unit   Top  ❌ ?
// unit  -|-  Node   Node ✅
// unit  -|   Rltn   Rltn
// unit  -|-  Path   Path ✅
// Node  -|-  unit   Node ✅
// Node  -|-  Node   Node ✅
// Node  -|   Rltn   Rltn
// Node  -|-  Path   Path ✅
// Path  -|-  unit   Path ✅
// Path  -|-  Node   Path ✅
// Path  -|   Rltn   Path ✅
// Path  -|-  Path   Path ✅

[<AutoOpen>]
module Ops = begin end

    //let ( -- ) () () = Path.Connect Path.EmptyNode (Undirected, Rltn.Empty) Path.EmptyNode

type Path with

    // ... -- ...
    static member ( -- ) (lhs, rhs) =
        Path.Connect lhs (Undirected, Rltn.Empty) rhs

    static member ( --> ) (lhs, rhs) =
        Path.Connect lhs (Right, Rltn.Empty) rhs

    static member ( <-- ) (lhs, rhs) =
        Path.Connect lhs (Left, Rltn.Empty) rhs

    // ... -- ()
    static member ( -- ) (lhs: Path, ()) = lhs -- Path.Unit
    static member ( --> ) (lhs: Path, ()) = lhs --> Path.Unit
    static member ( <-- ) (lhs: Path, ()) = lhs <-- Path.Unit

    // () -- ...
    static member ( -- ) ((), rhs: Path) = Path.Unit -- rhs
    static member ( --> ) ((), rhs: Path) = Path.Unit --> rhs
    static member ( <-- ) ((), rhs: Path) = Path.Unit <-- rhs

    // ... -- (node)
    static member ( -- ) (lhs: Path, rhs) = lhs -- Path.OfNode rhs
    static member ( --> ) (lhs: Path, rhs) = lhs --> Path.OfNode rhs
    static member ( <-- ) (lhs: Path, rhs) = lhs <-- Path.OfNode rhs

    // (node) -- ...
    static member ( -- ) (lhs, rhs: Path) = Path.OfNode lhs -- rhs
    static member ( --> ) (lhs, rhs: Path) = Path.OfNode lhs --> rhs
    static member ( <-- ) (lhs, rhs: Path) = Path.OfNode lhs <-- rhs




    // ... -| [rel]
    static member (-|) (lhs: Path, rhs: Rltn): Partial =
        Partial.Of (lhs, PartialDirection.Undirected, rhs)
        

    static member (<-|) (lhs: Path, rhs: Rltn): Partial =
        Partial.Of (lhs, PartialDirection.Left, rhs)

    // {partial} |- ...
    static member ( |- ) (part: Partial, rhs: Path) =
        part.Finish (Undirected, rhs)

    static member ( |-> ) (part: Partial, rhs: Path) =
        part.Finish (Right, rhs)

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

    // {partial} |- (node)
    static member ( |- ) (part, rhs) = part |- Path.OfNode rhs
    static member ( |-> ) (part, rhs) = part |- Path.OfNode rhs

type Rltn with
    
    // () -| [rel]
    static member ( -| ) ((), rhs) = Partial.Of (Path.Unit, PartialDirection.Undirected, rhs)
    static member ( <-| ) ((), rhs) = Partial.Of (Path.Unit, PartialDirection.Left, rhs)

    // (node) -| [rel]
    static member ( -| ) (lhs, rhs: Rltn) = Partial.Of (Path.OfNode lhs, PartialDirection.Undirected, rhs)
    static member ( <-| ) (lhs, rhs: Rltn) = Partial.Of (Path.OfNode lhs, PartialDirection.Left, rhs)

type Partial with

    // {partial} |- ()
    static member ( |- ) (part, ()) = part |- Path.Unit
    static member ( |-> ) (part, ()) = part |- Path.Unit