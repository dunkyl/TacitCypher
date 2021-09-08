module Pattern3

[<AutoOpen>]
module Util =
    let inline convert (a: ^NodeOrPath) =
        (^NodeOrPath: (static member Convert : ^NodeOrPath -> ^TPath) a)

type Direction = Undirected | Left | Right

type Relation () = class end
    //{
        // label
        // props
    //}

type Edge = 
    {
        Rel: Relation
        Dir: Direction
    }

type Node () = class end
    //{
        // label
        // props
    //}

type Path =
    {
        First: Node
        Rest: (Edge * Node) list
    }

type PathThen =
    {
        First: Node
        Middle: (Edge * Node) list
        Last: Edge
    }

type Relation<'Bind> () = class end

type Edge<'Bind> =
    {
        Rel: Relation<'Bind>
        Dir: Direction
    }

type Node<'Bind> () = class end

type Path with
    //static member Then (p, r, d) =
    //    { First = p.First; Middle = p.Rest; Last = { Rel = r; Dir = d } }

    static member Then (p, r: Relation<'a>, d) =
        { First = p.First; Middle = p.Rest; Last = { Rel = Relation(); Dir = d } }

type Node with
    static member Convert (n: Node) = { First = n; Rest = [] }

module Ops =

    let inline ( -| ) (lhs: ^L) (rhs: ^R) =
        let leftpath = (^L: (static member Convert : ^L -> ^TPath) lhs)
        (^TPath: (static member Then : ^TPath * ^R * Direction -> ^TPathThen) (leftpath, rhs, Undirected))


    let un0 = Node()
    let ur0 = Relation()

    //let _0 = un0 -| ur0

    let bn0 = Node<int>()
    let br0 = Relation<int>()

    let _1 = un0 -| br0