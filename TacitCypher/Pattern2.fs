module Pattern2

type Direction = Undirected | Left | Right

type Rltn () =
    static member Empty = Rltn()

type Rltn<'a> () =
    static member Empty = Rltn<'a>()

and Path<'a> (first: Node, rest: (Rltn * Direction * Node) list) =
    
    member this.First = first
    member this.Rest = rest

    static member Extend (path: Path<'a>, dir: Direction, rel: Rltn, next: Path<'c>): Path<'a * 'c> =
        Path<'a * 'c>(path.First, (rel, dir, next.First)::next.Rest)

    static member AsPath (p: Path<'a>) = p

and Node () =
    
    static member AsPath (n: Node) = Path<unit * unit>(n, [])

and Node<'a> () =

    member this.Demote () = (), Node()
    
    static member AsPath (n: Node<'a>) =
        let bind, node = n.Demote()
        Path<'a>(node, [])

module Ops =

    let inline asPath (a: ^NodeOrPath): ^TPath =
        (^NodeOrPath: (static member AsPath: ^NodeOrPath -> ^TPath) a)

    let inline extend (path: ^TPath_a, dir: Direction, rel: Rltn, next: ^TPath_c) =
        (^TPath_a: (static member Extend: ^TPath_a * Direction * Rltn * ^TPath_c -> ^TPath_abc) (path, dir, rel, next))

    let inline ( -| ) (lhs: ^l) (rel: Rltn) = 
        (asPath lhs), Undirected, rel

    let inline ( |- ) (path: ^TPath_a, dir: Direction, rel: Rltn) (rhs: ^r) =
        let next: ^TPath_c = asPath rhs
        (extend (path, dir, rel, next))

    let inline ( -- ) (lhs: ^L) (rhs: ^R): ^TPath_abc =
        let lpath: ^TPath_a = asPath lhs
        let rpath: ^TPath_c = asPath rhs
        (^TPath_a: (static member Extend: ^TPath_a * Direction * Rltn * ^TPath_c -> ^TPath_abc) (lpath, Undirected, Rltn.Empty, rpath))

    let node0 = Node()
    let node1 = Node()

    let bindnode0 = Node<int>()
    let bindnode1 = Node<float>()

    let path0part = bindnode0 -| Rltn.Empty

    let a, b, c = path0part

    let next = asPath bindnode1

    let path0 = extend (a, b, c, next)

    let path_1bindpart = node0 -| Rltn.Empty

    let a', b', c' = path_1bindpart

    let next' = asPath bindnode0

    let path_nobinds = (Node.AsPath node0) -- (Node.AsPath node1)

    let path_1bind = ((Node.AsPath node0) -- (Node<_>.AsPath bindnode1))

    let path_2binds = bindnode1 -- bindnode1
