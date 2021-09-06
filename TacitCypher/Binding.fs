module Binding

type Bound<'T> (arg: 'T, name: string) =
    
    member _.Inner = arg

    member _.Name = name

type Path<'TAll> =
    {
        Bindings: list<string * obj>
    }

    static member Empty =
        { Bindings = [] }

    member this.Add (a: Bound<'b>) =
        { Bindings = (a.Name, a.Inner :> obj)::this.Bindings}
        
    static member Extend (path: Path<'A * 'B>, bind: Bound<'C>): Path<'A * 'B * 'C> =
        path.Add bind

    static member Extend (path: Path<'A * 'B * 'C>, bind: Bound<'D>): Path<'A * 'B * 'C * 'D> =
        path.Add bind

    static member Extend (path: Path<'A * 'B * 'C * 'D>, bind: Bound<'E>): Path<'A * 'B * 'C * 'D * 'E> =
        path.Add bind

    static member Extend (path: Path<'A * 'B * 'C * 'D * 'E>, bind: Bound<'F>): Path<'A * 'B * 'C * 'D * 'E * 'F> =
        path.Add bind

    static member Extend (path: Path<'A * 'B * 'C * 'D * 'E * 'F>, bind: Bound<'G>): Path<'A * 'B * 'C * 'D * 'F * 'G> =
        path.Add bind


type PathUnpack =
    static member Unpack (path: Path<'A * 'B * 'C * 'D>) =
        let [a; b; c; d] = List.map (fun (a, b) -> b) path.Bindings
        a :> obj :?> 'A,
        b :> obj :?> 'B,
        c :> obj :?> 'C,
        d :> obj :?> 'D

module A =

    let PathOf (arg: Bound<'a>) : Path<'a> =
        { Bindings = [ arg.Name, arg.Inner :> obj ] }

    let Extend (path: Path<'a>) (bind: Bound<'b>): Path<'a * 'b> =
        { Bindings = (bind.Name, bind.Inner :> obj)::path.Bindings}
        
    
    let rec x = Bound(1, nameof x)

    let rec y = Bound(2, nameof y)

    let rec z = Bound(3, nameof z)

    let rec p = Bound("alice", nameof p)

    let path1 = PathOf(x)

    let path2 = Extend path1 y

    let path3 = Path<_>.Extend(path2, z)

    let path4 = Path<_>.Extend(path3, p)

    let a, b, c, d = PathUnpack.Unpack path4