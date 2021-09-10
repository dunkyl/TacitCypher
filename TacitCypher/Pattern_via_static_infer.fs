module PatternStaticInfer

type Direction = Undirected | Left | Right with
    member this.Combine rhs =
        if rhs = Right then Right else this

type Pathable = Pathable

type Rel () = class end

type Rel<'B> () = class end

type IPath = interface end
    //abstract member Step: Pathable * Rel * Direction -> Partial
    //abstract member Step: Pathable * Rel<'A> * Direction -> Partial<'A>

and IPath<'A> = interface end
    //abstract member Step: Pathable * Rel * Direction -> Partial<'A>
    //abstract member Step: Pathable * Rel<'B> * Direction -> Partial<'A * 'B>

and Partial () = class end

and Partial<'A> () = class end

type Path () = 
    interface IPath
        member this.Step (Pathable, rel: Rel, dir: Direction) = Partial()//TODO
        member this.Step (Pathable, rel: Rel<'A>, dir: Direction) = Partial<'A>()//TODO

and Path<'A> () =
    interface IPath<'A>
        member this.Step (Pathable, rel: Rel, dir: Direction) = Partial<'A>()
        member this.Step (Pathable, rel: Rel<'B>, dir: Direction) = Partial<'A * 'B>()//TODO

type Node () =
    interface IPath
        member this.Step (Pathable, rel: Rel, dir: Direction) = Partial()//TODO
        member this.Step (Pathable, rel: Rel<'A>, dir: Direction) = Partial<'A>()//TODO

type Node<'A> () =
    interface IPath<'A>
        member this.Step (Pathable, rel: Rel, dir: Direction) = Partial<'A>()//TODO
        member this.Step (Pathable, rel: Rel<'B>, dir: Direction) = Partial<'A * 'B>()//TODO

type Partial with
    member this.Step (Pathable, rhs: IPath, dir) = Path()
    member this.Step (Pathable, rhs: IPath<'B>, dir) = Path<'B>()

type Partial<'A> with
    member this.Step (Pathable, rhs: IPath, dir) = Path<'A>()
    member Step.Step (Pathable, rhs: IPath<'B>, dir) = Path<'A * 'B>()

[<AutoOpen>]
module Operators =
    
    let inline ( -| ) (lhs: ^L) (rhs: ^R) =
        ((^L): (member Step : Pathable * ^R * Direction -> ^TPartial) (lhs, Pathable, rhs, Undirected))

    let inline ( <-| ) (lhs: ^L) (rhs: ^R) =
        ((^L): (member Step : Pathable * ^R * Direction -> ^TPartial) (lhs, Pathable, rhs, Left))

    let inline ( |- ) (lhs: ^L) (rhs: ^R) =
        ((^L): (member Step : Pathable * ^R * Direction -> ^TPath) (lhs, Pathable, rhs, Undirected))

    let inline ( |-> ) (lhs: ^L) (rhs: ^R) =
        ((^L): (member Step : Pathable * ^R * Direction -> ^TPath) (lhs, Pathable, rhs, Right))

    let inline ( -- ) lhs rhs = lhs -| Rel() |- rhs
    let inline ( --> ) lhs rhs = lhs -| Rel() |-> rhs
    let inline ( <-- ) lhs rhs = lhs <-| Rel() |- rhs


    let pathu = Path()
    let relu = Rel()

    let partu = pathu -| relu |- Node()

    let pathb = (Path<int>()) -| (Rel()) |- pathu