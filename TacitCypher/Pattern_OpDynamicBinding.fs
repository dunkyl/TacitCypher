module Pattern_OpDynamicBinding

[<RequireQualifiedAccess>]
type Dir = L | R | U
type LeftPat = LeftPat of l: obj * dir: Dir * rel: obj
type Pat = Pat of obj * Dir * obj * obj
let combineDir l r =
    match l, r with
    | d, Dir.U -> d
    | _, d -> d


module Operations =
    type LeftLink = LeftLink of Dir with
        static member (?<-) (LeftLink dir, l, rel) = LeftPat (l, dir, rel)

    type RightLink = RightLink of Dir with
    static member (?<-) (RightLink dir1, LeftPat(l, dir2, rel), r) = Pat (l, combineDir dir1 dir2, rel, r)

let inline ( -|) l r = (?<-) (Operations.LeftLink  Dir.U) l r
let inline (<-|) l r = (?<-) (Operations.LeftLink  Dir.L) l r
let inline (|- ) l r = (?<-) (Operations.RightLink Dir.U) l r
let inline (|->) l r = (?<-) (Operations.RightLink Dir.R) l r

let ( -->) l r = Pat (l, Dir.R, null, r)
let (<-- ) l r = Pat (l, Dir.L, null, r)
let ( -- ) l r = Pat (l, Dir.U, null, r)

let (|.) l r = (l, r)

let l<'t> a: 't = a

type N<'t> () =
    static member (?) (this, prop: string) =
        fun obj -> obj

let n<'t> = N<'t>()

let n_ = N<unit>()

let a<'t> = Unchecked.defaultof<'t>

let MATCH x = x
let RETURN x a = x

type A = { Xx: int }

let q1 = <@
    seq {
        MATCH (n<A>?a { Xx = 2 }) --> ()
    // MATCH (a:A { XX: 2 }) --> ()

        MATCH (n_?a { Xx = 2 }) --> ()
    // MATCH (a { XX: 2 }) --> ()

        MATCH (a:A) --> ()
    // MATCH (:A) --> ()
        
    }
@>