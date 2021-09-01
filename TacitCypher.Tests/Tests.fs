namespace TacitCypher.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Neo4j.Driver
open TacitCypher

open TacitCypher.Cypher

open System.Text.Json

[<AutoOpen>]
module TestContext = 
    type Person = {
        Name: string
        Color: string
    }

    type Technology () = class end

    let alice = { Name = "Alice"; Color = "Light Blue" }
    let bob = { Name = @"Bob '\ Killer' Hilber"; Color = "Blue" }
    let claire = {Name = "Claire"; Color = "Yellow"}

    let manybob = [
        for x in [1..100] ->
            { Name = "Bob#"+x.ToString(); Color = "Blue" }
       ]

    let BoundResult<'a> = Unchecked.defaultof<'a>

    let (?)<'a> (lhs: obj) (rhs: string) =
        lhs :?> 'a

[<TestClass>]
type TestQueryBuilding () =
    
    let toJson = JsonSerializer.Serialize

    [<TestMethod>]
    member this.TestCompExpr () =
        let a = 2
        let cypher = CypherBuilder("bolt://localhost:7687", Neo4j.Driver.AuthTokens.Basic(@"Neo", @"12345"))

        let LIKES = ()
        let N = ()

        

        let (--) lhs rhs =
            lhs, rhs

        let (-->) lhs rhs =
            ()

        let _ = cypher {
            let p, t = MATCH <| (N?p: Person) -- LIKES --> (N?t: Technology)
            
            return t
        }

        MATCH (CypherPath<_>())

    [<TestMethod>]
    member this.TestGraphing () =
        Assert.AreEqual(
            Graph(
                {Node.BindName = None; Label = Auto; Properties = Some (bob :> obj)},
                {Rltn.BindName = None; Label = Auto; Properties = Some (claire :> obj)},
                JustNode (Node.Empty)),
            (bob)-|claire|-()
        )

    [<TestMethod>]
    member this.TestSerialize () =

        Assert.AreEqual( @"(:Person {name: 'Bob \'\\ Killer\' Hilber', color: 'Blue'})", Cypher.AsAnonNode true bob )

    [<TestMethod>]
    member this.TestCodeQuote () =

        let MATCH (q: Quotations.Expr<Path>) = ()

        let mutable p, q, r = BoundResult in

            MATCH <@ (p:Person)-|(r:Person)|-(q:Person) @>

        printfn "%s" p.Name

        let mutable p', q', r' = BoundResult in
        
            MATCH <@ (p':int)-|[]|-(q':int) @>

        ()

[<TestClass>]
type TestQueryExecution () =

    let db = Neo4j("bolt://localhost:7687", Neo4j.Driver.AuthTokens.Basic(@"Neo", @"12345"))

    [<TestMethod>]
    member this.TestConnect () =
        ()


