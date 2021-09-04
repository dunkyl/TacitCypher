namespace TacitCypher.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open TacitCypher

open ExpectedObjects
open ExpectedObjects.Strategies

[<AutoOpen>]
module TestContext = 
    [<NodeClass>]
    type Person = {
        Name: string
        Color: string
    }

    [<RltnClass>]
    type Likes = {
        Amount: float
    }

    let veryMuch = {Amount = 999.0}

    type Technology () = class end

    let alice = { Name = "Alice"; Color = "Light Blue" }
    let bob = { Name = @"Bob '\ Killer' Hilber"; Color = "Blue" }
    let claire = {Name = "Claire"; Color = "Yellow"}

    let manybob = [
        for x in [1..100] ->
            { Name = "Bob#"+x.ToString(); Color = "Blue" }
       ]

    let BoundResult<'a> = Unchecked.defaultof<'a>

[<TestClass>]
type TestGraphing () =
    let node = Node.Empty
    let rltn = Rltn.Empty
    let path = Path.Unit

    let a: Path list = [
        () -- node
        () -- path
        () --> node
        () --> path
        () <-- node
        () <-- path

        node -- ()
        node -- node
        node -- path
        node --> ()
        node --> node
        node --> path
        node <-- ()
        node <-- node
        node <-- path

        path -- ()
        path -- node
        path -- path
        path --> ()
        path --> node
        path --> path
        path <-- ()
        path <-- node
        path <-- path

        () -| rltn |- ()
        () -| rltn |- node
        () -| rltn |- path
        () -| rltn |-> ()
        () -| rltn |-> node
        () -| rltn |-> path
        () <-| rltn |- ()
        () <-| rltn |- node
        () <-| rltn |- path

        node -| rltn |- ()
        node -| rltn |- node
        node -| rltn |- path
        node -| rltn |-> ()
        node -| rltn |-> node
        node -| rltn |-> path
        node <-| rltn |- ()
        node <-| rltn |- node
        node <-| rltn |- path

        path -| rltn |- ()
        path -| rltn |- node
        path -| rltn |- path
        path -| rltn |-> ()
        path -| rltn |-> node
        path -| rltn |-> path
        path <-| rltn |- ()
        path <-| rltn |- node
        path <-| rltn |- path

        node -- () -- ()
        node -- () -- () -- ()
        node -- () -- () -- () -- ()
        node -- () -- () -- () -- () -- ()
    ]

    let b: Partial list = [
        () -| rltn
        () <-| rltn
        node -| rltn
        node <-| rltn
        path -| rltn
        path <-| rltn
    ]

[<TestClass>]
type TestQueryBuilding () =
    let R o = {Rltn.Name = None; Label = None; Props = Some o}
    let N o = {Node.Name = None; Label = None; Props = Some o}

    let expectObj o =
        o.ToExpectedObject(
            fun c->
                //c.CompareByValue()
                c.RemoveStrategy<EqualsOverrideComparisonStrategy>()
                c.RemoveStrategy<ComparableComparisonStrategy>()
                |> ignore)

    [<TestMethod>]
    member this.TestCompExpr () =
        let cypher = CypherBuilder("bolt://localhost:7687", Neo4j.Driver.AuthTokens.Basic(@"Neo", @"12345"))

        ()

    [<TestMethod>]
    member this.TestGraphing0 () =
        let g = { First = N(bob); Rest= [Undirected, R(veryMuch), N(claire) ] }
        let g' = N(bob)-|R(veryMuch)|-N(claire)
        (expectObj g).ShouldEqual (g')

    [<TestMethod>]
    member this.TestGraphing1 () =
        let g = { First = Node.Empty; Rest= [Right, R(veryMuch), Node.Empty ] }
        let g' = ()-|R(veryMuch)|->()
        (expectObj g).ShouldEqual(g')

    [<TestMethod>]
    member this.TestGraphing2 () =
        let g  = { First = N(bob); Rest= [Left, R(veryMuch), Node.Empty ] }
        let g' = N(bob)<-|R(veryMuch)|-()
        (expectObj g).ShouldEqual(g')

    [<TestMethod>]
    member this.TestSerialize () =

        Assert.AreEqual( @"(:Person {name: 'Bob \'\\ Killer\' Hilber', color: 'Blue'})", N(bob).ToString() )

        Assert.AreEqual(
            @"(:Person {name: 'Bob \'\\ Killer\' Hilber', color: 'Blue'})-[:LIKES {amount: 999}]-()",
            (N(bob)-|R(veryMuch)|-()).ToString()
        )


//[<TestClass>]
//type TestQueryExecution () =

//    let db = Neo4j("bolt://localhost:7687", Neo4j.Driver.AuthTokens.Basic(@"Neo", @"12345"))

//    [<TestMethod>]
//    member this.TestConnect () =
//        ()


