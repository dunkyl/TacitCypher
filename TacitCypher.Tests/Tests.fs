namespace TacitCypher.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open TacitCypher

open ExpectedObjects
open ExpectedObjects.Strategies
open Neo4j.Driver

open FSharp.Control.Tasks

open Pattern

[<AutoOpen>]
module TestContext = 
    open Attributes  

    [<NodeClass>]
    type Person = {
        Name: string
        Color: string
    }

    [<RltnClass>]
    type Likes = {
        Amount: float
        TimesBroughtUp: int
    }

    [<NodeClass>]
    type NodeLabel = { NodeProperty: int }

    [<RltnClass>]
    type RelationLabel = { RelationProperty: int }

    let veryMuch = { Amount = 999.0; TimesBroughtUp = 6 }

    type Technology () = class end

    let alice = { Name = "Alice"; Color = "Blue" }
    let bob = { Name = @"Bob"; Color = "Blue" }
    let claire = {Name = "Claire"; Color = "Yellow"}

    let manybob = [
        for x in [1..20] ->
            { Name = "Bob#"+x.ToString(); Color = "Blue" }
       ]

    let BoundResult<'a> = Unchecked.defaultof<'a>

//[<AutoOpen>]
//module Helpers =
//    let N (props: 't) = { Node.Label = Some (typeof<'t>.Name); Props = Some (props :> obj)  }
//    let BindN (props: 't) name = { Name = name; Node = N props }

//    let R (props: 't) = { Rel.Label = Some (typeof<'t>.Name); Props = Some (props :> obj)  }
//    let BindR (props: 't) name = { Name = name; Rel = R props }

[<TestClass>]
type TestGraphing () =

    let node = Node.Any
    let rltn = Rel.Any
    let path = Path.Unit

    let node_props = { NodeProperty = 2 }
    let rltn_props = { RelationProperty = 2 }

    let node_t: Node<NodeLabel> = BindN node_props "n"
    let rltn_t: Rel<RelationLabel> = BindR rltn_props "r"
    let path_t: Path<NodeLabel> = { First = node_t.Node, Some node_t.Name; Rest = [] }

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

    let c: Path<NodeLabel> list = [
        () -- node_t
        () -- path_t
        () --> node_t
        () --> path_t
        () <-- node_t
        () <-- path_t

        node_t -- ()
        node_t -- node
        node_t -- path
        node_t --> ()
        node_t --> node
        node_t --> path
        node_t <-- ()
        node_t <-- node
        node_t <-- path

        path_t -- ()
        path_t -- node
        path_t -- path
        path_t --> ()
        path_t --> node
        path_t --> path
        path_t <-- ()
        path_t <-- node
        path_t <-- path


        () -| rltn |- node_t
        () -| rltn |- path_t

        () -| rltn |-> node_t
        () -| rltn |-> path_t

        () <-| rltn |- node_t
        () <-| rltn |- path_t

        node_t -| rltn |- ()
        node_t -| rltn |- node
        node_t -| rltn |- path
        node_t -| rltn |-> ()
        node_t -| rltn |-> node
        node_t -| rltn |-> path
        node_t <-| rltn |- ()
        node_t <-| rltn |- node
        node_t <-| rltn |- path

        path_t -| rltn |- ()
        path_t -| rltn |- node
        path_t -| rltn |- path
        path_t -| rltn |-> ()
        path_t -| rltn |-> node
        path_t -| rltn |-> path
        path_t <-| rltn |- ()
        path_t <-| rltn |- node
        path_t <-| rltn |- path

        node_t -- () -- ()
        node_t -- () -- () -- ()
        node_t -- () -- () -- () -- ()
        node_t -- () -- () -- () -- () -- ()
    ]

    let d: Path<RelationLabel> list = [
        () -| rltn_t |- ()
        () -| rltn_t |- node
        () -| rltn_t |- path
        () -| rltn_t |-> ()
        () -| rltn_t |-> node
        () -| rltn_t |-> path
        () <-| rltn_t |- ()
        () <-| rltn_t |- node
        () <-| rltn_t |- path

        node -| rltn_t |- ()
        node -| rltn_t |- node
        node -| rltn_t |- path
        node -| rltn_t |-> ()
        node -| rltn_t |-> node
        node -| rltn_t |-> path
        node <-| rltn_t |- ()
        node <-| rltn_t |- node
        node <-| rltn_t |- path

        path -| rltn_t |- ()
        path -| rltn_t |- node
        path -| rltn_t |- path
        path -| rltn_t |-> ()
        path -| rltn_t |-> node
        path -| rltn_t |-> path
        path <-| rltn_t |- ()
        path <-| rltn_t |- node
        path <-| rltn_t |- path
    ]

    let e: Path<NodeLabel * RelationLabel> list = [
        node_t -| rltn_t |- ()
        node_t -| rltn_t |- node
        node_t -| rltn_t |- path
        node_t -| rltn_t |-> ()
        node_t -| rltn_t |-> node
        node_t -| rltn_t |-> path
        node_t <-| rltn_t |- ()
        node_t <-| rltn_t |- node
        node_t <-| rltn_t |- path

        path_t -| rltn_t |- ()
        path_t -| rltn_t |- node
        path_t -| rltn_t |- path
        path_t -| rltn_t |-> ()
        path_t -| rltn_t |-> node
        path_t -| rltn_t |-> path
        path_t <-| rltn_t |- ()
        path_t <-| rltn_t |- node
        path_t <-| rltn_t |- path
    ]

    let f: Path<NodeLabel * NodeLabel> list = [
        node_t -- node_t
        node_t -- path_t
        node_t --> node_t
        node_t --> path_t
        node_t <-- node_t
        node_t <-- path_t

        path_t -- node_t
        path_t -- path_t
        path_t --> node_t
        path_t --> path_t
        path_t <-- node_t
        path_t <-- path_t

        node_t -| rltn |- node_t
        node_t -| rltn |- path_t
        node_t -| rltn |-> node_t
        node_t -| rltn |-> path_t
        node_t <-| rltn |- node_t
        node_t <-| rltn |- path_t

        path_t -| rltn |- node_t
        path_t -| rltn |- path_t
        path_t -| rltn |-> node_t
        path_t -| rltn |-> path_t
        path_t <-| rltn |- node_t
        path_t <-| rltn |- path_t
    ]

    let g: Path<(NodeLabel * RelationLabel) * NodeLabel> list = [
        node_t -| rltn_t |- node_t
        node_t -| rltn_t |- path_t
        node_t -| rltn_t |-> node_t
        node_t -| rltn_t |-> path_t
        node_t <-| rltn_t |- node_t
        node_t <-| rltn_t |- path_t

        path_t -| rltn_t |- node_t
        path_t -| rltn_t |- path_t
        path_t -| rltn_t |-> node_t
        path_t -| rltn_t |-> path_t
        path_t <-| rltn_t |- node_t
        path_t <-| rltn_t |- path_t
    ]

[<TestClass>]
type TestQueryBuilding () =

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
        let g: Path = { First = N(bob); Rest = [Undirected, R(veryMuch), N(claire) ] }
        let g' = N(bob)-|R(veryMuch)|-N(claire)
        (expectObj g).ShouldEqual (g')

    [<TestMethod>]
    member this.TestGraphing1 () =
        let g: Path = { First = Node.Any; Rest= [Right, R(veryMuch), Node.Any ] }
        let g' = ()-|R(veryMuch)|->()
        (expectObj g).ShouldEqual(g')

    [<TestMethod>]
    member this.TestGraphing2 () =
        let g: Path = { First = N(bob); Rest= [Left, R(veryMuch), Node.Any ] }
        let g' = N(bob)<-|R(veryMuch)|-()
        (expectObj g).ShouldEqual(g')

    [<TestMethod>]
    member this.TestSerializeNode () =

        Assert.AreEqual( "(:Person {name:'Bob',color:'Blue'})", N(bob).ToString() )

    [<TestMethod>]
    member this.TestSerializePath () =

        Assert.AreEqual(
            "(:Person {name:'Bob',color:'Blue'})-[:LIKES {amount:999.0,timesBroughtUp:6}]-()",
            (N(bob)-|R(veryMuch)|-()).ToString()
        )

    [<TestMethod>]
    member this.TestSerializePath2 () =

        Assert.AreEqual(
            "(p:Person {name:'Bob',color:'Blue'})-[rel:LIKES {amount:999.0,timesBroughtUp:6}]-()",
            (BindN bob "p" -| BindR veryMuch "rel" |- ()).ToString()
        )

    [<TestMethod>]
    member _.TestSerializeMany () =

        for i, bob' in List.indexed manybob do

            Assert.AreEqual( $"(:Person {{name:'Bob#{i+1}',color:'Blue'}})", N(bob').ToString() )

        //Assert.AreEqual(
        //    @"(:Person {name:'Bob',color:'Blue'})-[:LIKES {amount:999.0,timesBroughtUp:6}]-()",
        //    (N(bob)-|R(veryMuch)|-()).ToString()
        //)

open Clause
open System.Threading.Tasks

type IsCoworkersWith () = class end

[<TestClass>]
type TestQuerys2 () =

    let db = GraphDatabase.Driver("bolt://localhost:7687", Neo4j.Driver.AuthTokens.Basic(@"neo4j", @"12345"))

    [<TestMethod>]
    member this.Test1 () =

        let zack = { Name = "Zack"; Color = "Coral" }
        let yvon = { Name = "Yvon"; Color = "Lime" }
        
        //  (z:Person) -[:IS_COWORKERS_WITH]-> (y:Person {name: "Yvon"})
        //  N<Person>("z") -|R<IsCoworkersWith>()|-> N<Person>("y", {|name = "Yvon"|})
        let pattern =
            BindNErased<Person> {||} "z" -| RLabeled<IsCoworkersWith> |-> BindNErased<Person> {|name = "Yvon"|} "y"

        let p2 = pattern --> BindN 3 "x"

        let (zack_returned, yvon_returned) = pattern.ReturnValues()

        ()

[<TestClass>]
type TestQuerys () =

    let db = GraphDatabase.Driver("bolt://localhost:7687", Neo4j.Driver.AuthTokens.Basic(@"neo4j", @"12345"))

    

    [<TestMethod>]
    member this.TestConnect () =
        ()

    [<TestMethod>]
    member this.TestCreate () =
        
        let q = CREATE (BindN bob "p")

        Assert.AreEqual(
            $"CREATE (p:Person {{name:'Bob',color:'Blue'}})\n\
              RETURN p",
            q.ToString() )

        task {
            let! result = q.Run db
            for r in result do
                let keys = r.Values.Keys
                let values = r.Values.Values
                let r' = Seq.zip keys values |> Map.ofSeq
                printfn $"RECORD: {r'}"
        }
        |> Async.AwaitTask |> Async.RunSynchronously

    [<TestMethod>]
    member this.TestMatch () =
        
        let q = MATCH (BindN bob "p")

        Assert.AreEqual(
            $"MATCH (p:Person {{name:'Bob',color:'Blue'}})\n\
              RETURN p",
            q.ToString() )

        task {
            let! _ = (CREATE (N bob)).Run db

            let! result = q.Run db
            for r in result do
                let keys = r.Values.Keys
                let values = r.Values.Values |> Seq.map Query.assume<Person>
                let r' = Seq.zip keys values |> Map.ofSeq
                printfn $"RECORD: {r'}"

            let! _ = (DELETE (BindN bob "p")).Run db
            return ()
        }
        |> Async.AwaitTask |> Async.RunSynchronously

        let a (x: Neo4j.Driver.INode) = 
            x.As<'a> null

        ()

    [<TestMethod>]
    member this.TestMatchMany () =
        
        let q = MATCH (BindN bob "p")
        
        Assert.AreEqual(
            $"MATCH (p:Person {{name:'Bob',color:'Blue'}})\n\
                RETURN p",
            q.ToString() )
        
        task {
            let! bobs =
                manybob
                |> List.map ((fun b -> (CREATE (N b)).Run db))
                |> Task.WhenAll

            
        
            let! result = (MATCH (BindNLabeled<Person> "a")).Run db
            for r in result do
                let keys = r.Values.Keys
                let values = r.Values.Values |> Seq.map Query.assume<Person>
                ()//let r' = Seq.zip keys values |> Map.ofSeq
                //printfn $"RECORD: {r'}"

            let! _ =
                manybob
                |> List.map ((fun b -> (DELETE (BindN b "p")).Run db))
                |> Task.WhenAll
        
            let! _ = (DELETE (BindN bob "p")).Run db
            return ()
        }
        |> Async.AwaitTask |> Async.RunSynchronously
        
        let a (x: Neo4j.Driver.INode) = 
            x.As<'a> null
        
        ()

    [<TestMethod>]
    member this.TestDelete () =
        let d = DELETE (BindN bob "p")

        Assert.AreEqual(
            $"MATCH (p:Person {{name:'Bob',color:'Blue'}})\n\
              DELETE p",
            d.ToString() )

        task {
            let! result = d.Run db
            for r in result do
                let keys = r.Keys
                let values = r.Values
                let r' = Seq.zip keys values |> Map.ofSeq
                printfn $"RECORD: {r'}"
        }
        |> Async.AwaitTask |> Async.RunSynchronously


