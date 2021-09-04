namespace TacitCypher

open Neo4j.Driver
open System

[<AutoOpen>]
module Attributes =
    type NodeClassAttribute() =
        inherit Attribute()
    
    type RltnClassAttribute() =
        inherit Attribute()

type CypherQuery<'T> (inner) =

    //member this.QueryStr: string = ""

    //member this.Result: 'T option = inner

    new () =
        CypherQuery(None)

//type Neo4j (url: string, auth: IAuthToken) = let db = Neo4j.Driver.GraphDatabase.Driver(url, auth)


type CypherBuilder (url: string, auth: IAuthToken) =

    member _.Neo4j = Neo4j.Driver.GraphDatabase.Driver(url, auth)

    member this.Zero () = 
        CypherQuery()

    member this.Return (inner: 'T): CypherQuery<'T> =
        CypherQuery(Some inner)


