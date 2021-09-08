namespace TacitCypher

open Neo4j.Driver
open System


open Query

//type Neo4j (url: string, auth: IAuthToken) = let db = Neo4j.Driver.GraphDatabase.Driver(url, auth)


type CypherBuilder (url: string, auth: IAuthToken) =

    member _.Neo4j = Neo4j.Driver.GraphDatabase.Driver(url, auth)

    member this.Zero () = Query<unit>()

    member this.Return (inner: 'T): Query<'T> =
        Query<_>()


