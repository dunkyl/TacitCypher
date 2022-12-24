module Query

open Pattern
open Communication

open System.Threading
open FSharp.Control.Tasks
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Reflection
open System

type Query<'T> (querysting) =

    override this.ToString() =
        querysting
    
    member this.Run db =
        task {
            let! records = cypherMany db querysting
            //for record in records do
            //    record.
            return records
        }
        
let assume<'a> (recordvalue: obj) =
    match recordvalue with
    | :? Neo4j.Driver.IEntity as v ->
        let infos = FSharpType.GetRecordFields typeof<'a>
        let initizer = [|
            for info in infos ->
                v.Properties.[info.Name.ToLower()]
        |]
        Some (FSharpValue.MakeRecord(typeof<'a>, initizer))
    | _ ->
        None
