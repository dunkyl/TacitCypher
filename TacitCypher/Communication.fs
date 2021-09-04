module Communication

open FSharp.Control.Tasks

open Neo4j.Driver

let cypherMany (db: Neo4j.Driver.IDriver) (q: string) =
    let s = db.AsyncSession()
    s.WriteTransactionAsync(
        fun tx ->
            task {
                let! cursor = tx.RunAsync(q)
                return! cursor.ToListAsync (fun a -> a)
            }
    )

let cypherOne (db: Neo4j.Driver.IDriver) (q: string) =
    let s = db.AsyncSession()
    s.WriteTransactionAsync(
        fun tx ->
            task {
                let! cursor = tx.RunAsync(q)
                let! rexists = cursor.FetchAsync()
                match rexists with
                | true -> return Some cursor.Current
                | false -> return None
            }
    )