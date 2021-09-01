namespace TacitCypher

open System.Threading.Tasks
open FSharp.Control.Tasks

open Neo4j.Driver
open System
open System.Text.RegularExpressions
open System.Text.Json



type CypherQuery<'T> (inner) =

    member this.QueryStr: string = ""

    member this.Result: 'T option = inner

    new () =
        CypherQuery(None)

type CypherPath<'P> () = class end

type BindingContext<'T> () = class end

type Neo4j (url: string, auth: IAuthToken) =

    let db = Neo4j.Driver.GraphDatabase.Driver(url, auth)

[<AutoOpen>]
module CypherBuilder =

    let MATCH (path: 'a): 'BoundResult =
        (), ()
    
    let MATCH' (path: CypherPath<'Bind>): 'BoundResult =
        ()

module Cypher =

    // Cypher Type - Dotnet Type
    // Integer - Int32 | Int64 | Numerics.BigInteger
    // Float - Double | float32
    // String - String | Discriminated Union
    // Boolean - Boolean
    // Point - Double * Double | Double * Double * Double
    // Date - DateOnly
    // Time - TimeOnly
    // LocalTime - ?
    // DateTime - DateTime
    // LocalDateTime - ?
    // Duration  - TimeSpan

    let ValidName = Regex(@"(?:[a-z][a-z0-9_]*)", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)

    let EscapeSymbol  = @"_Q"
    let EscapeSequences =
        [
            EscapeSymbol, EscapeSymbol
            "`", "bt"
        ] |> List.map (fun (a, b) -> a, EscapeSymbol+b)
    
    let CypherNameEscape (text: string) =
        List.fold (fun (t: string) (uesc: string, esc) -> t.Replace(uesc, esc)) text EscapeSequences

    let AnyNode () = "()"

    let CleanName (name: string) =
        
        if ValidName.IsMatch name then
            name
        else
            "`" + CypherNameEscape name + "`"

    let textInfo = System.Globalization.CultureInfo("en-US", false).TextInfo

    let ConformName (name: string) =
        name.ToLower()
        
    let ConformNodeLabel (label: string) =
        label.ToLower() |> textInfo.ToTitleCase

    let ConformRelationLabel (label: string) =
        label.ToUpper()

    let BoundAnyNode (bindName: string) = "(" + CleanName bindName + ")"

    let rec AsPropertyLiteral (o: obj) =
        match o with
        | :? string as v ->
            "'" +
            (System.Text.Json.JsonEncodedText.Encode v).ToString()
                .Replace(@"\u0022", "\"")
                .Replace(@"\u0027", "\\'")
            + "'"
        | :? int64 as v ->
            v.ToString()
        | :? int as v ->
            v.ToString()
        | :? Numerics.BigInteger as v ->
            v.ToString()
        | :? float32 as v ->
            v.ToString()
        | :? float as v ->
            v.ToString()
        | :? System.Collections.Generic.IEnumerable<obj> as vs ->
            "[" +
            String.Join(", ", Seq.map AsPropertyLiteral vs)
            + "]"
        | _ ->
            let t = o.GetType()
            "{" +
            String.Join(", ", seq {
                for prop in t.GetProperties() ->
                    let cypherName = prop.Name |> CleanName |> ConformName
                    let cypherValue = AsPropertyLiteral (prop.GetValue o)
                    $"{cypherName}: {cypherValue}"
            })
            + "}"

    let AsNode<'T> (bindName: string) (useLabel: bool) (o: 'T) =
        let t = o.GetType()
        let label =
            if useLabel then
                ":" + CleanName t.Name
            else
                ""

        $"({bindName}{label} {AsPropertyLiteral o})"
    
    let AsAnonNode<'T> (useLabel: bool)  (o: 'T) = AsNode<'T> "" useLabel o
    
    type Label =
    | Erased
    | Auto
    | AutoNoconform
    | Specific of string

    //type CypherProperties = obj

    type Node = {
        BindName: string option
        Label: Label
        Properties: obj option
    }

    type Rltn = {
        BindName: string option
        Label: Label
        Properties: obj option
    }

    type Path =
    | Empty
    | JustNode of Node
    | Graph of Node * Rltn * Path

    let SerializeInner bindName' label properties' =
        let bindName = Option.defaultValue "" (Option.map (CleanName >> ConformName) bindName')
        bindName + label +
        match properties' with
        | Some p -> " " + AsPropertyLiteral properties'
        | None -> ""

    let SerializeLabel hint conform label =
        match label with
        | Erased -> ""
        | Specific s -> s
        | Auto ->
            ":" + conform(CleanName (hint))
        | AutoNoconform ->
            ":" + CleanName (hint)

    type Node with
        member this.Serialize () =

            let labelStr =
                let innerT = this.GetType().GenericTypeArguments.[0].Name
                SerializeLabel innerT ConformNodeLabel this.Label
            
            SerializeInner this.BindName labelStr this.Properties

        static member (-) (lhs: Node, rhs: Rltn) =
            Graph (lhs, rhs, Empty)

        static member OfObj (o) =
            let opted = Option.ofObj o
            let label = if Option.isNone opted then Erased else Auto

            {BindName = None; Label = label; Properties = opted}

        static member Empty =
            {BindName = None; Label = Erased; Properties = None}


    type Rltn with
        member this.Serialize () =

            let labelStr =
                let innerT = this.GetType().GenericTypeArguments.[0].Name
                SerializeLabel innerT ConformRelationLabel this.Label
        
            SerializeInner this.BindName labelStr this.Properties

        static member OfObj (o) =
            let opted = Option.ofObj o
            let label = if Option.isNone opted then Erased else Auto

            {BindName = None; Label = label; Properties = opted}

        static member Empty =
            {BindName = None; Label = Erased; Properties = None}

    
    let (-|) (lhs) (rhs) =          
        Node.OfObj lhs, Rltn.OfObj rhs

    let (|-) (n, r) (rhs) =
        Graph(n, r, JustNode (Node.OfObj rhs))

type CypherBuilder (url: string, auth: IAuthToken) =

    member _.Neo4j = Neo4j.Driver.GraphDatabase.Driver(url, auth)

    member this.Zero () = 
        CypherQuery()

    member this.Return (inner: 'T): CypherQuery<'T> =
        CypherQuery(Some inner)

    //member this.Run (c: CypherQuery<'T>) =
    //    let s = this.Neo4j.AsyncSession()
    //    s.WriteTransactionAsync(
    //        fun tx ->
    //            task {
    //                let! cursor = tx.RunAsync(c.queryStr)
    //                return! cursor.ToListAsync (fun a -> a)
    //            }
    //    )
    //    |> Async.AwaitTask
    //    |> async.


module J = 
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
