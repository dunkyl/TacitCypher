module Serialization

open System.Text.RegularExpressions
open System.Text.Json

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

// T - T option

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

let ToTitleCase = System.Globalization.CultureInfo("en-US", false).TextInfo.ToTitleCase

let ConformName (name: string) =
    name.ToLower()
        
let ConformNodeLabel (label: string) =
    label.ToLower() |> ToTitleCase

let ConformRelationLabel (label: string) =
    let a = [1; 2; 3]
    a.[^2]
    label.ToUpper()

let rec AsPropertyLiteral (o: obj) =
    let mutable opts = JsonSerializerOptions()
    let mutable namepolicy = JsonNamingPolicy.CamelCase
    opts.PropertyNamingPolicy <- namepolicy
    opts.DefaultIgnoreCondition <- Serialization.JsonIgnoreCondition.WhenWritingNull
    JsonSerializer.Serialize(o, opts)
    //match o with
    //| :? string as v ->
    //    "'" +
    //    (System.Text.Json.JsonEncodedText.Encode v).ToString()
    //        .Replace(@"\u0022", "\"")
    //        .Replace(@"\u0027", "\\'")
    //    + "'"
    //| :? int64 as v ->
    //    v.ToString()
    //| :? int as v ->
    //    v.ToString()
    //| :? Numerics.BigInteger as v ->
    //    v.ToString()
    //| :? float32 as v ->
    //    v.ToString()
    //| :? float as v ->
    //    v.ToString()
    //| :? option<obj> as v ->
    //    AsPropertyLiteral v.Value
    //| :? System.Collections.Generic.IEnumerable<obj> as vs ->
    //    "[" +
    //    String.Join(", ", Seq.map AsPropertyLiteral vs)
    //    + "]"
    //| :? unit ->
    //    ""
    //| _ ->
    //    let t = o.GetType()
    //    "{" +
    //    String.Join(", ", [
    //        for prop in t.GetProperties() ->
    //            if prop.GetIndexParameters().Length = 0 then
    //                let cypherName = prop.Name |> CleanName |> ConformName
    //                let value = 
    //                    match prop.GetValue o with
    //                    | :? option<obj> as v ->
    //                        v.Value
    //                    | v -> v
    //                if isNull value then
    //                    ""
    //                else
    //                    $"{cypherName}: {AsPropertyLiteral value}"
    //            else
    //                let cypherName = prop.Name |> CleanName |> ConformName
    //                $"{cypherName}: indexed"
    //    ])
    //    + "}"

let SerializeInner bindName' label properties' =
    let bindName = Option.defaultValue "" (Option.map (CleanName >> ConformName) bindName')
    bindName + label +
    match properties' with
    | Some p -> " " + AsPropertyLiteral p
    | None -> ""

type Label =
| Erased
| Auto
| AutoNoconform
| Specific of string

let SerializeLabel hint conform label =
    match label with
    | Erased -> ""
    | Specific s -> s
    | Auto ->
        ":" + conform(CleanName (hint))
    | AutoNoconform ->
        ":" + CleanName (hint)