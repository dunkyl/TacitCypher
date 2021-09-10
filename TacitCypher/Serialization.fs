module Serialization

open System.Text.RegularExpressions
//open System.Text.Json

open Newtonsoft.Json
open System.IO
open Newtonsoft.Json.Serialization

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
    label.ToUpper()

let AsPropertyLiteral (x: obj) =
    let contr = DefaultContractResolver( NamingStrategy = CamelCaseNamingStrategy())
    //let settings = JsonSerializerSettings( ContractResolver = contr)
    let serializer = JsonSerializer( ContractResolver = contr, NullValueHandling= NullValueHandling.Ignore )
    use stringWriter = new StringWriter()
    use jsonWriter = new JsonTextWriter(stringWriter, QuoteName = false, QuoteChar = '\'')
    serializer.Serialize (jsonWriter, x)
    stringWriter.ToString ()

let SerializeInner bindName' conform label properties' =
    let bindName = Option.defaultValue "" (Option.map (CleanName >> ConformName) bindName')
    let label' =
        match label with
        | None -> ""
        | Some l -> ":" + conform l
    bindName + label' +
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

