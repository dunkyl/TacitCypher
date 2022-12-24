# TacitCypher

> The most beautiful code I have ever written is [here](https://github.com/dunkyl/TacitCypher/blob/main/TacitCypher/Pattern_via_interface.fs#L185-L271).

An inglorious attempt to implement a subset of the [Cypher Query Language](https://neo4j.com/developer/cypher-query-language/) in F#. Abandoned since I never got to be as type safe or as terse as I wanted.

```fsharp
let coworker =  BindNErased<Person> {||} "coworker"
let yvon = BindNErased<Person> {|name = "Yvon"|} "y"
let p = (coworker) -| RLabeled<IsCoworkersWith> |-> (yvon)
// p : Path<Person, Person>
```
In real cypher: `MATCH (coworker)-[:IsCoworkersWith]->(yvon {name: "Yvon"})`

The path `p` only binds two values, both here with a label of :Person, and remembers their type and order. Subsequent binds in the path are tupled in 2-3-tuples, since F#/C# doesn't have variadic generic type parameters, e.g. `Path<((Person * Person) * Manages * Car), Dog>` in some theorical path which binds two people nodes, a manages relationship, etc, with any number of unbound nodes and relations inbetween.

A challenge I did not overcome is to make it work with RETURN statements that reorder or skip bindings, or that call functions, etc.

