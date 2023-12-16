<div align=center>
    <h1>Advent of Code | F#</h1>
    <em>By William Rågstad, 2023</em>
</div>


## Introduction

*Hello folks!*
For this year, I decided start trying to learn F#.
I've been wanting to learn another functional language for a while now, and I figured that AoC would be a great way to do it.
I'm not sure how far I'll get, but I'm hoping to **learn a lot** and **have fun** along the way.

You can find all tasks on [AoC 2023](https://adventofcode.com/2023).

## Run tasks
Simply run `./run.bat` to run all parts for each day.

# Learning F#
I'm using the following resources to learn F#:
- [F# for fun and profit](https://fsharpforfunandprofit.com/)
- [F# Docs](https://docs.microsoft.com/en-us/dotnet/fsharp/)

## Day 1
Today I learned about:
- The fact that F# does not differentiate between variables and functions, all defined with `let`.
- Parameters with explicit types require parentheses, e.g. `let func (param: int) = ...`
- The `|>` operator, which is used to pipe the result of one function into another.
- The `Seq` module
- The `List` module
- The `Array` module
- A `list` is a linked list, and an `array` is a contiguous block of memory in F#.
- Tuple types are defined with `let tuple: (int * int) = (1, 2)`
- Lists can be defined with `"A" :: "B" :: "C" :: []`
- Pattern matching statements look like this:
```fsharp
match list with
| [] -> ...
| head :: tail -> ...
```
- Match statements can also be used to destructure tuples:
```fsharp
match tuple with
| Some(a, b) -> ... // a and b are now bound from the tuple
| None -> ...
```
- Debugging with `printfn` and `|> debug` can look like this:
```fsharp
let debug value: 'a = // 'a
    printfn "%A" value // prints the value of any type
    value // returns the value
```

## Day 2
Today I learned about:
- Functions are curried by default.
- Type definitions with `type`
    - Records are defined with `type Round = { red: int; green: int; blue: int }`
    - Array types are defined with `Round array`
- Mutability
    - `let mutable x = 1`
    - `x <- 2`

## Day 3
Today I learned about:
- Records
    - Records are defined with `type Round = { red: int; green: int; blue: int }`
    - Records can be destructured with `let { red; green; blue } = round`
    - Records can be updated with `let newRound = { round with red = 1 }`
- A deeper dive into advanced parsing (2D input)
    - Directional scanning withing a bounded window
    - Hamming distances (Levenshtein distance, Manhattan distance, taxicab geometry)
- Composition of functions
    - Functions can be composed with `>>` and `<<`
    - Types can be composed with `*` (e.g. `(int * int) list`) and `->` (e.g. `int -> int -> int`)
- Arbitrary assignment to two variables depending on a condition
```fsharp
let mutable first = None
let mutable second = None
while condition do
    match test with
    | expr ->
        match first with
        | None -> first <- Some(expr)
        | Some(_) ->
            match second with
            | None -> second <- Some(expr)
            | Some(_) ->
                failwith "Expected only two matches"
```
- `List.choose` can be used to filter out `None` values from a list of `Option` types
```fsharp
let potential_gears: Symbol list = symbols |> List.filter potential_gear
let gears: Gear list = potential_gears |> List.choose (is_gear parts)
```

## Day 4
Today I learned about:
- `(debug >> int)` can be used to debug a value and convert it to an int in one go
- There is discussions about making the `fun` keyword optional in F# (see [1](https://stackoverflow.com/questions/70388340/do-fun-lambda-expressions-have-shorthand-syntax), [2](https://github.com/fsharp/fslang-suggestions/issues/634), [3](https://github.com/fsharp/fslang-suggestions/issues/168), and [4](https://github.com/fsharp/fslang-suggestions/issues/506))
- Worked with a mutable `Dictionary` from the .NET BCL (Base Class Library) in F#. It's a bit verbose, but it works.
```fsharp
let dict = Dictionary<string, int>() // Does not require `mutable` keyword
dict.Add("key", 1)
dict.["key"] <- 2
```
- Read up on the difference between `Dictionary`, `Hashtable` and `Map` in F#. [1](https://stackoverflow.com/questions/29077352/f-difference-between-dictionary-hashtable-and-map) and [2](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/fsharp-collection-types).
    - `Dictionary` this is a generic mutable collection implementation of the hashtable from .NET BCL.
    - `Hashtable` represents a collection of key/value pairs that are organized based on the hash code of the key. This is a mutable collection from .NET BCL.
    - `Map` is immutable and from the F# Core library. It is implemented based on AVL trees which is an entirely different data structure with different performance characteristics and use cases. [1](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-collections-fsharpmap-2.html) and [2](https://www.tutorialspoint.com/fsharp/fsharp_maps.htm).
    - `HashMap` is a mutable collection in the [`FSharp.Data.Adaptive`](https://fsprojects.github.io/FSharp.Data.Adaptive/) library. [1](https://fsprojects.github.io/FSharp.Data.Adaptive/reference/fsharp-data-adaptive-hashmapmodule.html).
- Create a sequence of numbers with `seq { 1 .. 10 }` (inclusive). [1](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/sequences) and [2](https://stackoverflow.com/questions/36780574/f-equivalent-of-python-range).
```fsharp
let sequence = seq { 1 .. 10 }
```
- Create a sequence of numbers with `seq { 1 .. 2 .. 10 }` (inclusive, step size)
- The `seq` keyword is **not required**, using only `{ 1..10 }` works just fine!

## Day 5
Today I learned about:
- One can write generic asserting test cases for the example inputs:
```fsharp
let example_assert (part: int) (example: int) (expected: 'a) (value: 'a) =
    if value = expected then
        printfn "P%d example %d: %A (correct)" part example value
    else
        printfn "P%d example %d: Expected %A, got %A" part example expected value
        Environment.Exit(1)


example_assert 1 1 35UL (example_1 |> parse parse_seeds |> solve_p1)
let p1 = input |> parse parse_seeds |> solve_p1
printfn "P1: %A" p1

example_assert 2 1 46UL (example_1 |> parse parse_seed_ranges |> solve_p2)
let p2 = input |> parse parse_seed_ranges |> solve_p2
printfn "P2: %A" p2
```
- The `|>` operator can be used to pipe the result of one function into another, e.g. `input |> parse parse_seeds |> solve_p1`
- Type aliases can be defined with `type`:
```fsharp
type Seed = int
type SeedRange = Seed * Seed
type Number = UInt64
```
- The `UInt64` type is an unsigned 64-bit integer

