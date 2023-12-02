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
- Match statements look like this:
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
