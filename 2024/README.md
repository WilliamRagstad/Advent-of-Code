<div align=center>
    <h1>Advent of Code | Haskell</h1>
    <em>By William Rågstad, 2024</em>
</div>

## Introduction

Hi everybody, it's *that time of the year again*! 🎄\
I'm back for another year of Advent of Code, and this time I'm going to try to solve the tasks in **Haskell**.
I've been wanting to learn Haskell for a while now, and I figured that AoC would be a great way to do it, like last year with **F#**.
I'm not sure how far I'll get, but I'm hoping to **learn a lot** and **have fun** along the way.

You can find all tasks on [AoC 2024](https://adventofcode.com/2024).

## Run tasks

Use the `runghc` command to run each day's tasks like this:

```powershell
type .\01\input.txt | runghc .\01\solve.hs
```

# Learning Haskell

I'm using the following resources to learn Haskell:

- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- [Haskell Wiki](https://wiki.haskell.org/Haskell)
<!-- - [Haskell Programming from First Principles](http://haskellbook.com/) -->

## Day 1

Today I learned about:

- How to read input from stdin using `interact`
- `read` is used to convert a string to a number
- `lines` is used to split a string into lines
- `where` can be used to define helper functions in a function
- `Data.Bifunctor (bimap)` is used to apply a function to two values at once
- `Data.List (sort)` is used to sort a list
- Lambdas are defined like this: `(\x -> x + 1)`
- `.` is used to compose functions: `(f . g) x = f (g x)`
- `$` is used to avoid parentheses: `f (g x) = f $ g x` (works sometimes)
- Operators can be curried like functions: `zipWith (-)` and `filter (== x)`.

