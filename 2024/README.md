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

## Day 2

- `uncurry` is used to convert a curried function to a function on pairs:

  ```haskell
  uncurry :: (a -> b -> c) -> (a, b) -> c
  x = uncurry (+) (1, 2) -- x = 3
  ```

- `i <- [0 .. length xs - 1]` generates a list of indices for a list `xs`
- `++` is used to concatenate two lists
- `elem` is used to check if an element is in a list
- `[take 3 xs | xs <- xss]` generates a list of the first 3 elements of each list in `xss`
- `take` is used to take the first `n` elements of a list
- `drop` is used to drop the first `n` elements of a list

## Day 3

- Leaned about `span` that splits a list into a prefix and a suffix based on a predicate

```haskell
span p xs = takeWhile p xs, dropWhile p xs
span (/= ' ') "hello world" -- ("hello", " world")
span (< 3) [1, 2, 3, 2] -- ([1, 2], [3, 2])
```

- Pattern matching can be done in multiple ways:
	1. Using `case`:

		```haskell
		case x of
			0 -> "zero"
			1 -> "one"
			_ -> "other"
		```

	2. Using **function guards**:

		```haskell
		f 0 = "zero"
		f 1 = "one"
		f _ = "other"

		-- or

		f x
			| x == 0 = "zero"
			| x == 1 = "one"
			| otherwise = "other"
		```

- It's important to use unique names for functions and variables, otherwise you'll get a `name clash` error which are **super sneaky** and **hard to debug**.

	> For example, find the bug here:
	>
	> ```haskell
	> mul xs = do
    > let (lhs, xs) = span is_num xs
    > if head xs == ','
    >   then do
    >     let (rhs, xs) = span is_num (tail xs)
    >     if head xs == ')'
    >       then do
    >         let lhs' :: Int = read lhs
    >         let rhs' :: Int = read rhs
    >         Just (lhs', rhs', xs)
    >       else Nothing
    >   else Nothing
	> ```
	>
	> One would expect that the `xs` in the inner `let` would shadow the outer `xs`, **but it doesn't**. The `xs` in the inner `let` is actually the same as the outer `xs`, which causes the `Just (lhs', rhs', xs)` to return the original `xs` instead of the modified `xs`.
