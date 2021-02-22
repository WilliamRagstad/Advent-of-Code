<div align=center>
    <h1>Advent of Code | Haskell</h1>
    <em>By William Rågstad, 2021-02-21</em>
</div>


## Introduction

*Hello folks!*
For this year, I decided start trying to learn Haskell for once. Allowing me a smooth start, Advent of Code will provide tasks in gradually increasing difficulty forcing me to gain more experience and knowledge in as short time as possible!

You can find all tasks on [AoC 2015](https://adventofcode.com/2015).

## Run tasks
> All the commands below are assuming that you are located in the `/Haskell/<day>` directory containing each subtask `<part>` of that day.

Usually, we want quick results as our task implementations won't need to be compiled, just evaluated.
Therefore you can quickly *interpret* each task using `runhaskell <part>`.

Compile a task to an executable using `ghc <part> && <part>`.
Where `<day>` is a number from *1 to 25* inclusive and `<part>` is a subtask of that particular day.

Run tasks interactively using `ghci <part>`, followed by calling the main function using `Prelude> main`.
