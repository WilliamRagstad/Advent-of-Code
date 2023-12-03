open System
let input = System.IO.File.ReadAllLines("input.txt")

let debug value =
    printfn "%A" value
    value

let lines (s: string) =
    s.Trim().Split('\n') |> Array.map (fun l -> l.Trim())

(*
The engineer explains that an engine part seems to be missing from the engine, but nobody can figure out which one.
If you can add up all the part numbers in the engine schematic, it should be easy to work out which part is missing.

The engine schematic (your puzzle input) consists of a visual representation of the engine.
There are lots of numbers and symbols you don't really understand,
but apparently any number adjacent to a symbol, even diagonally,
is a "part number" and should be included in your sum.
(Periods (.) do not count as a symbol.)

Here is an example engine schematic:

467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..

In this schematic, two numbers are not part numbers because they are not adjacent
to a symbol: 114 (top right) and 58 (middle right).
Every other number is adjacent to a symbol and so is a part number;
their sum is 4361.

Of course, the actual engine schematic is much larger.
What is the sum of all of the part numbers in the engine schematic?

---
Conclusion:
- Any number adjacent to a symbol, even diagonally, is a "part number" and should be included in the sum.
- Periods (.) do not count as a symbol
- add up all the part numbers in the engine schematic (input)
- 
*)

/// Search around the given range for a non period, and-non digit symbol.
/// For example if the input is:
/// ```
/// 467..114..
/// ...*......
/// ```
/// The range is: x=0 y=0 len=3
/// The result is: true
/// Because the symbol at (3, 1) is a `*` and not a period or digit.
/// 
/// This is done by scanning (represented by # below) around the given rectanguar range (represented by R):
/// ```
/// ..........
/// ..######..
/// ..#RRRR#..
/// ..######..
/// ..........
/// ```
let is_part_number (input: string array) (x: int) (y: int) (len: int) =
    let mutable result = false
    // Top left corner x=i y=j
    let start_col = min (max 0 (x - 1)) (input.[y].Length - 1)
    let start_row = min (max 0 (y - 1)) (input.Length - 1)
    let end_col = min (x + len) (input.[y].Length - 1)
    let end_row = min (y + 1) (input.Length - 1)
    let mutable col = start_col
    let mutable row = start_row
    // Move to the bottom right corner
    while not result && row <= end_row do
        let c = input.[row].[col]
        // printfn "Checking: %d %d %c" row col c
        if not (Char.IsDigit(c) || c = '.') then
            result <- true
        col <- col + 1
        if col > end_col then
            col <- start_col
            row <- row + 1
    result

let parse_input_1 (input: string array) : int list =
    let mutable part_numbers = []
    for y = 0 to input.Length-1 do
        let line = input.[y]
        // Get the indicies of all numbers in the line
        let mutable start_index = 0
        let mutable was_digit = false
        for x = 0 to line.Length do
            if x < line.Length && Char.IsDigit(line.[x]) then
                was_digit <- true
            else
                if was_digit then
                    let end_index = x - 1
                    let num = line.Substring(start_index, end_index - start_index + 1)
                    // printfn "Found number: %s" num
                    if is_part_number input start_index y num.Length then
                        // printfn "Was part number"
                        part_numbers <- int num :: part_numbers
                was_digit <- false
                start_index <- x + 1
    part_numbers |> List.rev

// --- Part One ---
let example_1 =
    @"
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
" |> lines

let example_2 =
    @"
...51.1.5
12*.....*
" |> lines

let solve_p1 (part_numbers: int list) =
    part_numbers |> debug |> List.sum
let p1 = input |> parse_input_1 |> solve_p1
printfn "P1: %d" p1
// Tries:
// 559433 - too high
// 554003 - correct

// --- Part Two ---
(*
The missing part wasn't the only issue - one of the gears in the engine is wrong.
A gear is any * symbol that is adjacent to exactly two part numbers.
Its gear ratio is the result of multiplying those two numbers together.

This time, you need to find the gear ratio of every gear and add them all up so
that the engineer can figure out which gear needs to be replaced.

Consider the same engine schematic again:
```
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
```
In this schematic, there are two gears.
The first is in the top left; it has part numbers 467 and 35,
so its gear ratio is 16345.
The second gear is in the lower right; its gear ratio is 451490.
(The * adjacent to 617 is not a gear because it is only adjacent to one part number.)
Adding up all of the gear ratios produces 467835.

What is the sum of all of the gear ratios in your engine schematic?

---
Conclusion:
- A gear is any * symbol that is adjacent to exactly two part numbers.
- Its gear ratio is the result of multiplying those two numbers together.
- Find the gear ratio of every gear and add them all up.
*)

type Symbol = { x: int; y: int; len: int; value: char }
type Part = { x: int; y: int; len: int; value: int }
type Gear = { first: Part; second: Part }

/// 1. Extract all numbers and * symbols into a list of structures
let extract_elements (input: string array) : Part list * Symbol list =
    let mutable parts: Part list = []
    let mutable symbols: Symbol list = []
    for y = 0 to input.Length-1 do
        let line = input.[y]
        // Get the indicies of all numbers in the line
        let mutable start_index = 0
        let mutable was_digit = false
        for x = 0 to line.Length do
            if x < line.Length && Char.IsDigit(line.[x]) then
                was_digit <- true
            else
                // Check for number
                if was_digit then
                    let end_index = x - 1
                    let num = line.Substring(start_index, end_index - start_index + 1)
                    parts <- { x = start_index; y = y; len = num.Length; value = int num } :: parts
                was_digit <- false
                // Check for symbol (non-period)
                if x < line.Length && line.[x] <> '.' then
                    symbols <- { x = x; y = y; len = 1; value = line.[x] } :: symbols
                start_index <- x + 1
    parts |> List.rev, symbols |> List.rev

/// Get all symbols adjacent to the given part
/// This is determined by all symbols that are in the proximity of the part
/// with 1 unit in each direction (including diagonals)
let adjacent_symbols (symbols: Symbol list) (part: Part) =
    let mutable result = []
    for symbol in symbols do
        if symbol.x >= part.x - 1           // Left
        && symbol.x <= part.x + part.len    // Right
        && symbol.y >= part.y - 1           // Top
        && symbol.y <= part.y + 1           // Bottom
        then
            // printfn "Found adjacent symbol: %A to %A" symbol part
            result <- symbol :: result
    result |> List.rev

let potential_gear (symbol: Symbol) : bool =
    symbol.value = '*'

let is_gear parts (symbol: Symbol) =
    let mutable first = None
    let mutable second = None
    for part in parts do
        // Check if the part is adjacent to the symbol
        // 1. A gear can only have 2 adjacent parts
        // 2. The parts must be adjacent to one symbol
        match adjacent_symbols [symbol] part with
        | [_] -> // Yes, the symbol is adjacent to the part
            match first with
            | None -> first <- Some(part)
            | Some(_) ->
                match second with
                | None -> second <- Some(part)
                | Some(_) ->
                    failwith "Too many parts???"
        | _ -> ()
    
    match first, second with
    | Some(first), Some(second) -> Some({ first = first; second = second })
    | _ -> None

let solve_p2 (input: string array) =
    let parts, symbols = input |> extract_elements
    let potential_gears = symbols |> List.filter potential_gear
    let gears = potential_gears |> List.choose (is_gear parts)
    // let _ = debug gears
    let gear_ratios = gears |> List.map (fun gear -> gear.first.value * gear.second.value)
    gear_ratios |> List.sum

let p2 = input |> solve_p2
printfn "P2: %d" p2
// Tries:
// 87263515 - correct
