open System
open System.Text.RegularExpressions

let format (s: string) = s.Trim().Replace("\r", "")

let lines (s: string) =
    (format s).Split('\n') |> Array.map (fun l -> l.Trim())

let input = System.IO.File.ReadAllText("input.txt") |> format

let debug value =
    printfn "%A" value
    value

let example_assert (part: int) (example: int) (expected: 'a) (value: 'a) =
    if value = expected then
        printfn "P%d example %d: %A (correct)" part example value
    else
        printfn "P%d example %d: Expected %A, got %A" part example expected value
        Environment.Exit(1)

// --- Part One ---
(** Conclusion:
- Calculate the number of ways to beat the record in each race by holding the button for a varying amount of time.
- Multiply the number of ways to win each race to get the final answer.
**)

let example_1 =
    @"
Time:      7  15   30
Distance:  9  40  200
    "

let example_2 = @""
let example_3 = @""

type Race = { time: uint64; distance: uint64 }

let parse_races (line: string) =
    line.Split(':')
    |> Array.item 1
    |> (fun str -> Regex.Replace(str, @"\s+", " "))
    |> (fun str -> str.Trim().Split(' '))
    |> Array.map uint64

let parse_input (parseLine) (lines: string[]) : Race list =
    let times = parseLine (lines.[0])
    let distances = parseLine (lines.[1])

    Array.zip times distances
    |> Array.map (fun (time, distance) -> { time = time; distance = distance })
    |> Array.toList

let waysToWin (race: Race) =
    [ 0UL .. race.time - 1UL ]
    |> List.map (fun holdTime -> (race.time - holdTime) * holdTime)
    |> List.filter (fun distance -> distance > race.distance)
    |> List.length

let solve_p1 (races: Race list) =
    races |> List.map waysToWin |> List.fold (*) 1 // Multiply the results together

example_assert 1 1 288 (example_1 |> lines |> parse_input parse_races |> solve_p1)
let p1 = input |> lines |> parse_input parse_races |> solve_p1
printfn "P1: %A" p1
// Tries:
// 252000 - correct

// --- Part Two ---
(**

---
Conclusion:
- 
**)

let parse_race (line: string) : uint64 array =
    [| Regex.Replace(line.Split(':').[1], @"\s+", "") |> uint64 |]

let solve_p2 (race: Race) = waysToWin race
example_assert 2 1 71503 (example_1 |> lines |> parse_input parse_race |> List.item 0 |> solve_p2)
let p2 = input |> lines |> parse_input parse_race |> List.item 0 |> solve_p2
printfn "P2: %A" p2
// Tries:
// 36992486 - correct
