open System

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
(**

---
Conclusion:
- 
**)

let example_1 = @"" |> lines
let example_2 = @"" |> lines
let example_3 = @"" |> lines

let solve_p1 a = ()
example_assert 1 1 () (example_1 |> solve_p1)
let p1 = input |> lines |> solve_p1
printfn "P1: %A" p1
// Tries:


// --- Part Two ---
(**

---
Conclusion:
- 
**)

let solve_p2 a = ()
example_assert 2 1 () (example_1 |> solve_p2)
let p2 = input |> lines |> solve_p2
printfn "P2: %A" p2
// Tries:
// 87263515 - correct
