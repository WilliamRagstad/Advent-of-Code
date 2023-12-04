open System
let input = System.IO.File.ReadAllLines("input.txt")

let debug value =
    printfn "%A" value
    value

let lines (s: string) =
    s.Trim().Split('\n') |> Array.map (fun l -> l.Trim())

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
let p1 = input |> solve_p1
printfn "P1: %A" p1
// Tries:


// --- Part Two ---
(**

---
Conclusion:
- 
**)

let solve_p2 a = ()

let p2 = input |> solve_p2
printfn "P2: %A" p2
// Tries:
// 87263515 - correct
