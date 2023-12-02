open System.IO

// The newly-improved calibration document consists of lines of text;
// each line originally contained a specific calibration value that the Elves now need to recover.
// On each line, the calibration value can be found by combining the first digit and the last digit (in that order) to form a single two-digit number.
// For example:
// ```
// 1abc2        --> 12
// pqr3stu8vwx  --> 38
// a1b2c3d4e5f  --> 15
// treb7uchet   --> 77
//                = 142
// ```
// In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.
//
// Consider your entire calibration document. What is the sum of all of the calibration values?

let input = File.ReadAllLines("input.txt")

let debug value =
    printfn "%A" value
    value

let parse_input_str (input: string) =
    input.Trim().Split('\n') |> Array.map (fun line -> line.Trim())

let findFirstDigit (line: string) =
    // let rec findFirstDigit' i =
    //     if System.Char.IsDigit line.[i] then
    //         line.[i] |> string
    //     else
    //         findFirstDigit' (i + 1)

    // findFirstDigit' 0
    line |> Seq.find System.Char.IsDigit |> string

let rev (line: string) =
    line |> Array.ofSeq |> Array.rev |> Array.map string |> String.concat ""

let firstAndLast (line: string) =
    (line |> findFirstDigit) + (line |> rev |> findFirstDigit)

// --- Part One ---
let example_1 =
    @"
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"
    |> parse_input_str

let p1_sum = input |> Seq.map firstAndLast |> Seq.map int |> Seq.sum

printfn "P1 Sum: %d" p1_sum

// --- Part Two ---
// It looks like some of the digits are actually spelled out with letters: one, two, three, four, five, six, seven, eight, and nine also count as valid "digits".
// Equipped with this new information, you now need to find the real first and last digit on each line. For example:
// ```
// two1nine
// eightwothree
// abcone2threexyz
// xtwone3four
// 4nineeightseven2
// zoneight234
// 7pqrstsixteen
// ```
// In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together produces 281.

/// Numbers in order of length, longest first
/// ``(number name * number value) array``
let numbers =
    "zero"
    :: "one"
    :: "two"
    :: "three"
    :: "four"
    :: "five"
    :: "six"
    :: "seven"
    :: "eight"
    :: "nine"
    :: []
    |> Array.ofList
    |> Array.mapi (fun i s -> (s, i))
// |> Array.sortBy (fun (s, _) -> s.Length)
// |> Array.rev

let replaceNumbers (numbers: (string * int) array) (line: string) =
    /// Check if there is a number text on the current index in the line (using substring comparison)
    /// If there is, replace it with the number value
    /// If not, continue to the next index
    let rec replaceNumbers' i (line: string) =
        if i >= line.Length then
            line
        else
            let first_match =
                numbers
                |> Array.tryFind (fun (s, _) ->
                    let maxIndex = min (i + s.Length) line.Length
                    let u = line.Substring(i, maxIndex - i)
                    u = s)

            match first_match with
            | Some(s, value) ->
                let line = line.Replace(s, value.ToString())
                replaceNumbers' (i + 1) line
            | None -> replaceNumbers' (i + 1) line

    replaceNumbers' 0 line

let replaceNumbersFromStart (line: string) = replaceNumbers numbers line

let replaceNumbersFromEnd (line: string) =
    let revNumbers = numbers |> Array.map (fun (s, i) -> (s |> rev, i))
    line |> rev |> replaceNumbers revNumbers |> rev

// Active Patterns
// let (|Prefix|_|) (p: string) (s: string) =
//     if s.StartsWith(p) then
//         Some(s.Substring(p.Length))
//     else
//         None

// let (|Suffix|_|) (p: string) (s: string) =
//     if s.EndsWith(p) then
//         Some(s.Substring(0, s.Length - p.Length))
//     else
//         None

// let rec replaceNumbersForwards (line: string) =
//     match line with
//     | "" -> ""
//     | Prefix "zero" rest -> "0" + (replaceNumbersForwards rest)
//     | Prefix "one" rest -> "1" + (replaceNumbersForwards rest)
//     | Prefix "two" rest -> "2" + (replaceNumbersForwards rest)
//     | Prefix "three" rest -> "3" + (replaceNumbersForwards rest)
//     | Prefix "four" rest -> "4" + (replaceNumbersForwards rest)
//     | Prefix "five" rest -> "5" + (replaceNumbersForwards rest)
//     | Prefix "six" rest -> "6" + (replaceNumbersForwards rest)
//     | Prefix "seven" rest -> "7" + (replaceNumbersForwards rest)
//     | Prefix "eight" rest -> "8" + (replaceNumbersForwards rest)
//     | Prefix "nine" rest -> "9" + (replaceNumbersForwards rest)
//     | _ -> (line.[0] |> string) + (line.[1..] |> replaceNumbersForwards)

let firstAndLastWords (line: string) =
    let first = line |> replaceNumbersFromStart |> findFirstDigit
    let last = line |> replaceNumbersFromEnd |> rev |> findFirstDigit
    first + last

let example_2 =
    @"
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
"
    |> parse_input_str

let example_3 =
    @"
twone3four
"
    |> parse_input_str

///! NOTICE
/// The digit words depend on which digit you are currently looking for.
/// 1. If you are looking for the **first** digit, you need to replace the digit words from the start of the line to the end
/// 2. If you are looking for the **last** digit, you need to replace the digit words from the end of the line to the start
let p2_sum = input |> Seq.map firstAndLastWords |> Seq.map int |> Seq.sum

printfn "P2 Sum: %d" p2_sum
