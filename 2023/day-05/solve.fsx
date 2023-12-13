open System

let format (s: string) =
    s.Trim().Replace("\r", "")
let input = System.IO.File.ReadAllText("input.txt") |> format

let debug value =
    printfn "%A" value
    value

let example_assert (part: int) (example: int) (expected: uint64) (value: uint64) =
    if value = expected then printfn "P%d example %d: %A (correct)" part example value
    else
        printfn "P%d example %d: Expected %A, got %A" part example expected value
        Environment.Exit(1)

// --- Part One ---
(**
The almanac (your puzzle input) lists all of the seeds that need to be planted.
It also lists what type of soil to use with each kind of seed,
what type of fertilizer to use with each kind of soil,
what type of water to use with each kind of fertilizer, and so on.
Every type of seed, soil, fertilizer and so on is identified with a number,
but numbers are reused by each category
 - that is, soil 123 and fertilizer 123 aren't necessarily related to each other.

```
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
```

The almanac starts by listing which seeds need to be planted: seeds 79, 14, 55, and 13.

The rest of the almanac contains a list of maps which describe how to convert numbers
from a source category into numbers in a destination category.
That is, the section that starts with seed-to-soil map: describes
how to convert a seed number (the source) to a soil number (the destination).
This lets the gardener and his team know which soil to use with which seeds,
which water to use with which fertilizer, and so on.

Rather than list every source number and its corresponding destination number
one by one, the maps describe entire ranges of numbers that can be converted.
Each line within a map contains three numbers: the destination range start,
the source range start, and the range length.

Consider again the example seed-to-soil map:
```
50 98 2
52 50 48
```
The first line has a destination range start of 50, a source range start of 98,
and a range length of 2. This line means that the source range starts at 98 and
contains two values: 98 and 99. The destination range is the same length,
but it starts at 50, so its two values are 50 and 51. With this information,
you know that seed number 98 corresponds to soil number 50 and that
seed number 99 corresponds to soil number 51.

The second line means that the source range starts at 50 and contains 48
values: 50, 51, ..., 96, 97. This corresponds to a destination range
starting at 52 and also containing 48 values: 52, 53, ..., 98, 99.
So, seed number 53 corresponds to soil number 55.

Any source numbers that aren't mapped correspond to the same destination number.
So, seed number 10 corresponds to soil number 10.

So, the entire list of seed numbers and their corresponding soil numbers looks like this:

```
seed  soil
0     0
1     1
...   ...
48    48
49    49
50    52
51    53
...   ...
96    98
97    99
98    50
99    51
```

With this map, you can look up the soil number required for each initial seed number:
```
Seed number 79 corresponds to soil number 81.
Seed number 14 corresponds to soil number 14.
Seed number 55 corresponds to soil number 57.
Seed number 13 corresponds to soil number 13.
```
The gardener and his team want to get started as soon as possible,
so they'd like to know the closest location that needs a seed.
Using these maps, find the lowest location number that corresponds to any of the initial seeds.
To do this, you'll need to convert each seed number through other categories
until you can find its corresponding location number.
In this example, the corresponding types are:
```
Seed 79, soil 81, fertilizer 81, water 81, light 74, temperature 78, humidity 78, location 82.
Seed 14, soil 14, fertilizer 53, water 49, light 42, temperature 42, humidity 43, location 43.
Seed 55, soil 57, fertilizer 57, water 53, light 46, temperature 82, humidity 82, location 86.
Seed 13, soil 13, fertilizer 52, water 41, light 34, temperature 34, humidity 35, location 35.
```
So, the lowest location number in this example is 35.

What is the lowest location number that corresponds to any of the initial seed numbers?

---
Conclusion:
- The almanac (your puzzle input) lists all of the seeds that need to be planted.
- It also lists what type of soil to use with each kind of seed,
- what type of fertilizer to use with each kind of soil,
- what type of water to use with each kind of fertilizer, and so on.
- Every type of seed, soil, fertilizer and so on is identified with a number, but numbers are reused by each category
- the maps describe entire ranges of numbers that can be converted.
- Each line within a map contains three numbers: the destination range start, the source range start, and the range length
- Any source numbers that aren't mapped correspond to the same destination number. So, seed number 10 corresponds to soil number 10.
- What is the lowest location number that corresponds to any of the initial seed numbers?
**)

let example_1 =
    @"
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
" |> format
let example_2 = @"" |> format
let example_3 = @"" |> format

type Number = uint64 // unsigned 64-bit integer
type MappingRange = { source: Number; destination: Number; length: Number }
type Mapping = { name: string; ranges: MappingRange array }
type Seed = Number

let parse (parse_initial_line) (input: string) =
    // First line is a list of seeds
    // Remaining lines are lists of mappings from top down
    let segments = input.Split("\n\n")
    let initial_state = parse_initial_line segments.[0]
    let mappings =
        segments.[1..]
        |> Array.map (fun s ->
            let lines = s.Split("\n")
            let name = lines.[0].Split(" ").[0]
            let parse_range (s: string) : MappingRange =
                let parts = s.Split(" ")
                { source = Number.Parse parts.[1]; destination = Number.Parse parts.[0]; length = Number.Parse parts.[2] }
            let ranges = lines.[1..] |> Array.map parse_range
            { name = name; ranges = ranges }
        )
    // seeds, mappings
    initial_state, mappings

let apply_mapping (mapping: Mapping) (value: Number) : Number =
    match mapping.ranges |> Array.tryFind (fun r -> r.source <= value && value < r.source + r.length) with
    | Some range -> value - range.source + range.destination
    | None -> value // No mapping found, return the same value

let solve_p1 ((seeds: Number array), (mappings: Mapping array)): Number =
    // Mutate the seeds array in place to get the final location
    let mutable values = seeds
    for mapping in mappings do values <- values |> Array.map (apply_mapping mapping)
    // Extract the lowest location number
    values |> Array.min

let parse_seeds (input: string) : Seed array =
    input.Split(" ") |> Array.skip(1) |> Array.map Seed.Parse

example_assert 1 1 35UL
    (example_1 |> parse parse_seeds |> solve_p1)
let p1 = input |> parse parse_seeds |> solve_p1
printfn "P1: %A" p1
// Tries:
// 662197086 - correct


// --- Part Two ---
(**
Everyone will starve if you only plant such a small number of seeds.
Re-reading the almanac, it looks like the seeds: line actually describes ranges of seed numbers.

The values on the initial seeds: line come in pairs.
Within each pair, the first value is the start of the range and the second value is the length of the range.
So, in the first line of the example above:
```
seeds: 79 14 55 13
```
This line describes two ranges of seed numbers to be planted in the garden.
The first range starts with seed number 79 and contains 14 values: 79, 80, ..., 91, 92.
The second range starts with seed number 55 and contains 13 values: 55, 56, ..., 66, 67.

Now, rather than considering four seed numbers, you need to consider a total of 27 seed numbers.

In the above example, the lowest location number can be obtained from seed number 82,
which corresponds to soil 84, fertilizer 84, water 84, light 77, temperature 45,
humidity 46, and location 46. So, the lowest location number is 46.

Consider all of the initial seed numbers listed in the ranges on the first line of the almanac.
What is the lowest location number that corresponds to any of the initial seed numbers?

---
Conclusion:
- The values on the initial `seeds:` line come in pairs.
- Within each pair, the first value is the start of the range and the second value is the length of the range.
**)

type Range = { start: Number; length: Number }

let parse_seed_ranges (input: string) : Range array =
    input.Split(" ") |> Array.skip(1) |> Array.chunkBySize 2 |> Array.map (fun parts ->
        { start = Number.Parse parts.[0]; length = Number.Parse parts.[1] }
    )

let solve_p2 ((seed_ranges: Range array), (mappings: Mapping array)): Number =
    // Approach:
    // Split up the seed ranges into sub-ranges that are covered by the mappings
    // Then, for each sub-range, split and apply the mappings to get (more) new sub-ranges
    // Notice:
    //      1. Never expand the ranges, only split them up into smaller ranges
    // Lastly, extract the lowest location number from the lowest sub-range
    let location_ranges = Array.fold (fun (ranges: Range array) (mapping: Mapping) ->
        // Split up the seed range into sub-ranges that are covered by the mappings
        let mutable new_ranges: Range list = []
        for mapping_range in mapping.ranges do
            for range in ranges do
                // 1. If a mapping range has a starting index inside of a range, add a new range to the list
                if range.start <= mapping_range.source && mapping_range.source < range.start + range.length then
                    new_ranges <- { start = range.start; length = mapping_range.source - range.start } :: new_ranges
                // 2. If a mapping range has an ending index inside of a range, add a new range to the list
                if range.start <= mapping_range.source + mapping_range.length && mapping_range.source + mapping_range.length < range.start + range.length then
                    new_ranges <- { start = mapping_range.source + mapping_range.length; length = range.start + range.length - mapping_range.source - mapping_range.length } :: new_ranges
                // 3. If a mapping range is a subset of a range, add a new range to the list
                if mapping_range.source <= range.start && range.start + range.length <= mapping_range.source + mapping_range.length then
                    new_ranges <- { start = range.start; length = mapping_range.source - range.start } :: new_ranges
                    new_ranges <- { start = mapping_range.source + mapping_range.length; length = range.start + range.length - mapping_range.source - mapping_range.length } :: new_ranges
                // 4. If a mapping range is a superset of a range, add a new range to the list
                if range.start <= mapping_range.source && mapping_range.source + mapping_range.length <= range.start + range.length then
                    // Do nothing
                    ()
                // 5. If a mapping range is disjoint from a range, add the range to the list
                if mapping_range.source + mapping_range.length <= range.start || range.start + range.length <= mapping_range.source then
                    new_ranges <- range :: new_ranges
            
        new_ranges |> Array.ofList) seed_ranges mappings
    // Extract the lowest location number
    location_ranges |> Array.map (fun r -> r.start) |> Array.min
    // 42UL

example_assert 2 1 46UL
    (example_1 |> parse parse_seed_ranges |> solve_p2)

let p2 = input |> parse parse_seed_ranges |> solve_p2
printfn "P2: %A" p2
// Tries:
// 
