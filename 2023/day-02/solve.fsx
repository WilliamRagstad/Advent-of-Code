let input = System.IO.File.ReadAllLines("input.txt")

let debug value =
    printfn "%A" value
    value

(*
For example, the record of a few games might look like this:

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
In game 1, three sets of cubes are revealed from the bag (and then put back again).
The first set is 3 blue cubes and 4 red cubes;
the second set is 1 red cube, 2 green cubes, and 6 blue cubes;
the third set is only 2 green cubes.

The Elf would first like to know which games would have been possible if the bag
contained only 12 red cubes, 13 green cubes, and 14 blue cubes?

In the example above, games 1, 2, and 5 would have been possible if the bag
had been loaded with that configuration.
However, game 3 would have been impossible because at one point the Elf
showed you 20 red cubes at once; similarly, game 4 would also have been impossible
because the Elf showed you 15 blue cubes at once.
If you add up the IDs of the games that would have been possible, you get 8.

Determine which games would have been possible if the bag had been loaded with
only 12 red cubes, 13 green cubes, and 14 blue cubes.
What is the sum of the IDs of those games?
*)

type Round = { red: int; green: int; blue: int }
type Game = { id: int; rounds: Round array }

let splitAt (c: char) (s: string) =
    let i = s.IndexOf(c)
    (s.Substring(0, i), s.Substring(i + 1))

let parse_round (round: string) =
    let parts = round.Split(',') |> Array.map (fun part -> part.Trim() |> splitAt ' ')
    let mutable red = 0
    let mutable green = 0
    let mutable blue = 0

    for count, color in parts do
        match color with
        | "red" -> red <- int count
        | "green" -> green <- int count
        | "blue" -> blue <- int count
        | _ -> failwith "Unknown color"

    { red = red
      green = green
      blue = blue }

let parse_game (line: string) =
    let parts = line.Split(':')
    let id = parts.[0].Trim().Split(' ')[1] |> int

    let rounds =
        parts.[1].Trim().Split(';')
        |> Array.map (fun round -> round.Trim())
        |> Array.map parse_round

    { id = id; rounds = rounds }

let parse_input (input: string array) =
    input |> Array.map (fun line -> line.Trim()) |> Array.map parse_game

// --- Part One ---
let solve_p1 (red: int) (green: int) (blue: int) (games: Game array) =
    games
    |> Array.filter (fun game ->
        game.rounds
        |> Array.forall (fun round -> round.red <= red && round.green <= green && round.blue <= blue))
    |> Array.map (fun game -> game.id)
    |> Array.sum

let p1 = parse_input input |> solve_p1 12 13 14
printfn "P1: %d" p1
// Tries:
// 2406 - correct

// --- Part Two ---
(*
As you continue your walk, the Elf poses a second question:
in each game you played, what is the fewest number of cubes of each color
that could have been in the bag to make the game possible?

Again consider the example games from earlier:

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

In game 1, the game could have been played with as few as 4 red, 2 green, and 6 blue cubes.
If any color had even one fewer cube, the game would have been impossible.
Game 2 could have been played with a minimum of 1 red, 3 green, and 4 blue cubes.
Game 3 must have been played with at least 20 red, 13 green, and 6 blue cubes.
Game 4 required at least 14 red, 3 green, and 15 blue cubes.
Game 5 needed no fewer than 6 red, 3 green, and 2 blue cubes in the bag.

## Task:
The power of a set of cubes is equal to the numbers of red, green, and blue cubes multiplied together.
The power of the minimum set of cubes in game 1 is 48. In games 2-5 it was 12, 1560, 630, and 36, respectively.
Adding up these five powers produces the sum 2286.
*)

let solve_p2 (games: Game array) =
    Array.map
        (fun game ->
            let mutable red = 0
            let mutable green = 0
            let mutable blue = 0

            for round in game.rounds do
                red <- max red round.red
                green <- max green round.green
                blue <- max blue round.blue

            red * green * blue)
        games
    |> Array.sum

let p2 = parse_input input |> solve_p2
printfn "P2: %d" p2

// Tries:
// 8000 - too low
// 78375 - correct
