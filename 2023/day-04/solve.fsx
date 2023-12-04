open System
open System.Collections.Generic

let input = System.IO.File.ReadAllLines("input.txt")

let debug value =
    printfn "%A" value
    value

let lines (s: string) =
    s.Trim().Split('\n') |> Array.map (fun l -> l.Trim())

// --- Part One ---
(**
The Elf leads you over to the pile of colorful cards.
There, you discover dozens of scratchcards, all with their opaque covering already scratched off.
Picking one up, it looks like each card has two lists of numbers separated
by a vertical bar (|): a list of winning numbers and then a list of numbers you have.
You organize the information into a table (your puzzle input).

As far as the Elf has been able to figure out, you have to figure out which of
the numbers you have appear in the list of winning numbers.
The first match makes the card worth one point and each
match after the first doubles the point value of that card.

For example:

Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
In the above example, card 1 has five winning numbers (41, 48, 83, 86, and 17) and
eight numbers you have (83, 86, 6, 31, 17, 9, 48, and 53). Of the numbers you have,
four of them (48, 83, 17, and 86) are winning numbers!
That means card 1 is worth 8 points (1 for the first match,
then doubled three times for each of the three matches after the first).

Card 2 has two winning numbers (32 and 61), so it is worth 2 points.
Card 3 has two winning numbers (1 and 21), so it is worth 2 points.
Card 4 has one winning number (84), so it is worth 1 point.
Card 5 has no winning numbers, so it is worth no points.
Card 6 has no winning numbers, so it is worth no points.
So, in this example, the Elf's pile of scratchcards is worth 13 points.

Take a seat in the large pile of colorful cards. How many points are they worth in total?

---
Conclusion:
- Each card has two lists of numbers separated by a vertical bar (|):
  a list of winning numbers and then a list of numbers you have.
- The first match makes the card worth one point and each match after the first doubles the point value of that card.
- Find the cards that have the same numbers in both lists.
- Add up the points for each card.

**)

let example_1 =
    @"
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"
    |> lines

let example_2 = @"" |> lines

type Card =
    { id: int
      winning: int array
      yours: int array }

let parse_cards (lines: string array) =
    lines
    |> Array.map (fun line ->
        let parts = line.Split(':')
        let id = parts.[0].Split(' ') |> Array.last |> int

        let parts =
            parts.[1].Split('|')
            |> Array.map (fun part -> part.Replace("  ", " ").Trim().Split(' '))

        let winning = parts.[0] |> Array.map int
        let yours = parts.[1] |> Array.map int

        { id = id
          winning = winning
          yours = yours })

let has_won (card: Card) =
    card.yours |> Array.filter (fun n -> card.winning |> Array.contains n)

let solve_p1 (cards: Card array) =
    cards
    |> Array.map (fun card ->
        let won = (has_won card).Length
        // Trick: 2^(len - 1) gives the correct points for each card
        if won > 0 then Math.Pow(2., float (won - 1)) |> int else 0)
    |> Array.sum

let p1 = input |> parse_cards |> solve_p1
printfn "P1: %A" p1
// Tries:
// 24848 - correct

// --- Part Two ---
(**
There's no such thing as "points".
Instead, scratchcards only cause you to win more scratchcards
equal to the number of winning numbers you have.

Specifically, you win copies of the scratchcards below the
winning card equal to the number of matches.
So, if card 10 were to have 5 matching numbers,
you would win one copy each of cards 11, 12, 13, 14, and 15.

Copies of scratchcards are scored like normal scratchcards
and have the same card number as the card they copied.
So, if you win a copy of card 10 and it has 5 matching numbers,
it would then win a copy of the same cards that the original
card 10 won: cards 11, 12, 13, 14, and 15.
This process repeats until none of the copies cause you to win any more cards.
(Cards will never make you copy a card past the end of the table.)

This time, the above example goes differently:
```
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
```
Card 1 has four matching numbers, so you win one copy each of the next four cards: cards 2, 3, 4, and 5.
Your original card 2 has two matching numbers, so you win one copy each of cards 3 and 4.
Your copy of card 2 also wins one copy each of cards 3 and 4.
Your four instances of card 3 (one original and three copies) have two matching numbers,
so you win four copies each of cards 4 and 5.
Your eight instances of card 4 (one original and seven copies) have one matching number,
so you win eight copies of card 5.
Your fourteen instances of card 5 (one original and thirteen copies) have no matching numbers and win no more cards.
Your one instance of card 6 (one original) has no matching numbers and wins no more cards.
Once all of the originals and copies have been processed,
you end up with 1 instance of card 1, 2 instances of card 2, 4 instances
of card 3, 8 instances of card 4, 14 instances of card 5, and 1 instance of card 6.
In total, this example pile of scratchcards causes you to ultimately have 30 scratchcards!

Process all of the original and copied scratchcards until no more scratchcards are won.
Including the original set of scratchcards, how many total scratchcards do you end up with?

---
Conclusion:
- There's no such thing as "points".
- Instead, scratchcards only cause you to win more scratchcards equal to **the number of winning numbers you have**.
- Specifically, you win copies of the scratchcards below the winning card equal to the number of matches.
  So, if card 10 were to have 5 matching numbers, you would win one copy each of cards 11, 12, 13, 14, and 15.
**)

let solve_p2 (cards: Card array) =
    /// Number of copies of each card
    let copies = Dictionary<int, int>()

    /// Initialize a card with 1 copy
    /// (if it doesn't exist yet)
    let init_copy id =
        if not (copies.ContainsKey(id)) then
            copies.Add(id, 1)

    cards
    |> Array.map (fun card ->
        // Add the current card
        init_copy card.id
        // Get the number of matches (number of cards to copy)
        let won = (has_won card).Length
        // ignore (debug (card.id, won, copies.[card.id]))
        // add a copy of each next cards
        Seq.iter (fun i ->
            let id = card.id + i
            init_copy id
            // Add a copy thanks to the current card
            copies.[id] <- copies.[id] + copies.[card.id]) {
            1..won
        }

        // Return the number of copies for this card
        copies.[card.id])
    |> Array.sum

let p2 = input |> parse_cards |> solve_p2
printfn "P2: %A" p2
// Tries:
// 1137 - too low
// 7258152 - correct
