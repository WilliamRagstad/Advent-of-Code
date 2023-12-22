open System

let format (s: string) = s.Trim().Replace("\r", "")

let lines (s: string) =
    s.Split('\n') |> Array.map (fun l -> l.Trim())

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
- Classify each hand based on the rules of Camel Cards.
- Order hands by their strength and type.
- Calculate total winnings by multiplying each hand's bid by its rank.
**)

let example_1 =
    @"
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
    "
    |> format

type HandType =
    | FiveOfAKind = 7
    | FourOfAKind = 6
    | FullHouse = 5
    | ThreeOfAKind = 4
    | TwoPair = 3
    | OnePair = 2
    | HighCard = 1

type Cards = string

type Hand =
    { Cards: Cards
      Bid: int
      Type: HandType }

let cardValue c =
    match c with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | n -> int (string n)

let cardsValues (s: Cards) = s.ToCharArray() |> Array.map cardValue

let classifyHand (cards: Cards) =
    let groups =
        cards.ToCharArray()
        |> Array.groupBy (fun card -> -cardValue card)
        |> Array.map (fun (value, cards) -> (value, Array.length cards))
        |> Array.sortByDescending (fun (_, count) -> count)
        |> Array.toList

    match groups with
    | [ (_, 5) ] -> HandType.FiveOfAKind
    | [ (_, 4); _ ] -> HandType.FourOfAKind
    | [ (_, 3); (_, 2) ] -> HandType.FullHouse
    | [ (_, 3); _; _ ] -> HandType.ThreeOfAKind
    | [ (_, 2); (_, 2); _ ] -> HandType.TwoPair
    | [ (_, 2); _; _; _ ] -> HandType.OnePair
    | _ -> HandType.HighCard

let parseHand (line: string) : Hand =
    let parts = line.Split(" ")
    let cards = parts.[0]
    let bid = int (parts.[1])

    { Cards = cards
      Bid = bid
      Type = classifyHand cards }

let compareHands h1 h2 =
    if h1.Type <> h2.Type then
        compare h2.Type h1.Type // Stronger type has higher value
    else
        // Tie-breaker
        (Array.zip (cardsValues h1.Cards) (cardsValues h2.Cards))
        |> Array.tryPick (fun (v1, v2) -> if v1 <> v2 then Some(compare v2 v1) else None)
        |> Option.defaultValue 0

let solve_p1 (hands: Hand array) =
    hands
    |> Array.sortWith compareHands
    |> Array.rev
    |> debug
    |> Array.mapi (fun i hand -> hand.Bid * (i + 1))
    // |> Array.map (fun hand -> hand.Bid * (handStrength hand))
    |> Array.sum

example_assert 1 1 6440 (example_1 |> lines |> Array.map parseHand |> solve_p1)
let p1 = input |> lines |> Array.map parseHand |> solve_p1
printfn "P1: %A" p1
// Tries:
// 248113761 - correct
