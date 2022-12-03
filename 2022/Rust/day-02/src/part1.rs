use crate::{parse_data, Action, Outcome};
struct RoundAction(Action, Action);

fn score_you_action(RoundAction(other, you): RoundAction) -> u32 {
    return match (other, you) {
        // Rules of rock-paper-scissors
        (Action::Rock,      Action::Rock)       => Outcome::Draw,
        (Action::Rock,      Action::Paper)      => Outcome::Win,
        (Action::Rock,      Action::Scissors)   => Outcome::Lose,
        (Action::Paper,     Action::Rock)       => Outcome::Lose,
        (Action::Paper,     Action::Paper)      => Outcome::Draw,
        (Action::Paper,     Action::Scissors)   => Outcome::Win,
        (Action::Scissors,  Action::Rock)       => Outcome::Win,
        (Action::Scissors,  Action::Paper)      => Outcome::Lose,
        (Action::Scissors,  Action::Scissors)   => Outcome::Draw,
    } as u32 + you as u32;
}

fn parse_round(char1: char, char2: char) -> RoundAction {
    let parse_action = |char: char| match char {
        'A' | 'X' => Action::Rock,
        'B' | 'Y' => Action::Paper,
        'C' | 'Z' => Action::Scissors,
        _ => panic!("Invalid action"),
    };
    RoundAction(parse_action(char1), parse_action(char2))
}

pub fn solution(input: &str) -> u32 {
    println!("{:?}", parse_data(input, parse_round)
        .map(score_you_action)
        .collect::<Vec<_>>()
    );

    parse_data(input, parse_round)
    .map(score_you_action)
    .sum()
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let input = "
A Y
B X
C Z
";
        let result = solution(input.trim());
        assert_eq!(result, 15);
    }
}
