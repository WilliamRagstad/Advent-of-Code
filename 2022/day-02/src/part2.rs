use crate::{parse_data, Action, Outcome};
struct RoundGoal(Action, Outcome);

fn score_you_goal(RoundGoal(other, goal): RoundGoal) -> u32 {
    return match goal {
        // First select what move you should make to achieve the goal
        Outcome::Win => match other {
            Action::Rock => Action::Paper,
            Action::Paper => Action::Scissors,
            Action::Scissors => Action::Rock,
        },
        Outcome::Draw => other,
        Outcome::Lose => match other {
            Action::Rock => Action::Scissors,
            Action::Paper => Action::Rock,
            Action::Scissors => Action::Paper,
        },
    } as u32 + goal as u32; // Then add the score of the goal
}

fn parse_round(char1: char, char2: char) -> RoundGoal {
    let parse_action = |char: char| match char {
        'A' => Action::Rock,
        'B' => Action::Paper,
        'C' => Action::Scissors,
        _ => panic!("Invalid action"),
    };
    let parse_goal = |char: char| match char {
        'X' => Outcome::Lose,
        'Y' => Outcome::Draw,
        'Z' => Outcome::Win,
        _ => panic!("Invalid goal"),
    };
    RoundGoal(parse_action(char1), parse_goal(char2))
}

pub fn solution(input: &str) -> u32 {
    println!("{:?}", parse_data(input, parse_round)
        .map(score_you_goal)
        .collect::<Vec<_>>()
    );

    parse_data(input, parse_round)
    .map(score_you_goal)
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
        assert_eq!(result, 12);
    }
}
