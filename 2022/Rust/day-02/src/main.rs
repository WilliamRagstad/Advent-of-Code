mod part1;
mod part2;

#[derive(Copy, Clone)]
pub enum Action {
    Rock = 1,
    Paper = 2,
    Scissors = 3,
}

pub enum Outcome {
    Lose = 0,
    Draw = 3,
    Win = 6,
}

pub fn parse_data<T>(input: &str, parse_round: fn(char, char) -> T)
    -> impl Iterator<Item = T> + '_
    where T: 'static
{
    input
        .lines()
        .map(move |seg| parse_round(seg.chars().nth(0).unwrap(), seg.chars().nth(2).unwrap()))

}

pub fn main() {
    let data = include_str!("../input.txt").replace("\r\n", "\n");
    println!("Result part 1: {}", part1::solution(data.trim()));
    println!("Result part 2: {}", part2::solution(data.trim()));
}
