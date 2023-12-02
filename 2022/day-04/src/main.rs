mod part1;
mod part2;

pub struct Range(u32, u32);

pub fn parse_data(input: &str) -> impl Iterator<Item = (Range, Range)> + '_ {
    let parse_range = |input: &str| {
        let (from, to) = input.split_once('-').unwrap();
        Range(from.parse().unwrap(), to.parse().unwrap())
    };
    input
        .lines()
        .map(move |seg| {
            let (elf1, elf2) = seg.split_once(',').unwrap();
            (parse_range(elf1), parse_range(elf2))
        })
}

pub fn main() {
    let data = include_str!("../input.txt").replace("\r\n", "\n");
    println!("Result part 1: {}", part1::solution(data.trim()));
    println!("Result part 2: {}", part2::solution(data.trim()));
}
