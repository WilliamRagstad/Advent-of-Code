mod part1;
mod part2;

pub fn parse_data(input: &str) -> impl Iterator<Item = u32> + '_ {
    input
        .split("\n\n")
        .map(|seg| {
            seg.lines()
                .map(|line|
                    line.parse::<u32>()
                    .unwrap_or(0)
                )
                .sum::<u32>()
        })
}

pub fn main() {
    // let data = std::fs::read_to_string("./input.txt").unwrap();
    let data = include_str!("../input.txt").replace("\r\n", "\n");
    println!("Result part 1: {}", part1::solution(&data));
    println!("Result part 2: {}", part2::solution(&data));
}
