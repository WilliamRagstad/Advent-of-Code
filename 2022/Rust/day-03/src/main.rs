use std::collections::HashSet;
use std::hash::Hash;

mod part1;
mod part2;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ItemType {
    Uppercase(char),
    Lowercase(char),
}

pub fn intersection<T>(lst1: &Vec<T>, lst2: &Vec<T>) -> HashSet<T>
where T: Clone + PartialEq + Eq + Hash + 'static
{
    lst1.iter().filter(|x| lst2.contains(x)).cloned().collect::<HashSet<_>>()
}

pub fn priority(item: ItemType) -> u32 { match item {
        ItemType::Uppercase(c) => c as u32 - 'A' as u32 + 27,
        ItemType::Lowercase(c) => c as u32 - 'a' as u32 + 1,
} }

pub fn main() {
    let data = include_str!("../input.txt").replace("\r\n", "\n");
    println!("Result part 1: {}", part1::solution(data.trim()));
    println!("Result part 2: {}", part2::solution(data.trim()));
}
