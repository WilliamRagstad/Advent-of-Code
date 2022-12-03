use std::collections::HashSet;

use crate::{ItemType, intersection, priority};

#[derive(Debug)]

struct Group<T>(Vec<T>, Vec<T>, Vec<T>);

fn parse_data(input: &str) -> impl Iterator<Item = Group<ItemType>> + '_ {
    let parse_item_list = |line: &str| line.chars()
            .map(|c|
                if c.is_ascii_uppercase() { ItemType::Uppercase(c) }
                else { ItemType::Lowercase(c) })
            .collect::<Vec<_>>();

    let mut lines = input.lines();
    let mut groups: Vec<Group<ItemType>> = Vec::new();
    while let (Some(a), Some(b), Some(c)) = (lines.next(), lines.next(), lines.next()) {
        groups.push(Group(parse_item_list(a), parse_item_list(b), parse_item_list(c)));
    }
    groups.into_iter()
}

pub fn solution(input: &str) -> u32 {
    parse_data(input)
        .map(|group| {
            // finding the one item type that is common between all three Elves in each group
            let i_01 = intersection(&group.0, &group.1);
            let i_12 = intersection(&group.1, &group.2);
            let intersection = i_01.intersection(&i_12).cloned().collect::<HashSet<_>>();
            // There must only be one item in the intersection.
            // println!("intersection: {:?}", intersection);
            assert_eq!(intersection.len(), 1);
            // Select the first unique item in the intersection and return its priority.
            priority(*intersection.iter().next().unwrap())
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let input = "
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
";
        let result = solution(input.trim());
        assert_eq!(result, 70);
    }
}
