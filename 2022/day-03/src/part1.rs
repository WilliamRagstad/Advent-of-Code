use crate::{ItemType, intersection, priority};

#[derive(Debug)]
struct Rucksack<T>(Vec<T>, Vec<T>);

fn parse_data(input: &str) -> impl Iterator<Item = Rucksack<ItemType>> + '_ {
    let parse_item_list = |line: &str| line.chars()
            .map(|c|
                if c.is_ascii_uppercase() { ItemType::Uppercase(c) }
                else { ItemType::Lowercase(c) })
            .collect::<Vec<_>>();
    input
        .lines()
        .map(move |line| {
            let (left, right) = line.split_at(line.len() / 2);
            Rucksack(parse_item_list(left), parse_item_list(right))
        })
}

pub fn solution(input: &str) -> u32 {
    parse_data(input)
        .map(|rucksack| {
            let intersection = intersection(&rucksack.0, &rucksack.1);
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
        assert_eq!(result, 157);
    }
}
