use crate::{parse_data, Range};

fn contained_in(range1: &Range, range2: &Range) -> bool {
    range1.0 >= range2.0 && range1.1 <= range2.1
}

pub fn solution(input: &str) -> u32 {
    parse_data(input)
        .filter(|(elf1_range, elf2_range)|
            contained_in(&elf1_range, &elf2_range) ||
            contained_in(&elf2_range, &elf1_range))
        .count() as u32
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let input = "
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
";
        let result = solution(input.trim());
        assert_eq!(result, 2);
    }
}
