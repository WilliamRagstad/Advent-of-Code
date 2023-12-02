use crate::{parse_data};

pub fn solution(input: &str) -> u32 {
    parse_data(input)
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
