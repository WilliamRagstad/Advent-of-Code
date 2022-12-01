use crate::parse_data;

pub fn solution(input: &str) -> u32 {
    parse_data(input)
        .max().unwrap()
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let input = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
";
        let result = solution(input);
        assert_eq!(result, 24000);
    }
}
