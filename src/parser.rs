use crate::types::{Ident, Include, StringConstant};
use nom::{
    branch::alt,
    bytes::complete::{escaped, is_a, tag, take_while, take_while_m_n},
    character::complete::none_of,
    combinator::{opt, recognize},
    sequence::{delimited, preceded, tuple},
    IResult,
};

fn is_alphabetic_or_underscore(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn is_alphanumeric_or_underscore(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

pub fn ident(input: &str) -> IResult<&str, Ident> {
    let (input, name) = recognize(preceded(
        take_while_m_n(1, 1, is_alphabetic_or_underscore),
        take_while(is_alphanumeric_or_underscore),
    ))(input)?;
    Ok((input, name.to_string()))
}

const WHITESPACE: &str = " \t\r\n";

pub fn string_constant(input: &str) -> IResult<&str, StringConstant> {
    let (input, string) = delimited(
        tag("\""),
        opt(escaped(none_of("\\\""), '\\', alt((tag("\\"), tag("\""))))),
        tag("\""),
    )(input)?;

    Ok((input, string.unwrap_or("").to_string()))
}

pub fn include(input: &str) -> IResult<&str, Include> {
    let (input, (_, path, _)) = delimited(
        tag("include"),
        tuple((is_a(WHITESPACE), string_constant, opt(is_a(WHITESPACE)))),
        tag(";"),
    )(input)?;

    Ok((input, Include(path)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_ident() {
        let result = ident("foo");
        assert_eq!(result, Ok(("", "foo".to_owned())));
    }

    #[test]
    fn test_underscore_prefix() {
        let result = ident("_foo");
        assert_eq!(result, Ok(("", "_foo".to_owned())));
    }

    #[test]
    fn test_just_underscore() {
        let result = ident("_");
        assert_eq!(result, Ok(("", "_".to_owned())));
    }

    #[test]
    fn test_id_with_number() {
        let result = ident("foo1");
        assert_eq!(result, Ok(("", "foo1".to_owned())));
    }

    #[test]
    fn test_invalid_ident_contains_valid() {
        let result = ident("1foo");
        assert!(result.is_err());
    }

    #[test]
    fn test_just_number_is_invalid() {
        let result = ident("1");
        assert!(result.is_err());
    }

    #[test]
    fn test_empty_ident() {
        let result = ident("");
        assert!(result.is_err());
    }

    #[test]
    fn test_include() {
        let result = include("include \"foo\";");
        assert_eq!(result, Ok(("", Include("foo".to_owned()))));
    }

    #[test]
    fn test_include_prefix_whitespace() {
        let result = include("include     \"foo\";");
        assert_eq!(result, Ok(("", Include("foo".to_owned()))));
    }

    #[test]
    fn test_include_no_prefix_whitespace() {
        let result = include("include\"foo\";");
        assert!(result.is_err());
    }

    #[test]
    fn test_include_trailing_whitespace() {
        let result = include("include \"foo\"    ;");
        assert_eq!(result, Ok(("", Include("foo".to_owned()))));
    }
}
