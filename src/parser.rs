use crate::types::{Ident, Include, StringConstant};
use nom::{
    branch::alt,
    bytes::complete::{escaped, is_a, tag, take_while, take_while_m_n},
    character::{complete::none_of, is_alphabetic, is_alphanumeric},
    combinator::{opt, recognize},
    sequence::{delimited, preceded, tuple},
    IResult,
};

fn is_alphabetic_or_underscore(c: u8) -> bool {
    is_alphabetic(c) || c == b'_'
}

fn is_alphanumeric_or_underscore(c: u8) -> bool {
    is_alphanumeric(c) || c == b'_'
}

pub fn ident(input: &[u8]) -> IResult<&[u8], Ident> {
    let (input, name) = recognize(preceded(
        take_while_m_n(1, 1, is_alphabetic_or_underscore),
        take_while(is_alphanumeric_or_underscore),
    ))(input)?;
    Ok((input, Ident::from_utf8(name.to_vec()).unwrap()))
}

const WHITESPACE: &str = " \t\r\n";

pub fn string_constant(input: &[u8]) -> IResult<&[u8], StringConstant> {
    let (input, string) = delimited(
        tag("\""),
        opt(escaped(none_of("\\\""), '\\', alt((tag("\\"), tag("\""))))),
        tag("\""),
    )(input)?;

    Ok((
        input,
        StringConstant::from_utf8(string.unwrap_or(&b""[..]).to_vec()).unwrap(),
    ))
}

pub fn include(input: &[u8]) -> IResult<&[u8], Include> {
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
        let result = ident(b"foo");
        assert_eq!(result, Ok((&b""[..], "foo".to_owned())));
    }

    #[test]
    fn test_invalid_ident() {
        let result = ident(b"1foo");
        assert!(result.is_err())
    }

    #[test]
    fn test_include() {
        let result = include(b"include \"foo\";");
        assert_eq!(result, Ok((&b""[..], Include("foo".to_owned()))));
    }

    #[test]
    fn test_include_prefix_whitespace() {
        let result = include(b"include     \"foo\";");
        assert_eq!(result, Ok((&b""[..], Include("foo".to_owned()))));
    }

    #[test]
    fn test_include_no_prefix_whitespace() {
        let result = include(b"include\"foo\";");
        assert!(result.is_err());
    }

    #[test]
    fn test_include_trailing_whitespace() {
        let result = include(b"include \"foo\"    ;");
        assert_eq!(result, Ok((&b""[..], Include("foo".to_owned()))));
    }
}
