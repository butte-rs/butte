use crate::types::*;
use hexf_parse::parse_hexf64;
use nom::{
    self,
    branch::alt,
    bytes::complete::{escaped, tag, take_while, take_while_m_n},
    character::complete::{
        char, digit0, digit1, hex_digit0, hex_digit1, multispace0, multispace1,
        none_of, one_of, space0, space1,
    },
    combinator::{map, map_res, opt, recognize, value as value_if_succeeds},
    multi::{many0, many1, separated_list, separated_nonempty_list},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::collections::HashMap;
use std::iter::FromIterator;
use std::str::FromStr;

fn plus_or_minus(input: &str) -> IResult<&str, char> {
    one_of("-+")(input)
}

#[test]
fn test_plus_or_minus() {
    let res = plus_or_minus("+");
    assert_eq!(res, Ok(("", '+')));

    let res = plus_or_minus("-");
    assert_eq!(res, Ok(("", '-')));

    let res = plus_or_minus("/");
    assert!(res.is_err());
}

fn double_quote(input: &str) -> IResult<&str, char> {
    char('\"')(input)
}

fn backslash(input: &str) -> IResult<&str, char> {
    char('\\')(input)
}

fn left_paren(input: &str) -> IResult<&str, char> {
    char('(')(input)
}

fn right_paren(input: &str) -> IResult<&str, char> {
    char(')')(input)
}

fn left_brace(input: &str) -> IResult<&str, char> {
    char('{')(input)
}

fn right_brace(input: &str) -> IResult<&str, char> {
    char('}')(input)
}

fn colon(input: &str) -> IResult<&str, char> {
    char(':')(input)
}

fn comma(input: &str) -> IResult<&str, char> {
    char(',')(input)
}

fn semicolon(input: &str) -> IResult<&str, char> {
    char(';')(input)
}

fn equals(input: &str) -> IResult<&str, char> {
    char('=')(input)
}

fn period(input: &str) -> IResult<&str, char> {
    char('.')(input)
}

fn zero(input: &str) -> IResult<&str, char> {
    char('0')(input)
}

fn ident(input: &str) -> IResult<&str, Ident> {
    map(
        recognize(preceded(
            take_while_m_n(1, 1, |c: char| c.is_alphabetic() || c == '_'),
            take_while(|c: char| c.is_alphanumeric() || c == '_'),
        )),
        Ident,
    )(input)
}

#[test]
fn test_simple_ident() {
    let result = ident("foo");
    assert_eq!(result, Ok(("", Ident("foo"))));
}

#[test]
fn test_underscore_prefix() {
    let result = ident("_foo");
    assert_eq!(result, Ok(("", Ident("_foo"))));
}

#[test]
fn test_just_underscore() {
    let result = ident("_");
    assert_eq!(result, Ok(("", Ident("_"))));
}

#[test]
fn test_id_with_number() {
    let result = ident("foo1");
    assert_eq!(result, Ok(("", Ident("foo1"))));
}

#[test]
fn test_invalid_ident_contains_valid() {
    let result = ident("1foo");
    assert!(result.is_err());
}

#[test]
fn test_number_is_invalid() {
    let result = ident("1");
    assert!(result.is_err());
}

#[test]
fn test_empty_ident_is_invalid() {
    let result = ident("");
    assert!(result.is_err());
}

fn string_constant(input: &str) -> IResult<&str, &str> {
    map(
        delimited(
            double_quote,
            opt(escaped(
                none_of("\\\""),
                '\\',
                alt((backslash, double_quote)),
            )),
            double_quote,
        ),
        |string| string.unwrap_or(""),
    )(input)
}

#[test]
fn test_string_constant() {
    let res = string_constant("\"a b c D \\\"z1\"");
    assert_eq!(res, Ok(("", "a b c D \\\"z1")));
}

#[test]
fn test_empty_string_constant() {
    let res = string_constant("\"\"");
    assert_eq!(res, Ok(("", "")));
}

fn element(input: &str) -> IResult<&str, Element> {
    alt((
        map(namespace_decl, Element::Namespace),
        map(type_decl, Element::ProductType),
        map(enum_decl, Element::Enum),
        map(root_decl, Element::Root),
        map(file_extension_decl, Element::FileExtension),
        map(file_identifier_decl, Element::FileIdentifier),
        map(attribute_decl, Element::Attribute),
        map(rpc_decl, Element::Rpc),
        map(object, Element::Object),
    ))(input)
}

pub fn schema(input: &str) -> IResult<&str, Schema> {
    map(
        terminated(
            tuple((
                many0(delimited(multispace0, include, multispace0)),
                many0(delimited(multispace0, element, multispace0)),
            )),
            multispace0,
        ),
        |(includes, body)| Schema { includes, body },
    )(input)
}

#[test]
fn test_elements_only_schema() {
    let input = "\
table MyMessage {
  message: string;
  foo: float64 = 2;
}";
    let res = schema(input);
    let expected = Schema {
        includes: vec![],
        body: vec![Element::ProductType(ProductType {
            kind: ProductKind::Table,
            name: Ident("MyMessage"),
            metadata: None,
            fields: vec![
                Field::new(Ident("message"), Type::String),
                Field::new(Ident("foo"), Type::Float64)
                    .with_scalar(Scalar::Integer(2)),
            ],
        })],
    };
    assert_eq!(res, Ok(("", expected)));
}

#[test]
fn test_includes_only_schema() {
    let input = r#"include "a";
include "b";


include "foo/bar/baz.fbs";

    "#;
    let res = schema(input);
    let expected = Schema {
        includes: vec![Include("a"), Include("b"), Include("foo/bar/baz.fbs")],
        body: vec![],
    };
    assert_eq!(res, Ok(("", expected)));
}

fn include(input: &str) -> IResult<&str, Include> {
    map(
        delimited(
            tag("include"),
            delimited(space1, string_constant, space0),
            semicolon,
        ),
        Include,
    )(input)
}

#[test]
fn test_include() {
    let result = include("include \"foo\";");
    assert_eq!(result, Ok(("", Include("foo"))));
}

#[test]
fn test_include_prefix_whitespace() {
    let result = include("include     \"foo\";");
    assert_eq!(result, Ok(("", Include("foo"))));
}

#[test]
fn test_include_no_prefix_whitespace() {
    let result = include("include\"foo\";");
    assert!(result.is_err());
}

#[test]
fn test_include_trailing_whitespace() {
    let result = include("include \"foo\"    ;");
    assert_eq!(result, Ok(("", Include("foo"))));
}

fn namespace_decl(input: &str) -> IResult<&str, Namespace> {
    map(
        delimited(
            tag("namespace"),
            delimited(space1, separated_nonempty_list(tag("."), ident), space0),
            semicolon,
        ),
        Namespace,
    )(input)
}

#[test]
fn test_one_level_namespace_decl() {
    let result = namespace_decl("namespace a;");
    let expected = Namespace(vec![Ident("a")]);
    assert_eq!(result, Ok(("", expected)));
}

#[test]
fn test_two_level_namespace_decl() {
    let result = namespace_decl("namespace a.b;");
    let expected = Namespace(vec![Ident("a"), Ident("b")]);
    assert_eq!(result, Ok(("", expected)));
}

#[test]
fn test_three_level_namespace_decl() {
    let result = namespace_decl("namespace a.b.c;");
    let expected = Namespace(vec![Ident("a"), Ident("b"), Ident("c")]);
    assert_eq!(result, Ok(("", expected)));
}

fn attribute_decl(input: &str) -> IResult<&str, Attribute> {
    map(
        delimited(
            tag("attribute"),
            delimited(
                multispace1,
                alt((ident, map(string_constant, Ident))),
                multispace0,
            ),
            semicolon,
        ),
        Attribute,
    )(input)
}

#[test]
fn test_simple_attribute_decl() {
    let result = attribute_decl("attribute a;");
    assert_eq!(result, Ok(("", Attribute(Ident("a")))));
}

#[test]
fn test_quoted_attribute_decl() {
    let result = attribute_decl("attribute \"a\";");
    assert_eq!(result, Ok(("", Attribute(Ident("a")))));
}

fn enum_decl(input: &str) -> IResult<&str, Enum> {
    map(
        tuple((
            alt((
                map(
                    tuple((
                        preceded(tag("enum"), delimited(space1, ident, space0)),
                        preceded(colon, ty),
                    )),
                    |(id, t)| (id, EnumKind::Enum(t)),
                ),
                map(
                    preceded(tag("union"), delimited(space1, ident, space0)),
                    |id| (id, EnumKind::Union),
                ),
            )),
            metadata,
            delimited(
                delimited(space0, left_brace, multispace0),
                separated_nonempty_list(comma, enumval_decl),
                preceded(multispace0, right_brace),
            ),
        )),
        |((ident, kind), metadata, values)| Enum {
            kind,
            metadata,
            values,
            ident,
        },
    )(input)
}

fn root_decl(input: &str) -> IResult<&str, Root> {
    // TODO: Use type scope mapping here or in codegen?
    map(
        delimited(
            tag("root_type"),
            delimited(space1, ident, space0),
            semicolon,
        ),
        Root,
    )(input)
}

#[test]
fn test_root_decl() {
    let res = root_decl("root_type Foo;");
    assert_eq!(res, Ok(("", Root(Ident("Foo")))));
}

fn field_decl(input: &str) -> IResult<&str, Field> {
    map(
        terminated(
            tuple((
                terminated(ident, tuple((space0, colon, space0))),
                ty,
                opt(preceded(
                    tuple((space0, equals, space0)),
                    terminated(scalar, space0),
                )),
                metadata,
            )),
            tuple((space0, semicolon)),
        ),
        |(name, ty, scalar, metadata)| Field {
            name,
            ty,
            scalar,
            metadata,
        },
    )(input)
}

#[test]
fn test_field_decl() {
    let input = "foo: float64 = 2;";
    let res = field_decl(input);
    assert_eq!(
        res,
        Ok((
            "",
            Field::new(Ident("foo"), Type::Float64)
                .with_scalar(Scalar::Integer(2))
        ))
    );
}

#[test]
fn test_field_decl_uint() {
    let input = "foo :uint=3.0;";
    let res = field_decl(input);
    assert_eq!(
        res,
        Ok((
            "",
            Field::new(Ident("foo"), Type::UInt)
                .with_scalar(Scalar::Float(3.0))
        ))
    );
}

#[test]
fn test_field_decl_no_scalar() {
    let input = "foo:float64    ;";
    let res = field_decl(input);
    let expected = Field::new(Ident("foo"), Type::Float64);
    assert_eq!(res, Ok(("", expected)));
}

fn rpc_decl(input: &str) -> IResult<&str, Rpc> {
    map(
        tuple((
            preceded(tag("rpc_service"), preceded(space1, ident)),
            delimited(
                delimited(multispace1, left_brace, multispace0),
                many1(rpc_method),
                delimited(multispace1, right_brace, multispace0),
            ),
        )),
        |(name, methods)| Rpc { name, methods },
    )(input)
}

fn rpc_method(input: &str) -> IResult<&str, RpcMethod> {
    map(
        terminated(
            tuple((
                terminated(ident, space1),
                delimited(
                    left_paren,
                    delimited(space0, ident, space0),
                    right_paren,
                ),
                preceded(
                    delimited(space0, colon, space0),
                    tuple((ident, preceded(space1, metadata))),
                ),
            )),
            preceded(space0, semicolon),
        ),
        |(name, request_type, (response_type, metadata))| RpcMethod {
            name,
            request_type,
            response_type,
            metadata,
        },
    )(input)
}

fn ty(input: &str) -> IResult<&str, Type> {
    alt((
        alt((
            value_if_succeeds(Type::Bool, tag("bool")),
            value_if_succeeds(Type::Byte, tag("byte")),
            value_if_succeeds(Type::UByte, tag("ubyte")),
            value_if_succeeds(Type::Short, tag("short")),
            value_if_succeeds(Type::UShort, tag("ushort")),
            value_if_succeeds(Type::Long, tag("long")),
            value_if_succeeds(Type::ULong, tag("ulong")),
            value_if_succeeds(Type::Double, tag("double")),
            value_if_succeeds(Type::Int8, tag("int8")),
            value_if_succeeds(Type::UInt8, tag("uint8")),
            value_if_succeeds(Type::Int16, tag("int16")),
            value_if_succeeds(Type::UInt16, tag("uint16")),
            value_if_succeeds(Type::Int32, tag("int32")),
            value_if_succeeds(Type::UInt32, tag("uint32")),
            value_if_succeeds(Type::Int64, tag("int64")),
            value_if_succeeds(Type::UInt64, tag("uint64")),
            value_if_succeeds(Type::Float32, tag("float32")),
            value_if_succeeds(Type::Float64, tag("float64")),
            value_if_succeeds(Type::Int, tag("int")),
            value_if_succeeds(Type::UInt, tag("uint")),
            value_if_succeeds(Type::Float, tag("float")),
        )),
        value_if_succeeds(Type::String, tag("string")),
        map(delimited(tag("["), ty, tag("]")), |t| {
            Type::Array(Box::new(t))
        }),
        map(ident, Type::Ident),
    ))(input)
}

fn enumval_decl(input: &str) -> IResult<&str, EnumVal> {
    map(tuple((ident, opt(integer_constant))), |(name, value)| {
        EnumVal { name, value }
    })(input)
}

fn metadata(input: &str) -> IResult<&str, Option<Metadata>> {
    opt(map(
        delimited(
            left_paren,
            separated_list(
                comma,
                separated_pair(ident, colon, opt(single_value)),
            ),
            right_paren,
        ),
        |values| Metadata(HashMap::from_iter(values)),
    ))(input)
}

fn scalar(input: &str) -> IResult<&str, Scalar> {
    alt((
        map(float_constant, Scalar::Float),
        map(integer_constant, Scalar::Integer),
    ))(input)
}

fn object(input: &str) -> IResult<&str, Object> {
    map(
        delimited(
            left_brace,
            separated_nonempty_list(comma, separated_pair(ident, colon, value)),
            right_brace,
        ),
        |values| Object(HashMap::from_iter(values)),
    )(input)
}

fn single_value(input: &str) -> IResult<&str, SingleValue> {
    alt((
        map(scalar, SingleValue::Scalar),
        map(string_constant, SingleValue::StringConstant),
    ))(input)
}

fn value(input: &str) -> IResult<&str, Value> {
    alt((
        map(single_value, Value::SingleValue),
        map(object, Value::Object),
    ))(input)
}

fn type_decl(input: &str) -> IResult<&str, ProductType> {
    map(
        tuple((
            alt((
                value_if_succeeds(ProductKind::Table, tag("table")),
                value_if_succeeds(ProductKind::Struct, tag("struct")),
            )),
            delimited(multispace1, ident, multispace0),
            terminated(metadata, multispace0),
            delimited(
                left_brace,
                delimited(multispace0, many1(field_decl), multispace0),
                right_brace,
            ),
        )),
        |(kind, name, metadata, fields)| ProductType {
            kind,
            name,
            fields,
            metadata,
        },
    )(input)
}

#[test]
fn test_product_type_multiple_fields() {
    let table = "\
table HelloReply {
  message: string;
  foo :uint=3.0;
}";
    let res = type_decl(table);
    assert_eq!(
        res,
        Ok((
            "",
            ProductType {
                kind: ProductKind::Table,
                name: Ident("HelloReply"),
                fields: vec![
                    Field {
                        name: Ident("message"),
                        ty: Type::String,
                        scalar: None,
                        metadata: None
                    },
                    Field {
                        name: Ident("foo"),
                        ty: Type::UInt,
                        scalar: Some(Scalar::Float(3.0)),
                        metadata: None,
                    }
                ],
                metadata: None
            }
        ))
    );
}

#[test]
fn test_product_type() {
    let table = "\
table HelloReply {
  message: string;
}";
    let res = type_decl(table);
    assert_eq!(
        res,
        Ok((
            "",
            ProductType {
                kind: ProductKind::Table,
                name: Ident("HelloReply"),
                fields: vec![Field {
                    name: Ident("message"),
                    ty: Type::String,
                    scalar: None,
                    metadata: None
                }],
                metadata: None
            }
        ))
    );
}

fn dec_integer_constant(input: &str) -> IResult<&str, IntegerConstant> {
    map_res(recognize(preceded(opt(plus_or_minus), digit1)), |value| {
        IntegerConstant::from_str_radix(value, 10)
    })(input)
}

#[test]
fn test_dec_integer_constant() {
    let res = dec_integer_constant("1234");
    assert_eq!(res, Ok(("", 1234)));

    let res = dec_integer_constant("-1234");
    assert_eq!(res, Ok(("", -1234)));
}

fn hex_integer_constant(input: &str) -> IResult<&str, IntegerConstant> {
    map(
        tuple((
            opt(plus_or_minus),
            preceded(
                zero,
                preceded(
                    one_of("xX"),
                    map_res(hex_digit1, |value| {
                        IntegerConstant::from_str_radix(value, 16)
                    }),
                ),
            ),
        )),
        |(sign, value)| {
            if let Some('-') = sign {
                -value
            } else {
                value
            }
        },
    )(input)
}

#[test]
fn test_hex_integer_constant() {
    let res = hex_integer_constant("0x1234ABCDEFabcdef");
    assert_eq!(res, Ok(("", 0x1234ABCDEFabcdef)));

    let res = hex_integer_constant("-0x1234ABCDEFabcdef");
    assert_eq!(res, Ok(("", -0x1234ABCDEFabcdef)));

    let res = hex_integer_constant("ABCDEFabcdef");
    assert!(res.is_err());
}

fn true_(input: &str) -> IResult<&str, bool> {
    value_if_succeeds(true, |input: &str| nom::re_match!(input, r"\btrue\b"))(
        input,
    )
}

#[test]
fn test_true() {
    let res = true_("true");
    assert_eq!(res, Ok(("", true)));
}

#[test]
fn test_invalid_true() {
    let res = true_("truez");
    assert!(res.is_err());
}

fn false_(input: &str) -> IResult<&str, bool> {
    value_if_succeeds(false, |input: &str| nom::re_match!(input, r"\bfalse\b"))(
        input,
    )
}

#[test]
fn test_false() {
    let res = false_("false");
    assert_eq!(res, Ok(("", false)));
}

#[test]
fn test_invalid_false() {
    let res = false_("falsez");
    assert!(res.is_err());
}

fn boolean_constant(input: &str) -> IResult<&str, bool> {
    alt((true_, false_))(input)
}

#[test]
fn test_boolean_constant() {
    let res = boolean_constant("true");
    assert_eq!(res, Ok(("", true)));

    let res = boolean_constant("false");
    assert_eq!(res, Ok(("", false)));

    let res = boolean_constant("waltz");
    assert!(res.is_err());
}

fn floating_exponent(input: &str) -> IResult<&str, &str> {
    recognize(tuple((one_of("eE"), opt(plus_or_minus), digit1)))(input)
}

// ([0-9](?:[0-9])*\.(?:[0-9](?:[0-9])*)?|\.[0-9](?:[0-9])*)
// ([eE][-+]?[0-9](?:[0-9])*)?
// |
// [0-9](?:[0-9])*[eE][-+]?[0-9](?:[0-9])*
fn floating_constant(input: &str) -> IResult<&str, FloatingConstant> {
    let parser = alt((
        recognize(terminated(
            alt((
                recognize(tuple((digit1, period, digit0))),
                recognize(preceded(period, digit1)),
            )),
            opt(floating_exponent),
        )),
        recognize(terminated(digit1, floating_exponent)),
    ));
    map_res(recognize(parser), FloatingConstant::from_str)(input)
}

/// A float constant is either a special float constant, a hex float constant or a double
fn float_constant(input: &str) -> IResult<&str, FloatingConstant> {
    alt((
        special_float_constant,
        hex_float_constant,
        floating_constant,
    ))(input)
}

#[test]
fn test_float_constant_nan() {
    let res = float_constant("nan");
    assert_eq!(
        res.map(|(input, value)| (
            input,
            value.is_nan() && value.is_sign_positive()
        )),
        Ok(("", true))
    );

    let res = float_constant("-nan");
    assert_eq!(
        res.map(|(input, value)| (
            input,
            value.is_nan() && value.is_sign_negative()
        )),
        Ok(("", true))
    );
}

#[test]
fn test_float_constant_inf() {
    let res = float_constant("inf");
    assert_eq!(res, Ok(("", std::f64::INFINITY)));

    let res = float_constant("-inf");
    assert_eq!(res, Ok(("", std::f64::NEG_INFINITY)));
}

#[test]
fn test_float_constant_infinity() {
    let res = float_constant("infinity");
    assert_eq!(res, Ok(("", std::f64::INFINITY)));

    let res = float_constant("-infinity");
    assert_eq!(res, Ok(("", std::f64::NEG_INFINITY)));
}

/// FIXME: This recognizes hex integers as well
fn hex_float_constant(input: &str) -> IResult<&str, FloatingConstant> {
    map_res(
        recognize(preceded(
            opt(plus_or_minus),
            preceded(
                zero,
                preceded(
                    one_of("xX"),
                    terminated(
                        alt((
                            recognize(preceded(period, hex_digit1)),
                            recognize(delimited(
                                hex_digit1, period, hex_digit0,
                            )),
                            hex_digit1,
                        )),
                        preceded(
                            one_of("pP"),
                            recognize(preceded(opt(plus_or_minus), digit1)),
                        ),
                    ),
                ),
            ),
        )),
        |value| parse_hexf64(value, false),
    )(input)
}

/// Parse `nan`
fn nan(input: &str) -> IResult<&str, FloatingConstant> {
    map(
        terminated(opt(plus_or_minus), |input| {
            nom::re_match!(input, r"\bnan\b")
        }),
        |sign| {
            if let Some('-') = sign {
                -std::f64::NAN
            } else {
                std::f64::NAN
            }
        },
    )(input)
}

#[test]
fn test_nan() {
    let res = nan("nan");
    assert_eq!(
        res.map(|(input, value)| (input, value.is_nan())),
        Ok(("", true))
    );
}

#[test]
fn test_invalid_nan() {
    let res = nan("nanz");
    assert!(res.is_err());
}

/// Parse `inf` or `infinity`
fn inf_or_infinity(input: &str) -> IResult<&str, FloatingConstant> {
    map(
        terminated(opt(plus_or_minus), |input| {
            nom::re_match!(input, r"\binf(inity)?\b")
        }),
        |sign| {
            if let Some('-') = sign {
                std::f64::NEG_INFINITY
            } else {
                std::f64::INFINITY
            }
        },
    )(input)
}

#[test]
fn test_inf_or_infinity_inf() {
    let res = inf_or_infinity("inf");
    assert_eq!(res, Ok(("", std::f64::INFINITY)));

    let res = inf_or_infinity("-inf");
    assert_eq!(res, Ok(("", std::f64::NEG_INFINITY)));
}

#[test]
fn test_inf_or_infinity_infinity() {
    let res = inf_or_infinity("infinity");
    assert_eq!(res, Ok(("", std::f64::INFINITY)));

    let res = inf_or_infinity("-infinity");
    assert_eq!(res, Ok(("", std::f64::NEG_INFINITY)));
}

/// Parse `nan`, `inf`, or `infiniity`
fn special_float_constant(input: &str) -> IResult<&str, FloatingConstant> {
    alt((nan, inf_or_infinity))(input)
}

#[test]
fn test_special_float_constant_nan() {
    let res = special_float_constant("nan");
    assert_eq!(
        res.map(|(input, value)| (
            input,
            value.is_nan(),
            value.is_sign_positive()
        )),
        Ok(("", true, true))
    );

    let res = special_float_constant("-nan");
    assert_eq!(
        res.map(|(input, value)| (
            input,
            value.is_nan(),
            value.is_sign_negative()
        )),
        Ok(("", true, true))
    );
}

#[test]
fn test_special_float_constant_inf() {
    let res = special_float_constant("inf");
    assert_eq!(res, Ok(("", std::f64::INFINITY)));

    let res = special_float_constant("-inf");
    assert_eq!(res, Ok(("", std::f64::NEG_INFINITY)));
}

#[test]
fn test_special_float_constant_infinity() {
    let res = special_float_constant("infinity");
    assert_eq!(res, Ok(("", std::f64::INFINITY)));

    let res = special_float_constant("-infinity");
    assert_eq!(res, Ok(("", std::f64::NEG_INFINITY)));
}

fn integer_constant(input: &str) -> IResult<&str, IntegerConstant> {
    alt((hex_integer_constant, dec_integer_constant))(input)
}

#[test]
fn test_integer_constant() {
    let res = integer_constant("1234");
    assert_eq!(res, Ok(("", 1234)));

    let res = integer_constant("-1234");
    assert_eq!(res, Ok(("", -1234)));

    let res = integer_constant("0x1234");
    assert_eq!(res, Ok(("", 0x1234)));

    let res = integer_constant("-0x1234");
    assert_eq!(res, Ok(("", -0x1234)));
}

fn file_extension_decl(input: &str) -> IResult<&str, FileExtension> {
    map(
        delimited(
            tag("file_extension"),
            delimited(space0, string_constant, space0),
            semicolon,
        ),
        FileExtension,
    )(input)
}

#[test]
fn test_file_extension_decl() {
    let res = file_extension_decl("file_extension \"foo\";");
    assert_eq!(res, Ok(("", FileExtension("foo"))));
}

#[test]
fn test_file_extension_decl_no_leading_space() {
    let res = file_extension_decl("file_extension\"foo\";");
    assert_eq!(res, Ok(("", FileExtension("foo"))));
}

#[test]
fn test_file_extension_decl_trailing_space() {
    let res = file_extension_decl("file_extension \"foo\"  ;");
    assert_eq!(res, Ok(("", FileExtension("foo"))));
}

#[test]
fn test_file_extension_decl_surrounding_space() {
    let res = file_extension_decl("file_extension   \"foo\"  ;");
    assert_eq!(res, Ok(("", FileExtension("foo"))));
}

fn file_identifier_decl(input: &str) -> IResult<&str, FileIdentifier> {
    map(
        delimited(
            tag("file_identifier"),
            delimited(space0, string_constant, space0),
            semicolon,
        ),
        FileIdentifier,
    )(input)
}

#[test]
fn test_file_identifier_decl() {
    let res = file_identifier_decl("file_identifier \"foo\";");
    assert_eq!(res, Ok(("", FileIdentifier("foo"))));
}

#[test]
fn test_file_identifier_decl_no_leading_space() {
    let res = file_identifier_decl("file_identifier\"foo\";");
    assert_eq!(res, Ok(("", FileIdentifier("foo"))));
}

#[test]
fn test_file_identifier_decl_trailing_space() {
    let res = file_identifier_decl("file_identifier \"foo\"  ;");
    assert_eq!(res, Ok(("", FileIdentifier("foo"))));
}

#[test]
fn test_file_identifier_decl_surrounding_space() {
    let res = file_identifier_decl("file_identifier   \"foo\"  ;");
    assert_eq!(res, Ok(("", FileIdentifier("foo"))));
}
