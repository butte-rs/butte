use crate::types::*;
use hexf_parse::parse_hexf64;
#[cfg(test)]
use nom::error::ErrorKind;
use nom::{
    self,
    branch::alt,
    bytes::complete::{escaped, tag, take_while, take_while_m_n},
    character::complete::{
        char, digit0, digit1, hex_digit0, hex_digit1, multispace0, multispace1,
        none_of, one_of, space0, space1,
    },
    combinator::{map, map_res, opt, recognize, value},
    multi::{many0, many1, separated_list, separated_nonempty_list},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::collections::HashMap;
use std::iter::FromIterator;
use std::str::FromStr;

#[cfg(test)]
macro_rules! assert_failed_parse {
    ($left:expr, $rest:expr, $error_kind:expr) => {
        assert_eq!($left, Err(nom::Err::Error(($rest, $error_kind))))
    };
}

#[cfg(test)]
macro_rules! assert_successful_parse {
    ($left:expr, $right:expr) => {
        assert_eq!($left, Ok(("", $right)))
    };
}

fn plus_or_minus(input: &str) -> IResult<&str, char> {
    one_of("-+")(input)
}

#[test]
fn test_plus_or_minus() {
    assert_successful_parse!(plus_or_minus("+"), '+');
    assert_successful_parse!(plus_or_minus("-"), '-');
}

#[test]
fn test_invalid_plus_or_minus() {
    assert_failed_parse!(plus_or_minus("/"), "/", ErrorKind::OneOf);
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

fn left_square_bracket(input: &str) -> IResult<&str, char> {
    char('[')(input)
}

fn right_square_bracket(input: &str) -> IResult<&str, char> {
    char(']')(input)
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
    assert_successful_parse!(ident("foo"), Ident("foo"));
}

#[test]
fn test_underscore_prefix() {
    assert_successful_parse!(ident("_foo"), Ident("_foo"));
}

#[test]
fn test_just_underscore() {
    assert_successful_parse!(ident("_"), Ident("_"));
}

#[test]
fn test_id_with_number() {
    assert_successful_parse!(ident("foo1"), Ident("foo1"));
}

#[test]
fn test_invalid_ident_contains_valid() {
    assert_failed_parse!(ident("1foo"), "1foo", ErrorKind::TakeWhileMN);
}

#[test]
fn test_number_is_invalid() {
    let result = ident("1");
    assert_failed_parse!(result, "1", ErrorKind::TakeWhileMN)
}

#[test]
fn test_empty_ident_is_invalid() {
    let result = ident("");
    assert_failed_parse!(result, "", ErrorKind::TakeWhileMN)
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
    let result = string_constant("\"a b c D \\\"z1\"");
    assert_successful_parse!(result, "a b c D \\\"z1");
}

#[test]
fn test_empty_string_constant() {
    let result = string_constant("\"\"");
    assert_successful_parse!(result, "");
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
    let result = schema(input);
    let expected = Schema {
        includes: vec![],
        body: vec![Element::ProductType(table(
            Ident("MyMessage"),
            vec![
                Field::new(Ident("message"), Type::String),
                Field::new(Ident("foo"), Type::Float64)
                    .with_scalar(Scalar::Integer(2)),
            ],
        ))],
    };
    assert_successful_parse!(result, expected);
}

#[test]
fn test_includes_only_schema() {
    let input = r#"include "a";
include "b";


include "foo/bar/baz.fbs";

    "#;
    let result = schema(input);
    let expected = Schema {
        includes: vec![Include("a"), Include("b"), Include("foo/bar/baz.fbs")],
        body: vec![],
    };
    assert_successful_parse!(result, expected);
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
    let expected = Include("foo");
    assert_successful_parse!(result, expected);
}

#[test]
fn test_include_prefix_whitespace() {
    let result = include("include     \"foo\";");
    let expected = Include("foo");
    assert_successful_parse!(result, expected);
}

#[test]
fn test_include_no_prefix_whitespace() {
    let result = include("include\"foo\";");
    assert_failed_parse!(result, "\"foo\";", ErrorKind::Space);
}

#[test]
fn test_include_trailing_whitespace() {
    let result = include("include \"foo\"    ;");
    let expected = Include("foo");
    assert_successful_parse!(result, expected);
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
    assert_successful_parse!(result, expected);
}

#[test]
fn test_two_level_namespace_decl() {
    let result = namespace_decl("namespace a.b;");
    let expected = Namespace(vec![Ident("a"), Ident("b")]);
    assert_successful_parse!(result, expected);
}

#[test]
fn test_three_level_namespace_decl() {
    let result = namespace_decl("namespace a.b.c;");
    let expected = Namespace(vec![Ident("a"), Ident("b"), Ident("c")]);
    assert_successful_parse!(result, expected);
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
    let expected = Attribute(Ident("a"));
    assert_successful_parse!(result, expected);
}

#[test]
fn test_quoted_attribute_decl() {
    let result = attribute_decl("attribute \"a\";");
    let expected = Attribute(Ident("a"));
    assert_successful_parse!(result, expected);
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
    let result = root_decl("root_type Foo;");
    let expected = Root(Ident("Foo"));
    assert_successful_parse!(result, expected);
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
    let result = field_decl(input);
    let expected =
        Field::new(Ident("foo"), Type::Float64).with_scalar(Scalar::Integer(2));
    assert_successful_parse!(result, expected);
}

#[test]
fn test_field_decl_uint() {
    let input = "foo :uint=3.0;";
    let result = field_decl(input);
    let expected =
        Field::new(Ident("foo"), Type::UInt).with_scalar(Scalar::Float(3.0));
    assert_successful_parse!(result, expected);
}

#[test]
fn test_field_decl_no_scalar() {
    let input = "foo:float64    ;";
    let result = field_decl(input);
    let expected = Field::new(Ident("foo"), Type::Float64);
    assert_successful_parse!(result, expected);
}

fn rpc_decl(input: &str) -> IResult<&str, Rpc> {
    map(
        tuple((
            preceded(
                tag("rpc_service"),
                delimited(multispace1, ident, multispace0),
            ),
            delimited(
                left_brace,
                many1(delimited(multispace0, rpc_method, multispace0)),
                right_brace,
            ),
        )),
        |(name, methods)| Rpc::new(name, methods),
    )(input)
}

#[test]
fn test_rpc_decl_single_method() {
    let input = "\
rpc_service Greeter {
  SayHello(HelloRequest):HelloReply;
}";
    let result = rpc_decl(input);
    let expected = Rpc::new(
        Ident("Greeter"),
        vec![RpcMethod {
            name: Ident("SayHello"),
            request_type: Ident("HelloRequest"),
            response_type: Ident("HelloReply"),
            metadata: None,
        }],
    );
    assert_successful_parse!(result, expected);
}

#[test]
fn test_rpc_decl_multiple_methods() {
    let input = "\
rpc_service Greeter {
  SayHello(HelloRequest):HelloReply;
  SayManyHellos(ManyHellosRequest):HelloReply(streaming:\"server\");
}";
    let result = rpc_decl(input);
    let expected = Rpc::new(
        Ident("Greeter"),
        vec![
            RpcMethod {
                name: Ident("SayHello"),
                request_type: Ident("HelloRequest"),
                response_type: Ident("HelloReply"),
                metadata: None,
            },
            RpcMethod {
                name: Ident("SayManyHellos"),
                request_type: Ident("ManyHellosRequest"),
                response_type: Ident("HelloReply"),
                metadata: Some(Metadata(HashMap::from_iter(vec![(
                    Ident("streaming"),
                    Some(SingleValue::StringConstant("server")),
                )]))),
            },
        ],
    );
    assert_successful_parse!(result, expected);
}

fn rpc_method(input: &str) -> IResult<&str, RpcMethod> {
    map(
        terminated(
            tuple((
                terminated(ident, space0),
                delimited(
                    left_paren,
                    delimited(space0, ident, space0),
                    right_paren,
                ),
                preceded(
                    delimited(space0, colon, space0),
                    tuple((ident, metadata)),
                ),
            )),
            terminated(space0, semicolon),
        ),
        |(name, request_type, (response_type, metadata))| RpcMethod {
            name,
            request_type,
            response_type,
            metadata,
        },
    )(input)
}

#[test]
fn test_rpc_method() {
    let input = "SayHello(HelloRequest):HelloReply;";
    let result = rpc_method(input);
    let expected = RpcMethod {
        name: Ident("SayHello"),
        request_type: Ident("HelloRequest"),
        response_type: Ident("HelloReply"),
        metadata: None,
    };
    assert_successful_parse!(result, expected);
}

fn ty(input: &str) -> IResult<&str, Type> {
    alt((
        alt((
            value(Type::Bool, tag("bool")),
            value(Type::Byte, tag("byte")),
            value(Type::UByte, tag("ubyte")),
            value(Type::Short, tag("short")),
            value(Type::UShort, tag("ushort")),
            value(Type::Long, tag("long")),
            value(Type::ULong, tag("ulong")),
            value(Type::Double, tag("double")),
            value(Type::Int8, tag("int8")),
            value(Type::UInt8, tag("uint8")),
            value(Type::Int16, tag("int16")),
            value(Type::UInt16, tag("uint16")),
            value(Type::Int32, tag("int32")),
            value(Type::UInt32, tag("uint32")),
            value(Type::Int64, tag("int64")),
            value(Type::UInt64, tag("uint64")),
            value(Type::Float32, tag("float32")),
            value(Type::Float64, tag("float64")),
            value(Type::Int, tag("int")),
            value(Type::UInt, tag("uint")),
            value(Type::Float, tag("float")),
        )),
        value(Type::String, tag("string")),
        map(delimited(tag("["), ty, tag("]")), |t| {
            Type::Array(Box::new(t))
        }),
        map(ident, Type::Ident),
    ))(input)
}

fn enumval_decl(input: &str) -> IResult<&str, EnumVal> {
    let parser = tuple((
        ident,
        opt(preceded(
            space0,
            preceded(equals, preceded(space0, integer_constant)),
        )),
    ));
    map(parser, |(name, value)| EnumVal::new(name, value))(input)
}

fn metadata(input: &str) -> IResult<&str, Option<Metadata>> {
    opt(map(
        delimited(
            terminated(left_paren, space0),
            separated_list(
                delimited(space0, comma, space0),
                tuple((
                    ident,
                    opt(preceded(
                        space0,
                        preceded(colon, preceded(space0, single_value)),
                    )),
                )),
            ),
            preceded(space0, right_paren),
        ),
        |values| Metadata(HashMap::from_iter(values)),
    ))(input)
}

#[test]
fn test_simple_metadata() {
    let input = "(a: \"b\")";
    let result = metadata(input);
    let expected = Some(Metadata(HashMap::from_iter(vec![(
        Ident("a"),
        Some(SingleValue::StringConstant("b")),
    )])));
    assert_successful_parse!(result, expected);
}

#[test]
fn test_multiple_field_metadata() {
    let input = "(x, a: \"news\", b: 42,c: 42.42, d, e)";
    let result = metadata(input);
    let expected = Some(Metadata(HashMap::from_iter(vec![
        (Ident("x"), None),
        (Ident("a"), Some(SingleValue::StringConstant("news"))),
        (Ident("b"), Some(SingleValue::Scalar(Scalar::Integer(42)))),
        (Ident("c"), Some(SingleValue::Scalar(Scalar::Float(42.42)))),
        (Ident("d"), None),
        (Ident("e"), None),
    ])));
    assert_successful_parse!(result, expected);
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
            terminated(left_brace, multispace0),
            separated_nonempty_list(
                delimited(space0, comma, space0),
                separated_pair(ident, delimited(space0, colon, space0), value_),
            ),
            preceded(multispace0, right_brace),
        ),
        |values| Object(HashMap::from_iter(values)),
    )(input)
}

/// `SingleValue`s are `Scalar`s or `StringConstant`s.
fn single_value(input: &str) -> IResult<&str, SingleValue> {
    alt((
        map(scalar, SingleValue::Scalar),
        map(string_constant, SingleValue::StringConstant),
    ))(input)
}

/// A value list is a comma-separated list of `Value`s.
fn value_list(input: &str) -> IResult<&str, Vec<Value>> {
    delimited(
        terminated(left_square_bracket, space0),
        separated_list(delimited(space0, comma, space0), value_),
        preceded(space0, right_square_bracket),
    )(input)
}

/// `Value`s are `SingleValue`s or `Object`s.
fn value_(input: &str) -> IResult<&str, Value> {
    alt((
        map(single_value, Value::SingleValue),
        map(object, Value::Object),
        map(value_list, Value::List),
    ))(input)
}

#[test]
fn test_value_single_value() {
    let result = value_("1");
    assert_successful_parse!(
        result,
        Value::SingleValue(SingleValue::Scalar(Scalar::Integer(1)))
    );

    let result = value_("2.3");
    assert_successful_parse!(
        result,
        Value::SingleValue(SingleValue::Scalar(Scalar::Float(2.3)))
    );

    let result = value_("\"abc d\"");
    assert_successful_parse!(
        result,
        Value::SingleValue(SingleValue::StringConstant("abc d"))
    );
}

#[test]
fn test_value_object() {
    let result = value_("{a: 1}");
    assert_successful_parse!(
        result,
        Value::Object(Object(HashMap::from_iter(vec![(
            Ident("a"),
            Value::SingleValue(SingleValue::Scalar(Scalar::Integer(1)))
        )])))
    );

    let result = value_("{b: 2.42}");
    assert_successful_parse!(
        result,
        Value::Object(Object(HashMap::from_iter(vec![(
            Ident("b"),
            Value::SingleValue(SingleValue::Scalar(Scalar::Float(2.42)))
        )])))
    );

    let result = value_("{c: [\"a\"]}");
    assert_successful_parse!(
        result,
        Value::Object(Object(HashMap::from_iter(vec![(
            Ident("c"),
            Value::List(vec![Value::SingleValue(SingleValue::StringConstant(
                "a"
            ))])
        )])))
    );

    // What kind of person would put this in a flatbuffer schema?
    let result = value_("{d: [\"a\", {b: [1, [\"z\"]]}]}");
    assert_successful_parse!(
        result,
        Value::Object(Object(HashMap::from_iter(vec![(
            Ident("d"),
            Value::List(vec![
                Value::SingleValue(SingleValue::StringConstant("a")),
                Value::Object(Object(HashMap::from_iter(vec![(
                    Ident("b"),
                    Value::List(vec![
                        Value::SingleValue(SingleValue::Scalar(
                            Scalar::Integer(1)
                        )),
                        Value::List(vec![Value::SingleValue(
                            SingleValue::StringConstant("z")
                        )])
                    ])
                )])))
            ])
        )])))
    );
}

#[test]
fn test_value_value_list() {
    let result = value_("[\"a\", 1]");
    let expected = Value::List(vec![
        Value::SingleValue(SingleValue::StringConstant("a")),
        Value::SingleValue(SingleValue::Scalar(Scalar::Integer(1))),
    ]);
    assert_successful_parse!(result, expected);
}

fn type_decl(input: &str) -> IResult<&str, ProductType> {
    map(
        tuple((
            alt((
                value(ProductKind::Table, tag("table")),
                value(ProductKind::Struct, tag("struct")),
            )),
            delimited(multispace1, ident, multispace0),
            terminated(metadata, multispace0),
            delimited(
                left_brace,
                many1(delimited(multispace0, field_decl, multispace0)),
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
fn test_table_multiple_fields() {
    let input = "\
table HelloReply
{
  message: string;
  foo :uint=3.0;

  bar :      [int8] =    423;
}";
    let result = type_decl(input);
    let expected = table(
        Ident("HelloReply"),
        vec![
            Field::new(Ident("message"), Type::String),
            Field::new(Ident("foo"), Type::UInt)
                .with_scalar(Scalar::Float(3.0)),
            Field::new(Ident("bar"), Type::Array(Box::new(Type::Int8)))
                .with_scalar(Scalar::Integer(423)),
        ],
    );
    assert_successful_parse!(result, expected);
}

#[test]
fn test_table() {
    let input = "\
table HelloReply {
  message: string;
}";
    let result = type_decl(input);
    let expected = table(
        Ident("HelloReply"),
        vec![Field::new(Ident("message"), Type::String)],
    );
    assert_successful_parse!(result, expected);
}

/// A decimal integer constant
fn dec_integer_constant(input: &str) -> IResult<&str, IntegerConstant> {
    map_res(
        recognize(preceded(opt(plus_or_minus), digit1)),
        IntegerConstant::from_str,
    )(input)
}

#[test]
fn test_dec_integer_constant() {
    let result = dec_integer_constant("1234");
    assert_successful_parse!(result, 1234);

    let result = dec_integer_constant("-1234");
    assert_successful_parse!(result, -1234);
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
    let result = hex_integer_constant("0x1234ABCDEFabcdef");
    assert_successful_parse!(result, 0x1234ABCDEFabcdef);

    let result = hex_integer_constant("-0x1234ABCDEFabcdef");
    assert_successful_parse!(result, -0x1234ABCDEFabcdef);
}

#[test]
fn test_invalid_hex_integer_constant() {
    let result = hex_integer_constant("ABCDEFabcdef");
    assert_failed_parse!(result, "ABCDEFabcdef", ErrorKind::Char);
}

fn true_(input: &str) -> IResult<&str, bool> {
    value(true, |input: &str| nom::re_match!(input, r"\btrue\b"))(input)
}

#[test]
fn test_true() {
    let result = true_("true");
    assert_successful_parse!(result, true);
}

#[test]
fn test_invalid_true() {
    let result = true_("truez");
    assert_failed_parse!(result, "truez", ErrorKind::RegexpMatch);
}

fn false_(input: &str) -> IResult<&str, bool> {
    value(false, |input: &str| nom::re_match!(input, r"\bfalse\b"))(input)
}

#[test]
fn test_false() {
    let result = false_("false");
    assert_successful_parse!(result, false);
}

#[test]
fn test_invalid_false() {
    let result = false_("falsez");
    assert_failed_parse!(result, "falsez", ErrorKind::RegexpMatch);
}

/// TODO: Where can this be used in a schema?
fn boolean_constant(input: &str) -> IResult<&str, bool> {
    alt((true_, false_))(input)
}

#[test]
fn test_boolean_constant() {
    let result = boolean_constant("true");
    assert_successful_parse!(result, true);

    let result = boolean_constant("false");
    assert_successful_parse!(result, false);

    let result = boolean_constant("waltz");
    assert_failed_parse!(result, "waltz", ErrorKind::RegexpMatch);
}

fn dec_float_exponent(input: &str) -> IResult<&str, &str> {
    recognize(tuple((one_of("eE"), opt(plus_or_minus), digit1)))(input)
}

// Taken from Python's tokenize.Floatnumber
fn dec_float_constant(input: &str) -> IResult<&str, FloatingConstant> {
    let parser = recognize(preceded(
        opt(plus_or_minus),
        alt((
            terminated(
                alt((
                    terminated(digit1, terminated(period, digit0)),
                    preceded(period, digit1),
                )),
                opt(dec_float_exponent),
            ),
            terminated(digit1, dec_float_exponent),
        )),
    ));
    map_res(parser, FloatingConstant::from_str)(input)
}

#[test]
fn test_dec_float_constant() {
    let result = dec_float_constant("-2.1");
    assert_successful_parse!(result, -2.1);
}

/// A float constant is either a special float constant (nan, inf, or infinity), a hex float
/// constant or a double.
fn float_constant(input: &str) -> IResult<&str, FloatingConstant> {
    alt((
        special_float_constant,
        hex_float_constant,
        dec_float_constant,
    ))(input)
}

#[test]
fn test_float_constant_nan() {
    let result = float_constant("nan");
    assert_eq!(
        result.map(|(input, value)| (
            input,
            value.is_nan(),
            value.is_sign_positive()
        )),
        Ok(("", true, true))
    );

    let result = float_constant("+nan");
    assert_eq!(
        result.map(|(input, value)| (
            input,
            value.is_nan(),
            value.is_sign_positive()
        )),
        Ok(("", true, true))
    );

    let result = float_constant("-nan");
    assert_eq!(
        result.map(|(input, value)| (
            input,
            value.is_nan(),
            value.is_sign_negative()
        )),
        Ok(("", true, true))
    );
}

#[test]
fn test_float_constant_inf() {
    let result = float_constant("inf");
    assert_successful_parse!(result, std::f64::INFINITY);

    let result = float_constant("+inf");
    assert_successful_parse!(result, std::f64::INFINITY);

    let result = float_constant("-inf");
    assert_successful_parse!(result, std::f64::NEG_INFINITY);
}

#[test]
fn test_float_constant_infinity() {
    let result = float_constant("infinity");
    assert_successful_parse!(result, std::f64::INFINITY);

    let result = float_constant("+infinity");
    assert_successful_parse!(result, std::f64::INFINITY);

    let result = float_constant("-infinity");
    assert_successful_parse!(result, std::f64::NEG_INFINITY);
}

// Adapted from Python's tokenize.Floatnumber
fn hex_float_exponent(input: &str) -> IResult<&str, &str> {
    recognize(terminated(
        one_of("pP"),
        terminated(opt(plus_or_minus), digit1),
    ))(input)
}

fn hex_float_constant(input: &str) -> IResult<&str, FloatingConstant> {
    let parser = tuple((
        terminated(opt(plus_or_minus), terminated(zero, one_of("xX"))),
        // parse the number without the sign to avoid having to spell out the
        // return type of a closure to map_res
        map_res(
            recognize(alt((
                terminated(
                    alt((
                        terminated(terminated(hex_digit1, period), hex_digit0),
                        preceded(period, hex_digit1),
                    )),
                    opt(hex_float_exponent),
                ),
                terminated(hex_digit1, hex_float_exponent),
            ))),
            |value| parse_hexf64(value, false),
        ),
    ));
    map(
        parser,
        |(sign, value)| {
            if let Some('-') = sign {
                -value
            } else {
                value
            }
        },
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
    let result = nan("nan");
    assert_eq!(
        result.map(|(input, value)| (
            input,
            value.is_nan(),
            value.is_sign_positive()
        )),
        Ok(("", true, true))
    );

    let result = nan("+nan");
    assert_eq!(
        result.map(|(input, value)| (
            input,
            value.is_nan(),
            value.is_sign_positive()
        )),
        Ok(("", true, true))
    );

    let result = nan("-nan");
    assert_eq!(
        result.map(|(input, value)| (
            input,
            value.is_nan(),
            value.is_sign_negative()
        )),
        Ok(("", true, true))
    );
}

#[test]
fn test_invalid_nan() {
    let result = nan("nanz");
    assert_failed_parse!(result, "nanz", ErrorKind::RegexpMatch);
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
fn test_inf_or_infinity_infinity() {
    let result = inf_or_infinity("inf");
    assert_successful_parse!(result, std::f64::INFINITY);

    let result = inf_or_infinity("+inf");
    assert_successful_parse!(result, std::f64::INFINITY);

    let result = inf_or_infinity("-inf");
    assert_successful_parse!(result, std::f64::NEG_INFINITY);

    let result = inf_or_infinity("infinity");
    assert_successful_parse!(result, std::f64::INFINITY);

    let result = inf_or_infinity("+infinity");
    assert_successful_parse!(result, std::f64::INFINITY);

    let result = inf_or_infinity("-infinity");
    assert_successful_parse!(result, std::f64::NEG_INFINITY);
}

/// Parse `nan`, `inf`, or `infiniity`
fn special_float_constant(input: &str) -> IResult<&str, FloatingConstant> {
    alt((nan, inf_or_infinity))(input)
}

#[test]
fn test_special_float_constant_nan() {
    let result = special_float_constant("nan");
    assert_eq!(
        result.map(|(input, value)| (
            input,
            value.is_nan(),
            value.is_sign_positive()
        )),
        Ok(("", true, true))
    );

    let result = special_float_constant("+nan");
    assert_eq!(
        result.map(|(input, value)| (
            input,
            value.is_nan(),
            value.is_sign_positive()
        )),
        Ok(("", true, true))
    );

    let result = special_float_constant("-nan");
    assert_eq!(
        result.map(|(input, value)| (
            input,
            value.is_nan(),
            value.is_sign_negative()
        )),
        Ok(("", true, true))
    );
}

#[test]
fn test_special_float_constant_inf() {
    let result = special_float_constant("inf");
    assert_successful_parse!(result, std::f64::INFINITY);

    let result = special_float_constant("-inf");
    assert_successful_parse!(result, std::f64::NEG_INFINITY);
}

#[test]
fn test_special_float_constant_infinity() {
    let result = special_float_constant("infinity");
    assert_successful_parse!(result, std::f64::INFINITY);

    let result = special_float_constant("-infinity");
    assert_successful_parse!(result, std::f64::NEG_INFINITY);
}

fn integer_constant(input: &str) -> IResult<&str, IntegerConstant> {
    alt((hex_integer_constant, dec_integer_constant))(input)
}

#[test]
fn test_integer_constant() {
    let result = integer_constant("1234");
    assert_successful_parse!(result, 1234);

    let result = integer_constant("-1234");
    assert_successful_parse!(result, -1234);

    let result = integer_constant("0x1234");
    assert_successful_parse!(result, 0x1234);

    let result = integer_constant("-0x1234");
    assert_successful_parse!(result, -0x1234);
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
    let result = file_extension_decl("file_extension \"foo\";");
    let expected = FileExtension("foo");
    assert_successful_parse!(result, expected);
}

#[test]
fn test_file_extension_decl_no_leading_space() {
    let result = file_extension_decl("file_extension\"foo\";");
    let expected = FileExtension("foo");
    assert_successful_parse!(result, expected);
}

#[test]
fn test_file_extension_decl_trailing_space() {
    let result = file_extension_decl("file_extension \"foo\"  ;");
    let expected = FileExtension("foo");
    assert_successful_parse!(result, expected);
}

#[test]
fn test_file_extension_decl_surrounding_space() {
    let result = file_extension_decl("file_extension   \"foo\"  ;");
    let expected = FileExtension("foo");
    assert_successful_parse!(result, expected);
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
    let result = file_identifier_decl("file_identifier \"foo\";");
    let expected = FileIdentifier("foo");
    assert_successful_parse!(result, expected);
}

#[test]
fn test_file_identifier_decl_no_leading_space() {
    let result = file_identifier_decl("file_identifier\"foo\";");
    let expected = FileIdentifier("foo");
    assert_successful_parse!(result, expected);
}

#[test]
fn test_file_identifier_decl_trailing_space() {
    let result = file_identifier_decl("file_identifier \"foo\"  ;");
    let expected = FileIdentifier("foo");
    assert_successful_parse!(result, expected);
}

#[test]
fn test_file_identifier_decl_surrounding_space() {
    let result = file_identifier_decl("file_identifier   \"foo\"  ;");
    let expected = FileIdentifier("foo");
    assert_successful_parse!(result, expected);
}
