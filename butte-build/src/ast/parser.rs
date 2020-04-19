use crate::ast::types::*;
use anyhow::{anyhow, Result};

#[cfg(test)]
use pretty_assertions::assert_eq;

use hexf_parse::parse_hexf64;

use nom::{
    self, branch::alt, bytes::complete::*, character::complete::*, combinator::*, multi::*,
    sequence::*, IResult,
};

use std::{iter::FromIterator, path::Path, str::FromStr};

#[cfg(test)]
macro_rules! assert_failed_parse {
    ($left:expr, $rest:expr, $error_kind:ident) => {
        $crate::ast::parser::assert_eq!(
            $left,
            Err(nom::Err::Error(($rest, nom::error::ErrorKind::$error_kind)))
        )
    };
}

#[cfg(test)]
macro_rules! assert_successful_parse {
    ($left:expr, $right:expr) => {
        // The first element of the tuple in the Result is the remaining input, which should be
        // empty when parsing is successful
        assert_successful_parse!($left, "", $right)
    };
    ($left:expr, $remaining:expr, $right:expr) => {
        // The first element of the tuple in the Result is the remaining input, which should be
        // empty when parsing is successful
        $crate::ast::parser::assert_eq!($left, Ok(($remaining, $right)))
    };
}

pub fn plus_or_minus(input: &str) -> IResult<&str, char> {
    one_of("-+")(input)
}

#[cfg(test)]
mod plus_or_minus_tests {
    use super::plus_or_minus;

    #[test]
    fn test_plus_or_minus() {
        assert_successful_parse!(plus_or_minus("+"), '+');
        assert_successful_parse!(plus_or_minus("-"), '-');
    }

    #[test]
    fn test_invalid_plus_or_minus() {
        assert_failed_parse!(plus_or_minus("/"), "/", OneOf);
    }
}

pub fn double_quote(input: &str) -> IResult<&str, char> {
    char('\"')(input)
}

pub fn backslash(input: &str) -> IResult<&str, char> {
    char('\\')(input)
}

pub fn left_paren(input: &str) -> IResult<&str, char> {
    char('(')(input)
}

pub fn right_paren(input: &str) -> IResult<&str, char> {
    char(')')(input)
}

pub fn left_brace(input: &str) -> IResult<&str, char> {
    char('{')(input)
}

pub fn right_brace(input: &str) -> IResult<&str, char> {
    char('}')(input)
}

pub fn colon(input: &str) -> IResult<&str, char> {
    char(':')(input)
}

pub fn comma(input: &str) -> IResult<&str, char> {
    char(',')(input)
}

pub fn semicolon(input: &str) -> IResult<&str, char> {
    char(';')(input)
}

pub fn equals(input: &str) -> IResult<&str, char> {
    char('=')(input)
}

pub fn period(input: &str) -> IResult<&str, char> {
    char('.')(input)
}

pub fn zero(input: &str) -> IResult<&str, char> {
    char('0')(input)
}

pub fn left_square_bracket(input: &str) -> IResult<&str, char> {
    char('[')(input)
}

pub fn right_square_bracket(input: &str) -> IResult<&str, char> {
    char(']')(input)
}

pub fn ident(input: &str) -> IResult<&str, Ident<'_>> {
    map(
        recognize(preceded(
            take_while_m_n(1, 1, |c: char| c.is_alphabetic() || c == '_'),
            take_while(|c: char| c.is_alphanumeric() || c == '_'),
        )),
        Ident::from,
    )(input)
}

#[cfg(test)]
mod ident_tests {
    use super::{ident, Ident};

    #[test]
    fn test_simple_ident() {
        assert_successful_parse!(ident("foo"), Ident::from("foo"));
    }

    #[test]
    fn test_underscore_prefix() {
        assert_successful_parse!(ident("_foo"), Ident::from("_foo"));
    }

    #[test]
    fn test_just_underscore() {
        assert_successful_parse!(ident("_"), Ident::from("_"));
    }

    #[test]
    fn test_id_with_number() {
        assert_successful_parse!(ident("foo1"), Ident::from("foo1"));
    }

    #[test]
    fn test_invalid_ident_contains_valid() {
        assert_failed_parse!(ident("1foo"), "1foo", TakeWhileMN);
    }

    #[test]
    fn test_number_is_invalid() {
        let result = ident("1");
        assert_failed_parse!(result, "1", TakeWhileMN)
    }

    #[test]
    fn test_empty_ident_is_invalid() {
        let result = ident("");
        assert_failed_parse!(result, "", TakeWhileMN)
    }
}

pub fn string_constant(input: &str) -> IResult<&str, &str> {
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

#[cfg(test)]
mod string_constant_tests {
    use super::string_constant;

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
}

pub fn element(input: &str) -> IResult<&str, Element<'_>> {
    alt((
        map(namespace_decl, Element::from),
        map(table_decl, Element::from),
        map(struct_decl, Element::from),
        map(enum_decl, Element::from),
        map(union_decl, Element::from),
        map(root_decl, Element::from),
        map(file_extension_decl, Element::from),
        map(file_identifier_decl, Element::from),
        map(attribute_decl, Element::from),
        map(rpc_decl, Element::from),
        map(object, Element::from),
    ))(input)
}

#[cfg(test)]
mod element_tests {
    use super::element;
    use crate::{element as elem, meta, method, rpc};

    #[test]
    fn test_element_schema() {
        let input = "\
rpc_service Greeter {
  SayHello(HelloRequest):HelloReply;
  SayManyHellos(ManyHellosRequest):HelloReply (streaming: \"server\");
}";
        let result = element(input);
        let expected = elem!(rpc!(
            Greeter,
            [
                method!(fn SayHello(HelloRequest) -> HelloReply),
                method!(fn SayManyHellos(ManyHellosRequest) -> HelloReply, [meta!(streaming, "server")])
            ]
        ));
        assert_successful_parse!(result, expected);
    }
}

/// Parse one line of a non-documentation comment.
pub fn comment(input: &str) -> IResult<&str, ()> {
    // two slashes followed by not-one-slash, followed by not-a-line-ending
    value((), tuple((tag("//"), not(tag("/")), not_line_ending)))(input)
}

pub fn comment_or_space(input: &str) -> IResult<&str, ()> {
    alt((complete(comment), value((), complete(multispace1))))(input)
}

pub fn comment_or_space0(input: &str) -> IResult<&str, ()> {
    value((), many0(comment_or_space))(input)
}

pub fn comment_or_space1(input: &str) -> IResult<&str, ()> {
    value((), many1(comment_or_space))(input)
}

/// Parse a flatbuffer schema.
pub fn schema_decl(input: &str) -> IResult<&str, Schema<'_>> {
    map(
        tuple((
            many0(delimited(
                comment_or_space0,
                include_decl,
                comment_or_space0,
            )),
            many0(delimited(comment_or_space0, element, comment_or_space0)),
        )),
        Schema::from,
    )(input)
}

#[cfg(test)]
mod schema_tests {
    use super::{comment_or_space0, delimited, include_decl, many0, schema_decl, Include, Path};
    use crate::{field, meta, method, namespace, rpc, schema, table};

    #[test]
    fn test_simple_include_with_comments() {
        let input = "//baz\ninclude \"a\"; // bar\n";
        let parser = many0(delimited(
            comment_or_space0,
            include_decl,
            comment_or_space0,
        ));
        let result = parser(input);
        let expected = vec![Include::builder().path(Path::new("a")).stem("a").build()];
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_includes_only_schema() {
        let input = r#"include "a"; // bar
        // foo
include "b";


include "foo/bar/baz.fbs";

    "#;
        let result = schema_decl(input);
        let expected = schema! {
            include {
                "a",
                "b",
                "foo/bar/baz.fbs"
            }
        };
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_elements_only_schema() {
        let input = "\
table MyMessage {
  message: string;
  foo: float64 = 2.0;
}";
        let result = schema_decl(input);
        let expected = schema! {
            table!(
                MyMessage,
                [field!(message, String), field!(foo, Float64 = 2.0)]
            )
        };
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_full_schema() {
        let input = r#"

include "foo/bar/baz.fbs";

table HelloReply {
  message:string;
} // bar

// foo!

table HelloRequest {
  name:string;
}

table ManyHellosRequest {
  name:string;
  num_greetings:int;
}

namespace foo.bar;

rpc_service Greeter {
  SayHello(HelloRequest):HelloReply;
  SayManyHellos(ManyHellosRequest):HelloReply (streaming: "server");
}

"#;
        let result = schema_decl(input);
        let expected = schema! {
            include {
                "foo/bar/baz.fbs"
            },
            table!(HelloReply, [field!(message, String)]),
            table!(HelloRequest, [field!(name, String)]),
            table!(
                ManyHellosRequest,
                [field!(name, String), field!(num_greetings, Int)]
            ),
            namespace!(foo::bar),
            rpc!(
                Greeter,
                [
                    method!(fn SayHello(HelloRequest) -> HelloReply),
                    method!(
                        fn SayManyHellos(ManyHellosRequest) -> HelloReply,
                        [meta!(streaming, "server")]
                    )
                ]
            )
        };
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_schema_with_namespaces() {
        let input = r#"
namespace a.b;

table A {
  message: string;
}

namespace c;

table C {
  field1: a.b.A;
}
"#;
        let result = schema_decl(input);
        let expected = schema! {
            namespace!(a::b),
            table!(A, [field!(message, String)]),
            namespace!(c),
            table!(C, [field!(field1, a::b::A)])
        };
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_schema_with_namespace() {
        let input = r#"

namespace baz;

table Hello{
  message:string;
  count:uint64; } // bar
"#;
        let result = schema_decl(input);
        let expected = schema! {
            namespace!(baz),
            table!(Hello, [field!(message, String), field!(count, UInt64)])
        };
        assert_successful_parse!(result, expected);
    }
}

pub fn include_decl(input: &str) -> IResult<&str, Include<'_>> {
    map_res(
        tuple((
            doc_comment,
            delimited(
                tag("include"),
                delimited(
                    comment_or_space1,
                    map(string_constant, Path::new),
                    comment_or_space0,
                ),
                semicolon,
            ),
        )),
        |(comment, path)| -> Result<Include<'_>> {
            let stem = path
                .file_stem()
                .ok_or_else(|| anyhow!("path has no file stem: {:?}", path))?
                .to_str()
                .ok_or_else(|| anyhow!("cannot convert OsStr to str {:?}", path))?;
            Ok(Include::builder()
                .doc(comment)
                .path(path)
                .stem(stem)
                .build())
        },
    )(input)
}

#[cfg(test)]
mod include_tests {
    use super::{include_decl, Include, Path};

    #[test]
    fn test_include_decl_with_comments() {
        let result = include_decl(
            "include //fizz
            //buzz\n\t\"foo\"//bzz\n;",
        );
        let expected = Include::builder()
            .path(Path::new("foo"))
            .stem("foo")
            .build();
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_include_decl() {
        let result = include_decl("include \"foo\";");
        let expected = Include::builder()
            .path(Path::new("foo"))
            .stem("foo")
            .build();
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_include_decl_prefix_whitespace() {
        let result = include_decl("include     \"foo\";");
        let expected = Include::builder()
            .path(Path::new("foo"))
            .stem("foo")
            .build();
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_include_decl_no_prefix_whitespace() {
        let result = include_decl("include\"foo\";");
        assert_failed_parse!(result, "\"foo\";", MultiSpace);
    }

    #[test]
    fn test_include_decl_trailing_whitespace() {
        let result = include_decl("include \"foo\"    ;");
        let expected = Include::builder()
            .path(Path::new("foo"))
            .stem("foo")
            .build();
        assert_successful_parse!(result, expected);
    }
}

pub fn qualified_ident(input: &str) -> IResult<&str, QualifiedIdent<'_>> {
    map(
        separated_nonempty_list(
            delimited(comment_or_space0, tag("."), comment_or_space0),
            ident,
        ),
        QualifiedIdent::from,
    )(input)
}

pub fn namespace_decl(input: &str) -> IResult<&str, Namespace<'_>> {
    map(
        tuple((
            doc_comment,
            delimited(
                tag("namespace"),
                delimited(comment_or_space1, qualified_ident, comment_or_space0),
                semicolon,
            ),
        )),
        |(comment, path)| Namespace::from((path, comment)),
    )(input)
}

#[cfg(test)]
mod namespace_tests {
    use super::namespace_decl;
    use crate::namespace;

    #[test]
    fn test_namespace_decl_with_comments() {
        let result =
            namespace_decl("namespace // a namespace comment \na// Yet another one\t\n\n\n;");
        let expected = namespace!(a);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_namespace_decl_with_doc_comment() {
        let input = "\
/// foo
namespace foo;";
        let result = namespace_decl(input);
        let expected = namespace!(foo, [" foo"]);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_namespace_decl_with_doc_and_non_doc_comment() {
        let input = "\
/// foo
// Bar
namespace foo;";
        let result = namespace_decl(input);
        let expected = namespace!(foo, [" foo"]);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_namespace_decl_with_doc_space_and_non_doc_comment() {
        let input = "\
/// foo
//
// Bar

namespace foo;";
        let result = namespace_decl(input);
        let expected = namespace!(foo, [" foo"]);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_one_level_namespace_decl() {
        let result = namespace_decl("namespace a;");
        let expected = namespace!(a);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_two_level_namespace_decl() {
        let result = namespace_decl("namespace a.b;");
        let expected = namespace!(a::b);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_three_level_namespace_decl() {
        let result = namespace_decl("namespace a.b.c;");
        let expected = namespace!(a::b::c);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_three_level_namespace_decl_ws() {
        let result = namespace_decl("namespace \r\na  .b.c\n\t;");
        let expected = namespace!(a::b::c);
        assert_successful_parse!(result, expected);
    }
}

pub fn attribute_decl(input: &str) -> IResult<&str, Attribute<'_>> {
    map(
        tuple((
            doc_comment,
            delimited(
                tag("attribute"),
                delimited(
                    comment_or_space1,
                    alt((ident, map(string_constant, Ident::from))),
                    comment_or_space0,
                ),
                semicolon,
            ),
        )),
        |(_, attr)| Attribute::builder().attr(attr).build(),
    )(input)
}

#[cfg(test)]
mod attribute_tests {
    use super::attribute_decl;
    use crate::attr;

    #[test]
    fn test_simple_attribute_decl_with_comments() {
        let result = attribute_decl(
            "attribute// gah, an attr!
            a;",
        );
        let expected = attr!(a);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_attribute_decl_with_doc_comments() {
        let input = "\
/// An
/// Attribute
// implementation notes
/// Further comments
attribute a;";
        let result = attribute_decl(input);
        // NB: we don't store doc comments on purpose for attribute, as it's not clear where they
        // would go in the generated code.
        let expected = attr!(a);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_simple_attribute_decl() {
        let result = attribute_decl("attribute a;");
        let expected = attr!(a);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_quoted_attribute_decl() {
        let result = attribute_decl("attribute \"a\";");
        let expected = attr!(a);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_attribute_decl_ws() {
        let result = attribute_decl("attribute\t\n\t my_attr\n\n\r\n;");
        let expected = attr!(my_attr);
        assert_successful_parse!(result, expected);
    }
}

pub fn enum_body(input: &str) -> IResult<&str, Vec<EnumVariant<'_>>> {
    delimited(
        delimited(comment_or_space0, left_brace, comment_or_space0),
        terminated(
            separated_nonempty_list(
                delimited(comment_or_space0, comma, comment_or_space0),
                enumval_decl,
            ),
            opt(tuple((comment_or_space0, comma, comment_or_space0))),
        ),
        preceded(comment_or_space0, right_brace),
    )(input)
}

pub fn enum_decl(input: &str) -> IResult<&str, Enum<'_>> {
    let parser = tuple((
        doc_comment,
        preceded(
            tag("enum"),
            delimited(comment_or_space1, ident, comment_or_space0),
        ),
        preceded(colon, preceded(comment_or_space0, type_)),
        metadata,
        enum_body,
    ));
    map(parser, |(comment, name, base_type, metadata, variants)| {
        Enum::builder()
            .doc(comment)
            .id(name)
            .base_type(base_type)
            .variants(variants)
            .metadata(metadata)
            .build()
    })(input)
}

#[cfg(test)]
mod enum_tests {
    use super::enum_decl;
    use crate::{e_item, enum_};

    #[test]
    fn test_simple_enum_with_comments() {
        let input = "enum // thing
        MyEnum //other thing!
        // jazz?
: int32 // fiz!
        { foo // bar
            = 1, bar //ugh!
        }";
        let result = enum_decl(input);
        let expected = enum_!(MyEnum, Int32, [e_item!(foo = 1), e_item!(bar)]);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_enum_with_doc_comments() {
        let input = "\
/// The enum!
enum MyEnum : int32 {
    // Not a doc commment
    /// Maybe it's a foo
    foo = 1,
    // Hm,
    /// Could be a bar though
    bar,

    // Nope, it's a baz
    baz = 9,
}";
        let result = enum_decl(input);
        let expected = enum_!(
            MyEnum,
            Int32,
            [
                e_item!(foo = 1, [" Maybe it's a foo"]),
                e_item!(bar, [" Could be a bar though"]),
                e_item!(baz = 9)
            ],
            [" The enum!"]
        );
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_simple_enum() {
        let input = "enum MyEnum : int32 { foo = 1, bar }";
        let result = enum_decl(input);
        let expected = enum_!(MyEnum, Int32, [e_item!(foo = 1), e_item!(bar)]);
        assert_successful_parse!(result, expected);
    }

    // Flatc tolerates trailing commas
    #[test]
    fn test_simple_enum_with_trailing_comma() {
        let input = "enum MyEnum : int32 { foo = 1, bar, }";
        let result = enum_decl(input);
        let expected = enum_!(MyEnum, Int32, [e_item!(foo = 1), e_item!(bar)]);
        assert_successful_parse!(result, expected);
    }
}

pub fn union_decl(input: &str) -> IResult<&str, Union<'_>> {
    let parser = tuple((
        doc_comment,
        preceded(
            tag("union"),
            delimited(comment_or_space1, ident, comment_or_space0),
        ),
        metadata,
        enum_body,
    ));
    map(parser, |(comment, name, metadata, variants)| {
        Union::builder()
            .doc(comment)
            .id(name)
            .variants(variants)
            .metadata(metadata)
            .build()
    })(input)
}

#[cfg(test)]
mod union_tests {
    use super::union_decl;
    use crate::{e_item, union};

    #[test]
    fn test_simple_union_with_comments() {
        let input = "union //foo!\nMyUnion // comment
            //comment\n// thing\n{ // comm
            foo // foo!
            = 1//bar
            ,\t\tbar, //baz! field\nBaz=     234 \n\n // ha!\n}";
        let result = union_decl(input);
        let expected = union!(
            MyUnion,
            [e_item!(foo = 1), e_item!(bar), e_item!(Baz = 234)]
        );
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_union_with_doc_comments() {
        let input = "\
/// Unionize!
union MyUnion {
    // Not a doc commment
    /// Maybe it's a foo
    foo = 1,
    // Hm,
    /// Could be a bar though
    bar,

    // Nope, it's a baz
    baz = 9,
}";
        let result = union_decl(input);
        let expected = union!(
            MyUnion,
            [
                e_item!(foo = 1, [" Maybe it's a foo"]),
                e_item!(bar, [" Could be a bar though"]),
                e_item!(baz = 9)
            ],
            [" Unionize!"]
        );
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_simple_union() {
        let input = "union MyUnion { foo = 1, bar, Baz = 234 }";
        let result = union_decl(input);
        let expected = union!(
            MyUnion,
            [e_item!(foo = 1), e_item!(bar), e_item!(Baz = 234)]
        );
        assert_successful_parse!(result, expected);
    }
}

pub fn root_decl(input: &str) -> IResult<&str, Root<'_>> {
    map(
        tuple((
            doc_comment,
            delimited(
                tag("root_type"),
                delimited(comment_or_space1, ident, comment_or_space0),
                semicolon,
            ),
        )),
        |(_, typename)| Root::builder().typename(typename).build(),
    )(input)
}

#[cfg(test)]
mod root_tests {
    use super::root_decl;
    use crate::root_type;

    #[test]
    fn test_root_decl() {
        let result = root_decl("root_type Foo;");
        let expected = root_type!(Foo);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_root_decl_with_doc_comments() {
        let input = "\
/// I am Foo.
// Not a doc

root_type Foo;";
        let result = root_decl(input);
        // NB: we don't store doc comments on purpose for root_type, as it's not clear where they
        // would go in the generated code.
        let expected = root_type!(Foo);
        assert_successful_parse!(result, expected);
    }
}

pub fn field_decl(input: &str) -> IResult<&str, Field<'_>> {
    map(
        terminated(
            tuple((
                doc_comment,
                terminated(ident, tuple((comment_or_space0, colon, comment_or_space0))),
                type_,
                opt(preceded(
                    tuple((comment_or_space0, equals, comment_or_space0)),
                    default_value,
                )),
                preceded(comment_or_space0, metadata),
            )),
            tuple((comment_or_space0, semicolon)),
        ),
        |(comment, name, ty, default_value, metadata)| {
            Field::builder()
                .doc(comment)
                .id(name)
                .ty(ty)
                .default_value(default_value)
                .metadata(metadata)
                .build()
        },
    )(input)
}

#[cfg(test)]
mod field_tests {
    use super::{field_decl, Field, Ident, QualifiedIdent, Type};
    use crate::{default_value, field, meta};

    #[test]
    fn test_field_decl_with_metadata() {
        let input = "message: string (required);";
        let result = field_decl(input);
        let expected = field!(message, String, [meta!(required)]);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_field_decl_with_comments() {
        // please never write a field like this
        let input = "\
foo // bar
// baz
 :
\tfloat64 // fizz
=         // buuzzzz
\t\t\t2.0


;";
        let result = field_decl(input);
        let expected = field!(foo, Float64 = 2.0);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_field_decl_with_doc_comment() {
        let input = "\
/// A really great field
great: int64;";

        let expected = field!(great, Int64, [" A really great field"]);
        let result = field_decl(input);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_field_decl_with_doc_and_non_doc_comment() {
        let input = "\
/// A really great field
// Not part of the doc

great: int64;";

        let expected = field!(great, Int64, [" A really great field"]);
        let result = field_decl(input);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_field_decl_with_spaced_doc_comment() {
        let input = "\
/// A really great field

great: int64;";

        let expected = field!(great, Int64, [" A really great field"]);
        let result = field_decl(input);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_field_decl_with_space_doc_and_non_doc_comment() {
        let input = "\
/// A really great field
// comment


// Thing!
great: int64;";

        let expected = field!(great, Int64, [" A really great field"]);
        let result = field_decl(input);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_field_decl() {
        let input = "foo: float64 = 2.0;";
        let result = field_decl(input);
        let expected = field!(foo, Float64 = 2.0);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_field_decl_uint() {
        let input = "foo :uint=3;";
        let result = field_decl(input);
        let expected = field!(foo, UInt = 3);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_field_decl_no_scalar() {
        let input = "foo:float64    //faz\n;";
        let result = field_decl(input);
        let expected = field!(foo, Float64);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_field_decl_enum_default_value() {
        let input = "foo : MyEnum = Foo;";
        let result = field_decl(input);
        let expected = Field::builder()
            .id("foo".into())
            .ty(Type::Ident(QualifiedIdent::from(vec![Ident::from(
                "MyEnum",
            )])))
            .default_value(Some(default_value!("Foo")))
            .build();
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_field_decl_boolean_default_value_explicit_false() {
        let input = "foo : bool = false;";
        let result = field_decl(input);
        let expected = Field::builder()
            .id("foo".into())
            .ty(Type::Bool)
            .default_value(Some(default_value!(false)))
            .build();
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_field_decl_boolean_default_value_true() {
        let input = "foo : bool = true;";
        let result = field_decl(input);
        let expected = Field::builder()
            .id("foo".into())
            .ty(Type::Bool)
            .default_value(Some(default_value!(true)))
            .build();
        assert_successful_parse!(result, expected);
    }
}

pub fn rpc_decl(input: &str) -> IResult<&str, Rpc<'_>> {
    map(
        tuple((
            doc_comment,
            preceded(
                terminated(tag("rpc_service"), comment_or_space1),
                terminated(ident, comment_or_space0),
            ),
            delimited(
                left_brace,
                many1(delimited(comment_or_space0, rpc_method, comment_or_space0)),
                right_brace,
            ),
        )),
        |(comment, name, methods)| {
            Rpc::builder()
                .doc(comment)
                .id(name)
                .methods(methods)
                .build()
        },
    )(input)
}

#[cfg(test)]
mod rpc_tests {
    use super::rpc_decl;
    use crate::{meta, method, rpc};

    #[test]
    fn test_rpc_decl_single_method_with_comments() {
        let input = "\
rpc_service Greeter { // foo
    //b baz
  SayHello(// foo
  HelloRequest):HelloReply; // zaz
// zu
}";
        let result = rpc_decl(input);
        let expected = rpc!(Greeter, [method!(fn SayHello(HelloRequest) -> HelloReply)]);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_rpc_decl_single_method() {
        let input = "\
rpc_service Greeter {
  SayHello(HelloRequest):HelloReply;
}";
        let result = rpc_decl(input);
        let expected = rpc!(Greeter, [method!(fn SayHello(HelloRequest) -> HelloReply)]);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_rpc_decl_multiple_methods() {
        let input = "\
rpc_service Greeter {
  SayHello   (HelloRequest ):HelloReply;
  SayManyHellos(ManyHellosRequest):HelloReply  (streaming: \"server\"  ) ;

}";
        let result = rpc_decl(input);
        let expected = rpc!(
            Greeter,
            [
                method!(fn SayHello(HelloRequest) -> HelloReply),
                method!(fn SayManyHellos(ManyHellosRequest) -> HelloReply, [
                    meta!(streaming, "server")
                ])
            ]
        );
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_rpc_decl_doc_comment() {
        let input = "\
/// A greeter service!
// impl
/// User facing
rpc_service Greeter {
  /// Hi there!
  SayHello   (HelloRequest ):HelloReply;
  /// Multiple hi theres!
  SayManyHellos(ManyHellosRequest):HelloReply  (streaming: \"server\"  ) ;

}";
        let result = rpc_decl(input);
        let expected = rpc!(
            Greeter,
            [
                method!(fn SayHello(HelloRequest) -> HelloReply, None, [ " Hi there!" ]),
                method!(fn SayManyHellos(ManyHellosRequest) -> HelloReply, [
                    meta!(streaming, "server")
                ], [ " Multiple hi theres!" ])
            ],
            [" A greeter service!", " User facing"]
        );
        assert_successful_parse!(result, expected);
    }
}

#[cfg(test)]
mod qualified_ident_tests {
    use super::{qualified_ident, Ident, QualifiedIdent};

    #[test]
    fn test_simple_qualified_ident() {
        let result = qualified_ident("a.b");
        let expected = QualifiedIdent::from(vec![Ident::from("a"), Ident::from("b")]);
        assert_successful_parse!(result, expected);
    }
}

pub fn rpc_method(input: &str) -> IResult<&str, RpcMethod<'_>> {
    map(
        tuple((
            doc_comment,
            terminated(ident, comment_or_space0),
            delimited(
                left_paren,
                delimited(comment_or_space0, qualified_ident, comment_or_space0),
                right_paren,
            ),
            preceded(
                delimited(comment_or_space0, colon, comment_or_space0),
                terminated(
                    tuple((qualified_ident, preceded(comment_or_space0, metadata))),
                    terminated(comment_or_space0, semicolon),
                ),
            ),
        )),
        |(comment, name, request_type, (response_type, metadata))| {
            RpcMethod::builder()
                .id(name)
                .request_type(request_type)
                .response_type(response_type)
                .metadata(metadata)
                .doc(comment)
                .build()
        },
    )(input)
}

#[cfg(test)]
mod rpc_method_tests {
    use super::rpc_method;
    use crate::{meta, method};

    #[test]
    fn test_rpc_method_with_comments() {
        let input = "\
SayHello // foo
(// bar
    HelloRequest
        // fiizz
        )
//buzz
// baz
:
    HelloReply // saz
// fzz
;";
        let result = rpc_method(input);
        let expected = method!(fn SayHello(HelloRequest) -> HelloReply);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_rpc_method() {
        let input = "SayHello(HelloRequest):HelloReply;";
        let result = rpc_method(input);
        let expected = method!(fn SayHello(HelloRequest) -> HelloReply);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_rpc_method_with_metadata() {
        let input =
            r#"SayHello(HelloRequest):HelloReply (streaming: "server", streaming: "server");"#;
        let result = rpc_method(input);
        let expected =
            method!(fn SayHello(HelloRequest) -> HelloReply, [meta!(streaming, "server")]);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_rpc_method_with_metadata_and_comments() {
        let input = r#"SayHello(HelloRequest):HelloReply (streaming:
            // fuzz
            "server", streaming: "server");"#;
        let result = rpc_method(input);
        let expected =
            method!(fn SayHello(HelloRequest) -> HelloReply, [meta!(streaming, "server")]);
        assert_successful_parse!(result, expected);
    }
}

pub fn type_(input: &str) -> IResult<&str, Type<'_>> {
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
        map(
            delimited(left_square_bracket, type_, right_square_bracket),
            |t| Type::from([t]),
        ),
        map(qualified_ident, Type::Ident),
    ))(input)
}

/// Parse the individual items of an enum or union.
pub fn enumval_decl(input: &str) -> IResult<&str, EnumVariant<'_>> {
    let parser = tuple((
        doc_comment,
        ident,
        opt(preceded(
            comment_or_space0,
            preceded(equals, preceded(comment_or_space0, integer_constant)),
        )),
    ));
    map(parser, |(comment, id, value)| {
        EnumVariant::builder()
            .id(id)
            .value(value)
            .doc(comment)
            .build()
    })(input)
}

/// Parse key-value metadata pairs.
pub fn raw_metadata(input: &str) -> IResult<&str, Metadata<'_>> {
    map(
        delimited(
            terminated(left_paren, comment_or_space0),
            separated_list(
                delimited(comment_or_space0, comma, comment_or_space0),
                tuple((
                    ident,
                    opt(preceded(
                        comment_or_space0,
                        preceded(colon, preceded(comment_or_space0, single)),
                    )),
                )),
            ),
            preceded(comment_or_space0, right_paren),
        ),
        Metadata::from,
    )(input)
}

/// Optionally parse key-value pairs. Metadata is never required wherever it's allowed.
pub fn metadata(input: &str) -> IResult<&str, Option<Metadata<'_>>> {
    opt(raw_metadata)(input)
}

#[cfg(test)]
mod metadata_tests {
    use super::{metadata, Metadata};
    use crate::meta;

    #[test]
    fn test_simple_metadata_with_comments() {
        let input = "\
(
    // fuzzy
    a // wuzzy
    : // wuz
    \"b\"
// a bear
\t)";
        let result = metadata(input);
        let expected = Some(Metadata::from(vec![meta!(a, "b")]));
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_simple_metadata() {
        let input = "(a: \"b\")";
        let result = metadata(input);
        let expected = Some(Metadata::from(vec![meta!(a, "b")]));
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_single_metadata() {
        let input = "(required)";
        let result = metadata(input);
        let expected = Some(Metadata::from(vec![meta!(required)]));
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_empty_metadata() {
        let input = "()";
        let result = metadata(input);
        let expected = Some(Metadata::builder().build());
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_keys_only_metadata() {
        let input = "(a, b, c, def)";
        let result = metadata(input);
        let expected = Some(Metadata::from(vec![
            meta!(a),
            meta!(b),
            meta!(c),
            meta!(def),
        ]));
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_multiple_field_metadata() {
        let input = "(x, a: \"news\", b: 42,c: 42.42, d, e)";
        let result = metadata(input);
        let expected = Some(Metadata::from(vec![
            meta!(x),
            meta!(a, "news"),
            meta!(b, 42),
            meta!(c, 42.42),
            meta!(d),
            meta!(e),
        ]));
        assert_successful_parse!(result, expected);
    }
}

/// Parse float, integer or boolean constants.
pub fn scalar(input: &str) -> IResult<&str, Scalar> {
    alt((
        map(float_constant, Scalar::from),
        map(integer_constant, Scalar::from),
        map(boolean_constant, Scalar::from),
    ))(input)
}

/// Parse JSON object-like data.
pub fn object(input: &str) -> IResult<&str, Object<'_>> {
    map(
        tuple((
            doc_comment,
            delimited(
                terminated(left_brace, comment_or_space0),
                separated_list(
                    delimited(comment_or_space0, comma, comment_or_space0),
                    separated_pair(
                        ident,
                        delimited(comment_or_space0, colon, comment_or_space0),
                        value_,
                    ),
                ),
                preceded(comment_or_space0, right_brace),
            ),
        )),
        |(comment, key_value_pairs)| {
            Object::builder()
                .doc(comment)
                .values(std::collections::HashMap::from_iter(key_value_pairs))
                .build()
        },
    )(input)
}

#[cfg(test)]
mod object_tests {
    use super::object;
    use crate::object as obj;

    #[test]
    fn test_object() {
        let input = r#"{a: ["b", 1.0, 2, [3]]}"#;
        let result = object(input);
        let expected = obj!({
            a => ["b", 1.0, 2, [3]]
        });
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_empty_object() {
        let input = "{}";
        let result = object(input);
        let expected = obj!({});
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_object_with_doc_comments() {
        let input = "\
/// A doc
//
/// Comment

{}";
        let result = object(input);
        let expected = obj!({}, [" A doc", " Comment"]);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_object_with_comments() {
        let input = r#"{
        // baz

            a //buzz
                : // fizz
                [// baz
"b" // foo
, // fizz
         1.0, 2, [//foo
             3//bar
         ]
                //baz
                ]
                // foo
}"#;
        let result = object(input);
        let expected = obj!({
            a => ["b", 1.0, 2, [3]]
        });
        assert_successful_parse!(result, expected);
    }
}

/// `Single`s are `Scalar`s or `StringConstant`s.
pub fn single(input: &str) -> IResult<&str, Single<'_>> {
    alt((
        map(scalar, Single::from),
        map(string_constant, Single::from),
    ))(input)
}

/// A value list is a comma-separated list of `Value`s.
pub fn value_list(input: &str) -> IResult<&str, Vec<Value<'_>>> {
    delimited(
        terminated(left_square_bracket, comment_or_space0),
        separated_list(
            delimited(comment_or_space0, comma, comment_or_space0),
            value_,
        ),
        preceded(comment_or_space0, right_square_bracket),
    )(input)
}

#[cfg(test)]
mod value_list_tests {
    use super::value_list;
    use crate::value as val;

    #[test]
    fn test_value_list() {
        let input = r#"["a",1, 2]"#;
        let result = value_list(input);
        assert_successful_parse!(result, vec![val!("a"), val!(1), val!(2)]);
    }

    #[test]
    fn test_value_list_with_comments() {
        let input = r#"[//thing
    "a",1
    // comment
    ,
           // a huge comment
           // on
           // mul-
           // tiple lines, some space too!

            2// fiz
                ]"#;
        let result = value_list(input);
        assert_successful_parse!(result, vec![val!("a"), val!(1), val!(2)]);
    }
}

/// `Value`s are `Single`s or `Object`s.
pub fn value_(input: &str) -> IResult<&str, Value<'_>> {
    alt((
        map(single, Value::from),
        map(object, Value::from),
        map(value_list, Value::from),
    ))(input)
}

#[cfg(test)]
mod value_tests {
    use super::{value_, Scalar, Single, Value};
    use crate::value as val;

    #[test]
    fn test_value_single_value() {
        let result = value_("1");
        assert_successful_parse!(
            result,
            Value::Single(Single::Scalar(Scalar::Integer(1.into())))
        );

        let result = value_("2.3");
        assert_successful_parse!(result, val!(2.3));

        let result = value_("\"abc d\"");
        assert_successful_parse!(result, val!("abc d"));

        let result = value_("true");
        assert_successful_parse!(result, val!(true));

        let result = value_("false");
        assert_successful_parse!(result, val!(false));
    }

    #[test]
    fn test_value_object() {
        let result = value_("{a: 1}");
        assert_successful_parse!(result, val!({ a => 1 }));

        let result = value_("{b: 2.42}");
        assert_successful_parse!(result, val!({ b => 2.42 }));

        let result = value_(r#"{c: ["a"]}"#);
        assert_successful_parse!(result, val!({ c => ["a"] }));

        // What kind of person would put this in a flatbuffer schema?
        let result = value_(r#"{d: ["a", {b: [1, ["z"]]}]}"#);
        assert_successful_parse!(
            result,
            val!({
                d => [
                    "a",
                    {
                        b => [1, ["z"]]
                    }
                ]
            })
        );
    }

    #[test]
    fn test_value_value_list() {
        let result = value_(r#"["a", 1]"#);
        let expected = val!(["a", 1]);
        assert_successful_parse!(result, expected);
    }
}

/// `DefaultValue`s are `Scalar`s or `Ident`s
/// `Ident`s refer to an enum variant on the field type
pub fn default_value(input: &str) -> IResult<&str, DefaultValue<'_>> {
    alt((
        map(scalar, DefaultValue::from),
        map(ident, DefaultValue::from),
    ))(input)
}

#[cfg(test)]
mod default_value_tests {
    use super::default_value;
    use crate::default_value as defval;

    #[test]
    fn test_default_value() {
        let result = default_value("1");
        assert_successful_parse!(result, defval!(1));

        let result = default_value("Foo");
        assert_successful_parse!(result, defval!("Foo"));

        let result = default_value("false");
        assert_successful_parse!(result, defval!(false));

        let result = default_value("true");
        assert_successful_parse!(result, defval!(true));
    }
}

// A type definition of the return type of a parsed table or struct body.
type ProductTypeTriple<'a> = (Ident<'a>, Option<Metadata<'a>>, Vec<Field<'a>>);

/// Parse the body of a table or struct.
pub fn product_type_body(input: &str) -> IResult<&str, ProductTypeTriple<'_>> {
    tuple((
        delimited(comment_or_space1, ident, comment_or_space0),
        terminated(metadata, comment_or_space0),
        delimited(
            preceded(left_brace, comment_or_space0),
            many0(delimited(comment_or_space0, field_decl, comment_or_space0)),
            terminated(comment_or_space0, right_brace),
        ),
    ))(input)
}

/// Parse a struct declaration.
pub fn struct_decl(input: &str) -> IResult<&str, Struct<'_>> {
    map(
        tuple((doc_comment, preceded(tag("struct"), product_type_body))),
        |(comment, (name, metadata, fields))| {
            Struct::builder()
                .doc(comment)
                .id(name)
                .metadata(metadata)
                .fields(fields)
                .build()
        },
    )(input)
}

/// Parse a table declaration.
pub fn table_decl(input: &str) -> IResult<&str, Table<'_>> {
    map(
        tuple((doc_comment, preceded(tag("table"), product_type_body))),
        |(comment, (name, metadata, fields))| {
            Table::builder()
                .doc(comment)
                .id(name)
                .metadata(metadata)
                .fields(fields)
                .build()
        },
    )(input)
}

#[cfg(test)]
mod table_tests {
    use super::{table_decl, Field, Ident, QualifiedIdent, Table, Type};
    use crate::{default_value, field, meta, table};

    #[test]
    fn test_empty_table_no_spaces() {
        let input = "table HelloReply {}";
        let result = table_decl(input);
        let expected = table!(HelloReply, []);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_empty_table() {
        let input = "\
table HelloReply {
}";
        let result = table_decl(input);
        let expected = table!(HelloReply, []);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_table_with_doc_commented_fields() {
        let input = "\
/// A response with a required field
table HelloReply {

  /// Sweet, sweet message
  message: string;
  // Not a doc comment
  /// A doc comment
  foo: [string];
  // Aha!

  bar: [float64];

}";
        let result = table_decl(input);
        let expected = table!(
            HelloReply,
            [
                field!(message, String, [" Sweet, sweet message"]),
                field!(foo, [String], [" A doc comment"]),
                field!(bar, [Float64])
            ],
            [" A response with a required field"]
        );
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_table_with_required_field() {
        let input = "\
/// A response with a required field
table HelloReply {
  message: string (required);
}";
        let result = table_decl(input);
        let expected = table!(
            HelloReply,
            [field!(message, String, [meta!(required)])],
            [" A response with a required field"]
        );
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_table_with_comments() {
        // but y tho?
        let input = "\
/// My awesome table!
table HelloReply // baz
// foo
{ // buz
  message: string; // fizz
  // buzzz
  // a
foo :uint=3;//baz
// fizzy
  bar // fuzzy
  :      [int8];
}";
        let result = table_decl(input);
        let expected = table!(
            HelloReply,
            [
                field!(message, String),
                field!(foo, UInt = 3),
                field!(bar, [Int8])
            ],
            [" My awesome table!"]
        );
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_table_multiple_fields() {
        let input = "\
/// My awesome table!
table HelloReply
{
  message: string;
  foo :uint=3;

  bar :      [int8];
}";
        let result = table_decl(input);
        let expected = table!(
            HelloReply,
            [
                field!(message, String),
                field!(foo, UInt = 3),
                field!(bar, [Int8])
            ],
            [" My awesome table!"]
        );
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_table() {
        let input = "\
table HelloReply {
  message: string;
}";
        let result = table_decl(input);
        let expected = table!(HelloReply, [field!(message, String)]);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_table_with_default_enum_value() {
        let input = "\
table HelloReply {
    message_type: MessageType = UNKNOWN;
}";
        let result = table_decl(input);
        let expected = Table::builder()
            .id(Ident::from("HelloReply"))
            .fields(vec![Field::builder()
                .id("message_type".into())
                .ty(Type::Ident(QualifiedIdent::from(vec![Ident::from(
                    "MessageType",
                )])))
                .default_value(Some(default_value!("UNKNOWN")))
                .build()])
            .build();

        assert_successful_parse!(result, expected);
    }
}

/// A decimal integer constant
pub fn dec_integer_constant(input: &str) -> IResult<&str, IntegerConstant> {
    map_res(
        recognize(preceded(opt(plus_or_minus), digit1)),
        IntegerConstant::from_str,
    )(input)
}

#[cfg(test)]
mod dec_integer_constant_tests {
    use super::{dec_integer_constant, IntegerConstant};
    use anyhow::Result;
    use std::convert::TryFrom;

    #[test]
    fn test_u64() -> Result<()> {
        let u64_max = std::u64::MAX;
        let u64_max_string = u64_max.to_string();
        let result = dec_integer_constant(&u64_max_string);
        let expected = IntegerConstant::try_from(u64_max)?;
        assert_successful_parse!(result, expected);
        Ok(())
    }

    #[test]
    fn test_i64_max() -> Result<()> {
        let i64_max = std::i64::MAX;
        let i64_max_string = i64_max.to_string();
        let result = dec_integer_constant(&i64_max_string);
        let expected = IntegerConstant::try_from(i64_max)?;
        assert_successful_parse!(result, expected);
        Ok(())
    }

    #[test]
    fn test_i64_min() -> Result<()> {
        let i64_min = std::i64::MIN;
        let i64_min_string = i64_min.to_string();
        let result = dec_integer_constant(&i64_min_string);
        let expected = IntegerConstant::try_from(i64_min)?;
        assert_successful_parse!(result, expected);
        Ok(())
    }

    #[test]
    fn test_dec_integer_constant() {
        let result = dec_integer_constant("1234");
        assert_successful_parse!(result, 1234.into());

        let result = dec_integer_constant("-1234");
        assert_successful_parse!(result, (-1234).into());
    }
}

pub fn hex_integer_constant(input: &str) -> IResult<&str, IntegerConstant> {
    map_res(
        tuple((
            opt(plus_or_minus),
            preceded(zero, preceded(one_of("xX"), hex_digit1)),
        )),
        |(sign, value)| -> Result<IntegerConstant> {
            let int_const = IntegerConstant::from_str_radix(value, 16)?;
            if let Some('-') = sign {
                int_const
                    .checked_neg()
                    .ok_or_else(|| anyhow!("invalid value for negation {}", int_const))
            } else {
                Ok(int_const)
            }
        },
    )(input)
}

#[cfg(test)]
mod hex_integer_constant_tests {
    use super::hex_integer_constant;

    #[test]
    fn test_hex_integer_constant() {
        let result = hex_integer_constant("0x1234ABCDEFabcdef");
        assert_successful_parse!(result, 0x1234_ABCD_EFAB_CDEF_i64.into());

        let result = hex_integer_constant("-0x1234ABCDEFabcdef");
        assert_successful_parse!(result, (-0x1234_ABCD_EFAB_CDEF_i64).into());
    }

    #[test]
    fn test_invalid_hex_integer_constant() {
        let result = hex_integer_constant("ABCDEFabcdef");
        assert_failed_parse!(result, "ABCDEFabcdef", Char);
    }
}

pub fn true_(input: &str) -> IResult<&str, BooleanConstant> {
    value(true, |input: &str| {
        nom::re_match!(input, r"\btrue\b")?;
        tag("true")(input)
    })(input)
}

pub fn false_(input: &str) -> IResult<&str, BooleanConstant> {
    value(false, |input: &str| {
        nom::re_match!(input, r"\bfalse\b")?;
        tag("false")(input)
    })(input)
}

#[cfg(test)]
mod true_false_tests {
    use super::{false_, true_};

    #[test]
    fn test_true() {
        let result = true_("true");
        assert_successful_parse!(result, true);
    }

    #[test]
    fn test_true_semicolon() {
        let result = true_("true;");
        assert_successful_parse!(result, ";", true);
    }

    #[test]
    fn test_invalid_true() {
        let result = true_("truez");
        assert_failed_parse!(result, "truez", RegexpMatch);
    }

    #[test]
    fn test_false() {
        let result = false_("false");
        assert_successful_parse!(result, false);
    }

    #[test]
    fn test_false_semicolon() {
        let result = false_("false;");
        assert_successful_parse!(result, ";", false);
    }

    #[test]
    fn test_invalid_false() {
        let result = false_("falsez");
        assert_failed_parse!(result, "falsez", RegexpMatch);
    }
}

/// TODO: This is unused by any other rule in the flatbuffers grammar. I've assumed it's a `Scalar`
/// here
pub fn boolean_constant(input: &str) -> IResult<&str, bool> {
    alt((true_, false_))(input)
}

#[cfg(test)]
mod boolean_constant_tests {
    use super::boolean_constant;

    #[test]
    fn test_boolean_constant() {
        let result = boolean_constant("true");
        assert_successful_parse!(result, true);

        let result = boolean_constant("false");
        assert_successful_parse!(result, false);
    }

    #[test]
    fn test_invalid_boolean_constant() {
        let result = boolean_constant("waltz");
        assert_failed_parse!(result, "waltz", RegexpMatch);
    }
}

pub fn dec_float_exponent(input: &str) -> IResult<&str, &str> {
    recognize(tuple((one_of("eE"), opt(plus_or_minus), digit1)))(input)
}

// Taken from Python's tokenize.Floatnumber
pub fn dec_float_constant(input: &str) -> IResult<&str, FloatingConstant> {
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

#[cfg(test)]
mod dec_float_constant_tests {
    use super::dec_float_constant;

    #[test]
    fn test_dec_float_constant() {
        let result = dec_float_constant("-2.1");
        assert_successful_parse!(result, -2.1);
    }
}

/// A float constant is either a special float constant (nan, inf, or infinity), a hex float
/// constant or a double.
pub fn float_constant(input: &str) -> IResult<&str, FloatingConstant> {
    alt((
        special_float_constant,
        hex_float_constant,
        dec_float_constant,
    ))(input)
}

#[cfg(test)]
mod float_constant_tests {
    use super::{assert_eq, float_constant};

    #[test]
    fn test_float_constant_nan() {
        let result = float_constant("nan");
        assert_eq!(
            result.map(|(input, value)| (input, value.is_nan(), value.is_sign_positive())),
            Ok(("", true, true))
        );

        let result = float_constant("+nan");
        assert_eq!(
            result.map(|(input, value)| (input, value.is_nan(), value.is_sign_positive())),
            Ok(("", true, true))
        );

        let result = float_constant("-nan");
        assert_eq!(
            result.map(|(input, value)| (input, value.is_nan(), value.is_sign_negative())),
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
}

// Adapted from Python's tokenize.Floatnumber
pub fn hex_float_exponent(input: &str) -> IResult<&str, &str> {
    recognize(terminated(
        one_of("pP"),
        terminated(opt(plus_or_minus), digit1),
    ))(input)
}

pub fn hex_float_constant(input: &str) -> IResult<&str, FloatingConstant> {
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
pub fn nan(input: &str) -> IResult<&str, FloatingConstant> {
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

#[cfg(test)]
mod nan_tests {
    use super::{assert_eq, nan};

    #[test]
    fn test_nan() {
        let result = nan("nan");
        assert_eq!(
            result.map(|(input, value)| (input, value.is_nan(), value.is_sign_positive())),
            Ok(("", true, true))
        );

        let result = nan("+nan");
        assert_eq!(
            result.map(|(input, value)| (input, value.is_nan(), value.is_sign_positive())),
            Ok(("", true, true))
        );

        let result = nan("-nan");
        assert_eq!(
            result.map(|(input, value)| (input, value.is_nan(), value.is_sign_negative())),
            Ok(("", true, true))
        );
    }

    #[test]
    fn test_invalid_nan() {
        let result = nan("nanz");
        assert_failed_parse!(result, "nanz", RegexpMatch);
    }
}

/// Parse `inf` or `infinity`
pub fn inf_or_infinity(input: &str) -> IResult<&str, FloatingConstant> {
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

#[cfg(test)]
mod inf_or_infinity_tests {
    use super::inf_or_infinity;

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
}

/// Parse `nan`, `inf`, or `infiniity`
pub fn special_float_constant(input: &str) -> IResult<&str, FloatingConstant> {
    alt((nan, inf_or_infinity))(input)
}

#[cfg(test)]
mod special_float_constant_tests {
    use super::{assert_eq, special_float_constant};

    #[test]
    fn test_special_float_constant_nan() {
        let result = special_float_constant("nan");
        assert_eq!(
            result.map(|(input, value)| (input, value.is_nan(), value.is_sign_positive())),
            Ok(("", true, true))
        );

        let result = special_float_constant("+nan");
        assert_eq!(
            result.map(|(input, value)| (input, value.is_nan(), value.is_sign_positive())),
            Ok(("", true, true))
        );

        let result = special_float_constant("-nan");
        assert_eq!(
            result.map(|(input, value)| (input, value.is_nan(), value.is_sign_negative())),
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
}

pub fn integer_constant(input: &str) -> IResult<&str, IntegerConstant> {
    alt((hex_integer_constant, dec_integer_constant))(input)
}

#[cfg(test)]
mod integer_constant_tests {
    use super::integer_constant;

    #[test]
    fn test_integer_constant() {
        let result = integer_constant("1234");
        assert_successful_parse!(result, 1234.into());

        let result = integer_constant("-1234");
        assert_successful_parse!(result, (-1234).into());

        let result = integer_constant("0x1234");
        assert_successful_parse!(result, 0x1234.into());

        let result = integer_constant("-0x1234");
        assert_successful_parse!(result, (-0x1234).into());
    }
}

pub fn file_extension_decl(input: &str) -> IResult<&str, FileExtension<'_>> {
    map(
        tuple((
            doc_comment,
            delimited(
                tag("file_extension"),
                delimited(comment_or_space0, string_constant, comment_or_space0),
                semicolon,
            ),
        )),
        |(comment, ext)| FileExtension::builder().doc(comment).ext(ext).build(),
    )(input)
}

#[cfg(test)]
mod file_extension_tests {
    use super::file_extension_decl;
    use crate::file_ext;

    #[test]
    fn test_file_extension_decl_with_comments() {
        let result = file_extension_decl(
            "file_extension  //bar
        // foo
            \"foo\"//baz\n\t;",
        );
        let expected = file_ext!("foo");
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_file_extension_decl_with_doc_comments() {
        let result = file_extension_decl(
            "\
/// Doc1
//

/// Doc2
file_extension \"foo\";",
        );
        let expected = file_ext!("foo", [" Doc1", " Doc2"]);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_file_extension_decl() {
        let result = file_extension_decl("file_extension \"foo\";");
        let expected = file_ext!("foo");
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_file_extension_decl_no_leading_space() {
        let result = file_extension_decl("file_extension\"foo\";");
        let expected = file_ext!("foo");
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_file_extension_decl_trailing_space() {
        let result = file_extension_decl("file_extension \"foo\"  ;");
        let expected = file_ext!("foo");
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_file_extension_decl_surrounding_space() {
        let result = file_extension_decl("file_extension   \"foo\"  ;");
        let expected = file_ext!("foo");
        assert_successful_parse!(result, expected);
    }
}

pub fn file_identifier_decl(input: &str) -> IResult<&str, FileIdentifier<'_>> {
    map_res(
        tuple((
            doc_comment,
            delimited(
                tag("file_identifier"),
                delimited(
                    comment_or_space0,
                    delimited(
                        double_quote,
                        nom::bytes::complete::take(4_usize),
                        double_quote,
                    ),
                    comment_or_space0,
                ),
                semicolon,
            ),
        )),
        |(comment, id)| {
            let id_bytes = id.as_bytes();
            let num_id_bytes = id_bytes.len();
            if num_id_bytes != 4 {
                return Err(nom::Err::Failure(format!(
                    "file identifier should be 4 bytes long, got {} bytes",
                    num_id_bytes
                )));
            }
            let mut buf = [0u8; 4];
            buf.copy_from_slice(id_bytes);
            Ok(FileIdentifier::builder()
                .doc(comment)
                // The `anychar` combinator only matches a byte, so the conversion here cannot be
                // incorrect if indeed that is what `anychar` does
                .id(buf)
                .build())
        },
    )(input)
}

#[cfg(test)]
mod file_identifier_tests {
    use super::file_identifier_decl;
    use crate::file_id;

    #[test]
    fn test_file_identifier_decl_with_comments() {
        let result = file_identifier_decl(
            "file_identifier // baz
            \t\t\"PAR3\"// buz!\n\t;",
        );
        let expected = file_id!(b"PAR3");
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_file_identifier_decl_with_doc_comments() {
        let input = "\
/// Some file id

//
/// Doc 2
file_identifier \"PAR3\";";
        let result = file_identifier_decl(input);
        let expected = file_id!(b"PAR3", [" Some file id", " Doc 2"]);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_file_identifier_decl() {
        let result = file_identifier_decl("file_identifier \"PAR3\";");
        let expected = file_id!(b"PAR3");
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_file_identifier_decl_no_leading_space() {
        let result = file_identifier_decl("file_identifier\"BAAR\";");
        let expected = file_id!(b"BAAR");
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_file_identifier_decl_trailing_space() {
        let result = file_identifier_decl("file_identifier \"DEF1\"  ;");
        let expected = file_id!(b"DEF1");
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_file_identifier_decl_surrounding_space() {
        let result = file_identifier_decl("file_identifier   \"ABCD\"  ;");
        let expected = file_id!(b"ABCD");
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_file_identifier_decl_with_multibyte_char() {
        let result = file_identifier_decl("file_identifier \"❤234\";");
        assert_failed_parse!(result, "file_identifier \"❤234\";", MapRes);
    }
}

/// Parse one line of a documentation comment.
pub fn raw_doc_comment(input: &str) -> IResult<&str, &str> {
    preceded(tag("///"), not_line_ending)(input)
}

/// Parse zero or more lines of documentation comments
pub fn doc_comment_lines(input: &str) -> IResult<&str, Vec<&str>> {
    many0(terminated(
        terminated(raw_doc_comment, line_ending),
        comment_or_space0,
    ))(input)
}

/// Wrap zero or more lines of documentation comments in an AST node.
pub fn doc_comment(input: &str) -> IResult<&str, Comment<'_>> {
    map(doc_comment_lines, Comment::from)(input)
}

#[cfg(test)]
mod comment_tests {
    use super::{comment, doc_comment, doc_comment_lines, raw_doc_comment, Comment};

    #[test]
    fn test_empty_comment() {
        let input = "//\n";
        let result = comment(input);
        assert_successful_parse!(result, "\n", ());

        let input = "//\r\n";
        let result = comment(input);
        assert_successful_parse!(result, "\r\n", ());
    }

    #[test]
    fn test_comment() {
        let input = "// a b c \n";
        let result = comment(input);
        assert_successful_parse!(result, "\n", ());
    }

    #[test]
    fn test_empty_raw_doc_comment() {
        let input = "///\n";
        let result = raw_doc_comment(input);

        // we eat the triple forward slash (as opposed to discarding them later like we do with
        // non-doc comments) since we need to keep track of the text but not the slashes
        let expected = "";
        assert_successful_parse!(result, "\n", expected);

        let input = "///\r\n";
        let result = raw_doc_comment(input);
        let expected = "";
        assert_successful_parse!(result, "\r\n", expected);
    }

    #[test]
    fn test_doc_comment_lines() {
        let input = "/// A\n///   B\n/// C\n";
        let result = doc_comment_lines(input);
        let expected = vec![" A", "   B", " C"];
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_doc_comment() {
        let input = "/// My awesome table!\n";
        let expected = Comment::from(vec![" My awesome table!"]);
        let result = doc_comment(input);
        assert_successful_parse!(result, expected);
    }

    #[test]
    fn test_doc_comment_multi_line() {
        let input = "/// My awesome table!\n/// Another comment w00t?\n";
        let expected = Comment::from(vec![" My awesome table!", " Another comment w00t?"]);
        let result = doc_comment(input);
        assert_successful_parse!(result, expected);

        let input = "/// My awesome table!\na";
        let expected = Comment::from(vec![" My awesome table!"]);
        let result = doc_comment(input);
        assert_successful_parse!(result, "a", expected);
    }
}
