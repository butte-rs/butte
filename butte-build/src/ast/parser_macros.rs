#[macro_export]
macro_rules! scalar {
    ($expr:expr) => {
        $crate::ast::types::Scalar::from($expr)
    };
}

#[macro_export]
macro_rules! single_value {
    ($value:tt) => {
        $crate::ast::types::Single::from($value)
    };
}

#[cfg(test)]
mod single_value_tests {
    #[test]
    fn test_single_value() {
        use crate::ast::types::{Scalar, Single};

        let val = single_value!(1);
        assert_eq!(val, Single::Scalar(Scalar::Integer(1i32.into())));

        let val = single_value!(1);
        assert_eq!(val, Single::Scalar(Scalar::Integer(1i64.into())));

        let val = single_value!(1u64);
        assert_eq!(val, Single::Scalar(Scalar::Integer(1u64.into())));

        let val = single_value!(1.0);
        assert_eq!(val, Single::Scalar(Scalar::Float(1.0)));

        let val = single_value!(true);
        assert_eq!(val, Single::Scalar(Scalar::Boolean(true)));

        let val = single_value!(false);
        assert_eq!(val, Single::Scalar(Scalar::Boolean(false)));

        let val = single_value!("a");
        assert_eq!(val, Single::String("a"));
    }
}

#[macro_export]
macro_rules! default_value {
    ($value:tt) => {
        $crate::ast::types::DefaultValue::from($value)
    };
}

#[cfg(test)]
mod default_value_tests {
    #[test]
    fn test_default_value() {
        use crate::ast::types::{DefaultValue, Ident, Scalar};

        let val = default_value!(1);
        assert_eq!(val, DefaultValue::Scalar(Scalar::Integer(1i32.into())));

        let val = default_value!(1);
        assert_eq!(val, DefaultValue::Scalar(Scalar::Integer(1i64.into())));

        let val = default_value!(1u64);
        assert_eq!(val, DefaultValue::Scalar(Scalar::Integer(1u64.into())));

        let val = default_value!(1.0);
        assert_eq!(val, DefaultValue::Scalar(Scalar::Float(1.0)));

        let val = default_value!(true);
        assert_eq!(val, DefaultValue::Scalar(Scalar::Boolean(true)));

        let val = default_value!(false);
        assert_eq!(val, DefaultValue::Scalar(Scalar::Boolean(false)));

        let val = default_value!("Foo");
        assert_eq!(val, DefaultValue::Ident(Ident::from("Foo")));
    }
}

#[macro_export]
macro_rules! namespace {
    ($path:path$(, $doc:literal)?) => {
        $crate::ast::types::Namespace::builder()
            .ident($crate::qualified_ident_from_path_string!($path))
            .doc(vec![ $($doc,)? ].into())
            .build()
    };
}

#[macro_export]
macro_rules! object {
    ({ $($key:ident => $value:tt),* }$(,[ $($doc:literal),+ ])?) => {{
        use std::iter::FromIterator;
        $crate::ast::types::Object::builder()
            .values(std::collections::HashMap::from_iter(vec![
                $(($crate::ast::types::Ident::from(stringify!($key)), $crate::value!($value))),*
            ]))
            .doc(vec![$($($doc,)+)?].into())
            .build()
    }};
}

#[macro_export]
macro_rules! value {
    ([ $($element:tt),* ]) => {
        $crate::ast::types::Value::from(vec![ $($crate::value!($element)),* ])
    };
    ({ $($key:ident => $value:tt),* }) => {
        $crate::ast::types::Value::from(
            $crate::object!({ $($key => $value),* })
        )
    };
    ($value:tt) => {
        $crate::ast::types::Value::from($crate::ast::types::Single::from($value))
    };
}

#[cfg(test)]
mod value_macro_tests {
    #[test]
    fn test_value_macro_simple() {
        use crate::ast::types::{Scalar, Single, Value};

        let result = value!("a");
        let expected = Value::Single(Single::String("a"));
        assert_eq!(result, expected);

        let result = value!(1);
        let expected = Value::Single(Single::Scalar(Scalar::Integer(1.into())));
        assert_eq!(result, expected);
    }

    #[test]
    fn test_value_macro_list() {
        use crate::ast::types::{Scalar, Single, Value};

        let result = value!(["a", "b", 1]);
        let expected = Value::List(vec![
            Value::from(Single::from("a")),
            Value::from(Single::from("b")),
            Value::from(Single::from(Scalar::from(1))),
        ]);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_value_macro_obj() {
        use crate::ast::types::{Ident, Scalar, Single, Value};

        let result = value!({ a => 1, b => "c" });
        let expected = Value::from(vec![
            (
                Ident::from("a"),
                Value::Single(Single::Scalar(Scalar::Integer(1.into()))),
            ),
            (Ident::from("b"), Value::Single(Single::String("c"))),
        ]);
        assert_eq!(result, expected);
    }
}

#[macro_export]
macro_rules! element {
    ($elem:expr) => {
        $crate::ast::types::Element::from($elem)
    };
}

#[macro_export]
macro_rules! field {
    ($name:ident, $ty:ident = $default:expr$(, $doc:literal)?) => {
        $crate::ast::types::Field::builder()
            .id($crate::ast::types::Ident::from(stringify!($name)))
            .ty($crate::ast::types::Type::$ty)
            .doc(vec![$($doc,)?].into())
            .default_value(Some($crate::default_value!($default)))
            .build()
    };
    ($name:ident, [ $ty:ident ]$(, $doc:literal)?) => {
        $crate::ast::types::Field::builder()
            .id($crate::ast::types::Ident::from(stringify!($name)))
            .ty($crate::ast::types::Type::Array(Box::new(
                $crate::ast::types::Type::$ty,
            )))
            .doc(vec![$($doc,)?].into())
            .build()
    };
    ($name:ident, $ty:ident$(, $doc:literal)?) => {
        $crate::ast::types::Field::builder()
            .id($crate::ast::types::Ident::from(stringify!($name)))
            .ty($crate::ast::types::Type::$ty)
            .doc(vec![$($doc,)?].into())
            .build()
    };
    ($name:ident, [ $ty:path ]$(, $doc:literal)?) => {
        $crate::ast::types::Field::builder()
            .id($crate::ast::types::Ident::from(stringify!($name)))
            .ty($crate::ast::types::Type::Array(Box::new(
                $crate::ast::types::Type::Ident($crate::qualified_ident_from_path_string!($ty)),
            )))
            .doc(vec![$($doc,)?].into())
            .build()
    };
    ($name:ident, $ty:path$(, $doc:literal)?) => {
        $crate::ast::types::Field::builder()
            .id($crate::ast::types::Ident::from(stringify!($name)))
            .ty($crate::ast::types::Type::Ident(
                $crate::qualified_ident_from_path_string!($ty),
            ))
            .doc(vec![$($doc,)?].into())
            .build()
    };
    ($name:ident, $ty:ident = $default:expr, [ $($meta:expr),* ]$(, $doc:literal)?) => {
        $crate::ast::types::Field::builder()
            .id($crate::ast::types::Ident::from(stringify!($name)))
            .ty($crate::ast::types::Type::$ty)
            .scalar(Some($crate::scalar!($default)))
            .metadata(Some($crate::ast::types::Metadata::from(vec![ $($meta),* ])))
            .doc(vec![$($doc,)?].into())
            .build()
    };
    ($name:ident, [ $ty:ident ], [ $($meta:expr),* ]$(, $doc:literal)?) => {
        $crate::ast::types::Field::builder()
            .id($crate::ast::types::Ident::from(stringify!($name)))
            .ty($crate::ast::types::Type::Array(Box::new(
                $crate::ast::types::Type::$ty,
            )))
            .metadata(Some($crate::ast::types::Metadata::from(vec![ $($meta),* ])))
            .doc(vec![$($doc,)?].into())
            .build()
    };
    ($name:ident, $ty:ident, [ $($meta:expr),* ]$(, $doc:literal)?) => {
        $crate::ast::types::Field::builder()
            .id($crate::ast::types::Ident::from(stringify!($name)))
            .ty($crate::ast::types::Type::$ty)
            .metadata(Some($crate::ast::types::Metadata::from(vec![ $($meta),* ])))
            .build()
    };
    ($name:ident, [ $ty:path ], [ $($meta:expr),* ]$(, $doc:literal)?) => {
        $crate::ast::types::Field::builder()
            .id($crate::ast::types::Ident::from(stringify!($name)))
            .ty($crate::ast::types::Type::Array(Box::new(
                $crate::ast::types::Type::Ident($crate::qualified_ident_from_path_string!($ty)),
            )))
            .metadata(Some($crate::ast::types::Metadata::from(vec![ $($meta),* ])))
            .doc(vec![$($doc,)?].into())
            .build()
    };
    ($name:ident, $ty:path, [ $($meta:expr),* ]$(, $doc:literal)?) => {
        $crate::ast::types::Field::builder()
            .id($crate::ast::types::Ident::from(stringify!($name)))
            .ty($crate::ast::types::Type::Ident(
                $crate::qualified_ident_from_path_string!($ty),
            ))
            .metadata(Some($crate::ast::types::Metadata::from(vec![ $($meta),* ])))
            .doc(vec![$($doc,)?].into())
            .build()
    };
}

#[macro_export]
macro_rules! qualified_ident_from_path_string {
    ($expr:expr) => {
        $crate::ast::types::QualifiedIdent::from(
            stringify!($expr)
                .split("::")
                .map(Ident::from)
                .collect::<Vec<_>>(),
        )
    };
}

#[macro_export]
macro_rules! table {
    ($name:ident, $($doc:literal,)? [ $($field:expr),* ]) => {
        $crate::ast::types::Table::builder()
            .doc(vec![ $($doc,)? ].into())
            .id($crate::ast::types::Ident::from(stringify!($name)))
            .fields(vec![ $($field),* ]).build()
    };
}

#[macro_export]
macro_rules! comment {
    () => {
        $crate::ast::types::Comment::from(vec![])
    };
    ($text:expr) => {
        $crate::ast::types::Comment::from($text.split_terminator("\n").collect::<Vec<_>>())
    };
}

#[macro_export]
macro_rules! meta {
    ($key:ident, $value:expr) => {
        (
            $crate::ast::types::Ident::from(stringify!($key)),
            Some($crate::single_value!($value)),
        )
    };
    ($key:ident) => {
        ($crate::ast::types::Ident::from(stringify!($key)), None)
    };
}

#[macro_export]
macro_rules! method {
    (fn $method_name:ident($req_ty:ident) -> $resp_ty:ident, [ $($meta:expr),* ]) => {
        $crate::ast::types::RpcMethod::builder()
            .id($crate::ast::types::Ident::from(stringify!($method_name)))
            .request_type($crate::qualified_ident_from_path_string!($req_ty))
            .response_type($crate::qualified_ident_from_path_string!($resp_ty))
            .metadata(Some($crate::ast::types::Metadata::from(vec![ $($meta),* ])))
            .build()
    };
    (fn $method_name:ident($req_ty:ident) -> $resp_ty:ident) => {
        $crate::ast::types::RpcMethod::builder()
            .id($crate::ast::types::Ident::from(stringify!($method_name)))
            .request_type($crate::qualified_ident_from_path_string!($req_ty))
            .response_type($crate::qualified_ident_from_path_string!($resp_ty))
            .build()
    };
}

#[macro_export]
macro_rules! rpc {
    ($name:ident, $($doc:literal,)? [ $($methods:expr),+ ]) => {
        $crate::ast::types::Rpc::builder()
            .id($crate::ast::types::Ident::from(stringify!($name)))
            .doc(vec![ $($doc,)? ].into())
            .methods(vec![ $($methods),+ ]).build()
    };
}

#[macro_export]
macro_rules! schema {
    {
        include {
            $($include:expr),+
        },
        $($body:expr),+
    } => {
        $crate::ast::types::Schema::builder()
        .includes(vec![ $($crate::ast::types::Include::builder()
                .path(std::path::Path::new($include))
                .stem(std::path::Path::new($include).file_stem().unwrap().to_str().unwrap())
                .build()),+ ])
            .elements(vec![ $($crate::ast::types::Element::from($body)),+ ]).build()
    };
    {
        include {
            $($include:expr),+
        }
    } => {
        $crate::ast::types::Schema::builder()
            .includes(vec![ $($crate::ast::types::Include::builder()
                    .path(std::path::Path::new($include))
                    .stem(std::path::Path::new($include).file_stem().unwrap().to_str().unwrap())
                    .build()),+ ]).build()
    };
    {
        $($body:expr),+
    } => {
        $crate::ast::types::Schema::builder()
            .elements(vec![ $($crate::ast::types::Element::from($body)),+ ]).build()
    };
    {} => {
        $crate::ast::types::Schema::builder().build()
    }
}

#[macro_export]
macro_rules! enum_ {
    ($name:ident, $base_ty:ident, [ $($value:expr),+ ]$(, $doc:literal)?) => {
        $crate::ast::types::Enum::builder()
            .id($crate::ast::types::Ident::from(stringify!($name)))
            .base_type($crate::ast::types::Type::$base_ty)
            .values(vec![ $($value),+ ])
            .doc(vec![ $($doc,)? ].into())
            .build()
    };
}

#[macro_export]
macro_rules! e_item {
    ($key:ident = $value:expr$(, $doc:literal)?) => {
        $crate::ast::types::EnumVal::builder()
            .id($crate::ast::types::Ident::from(stringify!($key)))
            .value(Some(($value).into()))
            .doc(vec![ $($doc,)? ].into())
            .build()
    };
    ($key:ident$(, $doc:literal)?) => {
        $crate::ast::types::EnumVal::builder()
            .id($crate::ast::types::Ident::from(stringify!($key)))
            .doc(vec![ $($doc,)? ].into())
            .build()
    };
}

#[macro_export]
macro_rules! union {
    ($name:ident, [ $($value:expr),+ ]$(, $doc:literal)?) => {
        $crate::ast::types::Union::builder()
            .id($crate::ast::types::Ident::from(stringify!($name)))
            .values(vec![ $($value),+ ])
            .doc(vec![ $($doc,)? ].into())
            .build()
    };
}

#[macro_export]
macro_rules! root_type {
    ($name:ident$(,$doc:literal)?) => {
        $crate::ast::types::Root::builder()
            .typename($crate::ast::types::Ident::from(stringify!($name)))
            .doc(vec![ $($($doc,)*)? ].into())
            .build()
    }
}

#[macro_export]
macro_rules! file_ext {
    ($ext:literal$(,[ $($doc:literal),+ ])?) => {
        $crate::ast::types::FileExtension::builder()
            .ext($ext)
            .doc(vec![ $($($doc,)*)? ].into())
            .build()
    }
}

#[macro_export]
macro_rules! file_id {
    ($ext:literal$(,[ $($doc:literal),+ ])?) => {
        $crate::ast::types::FileIdentifier::builder()
            .id([($ext)[0usize], ($ext)[1usize], ($ext)[2usize], ($ext)[3usize]])
            .doc(vec![ $($($doc,)*)? ].into())
            .build()
    }
}

#[macro_export]
macro_rules! attr {
    ($attr:ident$(,[ $($doc:literal),+ ])?) => {
        $crate::ast::types::Attribute::builder()
            .attr(stringify!($attr).into())
            .doc(vec![$($($doc,)+)?].into())
            .build()
    }
}
