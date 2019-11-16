#[macro_export]
macro_rules! scalar {
    ($expr:expr) => {
        $crate::types::Scalar::from($expr)
    };
}

#[macro_export]
macro_rules! single_value {
    ($value:tt) => {
        $crate::types::Single::from($value)
    };
}

#[cfg(test)]
mod single_value_tests {
    #[test]
    fn test_single_value() {
        use crate::types::*;

        let val = single_value!(1);
        assert_eq!(val, Single::Scalar(Scalar::Integer(1)));

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
macro_rules! namespace {
    ($path:path) => {
        $crate::types::Namespace::builder()
            .ident($crate::dotted_ident_from_path_string!($path))
            .build()
    };
}

#[macro_export]
macro_rules! object {
    ({ $($key:ident => $value:tt),* }) => {
        $crate::types::Object::from(
            vec![ $(($crate::types::Ident::from(stringify!($key)), $crate::value!($value))),* ]
        )
    };
}

#[macro_export]
macro_rules! value {
    ([ $($element:tt),* ]) => {
        $crate::types::Value::from(vec![ $($crate::value!($element)),* ])
    };
    ({ $($key:ident => $value:tt),* }) => {
        $crate::types::Value::from(
            $crate::object!({ $($key => $value),* })
        )
    };
    ($value:tt) => {
        $crate::types::Value::from($crate::types::Single::from($value))
    };
}

#[cfg(test)]
mod value_macro_tests {
    #[test]
    fn test_value_macro_simple() {
        use crate::types::*;

        let result = value!("a");
        let expected = Value::Single(Single::String("a"));
        assert_eq!(result, expected);

        let result = value!(1);
        let expected = Value::Single(Single::Scalar(Scalar::Integer(1)));
        assert_eq!(result, expected);
    }

    #[test]
    fn test_value_macro_list() {
        use crate::types::*;

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
        use crate::types::*;

        let result = value!({ a => 1, b => "c" });
        let expected = Value::from(vec![
            (
                Ident::from("a"),
                Value::Single(Single::Scalar(Scalar::Integer(1))),
            ),
            (Ident::from("b"), Value::Single(Single::String("c"))),
        ]);
        assert_eq!(result, expected);
    }
}

#[macro_export]
macro_rules! element {
    ($elem:expr) => {
        $crate::types::Element::from($elem)
    };
}

#[macro_export]
macro_rules! field {
    ($name:ident, $ty:ident = $default:expr) => {
        $crate::types::Field::builder()
            .id($crate::types::Ident::from(stringify!($name)))
            .ty($crate::types::Type::$ty)
            .scalar(Some($crate::scalar!($default)))
            .build()
    };
    ($name:ident, [ $ty:ident ]) => {
        $crate::types::Field::builder()
            .id($crate::types::Ident::from(stringify!($name)))
            .ty($crate::types::Type::Array(Box::new(
                $crate::types::Type::$ty,
            )))
            .build()
    };
    ($name:ident, $ty:ident) => {
        $crate::types::Field::builder()
            .id($crate::types::Ident::from(stringify!($name)))
            .ty($crate::types::Type::$ty)
            .build()
    };
    ($name:ident, [ $ty:path ]) => {
        $crate::types::Field::builder()
            .id($crate::types::Ident::from(stringify!($name)))
            .ty($crate::types::Type::Array(Box::new(
                $crate::types::Type::Ident($crate::dotted_ident_from_path_string!($ty)),
            )))
            .build()
    };
    ($name:ident, $ty:path) => {
        $crate::types::Field::builder()
            .id($crate::types::Ident::from(stringify!($name)))
            .ty($crate::types::Type::Ident(
                $crate::dotted_ident_from_path_string!($ty),
            ))
            .build()
    };
}

#[macro_export]
macro_rules! dotted_ident_from_path_string {
    ($expr:expr) => {
        $crate::types::DottedIdent::from(
            stringify!($expr)
                .split("::")
                .map(Ident::from)
                .collect::<Vec<_>>(),
        )
    };
}

#[macro_export]
macro_rules! table {
    ($name:ident, $doc:expr, [ $($field:expr),* ]) => {
        $crate::types::Table::builder()
            .doc($doc)
            .id($crate::types::Ident::from(stringify!($name)))
            .fields(vec![ $($field),* ]).build()
    };
    ($name:ident, [ $($field:expr),* ]) => {
        $crate::table!($name, vec![], [ $($field),* ])
    };
}

#[macro_export]
macro_rules! comment {
    () => {
        $crate::types::Comment::from(vec![])
    };
    ($text:expr) => {
        $crate::types::Comment::from($text.split_terminator("\n").collect::<Vec<_>>())
    };
}

#[macro_export]
macro_rules! meta {
    ($key:ident, $value:expr) => {
        (
            $crate::types::Ident::from(stringify!($key)),
            Some($crate::single_value!($value)),
        )
    };
    ($key:ident) => {
        ($crate::types::Ident::from(stringify!($key)), None)
    };
}

#[macro_export]
macro_rules! method {
    (fn $method_name:ident($req_ty:ident) -> $resp_ty:ident, [ $($meta:expr),* ]) => {
        $crate::types::RpcMethod::builder()
            .id($crate::types::Ident::from(stringify!($method_name)))
            .request_type($crate::dotted_ident_from_path_string!($req_ty))
            .response_type($crate::dotted_ident_from_path_string!($resp_ty))
            .metadata(Some($crate::types::Metadata::from(vec![ $($meta),* ])))
            .build()
    };
    (fn $method_name:ident($req_ty:ident) -> $resp_ty:ident) => {
        $crate::types::RpcMethod::builder()
            .id($crate::types::Ident::from(stringify!($method_name)))
            .request_type($crate::dotted_ident_from_path_string!($req_ty))
            .response_type($crate::dotted_ident_from_path_string!($resp_ty))
            .build()
    };
}

#[macro_export]
macro_rules! rpc {
    ($name:ident, $doc:expr, [ $($methods:expr),+ ]) => {
        $crate::types::Rpc::builder()
            .id($crate::types::Ident::from(stringify!($name)))
            .doc($doc)
            .methods(vec![ $($methods),+ ]).build()
    };
    ($name:ident, [ $($methods:expr),+ ]) => {
        $crate::types::Rpc::builder()
            .id($crate::types::Ident::from(stringify!($name)))
            .methods(vec![ $($methods),+ ]).build()
    }
}

#[macro_export]
macro_rules! schema {
    {
        include {
            $($include:expr),+
        },
        $($body:expr),+
    } => {
        $crate::types::Schema::builder()
        .includes(vec![ $($crate::types::Include::builder()
                .path(std::path::Path::new($include))
                .stem(std::path::Path::new($include).file_stem().unwrap().to_str().unwrap())
                .build()),+ ])
            .elements(vec![ $($crate::types::Element::from($body)),+ ]).build()
    };
    {
        include {
            $($include:expr),+
        }
    } => {
        $crate::types::Schema::builder()
            .includes(vec![ $($crate::types::Include::builder()
                    .path(std::path::Path::new($include))
                    .stem(std::path::Path::new($include).file_stem().unwrap().to_str().unwrap())
                    .build()),+ ]).build()
    };
    {
        $($body:expr),+
    } => {
        $crate::types::Schema::builder()
            .elements(vec![ $($crate::types::Element::from($body)),+ ]).build()
    };
    {} => {
        $crate::types::Schema::builder().build()
    }
}

#[macro_export]
macro_rules! enum_ {
    ($name:ident, $base_ty:ident, [ $($value:expr),+ ]) => {
        $crate::types::Enum::builder()
            .id($crate::types::Ident::from(stringify!($name)))
            .base_type($crate::types::Type::$base_ty)
            .values(vec![ $($value),+ ])
            .build()
    };
}

#[macro_export]
macro_rules! e_item {
    ($key:ident = $value:expr) => {
        $crate::types::EnumVal::builder()
            .id($crate::types::Ident::from(stringify!($key)))
            .value(Some($value))
            .build()
    };
    ($key:ident) => {
        $crate::types::EnumVal::builder()
            .id($crate::types::Ident::from(stringify!($key)))
            .build()
    };
}

#[macro_export]
macro_rules! union {
    ($name:ident, [ $($value:expr),+ ]) => {
        $crate::types::Union::builder()
            .id($crate::types::Ident::from(stringify!($name)))
            .values(vec![ $($value),+ ])
            .build()
    };
}
