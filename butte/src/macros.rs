/// Include generated flatbuffer code.
///
/// # Examples
///
/// ```compile_fail
/// # // This doesn't compile, because OUT_DIR isn't set
/// mod greeter {
///     butte::include_fbs!("greeter");
/// }
/// ```
#[macro_export]
macro_rules! include_fbs {
    ($package:tt) => {
        include!(concat!(env!("OUT_DIR"), "/", $package, ".rs"));
    };
}
