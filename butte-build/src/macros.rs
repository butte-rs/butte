/// Include generated flatbuffer code.
///
/// # Examples
///
/// ```ignore
/// # // This doesn't compile, because OUT_DIR isn't set
/// mod greeter {
///     butte_build::include_fbs!("greeter");
/// }
/// ```
#[macro_export]
macro_rules! include_fbs {
    ($package:tt) => {
        include!(concat!(env!("OUT_DIR"), "/", $package, ".rs"));
    };
}
