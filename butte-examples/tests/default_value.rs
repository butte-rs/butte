pub mod tests {
    butte_build::include_fbs!("default_value");
}

use anyhow::Result;
use butte as fb;
use tests::default_value;

#[test]
fn test_default_value() -> Result<()> {
    let mut builder = fb::FlatBufferBuilder::new();

    let input = default_value::Values::create(
        &mut builder,
        &default_value::ValuesArgs {
            ..Default::default()
        },
    );

    // Serialize.
    builder.finish_minimal(input);
    let raw_bytes = builder.finished_data();

    // Deserialize and verify the result.
    let output = default_value::Values::get_root(raw_bytes)?;

    // Test if the implicit and explicit default value of zero match
    // and are non-equal to the explicit non-zero default value.
    assert_eq!(output.bool_implicit()?, output.bool_explicit()?);
    assert_ne!(output.bool_implicit()?, output.bool_other()?);
    assert_eq!(output.float_implicit()?, output.float_explicit()?);
    assert_ne!(output.float_implicit()?, output.float_other()?);
    assert_eq!(output.int_implicit()?, output.int_explicit()?);
    assert_ne!(output.int_implicit()?, output.int_other()?);
    assert_eq!(output.long_implicit()?, output.long_explicit()?);
    assert_ne!(output.long_implicit()?, output.long_other()?);

    // Strings don't have a default value.
    assert_eq!(output.text()?, None);

    Ok(())
}
