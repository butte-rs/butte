use anyhow::{anyhow, Result};
use butte as fb;

pub mod greeter {
    butte_build::include_fbs!("greeter");
}

fn main() -> Result<()> {
    use greeter::foo::bar::{HelloRequest, HelloRequestArgs};
    let mut builder = fb::FlatBufferBuilder::new();
    let raw_name = "A Name";
    let name = builder.create_string(raw_name);
    let args = HelloRequestArgs { name };
    let req = HelloRequest::create(&mut builder, &args);
    builder.finish_minimal(req);
    let raw_bytes = builder.finished_data();
    let root = fb::get_root::<HelloRequest>(raw_bytes)?;
    let dname = root.name()?;
    let expected = Some(raw_name);
    if dname != expected {
        Err(anyhow!("Expected {:?}, got {:?}", expected, dname))
    } else {
        Ok(())
    }
}
