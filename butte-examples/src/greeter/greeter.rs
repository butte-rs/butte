use anyhow::{anyhow, Result};
use butte as fb;

pub mod greeter {
    butte_build::include_fbs!("greeter");
}

fn main() -> Result<()> {
    use greeter::foo::bar::{
        EitherHello, EitherHelloRequest, EitherHelloRequestArgs, EitherHelloType, HelloRequest,
        HelloRequestArgs,
    };
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
        return Err(anyhow!("Expected {:?}, got {:?}", expected, dname));
    }

    let mut builder = fb::FlatBufferBuilder::new();
    let raw_name = "Another Name";
    let name = builder.create_string(raw_name);
    let args = HelloRequestArgs { name };
    let req = HelloRequest::create(&mut builder, &args);

    let args = EitherHelloRequestArgs {
        something_type: EitherHelloType::HelloRequest,
        something: req.as_union_value(),
    };

    let req = EitherHelloRequest::create(&mut builder, &args);

    builder.finish_minimal(req);
    let raw_bytes = builder.finished_data();
    let root = fb::get_root::<EitherHelloRequest>(raw_bytes)?;

    let req_union = root.something()?;
    match req_union {
        Some(EitherHello::HelloRequest(req)) => {
            let dname = req.name()?;
            let expected = Some(raw_name);
            if dname != expected {
                return Err(anyhow!("Expected {:?}, got {:?}", expected, dname));
            }
        }
        ty => panic!("expected hello request variant, got {:?}", ty),
    };

    let req_type = root.something_type()?;
    let expected = Some(EitherHelloType::HelloRequest);
    if req_type != expected {
        return Err(anyhow!("Expected {:?}, got {:?}", expected, dname));
    }

    use greeter::baz::buzz::{Foo, FooBar, FooBarArgs};
    let mut builder = fb::FlatBufferBuilder::new();
    let raw_name = "A Name";
    let name = builder.create_string(raw_name);
    let args = FooBarArgs {
        name,
        my_foo: Foo::B,
    };
    let req = FooBar::create(&mut builder, &args);
    builder.finish_minimal(req);
    let raw_bytes = builder.finished_data();
    let root = fb::get_root::<FooBar>(raw_bytes)?;
    let dfoo = root.my_foo()?;
    let expected = Some(Foo::B);
    if dfoo != expected {
        return Err(anyhow!("Expected {:?}, got {:?}", expected, dfoo));
    }

    Ok(())
}
