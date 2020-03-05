use anyhow::{anyhow, Result};
use butte as fb;

pub mod greeter {
    butte_build::include_fbs!("greeter");
}

fn main() -> Result<()> {
    use greeter::foo::bar::{
        butte_gen::EitherHelloType, EitherHello, EitherHelloRequest, EitherHelloRequestArgs,
        HelloReply, HelloReplyArgs, HelloRequest, HelloRequestArgs,
    };
    let mut builder = fb::FlatBufferBuilder::new();
    let name = builder.create_string("John");
    let args = HelloRequestArgs { name };
    let req = HelloRequest::create(&mut builder, &args);
    builder.finish_minimal(req);
    let raw_bytes = builder.finished_data();
    let root = HelloRequest::get_root(raw_bytes)?;
    let dname = root.name()?;
    let expected = "John";
    if dname != expected {
        return Err(anyhow!("Expected {:?}, got {:?}", expected, dname));
    }

    let mut builder = fb::FlatBufferBuilder::new();
    let name = builder.create_string("Jane");
    let message = builder.create_string("Wassup Jane");

    let req = HelloRequest::create(&mut builder, &HelloRequestArgs { name });
    let reply = HelloReply::create(
        &mut builder,
        &HelloReplyArgs {
            message: Some(message),
        },
    );

    let args = EitherHelloRequestArgs {
        something_type: EitherHelloType::HelloRequest,
        something: Some(req.as_union_value()),
        something_required_type: EitherHelloType::HelloReply,
        something_required: reply.as_union_value(),
    };

    let req = EitherHelloRequest::create(&mut builder, &args);

    builder.finish_minimal(req);
    let raw_bytes = builder.finished_data();
    let root = EitherHelloRequest::get_root(raw_bytes)?;

    let req_union = root.something()?;
    match req_union {
        Some(EitherHello::HelloRequest(req)) => {
            let dname = req.name()?;
            let expected = "Jane";
            if dname != expected {
                return Err(anyhow!("Expected {:?}, got {:?}", expected, dname));
            }
        }
        ty => panic!("expected hello request variant, got {:?}", ty),
    };

    let req_union = root.something_required()?;
    match req_union {
        EitherHello::HelloReply(reply) => {
            let dmessage = reply.message()?;
            let expected = Some("Wassup Jane");
            if dmessage != expected {
                return Err(anyhow!("Expected {:?}, got {:?}", expected, dmessage));
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
    let name = builder.create_string("Joe");
    let args = FooBarArgs {
        name: Some(name),
        my_foo: Foo::B,
    };
    let req = FooBar::create(&mut builder, &args);
    builder.finish_minimal(req);
    let raw_bytes = builder.finished_data();
    let root = FooBar::get_root(raw_bytes)?;
    let dfoo = root.my_foo()?;
    let expected = Some(Foo::B);
    if dfoo != expected {
        return Err(anyhow!("Expected {:?}, got {:?}", expected, dfoo));
    }

    Ok(())
}
