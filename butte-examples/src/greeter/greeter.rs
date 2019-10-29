pub mod greeter {
    butte::include_fbs!("greeter");
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Hello, butte!");
    Ok(())
}
