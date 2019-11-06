use anyhow::Result;

fn main() -> Result<()> {
    butte::compile_fbs("fbs/greeter/greeter.fbs")
}
