use anyhow::Result;

fn main() -> Result<()> {
    butte_build::compile_fbs("fbs/greeter/greeter.fbs")
}
