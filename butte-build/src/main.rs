/// Compile flatbuffers files from the command line.
use butte_build::compile_fbs_generic;

use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "butte",
    about = "Generate Rust code from a flatbuffer schema file."
)]
struct Opt {
    /// Input file, stdin if not present
    #[structopt(parse(from_os_str))]
    input: Option<std::path::PathBuf>,

    /// Output file, stdout if not present
    #[structopt(parse(from_os_str))]
    output: Option<std::path::PathBuf>,

    /// Format generated code using `rustfmt`.
    #[structopt(short, long)]
    ugly: bool,
}

fn main() -> anyhow::Result<()> {
    let opt = Opt::from_args();

    let input: Box<dyn std::io::Read> = if let Some(input) = opt.input {
        Box::new(std::fs::File::open(input)?)
    } else {
        Box::new(std::io::stdin())
    };

    let output: Box<dyn std::io::Write> = if let Some(output) = opt.output {
        Box::new(std::fs::File::create(output)?)
    } else {
        Box::new(std::io::stdout())
    };

    compile_fbs_generic(opt.ugly, input, output)
}
