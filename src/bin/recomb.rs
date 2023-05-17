use clap::Parser;
use recomb::{interpreter, parser, Result};
use std::{
    eprintln,
    fs,
    io::{self, Read},
    path::PathBuf,
    process::exit,
    unreachable,
};

#[derive(Debug, Clone, Parser)]
struct Config {
    #[arg(short = 'l', long = "syntax-nesting-limit")]
    syntax_nesting_limit: Option<u32>,
    #[arg(short = 's', long = "step-limit")]
    step_limit: Option<u32>,
    #[arg(short = 'p', long = "program", group = "program")]
    program_arg: Option<String>,
    #[arg(short = 'f', long = "program-file", group = "program")]
    program_file: Option<PathBuf>,
    #[arg(short = 'i', long = "input", required_unless_present("program"))]
    input: Option<PathBuf>,
}

fn read_stdin() -> Result<String> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    Ok(input)
}

fn try_main() -> Result<()> {
    let config = Config::parse();
    let program = match (config.program_arg, config.program_file) {
        (Some(program), None) => program,
        (None, Some(path)) => fs::read_to_string(path)?,
        (None, None) => read_stdin()?,
        (Some(_), Some(_)) => unreachable!(),
    };
    let (prog_name, input) = match config.input {
        Some(path) => {
            (path.to_string_lossy().into_owned(), fs::read_to_string(path)?)
        },
        None => (String::from("<stdin>"), read_stdin()?),
    };
    let mut parse_config = parser::Config::new();
    if let Some(limit) = config.syntax_nesting_limit {
        parse_config.limit_stack(limit);
    }
    let mut interpreter_config = interpreter::Config::new();
    if let Some(limit) = config.step_limit {
        interpreter_config.limit_steps(limit);
    }
    let (output, flag) = recomb::eval(
        parse_config, interpreter_config, &prog_name, &program, &input,
    )?;
    if flag {
        println!("{}", output);
    } else {
        println!("{}", input);
    }
    Ok(())
}

fn main() {
    if let Err(error) = try_main() {
        eprintln!("{}", error);
        exit(1);
    }
}
